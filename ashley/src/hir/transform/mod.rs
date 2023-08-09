mod emit_spirv;
mod link;

use crate::{
    diagnostic::Diagnostics,
    hir,
    hir::{
        types::{Field, MemoryInterfaceLayout, StructType},
        Decoration, GlobalVariable, GlobalVariableData, InstructionData, Layout, Module, Operand, StructLayout, Type,
        TypeData, ValueData,
    },
    tast, utils,
};
use ::spirv::StorageClass;
use smallvec::smallvec;
use spirv::Op;

pub use self::{emit_spirv::write_spirv, link::link_module_pipeline};

#[derive(thiserror::Error, Debug)]
pub enum ShaderInterfaceTransformError {
    #[error("item not found")]
    NotFound,
    #[error("item already remapped")]
    AlreadyMapped,
    #[error("type mismatch")]
    TypeMismatch,
    #[error("incompatible binding")]
    IncompatibleBinding,
    #[error("no space left in push constant buffer")]
    PushConstantsFull,
    #[error("invalid layout")]
    InvalidLayout,
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Describes a field of an uniform buffer.
pub struct UniformBufferField {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
    /// Defines to which global variable the field maps to.
    pub var: Option<hir::GlobalVariable>,
}

enum InterfaceItemKind {
    UniformBuffer { fields: Vec<UniformBufferField> },
    // TODO the rest
}

struct InterfaceItem {
    set: usize,
    binding: usize,
    kind: InterfaceItemKind,
}

pub struct ShaderInterfaceTransform<'a, 'diag> {
    hir: &'a mut hir::Module,
    diag: &'a mut Diagnostics<'diag>,
    interface: Vec<InterfaceItem>,
}

fn find_interface_variables(hir: &hir::Module) -> Vec<hir::GlobalVariable> {
    let mut vars = vec![];
    for (g, gdata) in hir.globals.iter() {
        match gdata.storage_class {
            StorageClass::Input => {
                vars.push(g);
            }
            StorageClass::Output => {
                vars.push(g);
            }
            StorageClass::Uniform => {
                vars.push(g);
            }
            _ => {}
        }
    }
    vars
}

/// Rewrites uniform reads into uniform buffer reads
fn rewrite_uniform_access(
    hir: &mut hir::Module,
    var: hir::GlobalVariable,
    buffer_var: hir::GlobalVariable,
    field_index: i32,
) {
    let field_index = hir.const_i32(field_index);
    let const_zero = hir.const_i32(0);
    let ty = hir.globals[var].ty;
    let ptr_ty = hir.pointer_type(ty, StorageClass::Uniform);
    for (_, func) in hir.functions.iter_mut() {
        for (_, block) in func.blocks.iter_mut() {
            let num_instructions = block.instructions.len();
            let mut i = 0;
            while i < num_instructions {
                match block.instructions[i].opcode {
                    Op::AccessChain if block.instructions[i].operands[0] == Operand::GlobalRef(var) => {
                        trace!("rewrite OpAccessChain {} {:?}", func.name, block.instructions[i]);
                        block.instructions[i].operands[0] = Operand::GlobalRef(buffer_var);
                        block.instructions[i]
                            .operands
                            .insert(1, Operand::ConstantRef(field_index));
                        block.instructions[i]
                            .operands
                            .insert(1, Operand::ConstantRef(const_zero));
                    }
                    Op::Load if block.instructions[i].operands[0] == Operand::GlobalRef(var) => {
                        trace!("rewrite OpLoad {} {:?}", func.name, block.instructions[i]);
                        let result = func.values.alloc(ValueData::new(ptr_ty));
                        block.instructions.insert(
                            i,
                            InstructionData {
                                opcode: Op::AccessChain,
                                result: Some(result),
                                operands: smallvec![
                                    Operand::GlobalRef(buffer_var),
                                    Operand::ConstantRef(const_zero), // interface wrapper struct
                                    Operand::ConstantRef(field_index)
                                ],
                            },
                        );
                        i += 1;
                        block.instructions[i].operands[0] = Operand::ValueRef(result);
                    }
                    _ => {}
                }
                i += 1;
            }
        }
    }
}

impl<'a, 'diag> ShaderInterfaceTransform<'a, 'diag> {
    pub fn new(hir: &'a mut hir::Module, diag: &'a mut Diagnostics<'diag>) -> ShaderInterfaceTransform<'a, 'diag> {
        ShaderInterfaceTransform {
            hir,
            diag,
            interface: vec![],
        }
    }

    pub fn module(&mut self) -> &mut hir::Module {
        self.hir
    }

    /// Adds a uniform buffer to the interface, at the specified set and binding.
    ///
    /// This provides a value for every parameter with a name matching one of the fields.
    pub fn provide_uniform_buffer(
        &mut self,
        set: usize,
        binding: usize,
        fields: impl IntoIterator<Item = UniformBufferField>,
    ) {
        self.interface.push(InterfaceItem {
            set,
            binding,
            kind: InterfaceItemKind::UniformBuffer {
                fields: fields.into_iter().collect(),
            },
        })
    }

    pub fn provide_uniform_buffer_as_type<T: utils::MemoryLayout>(
        &mut self,
        set: u32,
        binding: u32,
    ) -> Result<(), ShaderInterfaceTransformError> {
        let ty = T::hir_type(self.hir);

        // wrapper type with a `Block` decoration
        let block_ty_data = TypeData::Struct(StructType {
            name: None,
            fields: vec![Field {
                ty,
                name: None,
                interpolation: None,
            }]
            .into(),
            layout: Some(StructLayout {
                offsets: vec![0],
                layouts: vec![],
            }),
            block: true,
        });
        let block_ty = self.hir.define_type(block_ty_data);

        // retrieve the StructType for T
        let tydata = &self.hir.types[ty];
        let struct_ty = match tydata {
            TypeData::Struct(st) => st,
            _ => panic!("T should be a struct type"),
        };

        // collect interface variables that match with a field in the uniform buffer
        let mut global_to_field: Vec<(GlobalVariable, i32)> = vec![];
        for (field_index, field) in struct_ty.fields.iter().enumerate() {
            if let Some(name) = &field.name {
                if let Some((global_id, global)) = self.hir.find_interface_variable(name) {
                    if field.ty != global.ty {
                        let field_ty_dbg = self.hir.debug_type(field.ty);
                        let global_ty_dbg = self.hir.debug_type(global.ty);
                        let field_name = field.name.as_deref().unwrap_or("<unnamed>");
                        let global_name = &global.name;
                        self.diag.error(format!("type mismatch: shader expects `{global_ty_dbg:?} {global_name}` but the application provides `{field_ty_dbg:?} {field_name}`"))
                            .primary_label_opt(global.source_location, "").emit();
                    } else {
                        global_to_field.push((global_id, field_index as i32));
                    }
                }
            }
        }

        // introduce a new uniform variable (pointer-typed) for the uniform buffer
        let decorations = vec![
            Decoration::Uniform,
            Decoration::DescriptorSet(set),
            Decoration::Binding(binding),
            Decoration::Block,
        ];
        let buffer_var = self.hir.define_global_variable(GlobalVariableData {
            name: "".to_string(), // TODO name
            ty: block_ty,
            storage_class: StorageClass::Uniform,
            source_location: None, // synthetic
            decorations,
            removed: false,
            linkage: None,
        });

        // replace accesses to any matched interface var with an access to a field of the buffer
        for (var, field_index) in global_to_field {
            rewrite_uniform_access(self.hir, var, buffer_var, field_index);
            self.hir.remove_global(var);
        }

        Ok(())
    }

    /*/// Adds a push constant buffer
    pub fn provide_push_constants(&mut self, desc: &BufferDesc) {
        // check that a push constant buffer hasn't already been provided
        todo!()
    }*/

    pub fn provide_input(&mut self, location: usize, ty: hir::Type) {
        todo!()
    }

    pub fn expect_output(&mut self, location: usize, ty: hir::Type) {
        todo!()
    }

    pub fn provide_texture(&mut self, set: usize, binding: usize, name: &str) {
        todo!()
    }

    /*/// Iterates over unbound uniform variables.
    pub fn uniforms(&self) -> impl Iterator<Item = (GlobalVariable, &'_ GlobalVariableData)> + '_ {
        self.unbound.iter().filter_map(|v| {
            let gdata = &self.hir.globals[*v];
            match gdata.storage_class {
                StorageClass::Uniform => match hir.type_data(gdata.ty) {
                    TypeData::Pointer {
                        storage_class,
                        pointee_type: _,
                    } => match storage_class {
                        StorageClass::Uniform => Some((g, gdata)),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            }
        })
    }*/

    pub fn finish(self) {}

    /*/// Iterates over images (textures & storage images)
    pub fn images(&self) -> impl Iterator<Item = (GlobalVariable, &'_ GlobalVariableData)> + '_ {

    }*/
}
