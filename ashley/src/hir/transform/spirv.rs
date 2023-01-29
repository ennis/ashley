use crate::{
    hir,
    hir::{
        types::{ImageSampling, ImageType, ScalarType, StructType},
        Block, Constant, ConstantData, ExtInstSet, Function, GlobalVariable, IdRef, Local, Module, Operand,
        TerminatingInstruction, Type, TypeData, Value, ValueOrConstant,
    },
    utils::IdMap,
};
use rspirv::{
    binary::Assemble,
    dr::InsertPoint,
    spirv,
    spirv::{AddressingModel, Capability, LinkageType, MemoryModel, Word},
};
use spirv::StorageClass;
use std::collections::HashMap;
use tracing::error;

type ExtInstSetMap = IdMap<ExtInstSet, Word>;
type TypeMap = IdMap<Type, Word>;
type ConstantMap = IdMap<Constant, Word>;
type FunctionMap = IdMap<Function, Word>;
type GlobalMap = IdMap<GlobalVariable, Word>;

struct Ctxt<'a> {
    module: &'a Module,
    builder: &'a mut rspirv::dr::Builder,
    type_map: TypeMap,
    ext_inst_sets: ExtInstSetMap,
    constant_map: ConstantMap,
    function_map: FunctionMap,
    global_map: GlobalMap,
}

impl<'a> Ctxt<'a> {
    fn emit_scalar_type(&mut self, scalar_type: ScalarType) -> Word {
        match scalar_type {
            ScalarType::Int => self.builder.type_int(32, 1),
            ScalarType::UnsignedInt => self.builder.type_int(32, 0),
            ScalarType::Float => self.builder.type_float(32),
            ScalarType::Double => self.builder.type_float(64),
            ScalarType::Bool => self.builder.type_bool(),
        }
    }

    fn emit_image_type(&mut self, image_type: &ImageType) -> Word {
        let sampled = match image_type.sampled {
            ImageSampling::Unknown => 0,
            ImageSampling::Sampled => 1,
            ImageSampling::ReadWrite => 2,
        };
        let depth = match image_type.depth {
            Some(true) => 1,
            Some(false) => 0,
            None => 2,
        };
        let ms = if image_type.ms { 1 } else { 0 };
        let arrayed = if image_type.arrayed { 1 } else { 0 };
        let sampled_type = self.emit_scalar_type(image_type.sampled_type);
        self.builder.type_image(
            sampled_type,
            image_type.dim,
            depth,
            arrayed,
            ms,
            sampled,
            image_type.image_format,
            image_type.access,
        )
    }

    fn emit_constant_recursive(&mut self, constant: Constant) -> Word {
        if let Some(word) = self.constant_map.get(constant) {
            return *word;
        }

        let cdata = &self.module.constants[constant];
        let ssa_id = match *cdata {
            ConstantData::F32(v) => {
                let ty = self.builder.type_float(32);
                self.builder.constant_f32(ty, v.0)
            }
            ConstantData::I32(v) => {
                let ty = self.builder.type_int(32, 1);
                self.builder.constant_u32(ty, v as u32)
            }
            ConstantData::U32(v) => {
                let ty = self.builder.type_int(32, 0);
                self.builder.constant_u32(ty, v)
            }
            ConstantData::F64(v) => {
                let ty = self.builder.type_float(64);
                self.builder.constant_f64(ty, v.0)
            }
            ConstantData::I64(v) => {
                let ty = self.builder.type_int(64, 1);
                self.builder.constant_u64(ty, v as u64)
            }
            ConstantData::U64(v) => {
                let ty = self.builder.type_int(64, 0);
                self.builder.constant_u64(ty, v)
            }
            ConstantData::Bool(v) => {
                let ty = self.builder.type_bool();
                if v {
                    self.builder.constant_true(ty)
                } else {
                    self.builder.constant_false(ty)
                }
            }
            ConstantData::Composite { ty, ref constituents } => {
                let ty = self.emit_type_recursive(ty);
                for c in constituents {
                    self.emit_constant_recursive(*c);
                }
                self.builder
                    .constant_composite(ty, constituents.iter().map(|c| self.constant_map[*c]))
            }
        };

        self.constant_map.insert(constant, ssa_id);
        ssa_id
    }

    fn emit_type_recursive(&mut self, ty: Type) -> Word {
        if let Some(word) = self.type_map.get(ty) {
            return *word;
        }

        let tydata = &self.module.types[ty];
        let ssa_id = match *tydata {
            TypeData::Unit => self.builder.type_void(),
            TypeData::Scalar(scalar_type) => self.emit_scalar_type(scalar_type),
            TypeData::Vector(component_type, len) => {
                let component_type = self.emit_scalar_type(component_type);
                self.builder.type_vector(component_type, len as u32)
            }
            TypeData::Matrix {
                component_type,
                columns,
                rows,
            } => {
                let component_type = self.emit_scalar_type(component_type);
                let column_type = self.builder.type_vector(component_type, rows as u32);
                self.builder.type_matrix(column_type, columns as u32)
            }
            TypeData::Array(elem_ty, len) => {
                let ty_u32 = self.builder.type_int(32, 0);
                let len = self.builder.constant_u32(ty_u32, len);
                let elem_ty = self.emit_type_recursive(elem_ty);
                self.builder.type_array(elem_ty, len)
            }
            TypeData::RuntimeArray(elem_ty) => {
                let elem_ty = self.emit_type_recursive(elem_ty);
                self.builder.type_runtime_array(elem_ty)
            }
            TypeData::Struct(ref struct_ty) => {
                for f in struct_ty.fields.iter() {
                    self.emit_type_recursive(f.ty);
                }
                let id = self
                    .builder
                    .type_struct(struct_ty.fields.iter().map(|f| self.type_map[f.ty]));
                if let Some(ref name) = struct_ty.name {
                    self.builder.name(id, name.to_string());
                }
                id
            }
            TypeData::SampledImage(img_ty) => {
                let image_type = self.emit_image_type(&img_ty);
                self.builder.type_sampled_image(image_type)
            }
            TypeData::Image(img_ty) => self.emit_image_type(&img_ty),
            TypeData::Pointer {
                pointee_type,
                storage_class,
            } => {
                let pointee_type = self.emit_type_recursive(pointee_type);
                self.builder.type_pointer(None, storage_class, pointee_type)
            }
            TypeData::Function(ref function_type) => {
                let return_type = self.emit_type_recursive(function_type.return_type);
                for arg_ty in function_type.arg_types.iter() {
                    self.emit_type_recursive(*arg_ty);
                }
                self.builder.type_function(
                    return_type,
                    function_type.arg_types.iter().map(|arg| self.type_map[*arg]),
                )
            }
            TypeData::Sampler => self.builder.type_sampler(),
            TypeData::String => {
                // Strings should be removed by then
                // TODO remove strings altogether
                error!("unrepresentable type encountered during SPIR-V translation: string");
                self.builder.type_void()
            }
            TypeData::Unknown => {
                error!("unknown type encountered during SPIR-V translation");
                self.builder.type_void()
            }
            TypeData::SamplerShadow => {
                // TODO
                self.builder.type_sampler()
            }
        };

        self.type_map.insert(ty, ssa_id);
        ssa_id
    }

    fn emit_linkage_decoration(&mut self, id: Word, name: &str, linkage: LinkageType) {
        self.builder.decorate(
            id,
            spirv::Decoration::LinkageAttributes,
            [
                rspirv::dr::Operand::LiteralString(name.to_string()),
                rspirv::dr::Operand::LinkageType(linkage),
            ],
        );
    }

    fn emit_function(&mut self, function: Function) -> Word {
        if let Some(word) = self.function_map.get(function) {
            return *word;
        }

        let fdata = &self.module.functions[function];
        let function_type_id = self.emit_type_recursive(fdata.function_type);
        let function_type = match self.module.types[fdata.function_type] {
            TypeData::Function(ref fty) => fty,
            _ => panic!("invalid function type"),
        };
        let result_type = self.emit_type_recursive(function_type.return_type);
        let function_id = self
            .builder
            .begin_function(result_type, None, fdata.function_control, function_type_id)
            .unwrap();
        self.builder.name(function_id, &fdata.name);

        let mut labels = IdMap::with_capacity(fdata.blocks.len());
        let mut values = IdMap::with_capacity(fdata.values.len());
        let mut locals = IdMap::with_capacity(fdata.locals.len());

        for (arg, ty) in fdata.arguments.iter().zip(function_type.arg_types.iter()) {
            let arg_ty = self.emit_type_recursive(*ty);
            values.insert(*arg, self.builder.function_parameter(arg_ty).unwrap());
        }

        for (b, _) in fdata.blocks.iter() {
            labels.insert(b, self.builder.begin_block(None).unwrap());
        }

        for (local, ldata) in fdata.locals.iter() {
            let ty = self.emit_type_recursive(ldata.ty);
            let ptr_ty = self.builder.type_pointer(None, StorageClass::Function, ty);
            let id = self.builder.variable(ptr_ty, None, StorageClass::Function, None);
            self.builder.name(id, &ldata.name);
            locals.insert(local, id);
        }

        for (ib, (_, bdata)) in fdata.blocks.iter().enumerate() {
            self.builder.select_block(Some(ib)).unwrap();
            for inst in bdata.instructions.iter() {
                let result_id;
                let result_type;
                if let Some(r) = inst.result {
                    let id = self.builder.id();
                    values.insert(r, id);
                    result_id = Some(id);
                    result_type = Some(self.emit_type_recursive(fdata.values[r].ty));
                } else {
                    result_type = None;
                    result_id = None;
                }

                let mut operands = Vec::with_capacity(inst.operands.len());
                for op in inst.operands.iter() {
                    operands.push(self.operand_to_rspirv(op, &values, &locals, &labels));
                }
                let spvinst = rspirv::dr::Instruction::new(inst.opcode, result_type, result_id, operands);
                self.builder.insert_into_block(InsertPoint::End, spvinst).unwrap();
            }
            match bdata.terminator {
                None => {
                    panic!("unterminated block")
                }
                Some(ref terminator) => match terminator {
                    TerminatingInstruction::Branch(target) => {
                        self.builder.branch(labels[*target]).unwrap();
                    }
                    TerminatingInstruction::BranchConditional {
                        condition,
                        true_block,
                        false_block,
                    } => {
                        let condition_id = match condition {
                            ValueOrConstant::Value(v) => values[*v],
                            ValueOrConstant::Constant(c) => self.emit_constant_recursive(*c),
                        };
                        self.builder
                            .branch_conditional(condition_id, labels[*true_block], labels[*false_block], [])
                            .unwrap();
                    }
                    TerminatingInstruction::Switch {
                        selector,
                        target,
                        default,
                    } => {
                        todo!("switch")
                    }
                    TerminatingInstruction::Return => {
                        self.builder.ret().unwrap();
                    }
                    TerminatingInstruction::ReturnValue(v) => {
                        let id = match v {
                            IdRef::Value(v) => values[*v],
                            IdRef::Constant(c) => self.emit_constant_recursive(*c),
                            IdRef::Global(g) => self.emit_global(*g),
                            IdRef::Local(l) => locals[*l],
                            IdRef::Function(_) => panic!("invalid return value"),
                        };
                        self.builder.ret_value(id).unwrap();
                    }
                    TerminatingInstruction::Unreachable => {
                        self.builder.unreachable().unwrap();
                    }
                    TerminatingInstruction::TerminateInvocation => {
                        self.builder.terminate_invocation().unwrap();
                    }
                },
            }
        }

        self.builder.end_function().unwrap();
        if let Some(linkage) = fdata.linkage {
            self.emit_linkage_decoration(function_id, &fdata.name, linkage);
        }
        self.function_map.insert(function, function_id);
        function_id
    }

    fn emit_global(&mut self, g: GlobalVariable) -> Word {
        if let Some(word) = self.global_map.get(g) {
            return *word;
        }

        let gdata = &self.module.globals[g];
        let ty = self.emit_type_recursive(gdata.ty);
        let ptr_ty = self.builder.type_pointer(None, gdata.storage_class, ty);
        let id = self.builder.variable(ptr_ty, None, gdata.storage_class, None); // TODO initializers
        if let Some(linkage) = gdata.linkage {
            self.emit_linkage_decoration(id, &gdata.name, linkage);
        }
        self.builder.name(id, &gdata.name);
        self.global_map.insert(g, id);
        id
    }

    fn operand_to_rspirv(
        &mut self,
        op: &Operand,
        values: &IdMap<Value, Word>,
        locals: &IdMap<Local, Word>,
        labels: &IdMap<Block, Word>,
    ) -> rspirv::dr::Operand {
        match *op {
            Operand::LocalRef(l) => rspirv::dr::Operand::IdRef(locals[l]),
            Operand::ConstantRef(constant) => rspirv::dr::Operand::IdRef(self.emit_constant_recursive(constant)),
            Operand::LiteralExtInstInteger(i) => rspirv::dr::Operand::LiteralExtInstInteger(i),
            Operand::FunctionRef(function) => rspirv::dr::Operand::IdRef(self.emit_function(function)),
            Operand::ValueRef(value) => rspirv::dr::Operand::IdRef(values[value]),
            Operand::BlockRef(block) => rspirv::dr::Operand::IdRef(labels[block]),
            Operand::TypeRef(ty) => rspirv::dr::Operand::IdRef(self.emit_type_recursive(ty)),
            Operand::GlobalRef(global) => rspirv::dr::Operand::IdRef(self.emit_global(global)),
            Operand::FPFastMathMode(v) => rspirv::dr::Operand::FPFastMathMode(v),
            Operand::SelectionControl(v) => rspirv::dr::Operand::SelectionControl(v),
            Operand::LoopControl(v) => rspirv::dr::Operand::LoopControl(v),
            Operand::FunctionControl(v) => rspirv::dr::Operand::FunctionControl(v),
            Operand::MemorySemantics(v) => rspirv::dr::Operand::MemorySemantics(v),
            Operand::MemoryAccess(v) => rspirv::dr::Operand::MemoryAccess(v),
            Operand::KernelProfilingInfo(v) => rspirv::dr::Operand::KernelProfilingInfo(v),
            Operand::RayFlags(v) => rspirv::dr::Operand::RayFlags(v),
            Operand::FragmentShadingRate(v) => rspirv::dr::Operand::FragmentShadingRate(v),
            Operand::SourceLanguage(v) => rspirv::dr::Operand::SourceLanguage(v),
            Operand::ExecutionModel(v) => rspirv::dr::Operand::ExecutionModel(v),
            Operand::AddressingModel(v) => rspirv::dr::Operand::AddressingModel(v),
            Operand::MemoryModel(v) => rspirv::dr::Operand::MemoryModel(v),
            Operand::ExecutionMode(v) => rspirv::dr::Operand::ExecutionMode(v),
            Operand::StorageClass(v) => rspirv::dr::Operand::StorageClass(v),
            Operand::Dim(v) => rspirv::dr::Operand::Dim(v),
            Operand::SamplerAddressingMode(v) => rspirv::dr::Operand::SamplerAddressingMode(v),
            Operand::SamplerFilterMode(v) => rspirv::dr::Operand::SamplerFilterMode(v),
            Operand::ImageFormat(v) => rspirv::dr::Operand::ImageFormat(v),
            Operand::ImageChannelOrder(v) => rspirv::dr::Operand::ImageChannelOrder(v),
            Operand::ImageChannelDataType(v) => rspirv::dr::Operand::ImageChannelDataType(v),
            Operand::FPRoundingMode(v) => rspirv::dr::Operand::FPRoundingMode(v),
            Operand::LinkageType(v) => rspirv::dr::Operand::LinkageType(v),
            Operand::AccessQualifier(v) => rspirv::dr::Operand::AccessQualifier(v),
            Operand::FunctionParameterAttribute(v) => rspirv::dr::Operand::FunctionParameterAttribute(v),
            Operand::Decoration(v) => rspirv::dr::Operand::Decoration(v),
            Operand::BuiltIn(v) => rspirv::dr::Operand::BuiltIn(v),
            Operand::Scope(v) => rspirv::dr::Operand::Scope(v),
            Operand::GroupOperation(v) => rspirv::dr::Operand::GroupOperation(v),
            Operand::KernelEnqueueFlags(v) => rspirv::dr::Operand::KernelEnqueueFlags(v),
            Operand::Capability(v) => rspirv::dr::Operand::Capability(v),
            Operand::RayQueryIntersection(v) => rspirv::dr::Operand::RayQueryIntersection(v),
            Operand::RayQueryCommittedIntersectionType(v) => rspirv::dr::Operand::RayQueryCommittedIntersectionType(v),
            Operand::RayQueryCandidateIntersectionType(v) => rspirv::dr::Operand::RayQueryCandidateIntersectionType(v),
            Operand::LiteralInt32(v) => rspirv::dr::Operand::LiteralInt32(v),
            Operand::LiteralInt64(v) => rspirv::dr::Operand::LiteralInt64(v),
            Operand::LiteralFloat32(v) => rspirv::dr::Operand::LiteralFloat32(v.0),
            Operand::LiteralFloat64(v) => rspirv::dr::Operand::LiteralFloat64(v.0),
            Operand::LiteralSpecConstantOpInteger(v) => rspirv::dr::Operand::LiteralSpecConstantOpInteger(v),
            Operand::LiteralString(ref v) => rspirv::dr::Operand::LiteralString(v.clone()),
            Operand::ImageOperands(v) => rspirv::dr::Operand::ImageOperands(v),
            Operand::PackedVectorFormat(v) => rspirv::dr::Operand::LiteralInt32(v as u32),
            Operand::ExtInstSet(s) => rspirv::dr::Operand::LiteralExtInstInteger(self.ext_inst_sets[s]),
        }
    }
}

pub fn write_spirv(module: &Module) -> Vec<u32> {
    let mut builder = rspirv::dr::Builder::new();

    builder.set_version(1, 5);
    builder.capability(Capability::Linkage);
    builder.capability(Capability::Shader);

    let mut ext_inst_sets = IdMap::with_capacity(module.ext_inst_sets.len());
    for (id, name) in module.ext_inst_sets.iter() {
        ext_inst_sets.insert(id, builder.ext_inst_import(name));
    }
    builder.memory_model(AddressingModel::Logical, MemoryModel::GLSL450);

    let mut ctxt = Ctxt {
        module,
        builder: &mut builder,
        type_map: TypeMap::with_capacity(module.types.len()),
        ext_inst_sets,
        constant_map: ConstantMap::with_capacity(module.constants.len()),
        function_map: FunctionMap::with_capacity(module.functions.len()),
        global_map: GlobalMap::with_capacity(module.globals.len()),
    };

    for (g, _) in module.globals.iter() {
        ctxt.emit_global(g);
    }

    for (f, _) in module.functions.iter() {
        ctxt.emit_function(f);
    }

    builder.module().assemble()
}
