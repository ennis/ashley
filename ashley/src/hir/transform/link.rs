use crate::hir::{constant::ConstantData, Constant, Function, FunctionData, Module, Operand, Type, TypeData};
use std::collections::HashMap;

struct Merger<'a> {
    module: &'a mut Module,
    constant_map: HashMap<Handle<Constant>, Handle<Constant>>,
    type_map: HashMap<Handle<Type>, Handle<Type>>,
    function_map: HashMap<Handle<FunctionData>, Handle<FunctionData>>,
    global_var_map: HashMap<Handle<GlobalVariable>, Handle<GlobalVariable>>,
}

struct LinkInput<'a> {
    module: &'a Module,
    // Maps: ID in src module -> ID in dst module
    constant_map: Vec<Constant>,
    type_map: Vec<Type>,
    function_map: Vec<FunctionData>,
}

// Replaces all uses of a function, constant, type or global in a function body with another.
//fn replace_uses_in_module(module: &mut Module)

/// Replaces all uses of a function, constant, type or global in a function body with another.
fn replace_uses_in_function(module: &mut Module, function: Function, to_replace: Operand, replace_with: Operand) {
    for (_, block) in module.functions[function].blocks.iter_mut() {
        for inst in block.instructions.iter_mut() {
            for operand in inst.operands.iter_mut() {
                if operand == to_replace {
                    *operand = replace_with;
                }
            }
        }
    }
}

fn merge_constant(dst: &mut Module, mut cdata: ConstantData, map: &[usize]) -> Constant {
    // TODO for constants with components, remap components
    /*match cdata {
        ConstantImpl::F64(_) => {}
        ConstantImpl::I64(_) => {}
        ConstantImpl::Boolean(_) => {}
    };*/
    dst.define_constant(cdata)
}

fn merge_type(dst: &mut Module, mut tydata: TypeData, map: &[usize]) -> Type {
    let remap = |ty| {
        *ty = Type::from_index(map[ty.index()]);
    };
    match tydata {
        TypeData::Array(ref mut ty, _) => {
            remap(ty);
        }
        TypeData::RuntimeArray(ref mut ty) => {
            remap(ty);
        }
        TypeData::Struct(ref mut struct_type) => {
            for f in struct_type.fields.iter_mut() {
                remap(&mut f.ty);
            }
        }
        TypeData::Pointer(ref mut ty) => {
            remap(ty);
        }
        TypeData::Function(ref mut function_type) => {
            remap(&mut function_type.return_ty);
            for argty in function_type.arg_types.iter_mut() {
                remap(argty);
            }
        }
        _ => {}
    }
    dst.define_type(tydata)
}

fn merge_function(
    dst: &mut Module,
    mut fdata: FunctionData,
    constant_map: &[usize],
    type_map: &[usize],
    function_map: &[usize],
) -> Function {
    let remap_operand = |op: &mut Operand| match op {
        Operand::Constant(ref mut constant) => {
            *constant = Constant::from_index(constant_map[constant.index()]);
        }
        Operand::ExtInst(_) => {}
        Operand::Function(ref mut function) => {
            *function = Function::from_index(function_map[function.index()]);
        }
        Operand::Value(_) => {}
        Operand::Block(_) => {}
        Operand::Type(ref mut ty) => {
            *ty = Type::from_index(type_map[ty.index()]);
        }
    };

    for (_, block) in fdata.blocks.iter_mut() {
        for inst in block.instructions.iter_mut() {
            for operand in inst.operands.iter_mut() {
                remap_operand(operand);
            }
        }
    }

    dst.functions.alloc(fdata)
}




/// Module linker.
///
/// It links functions and constants according to the SPIR-V linkage attributes.
///
pub fn link_module_pipeline(modules: &[Module]) -> Module {
    let mut output_module = Module::new();

    // first, merge constants, types and functions into the same module
    for module in modules {
        // index in source module -> index in target module.
        let mut constant_map = vec![0usize; module.constants.len()];
        let mut type_map = vec![0usize; module.types.len()];
        let mut function_map = vec![0usize; module.functions.len()];

        for (c, cdata) in module.constants {
            constant_map[c.index()] = merge_constant(&mut output_module, cdata, &constant_map).index();
        }
        for (ty, tydata) in module.types {
            type_map[ty.index()] = merge_type(&mut output_module, tydata, &type_map).index();
        }
        for (func, funcdata) in module.functions {
            function_map[func.index()] =
                merge_function(&mut output_module, funcdata, &constant_map, &type_map, &function_map).index();
        }
    }

    // then, link the interfaces together, by name

    // what about uniforms?
    //
    //
    //

    todo!()
}
