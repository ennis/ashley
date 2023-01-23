use crate::{
    hir::{
        types::{ImageSampling, ImageType, ScalarType, StructType},
        Block, Constant, ConstantData, Function, GlobalVariable, Module, Operand, TerminatingInstruction, Type,
        TypeData, Value, ValueOrConstant,
    },
    utils::IdMap,
};
use rspirv::{
    binary::Assemble,
    dr::InsertPoint,
    spirv,
    spirv::{AddressingModel, Capability, LinkageType, MemoryModel, Word},
};
use std::collections::HashMap;
use tracing::error;
use crate::hir::IdRef;

type TypeMap = IdMap<Type, Word>;
type ConstantMap = IdMap<Constant, Word>;
type FunctionMap = IdMap<Function, Word>;
type GlobalMap = IdMap<GlobalVariable, Word>;

fn emit_scalar_type(builder: &mut rspirv::dr::Builder, scalar_type: ScalarType) -> Word {
    match scalar_type {
        ScalarType::Int => builder.type_int(32, 1),
        ScalarType::UnsignedInt => builder.type_int(32, 0),
        ScalarType::Float => builder.type_float(32),
        ScalarType::Double => builder.type_float(64),
        ScalarType::Bool => builder.type_bool(),
    }
}

fn emit_image_type(builder: &mut rspirv::dr::Builder, image_type: &ImageType) -> Word {
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
    let sampled_type = emit_scalar_type(builder, image_type.sampled_type);
    builder.type_image(
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

fn emit_constants(module: &Module, builder: &mut rspirv::dr::Builder, type_ids: &TypeMap) -> ConstantMap {
    let mut map = ConstantMap::with_capacity(module.constants.len());

    for (constant, cdata) in module.constants {
        let ssa_id = match cdata {
            ConstantData::F32(v) => {
                let ty = builder.type_float(32);
                builder.constant_f32(ty, v.0)
            }
            ConstantData::I32(v) => {
                let ty = builder.type_int(32, 1);
                builder.constant_u32(ty, v as u32)
            }
            ConstantData::U32(v) => {
                let ty = builder.type_int(32, 0);
                builder.constant_u32(ty, v)
            }
            ConstantData::F64(v) => {
                let ty = builder.type_float(64);
                builder.constant_f64(ty, v.0)
            }
            ConstantData::I64(v) => {
                let ty = builder.type_int(64, 1);
                builder.constant_u64(ty, v as u64)
            }
            ConstantData::U64(v) => {
                let ty = builder.type_int(64, 0);
                builder.constant_u64(ty, v)
            }
            ConstantData::Bool(v) => {
                let ty = builder.type_bool();
                if v {
                    builder.constant_true(ty)
                } else {
                    builder.constant_false(ty)
                }
            }
            ConstantData::Composite { ty, constituents } => {
                let ty = type_ids[ty];
                builder.constant_composite(ty, constituents.iter().map(|c| map[*c]))
            }
        };

        map[constant] = ssa_id;
    }

    map
}

/// Returns the map from Type index -> SPIR-V SSA index
fn emit_types(module: &Module, builder: &mut rspirv::dr::Builder) -> TypeMap {
    let mut map = TypeMap::with_capacity(module.types.len());

    for (ty, tydata) in module.types {
        let ssa_id = match tydata {
            TypeData::Unit => builder.type_void(),
            TypeData::Scalar(scalar_type) => emit_scalar_type(builder, scalar_type),
            TypeData::Vector(component_type, len) => {
                let component_type = emit_scalar_type(builder, component_type);
                builder.type_vector(component_type, len as u32)
            }
            TypeData::Matrix(component_type, rows, cols) => {
                let component_type = emit_scalar_type(builder, component_type);
                let column_type = builder.type_vector(component_type, rows as u32);
                builder.type_matrix(column_type, cols as u32)
            }
            TypeData::Array(elem_ty, len) => {
                let ty_u32 = builder.type_int(32, 0);
                let len = builder.constant_u32(ty_u32, len);
                builder.type_array(map[elem_ty], len)
            }
            TypeData::RuntimeArray(elem_ty) => builder.type_runtime_array(map[elem_ty]),
            TypeData::Struct(ty) => {
                let id = builder.type_struct(ty.fields.iter().map(|f| map[f.ty]));
                if let Some(ref name) = ty.name {
                    builder.name(id, name.to_string());
                }
                id
            }
            TypeData::SampledImage(ty) => {
                let image_type = emit_image_type(builder, &ty);
                builder.type_sampled_image(image_type)
            }
            TypeData::Image(ty) => emit_image_type(builder, &ty),
            TypeData::Pointer {
                pointee_type,
                storage_class,
            } => {
                let pointee_type = map[pointee_type];
                builder.type_pointer(None, storage_class, pointee_type)
            }
            TypeData::Function(function_type) => {
                let return_type = map[function_type.return_type];
                builder.type_function(return_type, function_type.arg_types.iter().map(|arg| map[*arg]))
            }
            TypeData::Sampler => builder.type_sampler(),
            TypeData::String => {
                // Strings should be removed by then
                // TODO remove strings altogether
                error!("unrepresentable type encountered during SPIR-V translation: string");
                builder.type_void()
            }
            TypeData::Unknown => {
                error!("unknown type encountered during SPIR-V translation");
                builder.type_void()
            }
            _ => {
                todo!()
            }
        };

        map[ty] = ssa_id;
    }

    map
}

fn emit_linkage_decoration(builder: &mut rspirv::dr::Builder, id: Word, name: &str, linkage: LinkageType) {
    builder.decorate(
        id,
        spirv::Decoration::LinkageAttributes,
        [
            rspirv::dr::Operand::LiteralString(name.to_string()),
            rspirv::dr::Operand::LinkageType(linkage),
        ],
    );
}

fn emit_globals(
    module: &Module,
    builder: &mut rspirv::dr::Builder,
    type_map: &TypeMap,
    _constant_map: &ConstantMap,
) -> GlobalMap {
    let mut map = GlobalMap::with_capacity(module.globals.len());
    for (g, gdata) in module.globals.iter() {
        let ty = type_map[gdata.ty];
        let id = builder.variable(ty, None, gdata.storage_class, None); // TODO initializers
        if let Some(linkage) = gdata.linkage {
            emit_linkage_decoration(builder, id, &gdata.name, linkage);
        }
        map[g] = id;
    }
    map
}

fn operand_to_rspirv(
    op: &Operand,
    type_map: &TypeMap,
    constant_map: &ConstantMap,
    global_map: &GlobalMap,
    functions: &IdMap<Function, Word>,
    values: &IdMap<Value, Word>,
    labels: &IdMap<Block, Word>,
) -> rspirv::dr::Operand {
    match *op {
        Operand::ConstantRef(constant) => rspirv::dr::Operand::IdRef(constant_map[constant]),
        Operand::LiteralExtInstInteger(i) => rspirv::dr::Operand::LiteralExtInstInteger(i),
        Operand::FunctionRef(function) => rspirv::dr::Operand::IdRef(functions[function]),
        Operand::ValueRef(value) => rspirv::dr::Operand::IdRef(values[value]),
        Operand::BlockRef(block) => rspirv::dr::Operand::IdRef(labels[block]),
        Operand::TypeRef(ty) => rspirv::dr::Operand::IdRef(type_map[ty]),
        Operand::GlobalRef(global) => rspirv::dr::Operand::IdRef(global_map[global]),
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
        Operand::LiteralFloat32(v) => rspirv::dr::Operand::LiteralFloat32(v),
        Operand::LiteralFloat64(v) => rspirv::dr::Operand::LiteralFloat64(v),
        Operand::LiteralSpecConstantOpInteger(v) => rspirv::dr::Operand::LiteralSpecConstantOpInteger(v),
        Operand::LiteralString(ref v) => rspirv::dr::Operand::LiteralString(v.clone()),
        Operand::ImageOperands(v) => rspirv::dr::Operand::ImageOperands(v),
        Operand::PackedVectorFormat(v) => rspirv::dr::Operand::LiteralInt32(v as u32),
    }
}

fn emit_functions(
    module: &Module,
    builder: &mut rspirv::dr::Builder,
    type_map: &TypeMap,
    constant_map: &ConstantMap,
    global_map: &GlobalMap,
) {
    let mut function_map = FunctionMap::with_capacity(module.functions.len());
    for (f, fdata) in module.functions.iter() {
        let function_type_id = type_map[fdata.function_type];
        let function_type = match module.types[fdata.function_type] {
            TypeData::Function(ref fty) => fty,
            _ => panic!("invalid function type"),
        };
        let result_type = type_map[function_type.return_type];
        let id = builder
            .begin_function(result_type, None, fdata.function_control, function_type_id)
            .unwrap();

        let mut labels = IdMap::with_capacity(fdata.blocks.len());
        let mut values = IdMap::with_capacity(fdata.values.len());

        for (arg, ty) in fdata.arguments.iter().zip(function_type.arg_types.iter()) {
            values[*arg] = builder.function_parameter(type_map[*ty]).unwrap();
        }

        for (b, _) in fdata.blocks.iter() {
            labels[b] = builder.begin_block(None).unwrap();
        }

        for (ib, (_, bdata)) in fdata.blocks.iter().enumerate() {
            builder.select_block(Some(ib)).unwrap();
            for inst in bdata.instructions {
                let result_type = inst.result.map(|v| type_map[fdata.values[v].ty]);
                let result_id = if result_type.is_some() {
                    Some(builder.id())
                } else {
                    None
                };
                let mut operands = Vec::with_capacity(inst.operands.len());
                for op in inst.operands.iter() {
                    operands.push(operand_to_rspirv(
                        op,
                        type_map,
                        constant_map,
                        global_map,
                        &function_map,
                        &values,
                        &labels,
                    ));
                }
                let spvinst = rspirv::dr::Instruction::new(inst.opcode, result_type, result_id, operands);
                builder.insert_into_block(InsertPoint::End, spvinst).unwrap();
            }
            match bdata.terminator {
                None => {
                    panic!("unterminated block")
                }
                Some(ref terminator) => match terminator {
                    TerminatingInstruction::Branch(target) => {
                        builder.branch(labels[*target]).unwrap();
                    }
                    TerminatingInstruction::BranchConditional {
                        condition,
                        true_block,
                        false_block,
                    } => {
                        let condition_id = match condition {
                            ValueOrConstant::Value(v) => values[*v],
                            ValueOrConstant::Constant(c) => constant_map[*c],
                        };
                        builder
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
                        builder.ret().unwrap();
                    }
                    TerminatingInstruction::ReturnValue(v) => {
                        let id = match v {
                            IdRef::Value(v) => values[*v],
                            IdRef::Constant(c) => constant_map[*c],
                            IdRef::Global(g) => global_map[*g],
                        };
                        builder.ret_value(id).unwrap();
                    }
                    TerminatingInstruction::Unreachable => {
                        builder.unreachable().unwrap();
                    }
                    TerminatingInstruction::TerminateInvocation => {
                        builder.terminate_invocation().unwrap();
                    }
                },
            }
        }

        builder.end_function().unwrap();
        if let Some(linkage) = fdata.linkage {
            emit_linkage_decoration(builder, id, &fdata.name, linkage);
        }
        function_map[f] = id;
    }
}

pub fn write_spirv(module: &Module) -> Vec<u32> {
    let mut builder = rspirv::dr::Builder::new();

    builder.set_version(1, 6);
    builder.capability(Capability::Linkage);
    builder.capability(Capability::Shader);
    builder.ext_inst_import("GLSL.std.450");
    builder.memory_model(AddressingModel::Logical, MemoryModel::GLSL450);

    // Types and constants are interdependent, so emit_types may also emit constant instructions
    // before we enter emit_constants. However the rspirv builder dedups constant instructions so it's OK.
    let type_map = emit_types(module, &mut builder);
    let constant_map = emit_constants(module, &mut builder, &type_map);
    let global_map = emit_globals(module, &mut builder, &type_map, &constant_map);
    let _function_map = emit_functions(module, &mut builder, &type_map, &constant_map, &global_map);

    builder.module().assemble()
}
