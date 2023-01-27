//! Generator for the HIR builder
use crate::utils::to_ident;
use heck::ToSnakeCase;
use proc_macro2::{Ident, Span};
use quote::quote;
use serde::Deserialize;
use serde_json as json;
use std::{
    fs::File,
    io::{BufReader, Write},
};

//--------------------------------------------------------------------------------------------------

// TODO: use rspirv::grammar!

/// Instructions excluded from automatic builder method generation.
///
/// These should be implemented by hand in `hir::FunctionBuilder` or `hir::Module`
const EXCLUDED_INSTRUCTIONS: &[&str] = &[
    "OpLoopMerge",
    "OpSelectionMerge",
    "OpSwitch",
    "OpString",
    "OpExtInstImport",
    "OpDecorationGroup",
    "OpLabel",
    "OpTypeRayQueryKHR",
    "OpTypeHitObjectNV",
    "OpTypeAccelerationStructureNV",
    "OpTypeAccelerationStructureKHR",
    "OpTypeCooperativeMatrixNV",
    "OpGroupNonUniformRotateKHR",
    "OpSDot",
    "OpSDotKHR",
    "OpUDot",
    "OpUDotKHR",
    "OpSUDot",
    "OpSUDotKHR",
    "OpSDotAccSat",
    "OpSDotAccSatKHR",
    "OpUDotAccSat",
    "OpUDotAccSatKHR",
    "OpSUDotAccSat",
    "OpSUDotAccSatKHR",
    "OpHitObjectRecordHitMotionNV",
    "OpHitObjectRecordHitWithIndexMotionNV",
    "OpHitObjectRecordMissMotionNV",
    "OpHitObjectGetWorldToObjectNV",
    "OpHitObjectGetObjectToWorldNV",
    "OpHitObjectGetObjectRayDirectionNV",
    "OpHitObjectGetObjectRayOriginNV",
    "OpHitObjectTraceRayMotionNV",
    "OpHitObjectGetShaderRecordBufferHandleNV",
    "OpHitObjectGetShaderBindingTableRecordIndexNV",
    "OpHitObjectRecordEmptyNV",
    "OpHitObjectTraceRayNV",
    "OpHitObjectRecordHitNV",
    "OpHitObjectRecordHitWithIndexNV",
    "OpHitObjectRecordMissNV",
    "OpHitObjectExecuteShaderNV",
    "OpHitObjectGetCurrentTimeNV",
    "OpHitObjectGetAttributesNV",
    "OpHitObjectGetHitKindNV",
    "OpHitObjectGetPrimitiveIndexNV",
    "OpHitObjectGetGeometryIndexNV",
    "OpHitObjectGetInstanceIdNV",
    "OpHitObjectGetInstanceCustomIndexNV",
    "OpHitObjectGetWorldRayDirectionNV",
    "OpHitObjectGetWorldRayOriginNV",
    "OpHitObjectGetRayTMaxNV",
    "OpHitObjectGetRayTMinNV",
    "OpHitObjectIsEmptyNV",
    "OpHitObjectIsHitNV",
    "OpHitObjectIsMissNV",
    "OpReorderThreadWithHitObjectNV",
    "OpReorderThreadWithHintNV",
    "OpEmitMeshTasksEXT",
    "OpSetMeshOutputsEXT",
    "OpTraceMotionNV",
    "OpTraceRayMotionNV",
    "OpDemoteToHelperInvocation",
    "OpConvertUToImageNV",
    "OpConvertUToSamplerNV",
    "OpConvertImageToUNV",
    "OpConvertSamplerToUNV",
    "OpConvertUToSampledImageNV",
    "OpConvertSampledImageToUNV",
    "OpSamplerImageAddressingModeNV",
    "OpAtomicFMinEXT",
    "OpAtomicFMaxEXT",
    "OpAssumeTrueKHR",
    "OpExpectKHR",
    "OpControlBarrierArriveINTEL",
    "OpControlBarrierWaitINTEL",
    "OpGroupIMulKHR",
    "OpGroupFMulKHR",
    "OpGroupBitwiseAndKHR",
    "OpGroupBitwiseOrKHR",
    "OpGroupBitwiseXorKHR",
    "OpGroupLogicalAndKHR",
    "OpGroupLogicalOrKHR",
    "OpGroupLogicalXorKHR",
    "OpGroupMemberDecorate",
    "OpPhi",
];

//--------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq, Deserialize)]
enum OperandCategory {
    BitEnum,
    Composite,
    Id,
    Literal,
    ValueEnum,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Deserialize)]
enum InstructionClass {
    #[serde(rename = "@exclude")]
    Exclude,
    #[serde(rename = "Annotation")]
    Annotation,
    #[serde(rename = "Arithmetic")]
    Arithmetic,
    #[serde(rename = "Atomic")]
    Atomic,
    #[serde(rename = "Barrier")]
    Barrier,
    #[serde(rename = "Bit")]
    Bit,
    #[serde(rename = "Composite")]
    Composite,
    #[serde(rename = "Constant-Creation")]
    ConstantCreation,
    #[serde(rename = "Control-Flow")]
    ControlFlow,
    #[serde(rename = "Conversion")]
    Conversion,
    #[serde(rename = "Debug")]
    Debug,
    #[serde(rename = "Derivative")]
    Derivative,
    #[serde(rename = "Device-Side_Enqueue")]
    DeviceSideEnqueue,
    #[serde(rename = "Extension")]
    Extension,
    #[serde(rename = "Function")]
    Function,
    #[serde(rename = "Group")]
    Group,
    #[serde(rename = "Image")]
    Image,
    #[serde(rename = "Memory")]
    Memory,
    #[serde(rename = "Miscellaneous")]
    Miscellaneous,
    #[serde(rename = "Mode-Setting")]
    ModeSetting,
    #[serde(rename = "Non-Uniform")]
    NonUniform,
    #[serde(rename = "Pipe")]
    Pipe,
    #[serde(rename = "Primitive")]
    Primitive,
    #[serde(rename = "Relational_and_Logical")]
    RelationalAndLogical,
    #[serde(rename = "Reserved")]
    Reserved,
    #[serde(rename = "Type-Declaration")]
    TypeDeclaration,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Deserialize)]
enum OperandKind {
    ImageOperands,
    FPFastMathMode,
    SelectionControl,
    LoopControl,
    FunctionControl,
    MemorySemantics,
    MemoryAccess,
    KernelProfilingInfo,
    RayFlags,
    FragmentShadingRate,
    SourceLanguage,
    ExecutionModel,
    AddressingModel,
    MemoryModel,
    ExecutionMode,
    StorageClass,
    Dim,
    SamplerAddressingMode,
    SamplerFilterMode,
    ImageFormat,
    ImageChannelOrder,
    ImageChannelDataType,
    FPRoundingMode,
    FPDenormMode,
    QuantizationModes,
    FPOperationMode,
    OverflowModes,
    LinkageType,
    AccessQualifier,
    FunctionParameterAttribute,
    Decoration,
    BuiltIn,
    Scope,
    GroupOperation,
    KernelEnqueueFlags,
    Capability,
    RayQueryIntersection,
    RayQueryCommittedIntersectionType,
    RayQueryCandidateIntersectionType,
    PackedVectorFormat,
    IdResultType,
    IdResult,
    IdMemorySemantics,
    IdScope,
    IdRef,
    LiteralInteger,
    LiteralString,
    LiteralContextDependentNumber,
    LiteralExtInstInteger,
    LiteralSpecConstantOpInteger,
    PairLiteralIntegerIdRef,
    PairIdRefLiteralInteger,
    PairIdRefIdRef,
}

impl OperandKind {
    fn category(self) -> OperandCategory {
        match self {
            OperandKind::ImageOperands => OperandCategory::BitEnum,
            OperandKind::FPFastMathMode => OperandCategory::BitEnum,
            OperandKind::SelectionControl => OperandCategory::BitEnum,
            OperandKind::LoopControl => OperandCategory::BitEnum,
            OperandKind::FunctionControl => OperandCategory::BitEnum,
            OperandKind::MemorySemantics => OperandCategory::BitEnum,
            OperandKind::MemoryAccess => OperandCategory::BitEnum,
            OperandKind::KernelProfilingInfo => OperandCategory::BitEnum,
            OperandKind::RayFlags => OperandCategory::BitEnum,
            OperandKind::FragmentShadingRate => OperandCategory::BitEnum,
            OperandKind::SourceLanguage => OperandCategory::ValueEnum,
            OperandKind::ExecutionModel => OperandCategory::ValueEnum,
            OperandKind::AddressingModel => OperandCategory::ValueEnum,
            OperandKind::MemoryModel => OperandCategory::ValueEnum,
            OperandKind::ExecutionMode => OperandCategory::ValueEnum,
            OperandKind::StorageClass => OperandCategory::ValueEnum,
            OperandKind::Dim => OperandCategory::ValueEnum,
            OperandKind::SamplerAddressingMode => OperandCategory::ValueEnum,
            OperandKind::SamplerFilterMode => OperandCategory::ValueEnum,
            OperandKind::ImageFormat => OperandCategory::ValueEnum,
            OperandKind::ImageChannelOrder => OperandCategory::ValueEnum,
            OperandKind::ImageChannelDataType => OperandCategory::ValueEnum,
            OperandKind::FPRoundingMode => OperandCategory::ValueEnum,
            OperandKind::FPDenormMode => OperandCategory::ValueEnum,
            OperandKind::QuantizationModes => OperandCategory::ValueEnum,
            OperandKind::FPOperationMode => OperandCategory::ValueEnum,
            OperandKind::OverflowModes => OperandCategory::ValueEnum,
            OperandKind::LinkageType => OperandCategory::ValueEnum,
            OperandKind::AccessQualifier => OperandCategory::ValueEnum,
            OperandKind::FunctionParameterAttribute => OperandCategory::ValueEnum,
            OperandKind::Decoration => OperandCategory::ValueEnum,
            OperandKind::BuiltIn => OperandCategory::ValueEnum,
            OperandKind::Scope => OperandCategory::ValueEnum,
            OperandKind::GroupOperation => OperandCategory::ValueEnum,
            OperandKind::KernelEnqueueFlags => OperandCategory::ValueEnum,
            OperandKind::Capability => OperandCategory::ValueEnum,
            OperandKind::RayQueryIntersection => OperandCategory::ValueEnum,
            OperandKind::RayQueryCommittedIntersectionType => OperandCategory::ValueEnum,
            OperandKind::RayQueryCandidateIntersectionType => OperandCategory::ValueEnum,
            OperandKind::PackedVectorFormat => OperandCategory::ValueEnum,
            OperandKind::IdResultType => OperandCategory::Id,
            OperandKind::IdResult => OperandCategory::Id,
            OperandKind::IdMemorySemantics => OperandCategory::Id,
            OperandKind::IdScope => OperandCategory::Id,
            OperandKind::IdRef => OperandCategory::Id,
            OperandKind::LiteralInteger => OperandCategory::Literal,
            OperandKind::LiteralString => OperandCategory::Literal,
            OperandKind::LiteralContextDependentNumber => OperandCategory::Literal,
            OperandKind::LiteralExtInstInteger => OperandCategory::Literal,
            OperandKind::LiteralSpecConstantOpInteger => OperandCategory::Literal,
            OperandKind::PairLiteralIntegerIdRef => OperandCategory::Composite,
            OperandKind::PairIdRefLiteralInteger => OperandCategory::Composite,
            OperandKind::PairIdRefIdRef => OperandCategory::Composite,
        }
    }
}

#[derive(Deserialize)]
enum Quantifier {
    #[serde(rename = "?")]
    ZeroOrOne,
    #[serde(rename = "*")]
    ZeroOrMore,
}

#[derive(Deserialize)]
struct Operand {
    kind: OperandKind,
    quantifier: Option<Quantifier>,
    name: Option<String>,
}

#[derive(Deserialize)]
struct Instruction {
    opname: String,
    class: Option<InstructionClass>,
    opcode: u32,
    #[serde(default)]
    operands: Vec<Operand>,
}

#[derive(Deserialize)]
struct InstructionPrintingClass {
    tag: String,
    heading: Option<String>,
}

#[derive(Deserialize)]
pub(super) struct Grammar {
    copyright: Vec<String>,
    magic_number: String,
    major_version: i64,
    minor_version: i64,
    revision: i64,
    instruction_printing_class: Vec<InstructionPrintingClass>,
    instructions: Vec<Instruction>,
}

#[derive(Deserialize)]
pub(super) struct ExtGrammar {
    copyright: Vec<String>,
    version: i64,
    revision: i64,
    instructions: Vec<Instruction>,
}

fn generate_instruction(inst: &Instruction, ext: Option<(&str, &str)>) -> proc_macro2::TokenStream {
    let inst_name = if ext.is_none() {
        inst.opname.strip_prefix("Op").expect("unexpected instruction name")
    } else {
        inst.opname.as_str()
    };
    let mut opname = inst_name.to_snake_case();
    // name fixups
    match opname.as_str() {
        "d_pdx" => {
            opname = "dpdx".to_string();
        }
        "d_pdy" => {
            opname = "dpdy".to_string();
        }
        "d_pdx_fine" => {
            opname = "dpdx_fine".to_string();
        }
        "d_pdy_fine" => {
            opname = "dpdy_fine".to_string();
        }
        "d_pdx_coarse" => {
            opname = "dpdx_coarse".to_string();
        }
        "d_pdy_coarse" => {
            opname = "dpdy_coarse".to_string();
        }
        _ => {}
    };
    //let opname = as_ident(opname_snake_case);
    let inst_variant_name = to_ident(inst_name);
    let method_name = to_ident(if let Some(ext) = ext {
        format!("emit_{}_{opname}", ext.1)
    } else {
        format!("emit_{opname}")
    });

    let mut has_result = false;
    let mut arg_types = Vec::new();
    let mut arg_names = Vec::new();
    let mut operands = Vec::new();

    // all ext insts have a result
    if ext.is_some() {
        arg_types.push(quote!(Type));
        arg_names.push(to_ident("result_type"));
        has_result = true;
    }

    for (i, op) in inst.operands.iter().enumerate() {
        if op.kind == OperandKind::IdResult {
            // Instruction results are not operands
            has_result = true;
            continue;
        }

        let mut name = if let Some(ref name) = op.name {
            name.to_snake_case()
        } else {
            if op.kind == OperandKind::IdResultType {
                // the result type is always named "result_type" (the quote block below expects that)
                "result_type".to_string()
            } else {
                // no name given, make one from the operand kind
                let kind_str = format!("{:?}{i}", op.kind);
                kind_str.to_snake_case()
            }
        };

        // replace reserved identifiers
        let name = match name.as_str() {
            name @ "type" => Ident::new_raw(name, Span::call_site()),
            other => Ident::new(other, Span::call_site()),
        };

        let mut ty = match op.kind {
            OperandKind::ImageOperands => {
                quote!(ImageOperands)
            }
            OperandKind::FPFastMathMode => {
                quote!(spirv::FPFastMathMode)
            }
            OperandKind::SelectionControl => {
                quote!(spirv::SelectionControl)
            }
            OperandKind::LoopControl => {
                quote!(spirv::LoopControl)
            }
            OperandKind::FunctionControl => {
                quote!(spirv::FunctionControl)
            }
            OperandKind::MemorySemantics => {
                quote!(spirv::MemorySemantics)
            }
            OperandKind::MemoryAccess => {
                quote!(spirv::MemoryAccess)
            }
            OperandKind::KernelProfilingInfo => {
                quote!(spirv::KernelProfilingInfo)
            }
            OperandKind::RayFlags => {
                quote!(spirv::RayFlags)
            }
            OperandKind::FragmentShadingRate => {
                quote!(spirv::FragmentShadingRate)
            }
            OperandKind::SourceLanguage => {
                quote!(spirv::SourceLanguage)
            }
            OperandKind::ExecutionModel => {
                quote!(spirv::ExecutionModel)
            }
            OperandKind::AddressingModel => {
                quote!(spirv::AddressingModel)
            }
            OperandKind::MemoryModel => {
                quote!(spirv::MemoryModel)
            }
            OperandKind::ExecutionMode => {
                quote!(spirv::ExecutionMode)
            }
            OperandKind::StorageClass => {
                quote!(spirv::StorageClass)
            }
            OperandKind::Dim => {
                quote!(spirv::Dim)
            }
            OperandKind::SamplerAddressingMode => {
                quote!(spirv::SamplerAddressingMode)
            }
            OperandKind::SamplerFilterMode => {
                quote!(spirv::SamplerFilterMode)
            }
            OperandKind::ImageFormat => {
                quote!(spirv::ImageFormat)
            }
            OperandKind::ImageChannelOrder => {
                quote!(spirv::ImageChannelOrder)
            }
            OperandKind::ImageChannelDataType => {
                quote!(spirv::ImageChannelDataType)
            }
            OperandKind::FPRoundingMode => {
                quote!(spirv::FPRoundingMode)
            }
            OperandKind::FPDenormMode => {
                quote!(spirv::FPDenormMode)
            }
            OperandKind::QuantizationModes => {
                quote!(spirv::QuantizationModes)
            }
            OperandKind::FPOperationMode => {
                quote!(spirv::FPOperationMode)
            }
            OperandKind::OverflowModes => {
                quote!(spirv::OverflowModes)
            }
            OperandKind::LinkageType => {
                quote!(spirv::LinkageType)
            }
            OperandKind::AccessQualifier => {
                quote!(spirv::AccessQualifier)
            }
            OperandKind::FunctionParameterAttribute => {
                quote!(spirv::FunctionParameterAttribute)
            }
            OperandKind::Decoration => {
                quote!(spirv::Decoration)
            }
            OperandKind::BuiltIn => {
                quote!(spirv::BuiltIn)
            }
            OperandKind::Scope => {
                quote!(spirv::Scope)
            }
            OperandKind::GroupOperation => {
                quote!(spirv::GroupOperation)
            }
            OperandKind::KernelEnqueueFlags => {
                quote!(spirv::KernelEnqueueFlags)
            }
            OperandKind::Capability => {
                quote!(spirv::Capability)
            }
            OperandKind::RayQueryIntersection => {
                quote!(spirv::RayQueryIntersection)
            }
            OperandKind::RayQueryCommittedIntersectionType => {
                quote!(spirv::RayQueryCommittedIntersectionType)
            }
            OperandKind::RayQueryCandidateIntersectionType => {
                quote!(spirv::RayQueryCandidateIntersectionType)
            }
            OperandKind::PackedVectorFormat => {
                quote!(crate::hir::PackedVectorFormat)
            }
            OperandKind::IdMemorySemantics => {
                quote!(ValueOrConstant)
            }
            OperandKind::IdScope => {
                quote!(ValueOrConstant)
            }
            OperandKind::IdRef => match op.quantifier {
                None | Some(Quantifier::ZeroOrOne) => {
                    quote!(impl IntoIdRef)
                }
                Some(Quantifier::ZeroOrMore) => {
                    quote!(IdRef)
                }
            },
            OperandKind::IdResultType => {
                quote!(Type)
            }
            OperandKind::LiteralInteger => {
                quote!(i32)
            }
            OperandKind::LiteralString => {
                quote!(&str)
            }
            OperandKind::LiteralContextDependentNumber => {
                // TODO
                quote!(i32)
            }
            OperandKind::LiteralExtInstInteger => {
                quote!(u32)
            }
            OperandKind::LiteralSpecConstantOpInteger => {
                // TODO
                quote!(u32)
            }
            OperandKind::PairLiteralIntegerIdRef => {
                quote!((i32, Value))
            }
            OperandKind::PairIdRefLiteralInteger => {
                quote!((Value, i32))
            }
            OperandKind::PairIdRefIdRef => {
                quote!((Value, Value))
            }
            _ => {
                panic!("unsupported operand kind")
            }
        };

        // apply quantifier on the argument type
        match op.quantifier {
            Some(Quantifier::ZeroOrOne) => {
                ty = quote!(Option<#ty>);
            }
            Some(Quantifier::ZeroOrMore) => ty = quote!(&[#ty]),
            _ => {}
        }

        arg_types.push(ty);
        arg_names.push(name.clone());
        if op.kind != OperandKind::IdResultType {
            // The result type is set via `InstructionBuilder::set_result`, so don't add it as an operand.
            operands.push(name.clone());
        }
    }

    let inst_builder = if let Some(ext) = ext {
        let name = ext.0;
        let opcode = inst.opcode;
        quote! {
            let ext_id = self.import_extended_instruction_set(#name);
            let mut inst_builder = InstBuilder::new_ext_inst(ext_id, #opcode);
        }
    } else {
        quote! {
            let mut inst_builder = InstBuilder::new(spirv::Op::#inst_variant_name);
        }
    };

    let set_result_type = if has_result {
        quote! {
            inst_builder.set_result(result_type);
        }
    } else {
        quote!()
    };

    let return_result_type = if has_result { quote!( -> Value ) } else { quote!() };
    let append_inst_and_return_result = if has_result {
        quote!(self.append_inst(inst_builder).unwrap())
    } else {
        quote! { self.append_inst(inst_builder); }
    };

    quote! {
         pub fn #method_name(&mut self, #(#arg_names : #arg_types),*) #return_result_type {
            #inst_builder
            #set_result_type
            #(
                #operands.write_operand(&mut inst_builder);
            )*
            #append_inst_and_return_result
        }
    }
}

fn generate_instruction_methods(insts: &[Instruction], ext: Option<(&str, &str)>) -> proc_macro2::TokenStream {
    let mut methods = vec![];
    for inst in insts.iter() {
        // type and constant builders are hand-crafted
        if matches!(
            inst.class,
            Some(
                InstructionClass::TypeDeclaration
                    | InstructionClass::ConstantCreation
                    | InstructionClass::Function
                    | InstructionClass::Exclude
            )
        ) {
            continue;
        }
        // filter out excluded instructions
        if EXCLUDED_INSTRUCTIONS.contains(&&*inst.opname) {
            continue;
        }

        methods.push(generate_instruction(inst, ext));
    }

    quote! {
        impl<'a> FunctionBuilder<'a> {
            #(#methods)*
        }
    }
}

pub(super) fn generate_impl_block(grammar: &Grammar) -> proc_macro2::TokenStream {
    generate_instruction_methods(&grammar.instructions, None)
}

pub(super) fn generate_ext_impl_block(
    grammar: &ExtGrammar,
    ext_name: &str,
    ext_prefix: &str,
) -> proc_macro2::TokenStream {
    generate_instruction_methods(&grammar.instructions, Some((ext_name, ext_prefix)))
}

/// Generates instruction builders.
///
/// # Arguments
/// * ext_grammars: tuple of (extension name, instruction prefix, file)
pub(super) fn generate_spirv_builders(core_grammar: &str, ext_grammars: &[(&str, &str, &str)], output: &str) {}
