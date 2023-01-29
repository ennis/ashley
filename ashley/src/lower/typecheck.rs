//! Type-checking rules for built-in operations

use crate::{
    hir,
    hir::{types::ScalarType, FunctionBuilder, IdRef, Module, TypeData, Value},
    lower::{builtin::BuiltinTypes, LowerCtxt, TypedValue, Scope, FuncRef},
};
use smallvec::SmallVec;
use std::cmp::Ordering;

/// Pseudo-type used in signatures of built-in function.
///
/// This is mainly used so that we can specify vector overloads in a more compact way (`vecN` instead
/// of duplicating signatures four times).
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum PseudoType {
    void,
    float,
    double,
    int,
    uint,
    bool,
    vec2,
    vec3,
    vec4,
    ivec2,
    ivec3,
    ivec4,
    uvec2,
    uvec3,
    uvec4,
    dvec2,
    dvec3,
    dvec4,
    bvec2,
    bvec3,
    bvec4,
    vecN,
    dvecN,
    ivecN,
    uvecN,
    bvecN,

    mat2,
    mat3,
    mat4,
    mat2x2,
    mat2x3,
    mat2x4,
    mat3x2,
    mat3x3,
    mat3x4,
    mat4x2,
    mat4x3,
    mat4x4,
    dmat2,
    dmat3,
    dmat4,
    dmat2x2,
    dmat2x3,
    dmat2x4,
    dmat3x2,
    dmat3x3,
    dmat3x4,
    dmat4x2,
    dmat4x3,
    dmat4x4,

    highp_vecN,
    highp_ivecN,
    modf_result_vecN,
    modf_result_dvecN,
    frexp_result_highp_vecN,

    image1D,
    image1DArray,
    image2D,
    image2DArray,
    image2DMS,
    image2DMSArray,
    image2DRect,
    image3D,
    imageCube,
    imageCubeArray,
    imageBuffer,

    iimage1D,
    iimage1DArray,
    iimage2D,
    iimage2DArray,
    iimage2DMS,
    iimage2DMSArray,
    iimage2DRect,
    iimage3D,
    iimageCube,
    iimageCubeArray,
    iimageBuffer,

    uimage1D,
    uimage1DArray,
    uimage2D,
    uimage2DArray,
    uimage2DMS,
    uimage2DMSArray,
    uimage2DRect,
    uimage3D,
    uimageCube,
    uimageCubeArray,
    uimageBuffer,

    texture1D,
    texture1DArray,
    texture2D,
    texture2DArray,
    texture2DMS,
    texture2DMSArray,
    texture2DRect,
    texture3D,
    textureCube,
    textureCubeArray,
    textureBuffer,

    itexture1D,
    itexture1DArray,
    itexture2D,
    itexture2DArray,
    itexture2DMS,
    itexture2DMSArray,
    itexture2DRect,
    itexture3D,
    itextureCube,
    itextureCubeArray,
    itextureBuffer,

    utexture1D,
    utexture1DArray,
    utexture2D,
    utexture2DArray,
    utexture2DMS,
    utexture2DMSArray,
    utexture2DRect,
    utexture3D,
    utextureCube,
    utextureCubeArray,
    utextureBuffer,

    gimage1D,
    gimage1DArray,
    gimage2D,
    gimage2DArray,
    gimage2DMS,
    gimage2DMSArray,
    gimage2DRect,
    gimage3D,
    gimageCube,
    gimageCubeArray,
    gimageBuffer,

    gtexture1D,
    gtexture1DArray,
    gtexture2D,
    gtexture2DArray,
    gtexture2DMS,
    gtexture2DMSArray,
    gtexture2DRect,
    gtexture3D,
    gtextureCube,
    gtextureCubeArray,
    gtextureBuffer,

    gvec4,

    subpassInput,
    subpassInputMS,
    sampler,
    samplerShadow,
}

/// Signature of a built-in operation (function or operator).
#[derive(Copy, Clone)]
pub(super) struct BuiltinSignature {
    parameter_types: &'static [PseudoType],
    result_type: PseudoType,
    lower: fn(&LowerCtxt, &mut FunctionBuilder, args: &[IdRef], types: &[hir::Type], ret: hir::Type) -> Value,
}

/// Error returned by `check_signature`
#[derive(Debug)]
pub(super) struct SignatureMismatch;

type ImplicitConversionRanks = SmallVec<[i32; 2]>;

//
fn compare_conversion_ranks(a: &[i32], b: &[i32]) -> Ordering {
    // 6.1. Function Definitions:
    //
    //  A function declaration A is considered a better match than function declaration B if
    //
    //     for at least one function argument, the conversion for that argument in A is better than the corresponding conversion in B; and
    //
    //     there is no function argument for which the conversion in B is better than the corresponding conversion in A.
    //
    //
    if a.iter().zip(b.iter()).any(|(a, b)| a < b) && a.iter().zip(b.iter()).all(|(a, b)| a <= b) {
        Ordering::Less
    } else {
        Ordering::Equal
    }
}

fn resolve_preferred_overload(ranks: &mut [(usize, ImplicitConversionRanks)]) -> Option<usize> {
    ranks.sort_by(|(_, a), (_, b)| compare_conversion_ranks(a, b));
    if ranks.len() >= 2 && compare_conversion_ranks(&ranks[0].1, &ranks[1].1) == Ordering::Less {
        Some(ranks[0].0)
    } else {
        None
    }
}

/// Checks argument types against an operation signature.
///
/// # Arguments
/// * m module
/// * signature operation signature (declared parameter types)
/// * arguments the types of the arguments passed to the operation
///
/// # Return value
///
/// If the argument types match the signature, returns an `Ok` value with the rank of implicit conversions
/// that were applied to each argument. Otherwise, returns `Err(SignatureMismatch(_))`
///
/// If you have multiple possible signatures, just call `check_signature` on all of them, keeping only the
/// one with the lower `ImplicitConversionRanks` (it implements `Ord`)
///
pub(super) fn check_signature(
    m: &hir::Module,
    signature: &[hir::Type],
    arguments: &[hir::Type],
) -> Result<ImplicitConversionRanks, SignatureMismatch> {

    eprintln!("chksig: {:?} against arguments {:?}", signature, arguments);

    if arguments.len() != signature.len() {
        // return early on arg count mismatch
        return Err(SignatureMismatch);
    }

    let mut conversion_ranks = SmallVec::new();
    for (sigty, argty) in signature.iter().zip(arguments.iter()) {
        if sigty == argty {
            // direct match, no conversion necessary
            conversion_ranks.push(0);
            continue;
        }

        // check type of argument, if it doesn't work, retry with an implicit conversion
        //eprintln!("chk param sig:{:?} arg:{:?}", &m.types[*sigty], &m.types[*argty]);
        let conversion_rank = match (&m.types[*sigty], &m.types[*argty]) {
            // Source type => implicitly converts to
            (TypeData::Scalar(targ), TypeData::Scalar(tsig)) => match (targ, tsig) {
                (ScalarType::Int, ScalarType::UnsignedInt) => 1,
                (ScalarType::UnsignedInt | ScalarType::Int, ScalarType::Float) => 2,
                (ScalarType::Int | ScalarType::UnsignedInt | ScalarType::Float, ScalarType::Double) => 3,
                _ => {
                    break;
                }
            },
            (TypeData::Vector(targ, n2), TypeData::Vector(tsig, n1)) if n1 == n2 => match (targ, tsig) {
                (ScalarType::Int, ScalarType::UnsignedInt) => 1,
                (ScalarType::UnsignedInt | ScalarType::Int, ScalarType::Float) => 2,
                (ScalarType::Int | ScalarType::UnsignedInt | ScalarType::Float, ScalarType::Double) => 3,
                _ => {
                    break;
                }
            },
            (
                TypeData::Matrix {
                    component_type: targ,
                    rows: r1,
                    columns: c1,
                },
                TypeData::Matrix {
                    component_type: tsig,
                    rows: r2,
                    columns: c2,
                },
            ) if r1 == r2 && c1 == c2 => match (targ, tsig) {
                (ScalarType::Float, ScalarType::Double) => 1,
                _ => {
                    break;
                }
            },
            _ => break,
        };

        conversion_ranks.push(conversion_rank);
    }

    if conversion_ranks.len() != signature.len() {
        // we exited the loop early, meaning that we failed to match an argument
        return Err(SignatureMismatch);
    }

    eprintln!("chksig OK: {:?} against arguments {:?}", signature, arguments);
    Ok(conversion_ranks)
}

//--------------------------------------------------------------------------------------------------

#[derive(Copy, Clone)]
enum ImageClass {
    F,
    SI,
    UI,
}

fn pseudo_type_to_concrete_type(
    pseudo_type: PseudoType,
    builtins: &BuiltinTypes,
    vec_len: u8,
    image_class: ImageClass,
) -> hir::Type {
    use ImageClass as IC;
    match pseudo_type {
        PseudoType::void => builtins.void,
        PseudoType::float => builtins.float,
        PseudoType::double => builtins.double,
        PseudoType::int => builtins.int,
        PseudoType::uint => builtins.uint,
        PseudoType::bool => builtins.bool,
        PseudoType::vecN => match vec_len {
            1 => builtins.float,
            2 => builtins.vec2,
            3 => builtins.vec3,
            4 => builtins.vec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::dvecN => match vec_len {
            1 => builtins.double,
            2 => builtins.dvec2,
            3 => builtins.dvec3,
            4 => builtins.dvec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::ivecN => match vec_len {
            1 => builtins.int,
            2 => builtins.ivec2,
            3 => builtins.ivec3,
            4 => builtins.ivec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::uvecN => match vec_len {
            1 => builtins.uint,
            2 => builtins.uvec2,
            3 => builtins.uvec3,
            4 => builtins.uvec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::bvecN => match vec_len {
            1 => builtins.bool,
            2 => builtins.bvec2,
            3 => builtins.bvec3,
            4 => builtins.bvec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::vec2 => builtins.vec2,
        PseudoType::vec3 => builtins.vec3,
        PseudoType::vec4 => builtins.vec4,
        PseudoType::ivec2 => builtins.ivec2,
        PseudoType::ivec3 => builtins.ivec3,
        PseudoType::ivec4 => builtins.ivec4,
        PseudoType::uvec2 => builtins.uvec2,
        PseudoType::uvec3 => builtins.uvec3,
        PseudoType::uvec4 => builtins.uvec4,
        PseudoType::dvec2 => builtins.dvec2,
        PseudoType::dvec3 => builtins.dvec3,
        PseudoType::dvec4 => builtins.dvec4,
        PseudoType::bvec2 => builtins.bvec2,
        PseudoType::bvec3 => builtins.bvec3,
        PseudoType::bvec4 => builtins.bvec4,
        PseudoType::highp_vecN => {
            todo!()
        }
        PseudoType::highp_ivecN => {
            todo!()
        }
        PseudoType::modf_result_vecN => {
            todo!()
        }
        PseudoType::modf_result_dvecN => {
            todo!()
        }
        PseudoType::frexp_result_highp_vecN => {
            todo!()
        }

        PseudoType::mat2 => builtins.mat2,
        PseudoType::mat3 => builtins.mat3,
        PseudoType::mat4 => builtins.mat4,
        PseudoType::mat2x2 => builtins.mat2x2,
        PseudoType::mat2x3 => builtins.mat2x3,
        PseudoType::mat2x4 => builtins.mat2x4,
        PseudoType::mat3x2 => builtins.mat3x2,
        PseudoType::mat3x3 => builtins.mat3x3,
        PseudoType::mat3x4 => builtins.mat3x4,
        PseudoType::mat4x2 => builtins.mat4x2,
        PseudoType::mat4x3 => builtins.mat4x3,
        PseudoType::mat4x4 => builtins.mat4x4,
        PseudoType::dmat2 => builtins.dmat2,
        PseudoType::dmat3 => builtins.dmat3,
        PseudoType::dmat4 => builtins.dmat4,
        PseudoType::dmat2x2 => builtins.dmat2x2,
        PseudoType::dmat2x3 => builtins.dmat2x3,
        PseudoType::dmat2x4 => builtins.dmat2x4,
        PseudoType::dmat3x2 => builtins.dmat3x2,
        PseudoType::dmat3x3 => builtins.dmat3x3,
        PseudoType::dmat3x4 => builtins.dmat3x4,
        PseudoType::dmat4x2 => builtins.dmat4x2,
        PseudoType::dmat4x3 => builtins.dmat4x3,
        PseudoType::dmat4x4 => builtins.dmat4x4,

        PseudoType::image1D => builtins.image1D,
        PseudoType::image1DArray => builtins.image1DArray,
        PseudoType::image2D => builtins.image2D,
        PseudoType::image2DArray => builtins.image2DArray,
        PseudoType::image2DMS => builtins.image2DMS,
        PseudoType::image2DMSArray => builtins.image2DMSArray,
        PseudoType::image2DRect => builtins.image2DRect,
        PseudoType::image3D => builtins.image3D,
        PseudoType::imageCube => builtins.imageCube,
        PseudoType::imageCubeArray => builtins.imageCubeArray,
        PseudoType::imageBuffer => builtins.imageBuffer,
        PseudoType::uimage1D => builtins.uimage1D,
        PseudoType::uimage1DArray => builtins.uimage1DArray,
        PseudoType::uimage2D => builtins.uimage2D,
        PseudoType::uimage2DArray => builtins.uimage2DArray,
        PseudoType::uimage2DMS => builtins.uimage2DMS,
        PseudoType::uimage2DMSArray => builtins.uimage2DMSArray,
        PseudoType::uimage2DRect => builtins.uimage2DRect,
        PseudoType::uimage3D => builtins.uimage3D,
        PseudoType::uimageCube => builtins.uimageCube,
        PseudoType::uimageCubeArray => builtins.uimageCubeArray,
        PseudoType::uimageBuffer => builtins.uimageBuffer,
        PseudoType::iimage1D => builtins.iimage1D,
        PseudoType::iimage1DArray => builtins.iimage1DArray,
        PseudoType::iimage2D => builtins.iimage2D,
        PseudoType::iimage2DArray => builtins.iimage2DArray,
        PseudoType::iimage2DMS => builtins.iimage2DMS,
        PseudoType::iimage2DMSArray => builtins.iimage2DMSArray,
        PseudoType::iimage2DRect => builtins.iimage2DRect,
        PseudoType::iimage3D => builtins.iimage3D,
        PseudoType::iimageCube => builtins.iimageCube,
        PseudoType::iimageCubeArray => builtins.iimageCubeArray,
        PseudoType::iimageBuffer => builtins.iimageBuffer,

        PseudoType::texture1D => builtins.texture1D,
        PseudoType::texture1DArray => builtins.texture1DArray,
        PseudoType::texture2D => builtins.texture2D,
        PseudoType::texture2DArray => builtins.texture2DArray,
        PseudoType::texture2DMS => builtins.texture2DMS,
        PseudoType::texture2DMSArray => builtins.texture2DMSArray,
        PseudoType::texture2DRect => builtins.texture2DRect,
        PseudoType::texture3D => builtins.texture3D,
        PseudoType::textureCube => builtins.textureCube,
        PseudoType::textureCubeArray => builtins.textureCubeArray,
        PseudoType::textureBuffer => builtins.textureBuffer,

        PseudoType::itexture1D => builtins.itexture1D,
        PseudoType::itexture1DArray => builtins.itexture1DArray,
        PseudoType::itexture2D => builtins.itexture2D,
        PseudoType::itexture2DArray => builtins.itexture2DArray,
        PseudoType::itexture2DMS => builtins.itexture2DMS,
        PseudoType::itexture2DMSArray => builtins.itexture2DMSArray,
        PseudoType::itexture2DRect => builtins.itexture2DRect,
        PseudoType::itexture3D => builtins.itexture3D,
        PseudoType::itextureCube => builtins.itextureCube,
        PseudoType::itextureCubeArray => builtins.itextureCubeArray,
        PseudoType::itextureBuffer => builtins.itextureBuffer,

        PseudoType::utexture1D => builtins.utexture1D,
        PseudoType::utexture1DArray => builtins.utexture1DArray,
        PseudoType::utexture2D => builtins.utexture2D,
        PseudoType::utexture2DArray => builtins.utexture2DArray,
        PseudoType::utexture2DMS => builtins.utexture2DMS,
        PseudoType::utexture2DMSArray => builtins.utexture2DMSArray,
        PseudoType::utexture2DRect => builtins.utexture2DRect,
        PseudoType::utexture3D => builtins.utexture3D,
        PseudoType::utextureCube => builtins.utextureCube,
        PseudoType::utextureCubeArray => builtins.utextureCubeArray,
        PseudoType::utextureBuffer => builtins.utextureBuffer,

        PseudoType::gimage1D => match image_class {
            IC::F => builtins.image1D,
            IC::SI => builtins.iimage1D,
            IC::UI => builtins.uimage1D,
        },
        PseudoType::gimage1DArray => match image_class {
            IC::F => builtins.image1DArray,
            IC::SI => builtins.iimage1DArray,
            IC::UI => builtins.uimage1DArray,
        },
        PseudoType::gimage2D => match image_class {
            IC::F => builtins.image2D,
            IC::SI => builtins.iimage2D,
            IC::UI => builtins.uimage2D,
        },
        PseudoType::gimage2DArray => match image_class {
            IC::F => builtins.image2DArray,
            IC::SI => builtins.iimage2DArray,
            IC::UI => builtins.uimage2DArray,
        },
        PseudoType::gimage2DMS => match image_class {
            IC::F => builtins.image2DMS,
            IC::SI => builtins.iimage2DMS,
            IC::UI => builtins.uimage2DMS,
        },
        PseudoType::gimage2DMSArray => match image_class {
            IC::F => builtins.image2DMSArray,
            IC::SI => builtins.iimage2DMSArray,
            IC::UI => builtins.uimage2DMSArray,
        },
        PseudoType::gimage2DRect => match image_class {
            IC::F => builtins.image2DRect,
            IC::SI => builtins.iimage2DRect,
            IC::UI => builtins.uimage2DRect,
        },
        PseudoType::gimage3D => match image_class {
            IC::F => builtins.image3D,
            IC::SI => builtins.iimage3D,
            IC::UI => builtins.uimage3D,
        },
        PseudoType::gimageCube => match image_class {
            IC::F => builtins.imageCube,
            IC::SI => builtins.iimageCube,
            IC::UI => builtins.uimageCube,
        },
        PseudoType::gimageCubeArray => match image_class {
            IC::F => builtins.imageCubeArray,
            IC::SI => builtins.iimageCubeArray,
            IC::UI => builtins.uimageCubeArray,
        },
        PseudoType::gimageBuffer => match image_class {
            IC::F => builtins.imageBuffer,
            IC::SI => builtins.iimageBuffer,
            IC::UI => builtins.uimageBuffer,
        },

        PseudoType::gtexture1D => match image_class {
            IC::F => builtins.texture1D,
            IC::SI => builtins.itexture1D,
            IC::UI => builtins.utexture1D,
        },
        PseudoType::gtexture1DArray => match image_class {
            IC::F => builtins.texture1DArray,
            IC::SI => builtins.itexture1DArray,
            IC::UI => builtins.utexture1DArray,
        },
        PseudoType::gtexture2D => match image_class {
            IC::F => builtins.texture2D,
            IC::SI => builtins.itexture2D,
            IC::UI => builtins.utexture2D,
        },
        PseudoType::gtexture2DArray => match image_class {
            IC::F => builtins.texture2DArray,
            IC::SI => builtins.itexture2DArray,
            IC::UI => builtins.utexture2DArray,
        },
        PseudoType::gtexture2DMS => match image_class {
            IC::F => builtins.texture2DMS,
            IC::SI => builtins.itexture2DMS,
            IC::UI => builtins.utexture2DMS,
        },
        PseudoType::gtexture2DMSArray => match image_class {
            IC::F => builtins.texture2DMSArray,
            IC::SI => builtins.itexture2DMSArray,
            IC::UI => builtins.utexture2DMSArray,
        },
        PseudoType::gtexture2DRect => match image_class {
            IC::F => builtins.texture2DRect,
            IC::SI => builtins.itexture2DRect,
            IC::UI => builtins.utexture2DRect,
        },
        PseudoType::gtexture3D => match image_class {
            IC::F => builtins.texture3D,
            IC::SI => builtins.itexture3D,
            IC::UI => builtins.utexture3D,
        },
        PseudoType::gtextureCube => match image_class {
            IC::F => builtins.textureCube,
            IC::SI => builtins.itextureCube,
            IC::UI => builtins.utextureCube,
        },
        PseudoType::gtextureCubeArray => match image_class {
            IC::F => builtins.textureCubeArray,
            IC::SI => builtins.itextureCubeArray,
            IC::UI => builtins.utextureCubeArray,
        },
        PseudoType::gtextureBuffer => match image_class {
            IC::F => builtins.textureBuffer,
            IC::SI => builtins.itextureBuffer,
            IC::UI => builtins.utextureBuffer,
        },

        PseudoType::gvec4 => match image_class {
            IC::F => builtins.vec4,
            IC::SI => builtins.ivec4,
            IC::UI => builtins.uvec4,
        },

        PseudoType::subpassInput => {
            todo!()
        }
        PseudoType::subpassInputMS => {
            todo!()
        }
        PseudoType::sampler => builtins.sampler,
        PseudoType::samplerShadow => builtins.samplerShadow,
    }
}

/// Represents a candidate for function (or operator) overload resolution.
#[derive(Clone, Debug)]
pub(crate) struct OverloadCandidate {
    pub(crate) index: usize,
    pub(crate) conversion_ranks: ImplicitConversionRanks,
    pub(crate) parameter_types: SmallVec<[hir::Type; 2]>,
    pub(crate) result_type: hir::Type,
}

/// Helper function for `typecheck_builtin_operation`.
///
/// Signatures of builtins are specified using `PseudoTypes` to make them more compact (TODO: replace with regular TypeData),
/// so we use this function to convert them to `hir::Type`s before calling `check_signature`.
///
/// Returns true if `check_signature` returned an exact match was found, false otherwise.
fn typecheck_builtin_helper(
    module: &hir::Module,
    builtin_types: &BuiltinTypes,
    overload_index: usize,
    parameter_types: &[PseudoType],
    result_type: PseudoType,
    vec_len: u8,
    image_class: ImageClass,
    arguments: &[hir::Type],
    candidates: &mut Vec<OverloadCandidate>,
) -> bool {
    let mut sig: SmallVec<[hir::Type; 2]> = parameter_types
        .iter()
        .map(|ty| pseudo_type_to_concrete_type(*ty, builtin_types, vec_len, image_class))
        .collect();
    let result_type = pseudo_type_to_concrete_type(result_type, builtin_types, vec_len, image_class);
    if let Ok(conv) = check_signature(module, &sig, arguments) {
        let exact_match = conv.iter().all(|x| *x == 0);
        candidates.push(OverloadCandidate {
            index: overload_index,
            result_type,
            parameter_types: sig,
            conversion_ranks: conv,
        });
        if exact_match {
            return true;
        }
    }
    false
}

/// Checks the signatures of a built-in operation.
///
/// # Return value
///
/// The signature of the
pub(super) fn typecheck_builtin_operation(
    module: &hir::Module,
    builtin_types: &BuiltinTypes,
    op: BuiltinOperation,
    arguments: &[hir::Type],
) -> Result<OverloadCandidate, SignatureMismatch> {
    let mut candidates: Vec<OverloadCandidate> = Vec::new();

    'check_signatures: for (index, sig) in BUILTIN_OPERATION_SIGNATURES[op as usize].iter().enumerate() {
        let is_vector_generic = sig.parameter_types.iter().any(|ty| {
            use PseudoType::*;
            matches!(ty, vecN | bvecN | ivecN | uvecN | dvecN)
        });
        let is_image_type_generic = sig.parameter_types.iter().any(|ty| {
            use PseudoType::*;
            matches!(
                ty,
                gimage1D
                    | gimage1DArray
                    | gimage2D
                    | gimage2DArray
                    | gimage2DMS
                    | gimage2DMSArray
                    | gimage2DRect
                    | gimage3D
                    | gimageCube
                    | gimageCubeArray
                    | gimageBuffer
                    | gtexture1D
                    | gtexture1DArray
                    | gtexture2D
                    | gtexture2DArray
                    | gtexture2DMS
                    | gtexture2DMSArray
                    | gtexture2DRect
                    | gtexture3D
                    | gtextureCube
                    | gtextureCubeArray
                    | gtextureBuffer
                    | gvec4
            )
        });
        let max_vec_len = if is_vector_generic {
            4
        } else {
            /*dummy*/
            1
        };
        let image_classes = if is_image_type_generic {
            &[ImageClass::F, ImageClass::SI, ImageClass::UI][..]
        } else {
            /*dummy*/
            &[ImageClass::F][..]
        };

        for ic in image_classes {
            for vec_len in 1..=max_vec_len {

                if typecheck_builtin_helper(
                    module,
                    builtin_types,
                    index,
                    sig.parameter_types,
                    sig.result_type,
                    vec_len,
                    *ic,
                    arguments,
                    &mut candidates,
                ) {
                    break 'check_signatures;
                }
            }
        }
    }


    eprintln!("{op:?} candidates: {:?}", candidates);

    if candidates.is_empty() {
        return Err(SignatureMismatch);
    }

    if candidates.len() == 1 {
        // only one candidate
        Ok(candidates.into_iter().next().unwrap())
    } else {
        // rank candidates
        candidates.sort_by(|a, b| compare_conversion_ranks(&a.conversion_ranks, &b.conversion_ranks));

        // if there's a candidate above all others, select it
        if compare_conversion_ranks(&candidates[0].conversion_ranks, &candidates[1].conversion_ranks) == Ordering::Less
        {
            Ok(candidates.into_iter().next().unwrap())
        } else {
            Err(SignatureMismatch)
        }
    }
}

pub(super) fn lower_builtin_operation(
    ctxt: &LowerCtxt,
    fb: &mut FunctionBuilder,
    op: BuiltinOperation,
    overload_index: usize,
    args: &[IdRef],
    types: &[hir::Type],
    ret_type: hir::Type
) -> Value {
    (BUILTIN_OPERATION_SIGNATURES[op as usize][overload_index].lower)(ctxt, fb, args, types, ret_type)
}

//--------------------------------------------------------------------------------------------------
macro_rules! signatures {
    ($( ($($arg:ident),*) -> $ret_ty:ident => $builder_fn:expr; )*) => {
        &[ $(BuiltinSignature {
            parameter_types: &[$(PseudoType::$arg),*],
            result_type: PseudoType::$ret_ty,
            lower: $builder_fn
        }),* ]
    };
}

//--------------------------------------------------------------------------------------------------
// All built-in functions and their signatures

macro_rules! builtin_operations {
    ( $enum_name:ident, $signatures:ident, $register:ident; $($op_name:ident { $($sig:tt)* })*) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy,Clone,Debug,Eq,PartialEq)]
        pub(super) enum $enum_name {
            $($op_name),*
        }

        pub(super) static $signatures: &[&[BuiltinSignature]] = &[
            $(signatures!($($sig)*)),*
        ];

        pub(super) fn $register(m: &mut Module, scope: &mut Scope) {
            $(
                // FIXME this also registers operators as functions => don't do that
                scope.define_function(std::stringify!($op_name).to_string(), None, FuncRef::Builtin($enum_name::$op_name));
            )*
        }
    };
}

builtin_operations! {
    BuiltinOperation, BUILTIN_OPERATION_SIGNATURES, register_builtin_operations;

    // TODO: implicit conversions could be in this table as well

    //////////////////////////////////////////////////////
    // Operators
    //////////////////////////////////////////////////////
    And {
       (bool,bool)   -> bool  => |_ctxt, fb, args, _types, ret| fb.emit_logical_and(ret, args[0], args[1]);
    }
    Or {
       (bool,bool)   -> bool  => |_ctxt, fb, args, _types, ret| fb.emit_logical_or(ret, args[0], args[1]);
    }
    Eq {
       (vecN,vecN)   -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_equal(ret, args[0], args[1]);
       (dvecN,dvecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_equal(ret, args[0], args[1]);
       (ivecN,ivecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_i_equal(ret, args[0], args[1]);
       (uvecN,uvecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_i_equal(ret, args[0], args[1]);
       (bvecN,bvecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_logical_equal(ret, args[0], args[1]);
    }
    Ne {
       (vecN,vecN)   -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_not_equal(ret, args[0], args[1]);
       (dvecN,dvecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_not_equal(ret, args[0], args[1]);
       (ivecN,ivecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_i_not_equal(ret, args[0], args[1]);
       (uvecN,uvecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_i_not_equal(ret, args[0], args[1]);
       (bvecN,bvecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_logical_not_equal(ret, args[0], args[1]);
    }
    Gt {
        (vecN,vecN)   -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_greater_than(ret, args[0], args[1]);
        (dvecN,dvecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_greater_than(ret, args[0], args[1]);
        (ivecN,ivecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_s_greater_than(ret, args[0], args[1]);
        (uvecN,uvecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_u_greater_than(ret, args[0], args[1]);
    }
    Ge {
        (vecN,vecN)   -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_greater_than_equal(ret, args[0], args[1]);
        (dvecN,dvecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_greater_than_equal(ret, args[0], args[1]);
        (ivecN,ivecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_s_greater_than_equal(ret, args[0], args[1]);
        (uvecN,uvecN) -> bvecN => |_ctxt, fb, args, _types, ret| fb.emit_u_greater_than_equal(ret, args[0], args[1]);
    }
    Lt {
        (vecN,vecN)   -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_less_than(ret, args[0], args[1]);
        (dvecN,dvecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_less_than(ret, args[0], args[1]);
        (ivecN,ivecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_s_less_than(ret, args[0], args[1]);
        (uvecN,uvecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_u_less_than(ret, args[0], args[1]);
    }
    Le {
        (vecN,vecN)   -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_less_than_equal(ret, args[0], args[1]);
        (dvecN,dvecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_less_than_equal(ret, args[0], args[1]);
        (ivecN,ivecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_s_less_than_equal(ret, args[0], args[1]);
        (uvecN,uvecN) -> bvecN  => |_ctxt, fb, args, _types, ret| fb.emit_u_less_than_equal(ret, args[0], args[1]);
    }
    Add {
        (vecN,vecN)   -> vecN   => |_ctxt, fb, args, _types, ret| fb.emit_f_add(ret, args[0], args[1]);
        (dvecN,dvecN) -> dvecN  => |_ctxt, fb, args, _types, ret| fb.emit_f_add(ret, args[0], args[1]);
        (ivecN,ivecN) -> ivecN  => |_ctxt, fb, args, _types, ret| fb.emit_i_add(ret, args[0], args[1]);
        (uvecN,uvecN) -> uvecN  => |_ctxt, fb, args, _types, ret| fb.emit_i_add(ret, args[0], args[1]);
    }
    Mul {
        (vecN,float)   -> vecN  => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[0], args[1]);
        (float,vecN)   -> vecN  => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[1], args[0]);
        (vecN,vecN)   -> vecN   => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);

        (dvecN,double) -> dvecN  => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[0], args[1]);
        (double,dvecN) -> dvecN  => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[1], args[0]);
        (dvecN,dvecN) -> dvecN  => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);

        (int,ivecN) -> ivecN  => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN,int) -> ivecN  => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN,ivecN) -> ivecN  => |_ctxt, fb, args, _types, ret| fb.emit_i_mul(ret, args[0], args[1]);

        (uint,uvecN) -> uvecN  => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN,uint) -> uvecN  => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN,uvecN) -> uvecN  => |_ctxt, fb, args, _types, ret| fb.emit_i_mul(ret, args[0], args[1]);
    }
    Sub {
        (vecN,vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN,dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN,ivecN) -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN,uvecN) -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
    }
    Div {
        (vecN,vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN,dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN,ivecN) -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN,uvecN) -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
    }
    Rem {  }
    Shl {  }
    Shr {  }
    BitXor {  }
    BitOr {  }
    BitAnd {  }
    Not {  }
    Compl {  }
    UnaryMinus {  }

    //////////////////////////////////////////////////////
    // 8.1. Angle and Trigonometry Functions
    //////////////////////////////////////////////////////
    acos { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_acos(ret, args[0]); }
    acosh { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_acosh(ret, args[0]); }
    asin { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_asin(ret, args[0]); }
    asinh { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_asinh(ret, args[0]); }
    atan { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_atan(ret, args[0]);
           (vecN,vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_atan2(ret, args[0], args[1]); }
    atanh { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_atanh(ret, args[0]); }
    cos { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_cos(ret, args[0]); }
    cosh { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_cosh(ret, args[0]); }
    sin { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_sin(ret, args[0]); }
    sinh { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_sinh(ret, args[0]); }
    tan { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_tan(ret, args[0]); }
    tanh { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_tanh(ret, args[0]); }
    radians { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_radians(ret, args[0]); }
    degrees { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_degrees(ret, args[0]); }

    //////////////////////////////////////////////////////
    // 8.2. Exponential Functions
    //////////////////////////////////////////////////////
    pow {
        (vecN, vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_pow(ret, args[0], args[1]);
    }
    exp { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_exp(ret, args[0]); }
    log { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_log(ret, args[0]); }
    exp2 { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_exp2(ret, args[0]); }
    log2 { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_log2(ret, args[0]); }
    sqrt { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_sqrt(ret, args[0]);
           (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_sqrt(ret, args[0]); }
    inversesqrt { (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_inverse_sqrt(ret, args[0]);
                  (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_inverse_sqrt(ret, args[0]); }

    //////////////////////////////////////////////////////
    // 8.3. Common Functions
    //////////////////////////////////////////////////////

    abs {
        (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_abs(ret, args[0]);
        (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_abs(ret, args[0]);
        (ivecN) -> ivecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_s_abs(ret, args[0]);
    }
    sign {
        (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_sign(ret, args[0]);
        (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_sign(ret, args[0]);
        (ivecN) -> ivecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_s_sign(ret, args[0]);
    }
    floor {
        (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_floor(ret, args[0]);
        (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_floor(ret, args[0]);
    }
    trunc {
        (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_trunc(ret, args[0]);
        (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_trunc(ret, args[0]);
    }
    round {
        (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_round(ret, args[0]);
        (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_round(ret, args[0]);
    }
    roundEven {
        (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_round_even(ret, args[0]);
        (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_round_even(ret, args[0]);
    }
    ceil {
        (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_ceil(ret, args[0]);
        (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_ceil(ret, args[0]);
    }
    fract {
        (vecN) -> vecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_fract(ret, args[0]);
        (dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| fb.emit_glsl_fract(ret, args[0]);
    }

    mod_ {
        (vecN,vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN,dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (vecN,float) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN,double) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    modf {
        (vecN) -> modf_result_vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN) -> modf_result_dvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    min {
        (vecN,vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (vecN,float) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN,dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN,double) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN,ivecN) -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN,int) -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN,uvecN) -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN,uint) -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    max {
        (vecN,vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (vecN,float) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN,dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN,double) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN,ivecN) -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN,int) -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN,uvecN) -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN,uint) -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    clamp {
        (vecN, vecN, vecN)      -> vecN  => |_ctxt, fb, args, _types, ret| todo!();
        (vecN, float, float)    -> vecN  => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, dvecN, dvecN)   -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, double, double) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN, ivecN, ivecN)   -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN, int, int)       -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN, uvecN, uvecN)   -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN, uint, uint)     -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    mix {
        (vecN, vecN, vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (vecN, vecN, float) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, dvecN, dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, dvecN, double) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (vecN, vecN, bvecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, dvecN, bvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (ivecN, ivecN, bvecN) -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
        (uvecN, uvecN, bvecN) -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
        (bvecN, bvecN, bvecN) -> bvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    step {
        (vecN, vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (float, vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (double, dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    smoothstep {
        (vecN, vecN, vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (float, float, vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, dvecN, dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
        (double, double, dvecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    isnan {
        (vecN) -> bvecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN) -> bvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    isinf {
        (vecN) -> bvecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN) -> bvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    floatBitsToInt {
        (highp_vecN) -> ivecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    floatBitsToUint {
        (highp_vecN) -> uvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    intBitsToFloat {
        (highp_ivecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    uintBitsToFloat {
        (uvecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    fma {
        (vecN, vecN, vecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, dvecN, dvecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    frexp {
        (highp_vecN) -> frexp_result_highp_vecN => |_ctxt, fb, args, _types, ret| todo!();
        //(dvecN) -> frexp_result_dvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    ldexp {
        (highp_vecN, highp_ivecN) -> vecN => |_ctxt, fb, args, _types, ret| todo!();
        (dvecN, ivecN) -> dvecN => |_ctxt, fb, args, _types, ret| todo!();
    }

    //////////////////////////////////////////////////////
    // 8.5. Geometric Functions
    //////////////////////////////////////////////////////
    length {
        (vecN) -> float => |_ctxt, fb, args, _types, ret| fb.emit_glsl_length(ret, args[0]);
        (dvecN) -> double => |_ctxt, fb, args, _types, ret| fb.emit_glsl_length(ret, args[0]);
    }

    distance {
        (vecN,vecN) -> float => |_ctxt, fb, args, _types, ret| fb.emit_glsl_distance(ret, args[0], args[1]);
        (dvecN,dvecN) -> double => |_ctxt, fb, args, _types, ret| fb.emit_glsl_distance(ret, args[0], args[1]);
    }

    // TODO

    //////////////////////////////////////////////////////
    // 8.6. Matrix functions
    //////////////////////////////////////////////////////
    matrixCompMult {
        (mat2x2,mat2x2) -> mat2x2 => lower_matrix_comp_mult;
        (mat2x3,mat2x3) -> mat2x3 => lower_matrix_comp_mult;
        (mat2x4,mat2x4) -> mat2x4 => lower_matrix_comp_mult;
        (mat3x2,mat3x2) -> mat3x2 => lower_matrix_comp_mult;
        (mat3x3,mat3x3) -> mat3x3 => lower_matrix_comp_mult;
        (mat3x4,mat3x4) -> mat3x4 => lower_matrix_comp_mult;
        (mat4x2,mat4x2) -> mat4x2 => lower_matrix_comp_mult;
        (mat4x3,mat4x3) -> mat4x3 => lower_matrix_comp_mult;
        (mat4x4,mat4x4) -> mat4x4 => lower_matrix_comp_mult;
        (dmat2x2,dmat2x2) -> dmat2x2 => lower_matrix_comp_mult;
        (dmat2x3,dmat2x3) -> dmat2x3 => lower_matrix_comp_mult;
        (dmat2x4,dmat2x4) -> dmat2x4 => lower_matrix_comp_mult;
        (dmat3x2,dmat3x2) -> dmat3x2 => lower_matrix_comp_mult;
        (dmat3x3,dmat3x3) -> dmat3x3 => lower_matrix_comp_mult;
        (dmat3x4,dmat3x4) -> dmat3x4 => lower_matrix_comp_mult;
        (dmat4x2,dmat4x2) -> dmat4x2 => lower_matrix_comp_mult;
        (dmat4x3,dmat4x3) -> dmat4x3 => lower_matrix_comp_mult;
        (dmat4x4,dmat4x4) -> dmat4x4 => lower_matrix_comp_mult;
    }


    //////////////////////////////////////////////////////
    // 8.9. Texture Functions
    // (this is not strictly GLSL, since textures & samplers are separated)
    //////////////////////////////////////////////////////
    textureSample {
        (gtexture1D, sampler, float) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();
        (gtexture2D, sampler, vec2) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();         // coords
        (gtexture2D, sampler, vec2, ivec2) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();   // coords,offset
        (gtexture2DArray, sampler, vec2, int) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();   // coords,array_index
        (gtexture2DArray, sampler, vec2, uint) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();   // coords,array_index
        (gtexture2DArray, sampler, vec2, int, ivec2) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();   // coords,array_index,offset
        (gtexture2DArray, sampler, vec2, uint, ivec2) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();   // coords,array_index,offset

        (gtexture3D, sampler, vec3) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();
        (gtextureCube, sampler, vec3) -> gvec4 => |_ctxt, fb, args, _types, ret| todo!();
    }

    //////////////////////////////////////////////////////
    // 8.12. Image Functions
    //////////////////////////////////////////////////////
    imageSize {
         (gimage1D)        -> int    => |_ctxt, fb, args, _types, ret| todo!();
         (gimage2D)        -> ivec2  => |_ctxt, fb, args, _types, ret| todo!();
         (gimage3D)        -> ivec3  => |_ctxt, fb, args, _types, ret| todo!();
         (gimageCube)      -> ivec2  => |_ctxt, fb, args, _types, ret| todo!();
         (gimageCubeArray) -> ivec3  => |_ctxt, fb, args, _types, ret| todo!();
         (gimage2DArray)   -> ivec3  => |_ctxt, fb, args, _types, ret| todo!();
         (gimage1DArray)   -> ivec2  => |_ctxt, fb, args, _types, ret| todo!();
         (gimage2DMS)      -> ivec2  => |_ctxt, fb, args, _types, ret| todo!();
         (gimage2DMSArray) -> ivec3  => |_ctxt, fb, args, _types, ret| todo!();
         (gimageBuffer)    -> int    => |_ctxt, fb, args, _types, ret| todo!();
    }

    //////////////////////////////////////////////////////
    // 5.4. Constructors
    //////////////////////////////////////////////////////
    vec4 {
        (float) -> vec4 => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        (float,float,float,float) -> vec4 => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);

        (vec2, float, float) -> vec4 => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        (float, vec2, float) -> vec4 => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        (float, float, vec2) -> vec4 => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);

        (vec3, float) -> vec4 => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        (float, vec3) -> vec4 => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);

        (vec4) -> vec4 => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
    }
}

/*fn lower_logical_and(
    ctxt: &LowerCtxt,
    fb: &mut FunctionBuilder,
    lhs: &[TypedValue],
) -> hir::Value {
    fb.emit_logical_and(lhs.ty, lhs.val, rhs.val)
}*/

fn lower_matrix_comp_mult(
    ctxt: &LowerCtxt,
    fb: &mut FunctionBuilder,
    args: &[IdRef],
    types: &[hir::Type],
    result_type: hir::Type,
) -> hir::Value {
    let lhs = args[0];
    let rhs = args[1];
    let lhs_ty = types[0];
    let rhs_ty = types[1];
    let TypeData::Matrix { component_type, columns, rows } = fb.types[lhs_ty] else {
        panic!("invalid operands to lower_matrix_comp_mult")
    };
    let column_ty = match (component_type, rows) {
        (ScalarType::Float, 2) => ctxt.types.vec2,
        (ScalarType::Float, 3) => ctxt.types.vec3,
        (ScalarType::Float, 4) => ctxt.types.vec4,
        (ScalarType::Double, 2) => ctxt.types.dvec2,
        (ScalarType::Double, 3) => ctxt.types.dvec3,
        (ScalarType::Double, 4) => ctxt.types.dvec4,
        _ => panic!("invalid vector size"),
    };

    match columns {
        2 => {
            let a0 = fb.emit_composite_extract(column_ty, lhs, &[0]);
            let b0 = fb.emit_composite_extract(column_ty, rhs, &[0]);
            let c0 = fb.emit_f_mul(column_ty, a0, b0);
            let a1 = fb.emit_composite_extract(column_ty, lhs, &[1]);
            let b1 = fb.emit_composite_extract(column_ty, rhs, &[1]);
            let c1 = fb.emit_f_mul(column_ty, a1, b1);
            fb.emit_composite_construct(result_type, &[c0.into(), c1.into()])
        }
        3 => {
            let a0 = fb.emit_composite_extract(column_ty, lhs, &[0]);
            let b0 = fb.emit_composite_extract(column_ty, rhs, &[0]);
            let c0 = fb.emit_f_mul(column_ty, a0, b0);
            let a1 = fb.emit_composite_extract(column_ty, lhs, &[1]);
            let b1 = fb.emit_composite_extract(column_ty, rhs, &[1]);
            let c1 = fb.emit_f_mul(column_ty, a1, b1);
            let a2 = fb.emit_composite_extract(column_ty, lhs, &[2]);
            let b2 = fb.emit_composite_extract(column_ty, rhs, &[2]);
            let c2 = fb.emit_f_mul(column_ty, a2, b2);
            fb.emit_composite_construct(result_type, &[c0.into(), c1.into(), c2.into()])
        }
        4 => {
            let a0 = fb.emit_composite_extract(column_ty, lhs, &[0]);
            let b0 = fb.emit_composite_extract(column_ty, rhs, &[0]);
            let c0 = fb.emit_f_mul(column_ty, a0, b0);
            let a1 = fb.emit_composite_extract(column_ty, lhs, &[1]);
            let b1 = fb.emit_composite_extract(column_ty, rhs, &[1]);
            let c1 = fb.emit_f_mul(column_ty, a1, b1);
            let a2 = fb.emit_composite_extract(column_ty, lhs, &[2]);
            let b2 = fb.emit_composite_extract(column_ty, rhs, &[2]);
            let c2 = fb.emit_f_mul(column_ty, a2, b2);
            let a3 = fb.emit_composite_extract(column_ty, lhs, &[3]);
            let b3 = fb.emit_composite_extract(column_ty, rhs, &[3]);
            let c3 = fb.emit_f_mul(column_ty, a3, b3);
            fb.emit_composite_construct(result_type, &[c0.into(), c1.into(), c2.into(), c3.into()])
        }
        _ => panic!("invalid vector size"),
    }
}
