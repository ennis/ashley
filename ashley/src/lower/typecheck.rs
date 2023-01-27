//! Type-checking rules for built-in operations

use crate::{
    hir,
    hir::{types::ScalarType, Module, TypeData},
    lower::{builtin::BuiltinTypes, LowerCtxt},
    syntax::LogicOp::Or,
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
#[derive(Copy, Clone, Debug)]
pub(super) struct BuiltinSignature {
    parameter_types: &'static [PseudoType],
    result_type: PseudoType,
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
    if arguments.len() != signature.len() {
        // return early on arg count mismatch
        return Err(SignatureMismatch);
    }

    let mut conversion_ranks = SmallVec::new();
    for (sigty, argty) in signature.iter().zip(arguments.iter()) {
        if sigty == argty {
            // direct match, no conversion necessary
            conversion_ranks.push(0);
        }

        // check type of argument, if it doesn't work, retry with an implicit conversion
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
            (TypeData::Matrix(targ, r1, c1), TypeData::Matrix(tsig, r2, c2)) if r1 == r2 && c1 == c2 => {
                match (targ, tsig) {
                    (ScalarType::Float, ScalarType::Double) => 1,
                    _ => {
                        break;
                    }
                }
            }
            _ => break,
        };
        conversion_ranks.push(conversion_rank);
    }

    if conversion_ranks.len() != signature.len() {
        // we exited the loop early, meaning that we failed to match an argument
        return Err(SignatureMismatch);
    }

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
            2 => builtins.vec2,
            3 => builtins.vec3,
            4 => builtins.vec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::dvecN => match vec_len {
            2 => builtins.dvec2,
            3 => builtins.dvec3,
            4 => builtins.dvec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::ivecN => match vec_len {
            2 => builtins.ivec2,
            3 => builtins.ivec3,
            4 => builtins.ivec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::uvecN => match vec_len {
            2 => builtins.uvec2,
            3 => builtins.uvec3,
            4 => builtins.uvec4,
            _ => panic!("invalid vector length"),
        },
        PseudoType::bvecN => match vec_len {
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
#[derive(Clone)]
pub(crate) struct OverloadCandidate {
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
        if conv.iter().all(|x| *x == 0) {
            return true;
        }
        candidates.push(OverloadCandidate {
            result_type,
            parameter_types: sig,
            conversion_ranks: conv,
        });
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

//--------------------------------------------------------------------------------------------------
macro_rules! signatures {
    ($( ($($arg:ident),*) -> $ret_ty:ident $(=> $builder_func:ident)? ; )*) => {
        &[ $(BuiltinSignature { parameter_types: &[$(PseudoType::$arg),*], result_type: PseudoType::$ret_ty }),* ]
    };
}

//--------------------------------------------------------------------------------------------------
// All built-in functions and their signatures

macro_rules! builtin_operations {
    ( $enum_name:ident, $signatures:ident; $($op_name:ident { $($sig:tt)* })*) => {
        #[allow(non_camel_case_types)]
        #[derive(Copy,Clone,Debug,Eq,PartialEq)]
        pub(super) enum $enum_name {
            $($op_name),*
        }

        pub(super) static $signatures: &[&[BuiltinSignature]] = &[
            $(signatures!($($sig)*)),*
        ];
    };
}

builtin_operations! {
    BuiltinOperation, BUILTIN_OPERATION_SIGNATURES;

    //////////////////////////////////////////////////////
    // Operators
    //////////////////////////////////////////////////////
    And {
       (bool,bool)   -> bool  => emit_logical_and;
    }
    Or {
       (bool,bool)   -> bool  => emit_logical_or;
    }
    Eq {
       (vecN,vecN)   -> vecN  => emit_f_ord_equal;
       (dvecN,dvecN) -> dvecN => emit_f_ord_equal;
       (ivecN,ivecN) -> ivecN => emit_i_equal;
       (uvecN,uvecN) -> uvecN => emit_u_equal;
       (bvecN,uvecN) -> bvecN => emit_logical_equal;
    }
    Ne {
       (vecN,vecN)   -> vecN  => emit_f_ord_not_equal;
       (dvecN,dvecN) -> dvecN => emit_f_ord_not_equal;
       (ivecN,ivecN) -> ivecN => emit_i_not_equal;
       (uvecN,uvecN) -> uvecN => emit_i_not_equal;
       (bvecN,uvecN) -> bvecN => emit_logical_not_equal;
    }
    Gt {
        (vecN,vecN)   -> vecN   => emit_f_ord_greater_than;
        (dvecN,dvecN) -> dvecN  => emit_f_ord_greater_than;
        (ivecN,ivecN) -> ivecN  => emit_s_greater_than;
        (uvecN,uvecN) -> uvecN  => emit_u_greater_than;
    }
    Ge {
        (vecN,vecN)   -> vecN  => emit_f_ord_greater_than_equal;
        (dvecN,dvecN) -> dvecN => emit_f_ord_greater_than_equal;
        (ivecN,ivecN) -> ivecN => emit_s_greater_than_equal;
        (uvecN,uvecN) -> uvecN => emit_u_greater_than_equal;
    }
    Lt {
        (vecN,vecN)   -> vecN   => emit_f_ord_less_than;
        (dvecN,dvecN) -> dvecN  => emit_f_ord_less_than;
        (ivecN,ivecN) -> ivecN  => emit_s_less_than;
        (uvecN,uvecN) -> uvecN  => emit_u_less_than;
    }
    Le {
        (vecN,vecN)   -> vecN   => emit_f_ord_less_than_equal;
        (dvecN,dvecN) -> dvecN  => emit_f_ord_less_than_equal;
        (ivecN,ivecN) -> ivecN  => emit_s_less_than_equal;
        (uvecN,uvecN) -> uvecN  => emit_u_less_than_equal;
    }
    Add {
        (vecN,vecN)   -> vecN   => emit_f_add;
        (dvecN,dvecN) -> dvecN  => emit_f_add;
        (ivecN,ivecN) -> ivecN  => emit_i_add;
        (uvecN,uvecN) -> uvecN  => emit_i_add;
    }
    Mul {
        (vecN,vecN)   -> vecN   => emit_f_mul;
        (dvecN,dvecN) -> dvecN  => emit_f_mul;
        (ivecN,ivecN) -> ivecN  => emit_i_mul;
        (uvecN,uvecN) -> uvecN  => emit_i_mul;
    }
    Sub {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
    }
    Div {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
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
    acos { (vecN) -> vecN => emit_glsl_cos; }
    acosh { (vecN) -> vecN => emit_glsl_acosh; }
    asin { (vecN) -> vecN => emit_glsl_asin; }
    asinh { (vecN) -> vecN => emit_glsl_asinh; }
    atan { (vecN) -> vecN => emit_glsl_atan;
           (vecN,vecN) -> vecN => emit_glsl_atan2; }
    atanh { (vecN) -> vecN => emit_glsl_atanh; }
    cos { (vecN) -> vecN => emit_glsl_cos; }
    cosh { (vecN) -> vecN => emit_glsl_cosh; }
    sin { (vecN) -> vecN => emit_glsl_sin; }
    sinh { (vecN) -> vecN => emit_glsl_sinh; }
    tan { (vecN) -> vecN => emit_glsl_tan; }
    tanh { (vecN) -> vecN => emit_glsl_tanh; }
    radians { (vecN) -> vecN => emit_glsl_radians; }
    degrees { (vecN) -> vecN => emit_glsl_degrees; }

    //////////////////////////////////////////////////////
    // 8.2. Exponential Functions
    //////////////////////////////////////////////////////
    pow { (vecN) -> vecN => emit_glsl_pow; }
    exp { (vecN) -> vecN => emit_glsl_exp; }
    log { (vecN) -> vecN => emit_glsl_log; }
    exp2 { (vecN) -> vecN => emit_glsl_exp2; }
    log2 { (vecN) -> vecN => emit_glsl_log2; }
    sqrt { (vecN) -> vecN => emit_glsl_sqrt;
           (dvecN) -> dvecN => emit_glsl_sqrt; }
    inversesqrt { (vecN) -> vecN => emit_glsl_inversesqrt;
                  (dvecN) -> dvecN => emit_glsl_inversesqrt; }

    //////////////////////////////////////////////////////
    // 8.3. Common Functions
    //////////////////////////////////////////////////////

    abs { (vecN) -> vecN;
          (dvecN) -> dvecN;
          (ivecN) -> ivecN; }
    sign { (vecN) -> vecN;
           (dvecN) -> dvecN;
           (ivecN) -> ivecN; }
    floor { (vecN) -> vecN;
           (dvecN) -> dvecN; }
    trunc { (vecN) -> vecN;
            (dvecN) -> dvecN; }
    round { (vecN) -> vecN;
            (dvecN) -> dvecN; }
    roundEven { (vecN) -> vecN;
                (dvecN) -> dvecN; }
    ceil { (vecN) -> vecN;
           (dvecN) -> dvecN; }
    fract { (vecN) -> vecN;
            (dvecN) -> dvecN;  }

    mod_ { (vecN,vecN) -> vecN;
           (dvecN,dvecN) -> dvecN;
           (vecN,float) -> vecN;
           (dvecN,double) -> dvecN; }

    modf { (vecN) -> modf_result_vecN;
           (dvecN) -> modf_result_dvecN; }

    min { (vecN,vecN) -> vecN;
          (vecN,float) -> vecN;
          (dvecN,dvecN) -> dvecN;
          (dvecN,double) -> dvecN;
          (ivecN,ivecN) -> ivecN;
          (ivecN,int) -> ivecN;
          (uvecN,uvecN) -> uvecN;
          (uvecN,uint) -> uvecN; }

    max { (vecN,vecN) -> vecN;
          (vecN,float) -> vecN;
          (dvecN,dvecN) -> dvecN;
          (dvecN,double) -> dvecN;
          (ivecN,ivecN) -> ivecN;
          (ivecN,int) -> ivecN;
          (uvecN,uvecN) -> uvecN;
          (uvecN,uint) -> uvecN; }

    clamp {
        (vecN, vecN, vecN)      -> vecN  => emit_glsl_f_clamp;
        (vecN, float, float)    -> vecN  => emit_glsl_f_clamp_vector_scalar;
        (dvecN, dvecN, dvecN)   -> dvecN => emit_glsl_f_clamp;
        (dvecN, double, double) -> dvecN => emit_glsl_f_clamp_vector_scalar;
        (ivecN, ivecN, ivecN)   -> ivecN => emit_glsl_s_clamp;
        (ivecN, int, int)       -> ivecN => emit_glsl_s_clamp_vector_scalar;
        (uvecN, uvecN, uvecN)   -> uvecN => emit_glsl_u_clamp;
        (uvecN, uint, uint)     -> uvecN => emit_glsl_u_clamp_vector_scalar;
    }

    mix {
        (vecN, vecN, vecN) -> vecN;
        (vecN, vecN, float) -> vecN;
        (dvecN, dvecN, dvecN) -> dvecN;
        (dvecN, dvecN, double) -> dvecN;
        (vecN, vecN, bvecN) -> vecN;
        (dvecN, dvecN, bvecN) -> dvecN;
        (ivecN, ivecN, bvecN) -> ivecN;
        (uvecN, uvecN, bvecN) -> uvecN;
        (bvecN, bvecN, bvecN) -> bvecN;
    }

    step {
        (vecN, vecN) -> vecN;
        (float, vecN) -> vecN;
        (dvecN, dvecN) -> dvecN;
        (double, dvecN) -> dvecN;
    }

    smoothstep {
        (vecN, vecN, vecN) -> vecN;
        (float, float, vecN) -> vecN;
        (dvecN, dvecN, dvecN) -> dvecN;
        (double, double, dvecN) -> dvecN;
    }

    isnan {
        (vecN) -> bvecN;
        (dvecN) -> bvecN;
    }

    isinf {
        (vecN) -> bvecN;
        (dvecN) -> bvecN;
    }

    floatBitsToInt {
        (highp_vecN) -> ivecN;
    }

    floatBitsToUint {
        (highp_vecN) -> uvecN;
    }

    intBitsToFloat {
        (highp_ivecN) -> vecN;
    }

    uintBitsToFloat {
        (uvecN) -> vecN;
    }

    fma {
        (vecN, vecN, vecN) -> vecN;
        (dvecN, dvecN, dvecN) -> vecN;
    }

    frexp {
        (highp_vecN) -> frexp_result_highp_vecN;
        //(dvecN) -> frexp_result_dvecN;
    }

    ldexp {
        (highp_vecN, highp_ivecN) -> vecN;
        (dvecN, ivecN) -> dvecN;
    }


    //////////////////////////////////////////////////////
    // 8.9. Texture Functions
    // (this is not strictly GLSL, since textures & samplers are separated)
    //////////////////////////////////////////////////////
    textureSample {
        (gtexture1D, sampler, float) -> gvec4;
        (gtexture2D, sampler, vec2) -> gvec4;         // coords
        (gtexture2D, sampler, vec2, ivec2) -> gvec4;   // coords,offset
        (gtexture2DArray, sampler, vec2, int) -> gvec4;   // coords,array_index
        (gtexture2DArray, sampler, vec2, uint) -> gvec4;   // coords,array_index
        (gtexture2DArray, sampler, vec2, int, ivec2) -> gvec4;   // coords,array_index,offset
        (gtexture2DArray, sampler, vec2, uint, ivec2) -> gvec4;   // coords,array_index,offset

        (gtexture3D, sampler, vec3) -> gvec4;
        (gtextureCube, sampler, vec3) -> gvec4;
    }

    //////////////////////////////////////////////////////
    // 8.12. Image Functions
    //////////////////////////////////////////////////////
    imageSize {
         (gimage1D)        -> int    => TODO;
         (gimage2D)        -> ivec2  => TODO;
         (gimage3D)        -> ivec3  => TODO;
         (gimageCube)      -> ivec2  => TODO;
         (gimageCubeArray) -> ivec3  => TODO;
         (gimage2DArray)   -> ivec3  => TODO;
         (gimage1DArray)   -> ivec2  => TODO;
         (gimage2DMS)      -> ivec2  => TODO;
         (gimage2DMSArray) -> ivec3  => TODO;
         (gimageBuffer)    -> int    => TODO;
    }
}
