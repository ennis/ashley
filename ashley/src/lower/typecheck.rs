//! Type-checking rules for built-in operations

use crate::{
    hir,
    hir::{types::ScalarType, Module, TypeData},
    lower::{builtin::BuiltinTypes, LowerCtxt},
};
use smallvec::SmallVec;


/// Pseudo-type used in signatures.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum PseudoType {
    Unit,
    Float,
    Double,
    Int,
    Uint,
    VecN,
    DVecN,
    IVecN,
    UVecN,
    BVecN,
    Bool,
    HighpVecN,
    HighpIVecN,
    ModfResultVecN,
    FrexpResultHighpVecN,
}

/// Signature of a built-in operation (function or operator).
#[derive(Copy, Clone, Debug)]
struct BuiltinSignature {
    parameter_types: &'static [PseudoType],
    result_type: PseudoType,
}

/*/// Signature of a function or operator.
struct Signature<'a> {
    argument_types: &'a [hir::Type],
    result_type: hir::Type,
}*/

/// Error returned by `check_signature`
#[derive(Debug)]
struct SignatureMismatch;

type ImplicitConversionRanks = SmallVec<[i32; 2]>;

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
pub(crate) fn check_signature(
    m: &hir::Module,
    signature: &[hir::TypeData],
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
        let conversion_rank = match (sigty, &m.types[*argty]) {
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

macro_rules! pseudo_type {
    (void) => {
        PseudoType::Unit
    };
    (float) => {
        PseudoType::Float
    };
    (double) => {
        PseudoType::Double
    };
    (int) => {
        PseudoType::Int
    };
    (uint) => {
        PseudoType::Uint
    };
    (vecN) => {
        PseudoType::VecN
    };
    (dvecN) => {
        PseudoType::DVecN
    };
    (ivecN) => {
        PseudoType::IVecN
    };
    (uvecN) => {
        PseudoType::UVecN
    };
    (bvecN) => {
        PseudoType::BVecN
    };
    (bool) => {
        PseudoType::Bool
    };
    (highp_vecN) => {
        PseudoType::HighpVecN
    };
    (highp_ivecN) => {
        PseudoType::HighpIVecN
    };
    (modf_result_vecN) => {
        PseudoType::ModfResultVecN
    };
    (frexp_result_highp_vecN) => {
        PseudoType::FrexpResultHighpVecN
    };
    (AbstractInt) => {
        PseudoType::AbstractInt
    };
    (AbstractFloat) => {
        PseudoType::AbstractFloat
    };
}

macro_rules! signatures {
    ($( ($($arg:ident),*) -> $ret_ty:ident ; )*) => {
        &[ $(BuiltinSignature { params: &[$(pseudo_type!($arg)),*], ret_ty: pseudo_type!($ret_ty) }),* ]
    };
}

//--------------------------------------------------------------------------------------------------
// All built-in functions and their signatures

macro_rules! builtin_operations {
    ( $enum_name:ident, $signatures:ident; $($op_name:ident { $($sig:tt)* })*) => {
        #[derive(Copy,Clone,Debug,Eq,PartialEq)]
        pub enum $enum_name {
            $($op_name),*
        }

        pub static $signatures: &[&[BuiltinSignature]] = &[
            $(signatures!($($sig)*)),*
        ];
    };
}

pub(crate) struct SignatureMatch {
    pub(crate) parameter_types: SmallVec<[hir::Type; 2]>,
    pub(crate) result_type: hir::Type,
}

fn pseudo_type_to_concrete_type(pseudo_type: PseudoType, vec_len: u8) -> hir::TypeData {
    match pseudo_type {
        PseudoType::Unit => TypeData::Unit,
        PseudoType::Float => TypeData::Scalar(ScalarType::Float),
        PseudoType::Double => TypeData::Scalar(ScalarType::Double),
        PseudoType::Int => TypeData::Scalar(ScalarType::Int),
        PseudoType::Uint => TypeData::Scalar(ScalarType::UnsignedInt),
        PseudoType::Bool => TypeData::Scalar(ScalarType::Bool),
        PseudoType::VecN => TypeData::Vector(ScalarType::Float, vec_len),
        PseudoType::DVecN => TypeData::Vector(ScalarType::Double, vec_len),
        PseudoType::IVecN => TypeData::Vector(ScalarType::Int, vec_len),
        PseudoType::UVecN => TypeData::Vector(ScalarType::UnsignedInt, vec_len),
        PseudoType::BVecN => TypeData::Vector(ScalarType::Bool, vec_len),
        PseudoType::HighpVecN => {
            todo!()
        }
        PseudoType::HighpIVecN => {
            todo!()
        }
        PseudoType::ModfResultVecN => {
            todo!()
        }
        PseudoType::FrexpResultHighpVecN => {
            todo!()
        }
    }
}

/// Helper function for `typecheck_builtin_operation`
///
/// Returns true if an exact match was found, false otherwise.
fn typecheck_builtin_helper(
    m: &hir::Module,
    index: usize,
    parameter_types: &[PseudoType],
    vec_len: u8,
    arguments: &[hir::Type],
    candidates: &mut Vec<(usize, ImplicitConversionRanks)>,
) -> bool {
    let mut sig: SmallVec<[TypeData; 2]> = parameter_types
        .iter()
        .map(|ty| pseudo_type_to_concrete_type(*ty, vec_len))
        .collect();
    if let Ok(conv) = check_signature(m, &sig, arguments) {
        if conv.iter().all(|x| x == 0) {
            return true;
        }
        candidates.push((index, conv));
    }
    false
}

/// Checks the signatures of a built-in operation.
///
/// # Return value
///
/// The signature of the
pub(crate) fn typecheck_builtin_operation(
    ctxt: &LowerCtxt,
    op: BuiltinOperation,
    arguments: &[hir::Type],
) -> Result<SignatureMatch, SignatureMismatch> {
    let mut candidates: Vec<(usize, ImplicitConversionRanks)> = Vec::new();

    for (index, sig) in BUILTIN_OPERATION_SIGNATURES[op as usize].iter().enumerate() {
        let is_vector_generic = sig.parameter_types.iter().any(|ty| {
            matches!(
                ty,
                PseudoType::VecN | PseudoType::BVecN | PseudoType::IVecN | PseudoType::UVecN | PseudoType::DVecN
            )
        });
        if is_vector_generic {
            for vec_len in 1..=4 {
                if typecheck_builtin_helper(ctxt.m, index, sig.parameter_types, vec_len, arguments, &mut candidates) {
                    // exact match found, exit early
                    return Ok(SignatureMatch {
                        parameter_types: Default::default(),
                        result_type: ctxt.types.void,
                    });
                }
            }
        } else {
            let mut sig: SmallVec<[TypeData; 2]> = sig
                .parameter_types
                .iter()
                .map(|ty| pseudo_type_to_concrete_type(*ty, 1))
                .collect();
            if let Ok(conv) = check_signature(ctxt.m, &sig, arguments) {
                candidates.push((index, conv));
            }
        }
    }

    if candidates.is_empty() {
        return Err(SignatureMismatch);
    }

    // rank candidates
    if candidates.len() > 1 {
        candidates.sort_by(|(_, rank_a), (_, rank_b)| rank_a.cmp(rank_b));
        if candidates[0] == candidates[1] {}
    }

    todo!();

    Ok(SignatureMatch {
        parameter_types: Default::default(),
        result_type: ctxt.types.void,
    })
}

builtin_operations! {
    BuiltinOperation, BUILTIN_OPERATION_SIGNATURES;

    //////////////////////////////////////////////////////
    // Operators
    //////////////////////////////////////////////////////
    And { (bool,bool) -> bool; }
    Or { (bool,bool) -> bool; }
    Eq {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
        (bvecN,uvecN) -> bvecN;
    }
    Ne {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
        (bvecN,uvecN) -> bvecN;
    }
    Gt {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
    }
    Ge {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
    }
    Lt {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
    }
    Le {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
    }
    Add {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;

    }
    Mul {
        (vecN,vecN) -> vecN;
        (dvecN,dvecN) -> dvecN;
        (ivecN,ivecN) -> ivecN;
        (uvecN,uvecN) -> uvecN;
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
    acos { (vecN) -> vecN; }
    acosh { (vecN) -> vecN; }
    asin { (vecN) -> vecN; }
    asinh { (vecN) -> vecN; }
    atan { (vecN) -> vecN;
           (vecN,vecN) -> vecN; }
    atanh { (vecN) -> vecN; }
    cos { (vecN) -> vecN; }
    cosh { (vecN) -> vecN; }
    sin { (vecN) -> vecN; }
    sinh { (vecN) -> vecN; }
    tan { (vecN) -> vecN; }
    tanh { (vecN) -> vecN; }
    radians { (vecN) -> vecN; }
    degrees { (vecN) -> vecN; }

    //////////////////////////////////////////////////////
    // 8.2. Exponential Functions
    //////////////////////////////////////////////////////
    pow { (vecN) -> vecN; }
    exp { (vecN) -> vecN; }
    log { (vecN) -> vecN; }
    exp2 { (vecN) -> vecN; }
    log2 { (vecN) -> vecN; }
    sqrt { (vecN) -> vecN;
           (dvecN) -> dvecN; }
    inversesqrt { (vecN) -> vecN;
                  (dvecN) -> dvecN; }

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
        (vecN, vecN, vecN) -> vecN;
        (vecN, float, float) -> vecN;
        (dvecN, dvecN, dvecN) -> dvecN;
        (dvecN, double, double) -> dvecN;
        (ivecN, ivecN, ivecN) -> ivecN;
        (ivecN, int, int) -> ivecN;
        (uvecN, uvecN, uvecN) -> uvecN;
        (uvecN, uint, uint) -> uvecN;
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
        (highp_uvecN) -> vecN;
    }

    fma {
        (vecN, vecN, vecN) -> vecN;
        (dvecN, dvecN, dvecN) -> vecN;
    }

    frexp {
        (highp_vecN) -> frexp_result_highp_vecN;
        (dvecN) -> frexp_result_dvecN;
    }

    ldexp {
        (highp_vecN, highp_ivecN) -> vecN;
        (dvecN, ivecN) -> dvecN;
    }
}
