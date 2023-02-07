//! Overload resolution.
use crate::{
    builtins::{BuiltinOperation, BuiltinTypes, PseudoType},
    tast::{DefId, ScalarType, Type, TypeKind},
};
use ashley::tast::TypeCheckBodyCtxt;
use smallvec::SmallVec;
use std::{cmp::Ordering, ops::Deref};
use crate::tast::TypeCheckCtxt;

/// Error returned by `check_signature`
#[derive(Debug)]
pub(super) struct SignatureMismatch;

type ImplicitConversionRanks = SmallVec<[i32; 2]>;

/// Compares the conversion ranks of two overloads
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

/// Resolves the preferred overload from a list of conversion ranks.
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
pub(crate) fn check_signature(
    signature: &[Type],
    arguments: &[Type],
) -> Result<ImplicitConversionRanks, SignatureMismatch> {
    use TypeKind as TK;

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
        let conversion_rank = match (sigty, argty) {
            // Source type => implicitly converts to
            (TK::Scalar(targ), TK::Scalar(tsig)) => match (targ, tsig) {
                (ScalarType::Int, ScalarType::UnsignedInt) => 1,
                (ScalarType::UnsignedInt | ScalarType::Int, ScalarType::Float) => 2,
                (ScalarType::Int | ScalarType::UnsignedInt | ScalarType::Float, ScalarType::Double) => 3,
                _ => {
                    break;
                }
            },
            (TK::Vector(targ, n2), TK::Vector(tsig, n1)) if n1 == n2 => match (targ, tsig) {
                (ScalarType::Int, ScalarType::UnsignedInt) => 1,
                (ScalarType::UnsignedInt | ScalarType::Int, ScalarType::Float) => 2,
                (ScalarType::Int | ScalarType::UnsignedInt | ScalarType::Float, ScalarType::Double) => 3,
                _ => {
                    break;
                }
            },
            (
                TK::Matrix {
                    component_type: targ,
                    rows: r1,
                    columns: c1,
                },
                TK::Matrix {
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

/// Represents a candidate for function (or operator) overload resolution.
#[derive(Clone, Debug)]
pub(crate) struct OverloadCandidate {
    pub(crate) index: usize,
    pub(crate) conversion_ranks: ImplicitConversionRanks,
    pub(crate) parameter_types: SmallVec<[Type; 2]>,
    pub(crate) result_type: Type,
}

/// Helper function for `typecheck_builtin_operation`.
///
/// Signatures of builtins are specified using `PseudoTypes` to make them more compact,
/// so we use this function to convert them to `hir::Type`s before calling `check_signature`.
///
/// Returns true if `check_signature` returned an exact match was found, false otherwise.
fn typecheck_builtin_helper(
    builtin_types: &BuiltinTypes,
    overload_index: usize,
    parameter_types: &[PseudoType],
    result_type: PseudoType,
    vec_len: u8,
    image_class: ImageClass,
    arguments: &[Type],
    candidates: &mut Vec<OverloadCandidate>,
) -> bool {
    let mut sig: SmallVec<[Type; 2]> = parameter_types
        .iter()
        .map(|ty| pseudo_type_to_concrete_type(*ty, builtin_types, vec_len, image_class))
        .collect();
    let result_type = pseudo_type_to_concrete_type(result_type, builtin_types, vec_len, image_class);
    if let Ok(conv) = check_signature(&sig, arguments) {
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

pub(crate) enum OverloadResolutionError {
    NoMatch,
    Ambiguous,
}


impl TypeCheckBodyCtxt<'_, '_> {
    ///
    pub(crate) fn resolve_overload(&mut self, overloads: &[DefId], args: &[Type]) -> Result<usize, OverloadResolutionError> {
        let mut candidates : Vec<(usize, ImplicitConversionRanks)> = Vec::new();
        for (i,overload) in overloads.iter().enumerate() {
            let func = self.module.def(*overload).as_function().unwrap();
            let fty = func.function_type.as_function().unwrap();
            if fty.arg_types.len() != args.len() {
                continue;
            }
            match check_signature(&fty.arg_types, &args) {
                Ok(conversion_ranks) => {
                    candidates.push((i, conversion_ranks));
                    // break early if it is an exact match
                    if conversion_ranks.iter().all(|r| *r == 0) {
                        break;
                    }
                }
                Err(_) => continue,
            }
        }

        match &candidates[..] {
            &[] => Err(OverloadResolutionError::NoMatch),
            &[(overload, _)] => Ok(overload),
            _ => {
                // rank candidates
                candidates.sort_by(|a, b| compare_conversion_ranks(&a.1, &b.1));

                // if there's a candidate above all others, select it
                if compare_conversion_ranks(&candidates[0].1, &candidates[1].1) == Ordering::Less {
                    Ok(candidates.into_iter().next().unwrap().0)
                } else {
                    Err(OverloadResolutionError::Ambiguous)
                }
            }
        }
    }

    // TODO: this is only used for operators, the rest goes through `resolve_overload`.
    // Maybe refactor so that operators also go through `resolve_overload`.
    // Or maybe refactor so that this only handles operators.
    pub(crate) fn typecheck_builtin_operation(
        &mut self,
        op: &BuiltinOperation,
        arguments: &[Type],
    ) -> Result<OverloadCandidate, SignatureMismatch> {
        let mut candidates: Vec<OverloadCandidate> = Vec::new();

        'check_signatures: for (index, sig) in op.signatures.iter().enumerate() {
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
                        &self.tyctxt.builtins,
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
            if compare_conversion_ranks(&candidates[0].conversion_ranks, &candidates[1].conversion_ranks)
                == Ordering::Less
            {
                Ok(candidates.into_iter().next().unwrap())
            } else {
                Err(SignatureMismatch)
            }
        }
    }
}
