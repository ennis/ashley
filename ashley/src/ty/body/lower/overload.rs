//! Overload resolution.
use crate::{
    builtins::{BuiltinOperation, ImageClass, PseudoType},
    def::Function,
    ty::{
        body::{lower::TyBodyLowerCtxt, ExprAstId},
        FunctionSignature, PrimitiveTypes, ScalarType, TyDiagnostic, Type, TypeKind,
    },
};
use ashley::def::FunctionId;
use smallvec::SmallVec;
use spirv::CLOp::sign;
use std::{cmp::Ordering, ops::Deref};

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

/*/// Resolves the preferred overload from a list of conversion ranks.
fn resolve_preferred_overload(ranks: &mut [(usize, ImplicitConversionRanks)]) -> Option<usize> {
    ranks.sort_by(|(_, a), (_, b)| compare_conversion_ranks(a, b));
    if ranks.len() >= 2 && compare_conversion_ranks(&ranks[0].1, &ranks[1].1) == Ordering::Less {
        Some(ranks[0].0)
    } else {
        None
    }
}*/

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
pub(crate) fn check_signature(signature: &[Type], arguments: &[Type]) -> Option<ImplicitConversionRanks> {
    use TypeKind as TK;

    //eprintln!("chksig: {:?} against arguments {:?}", signature, arguments);

    if arguments.len() != signature.len() {
        // return early on arg count mismatch
        return None;
    }

    let mut conversion_ranks = SmallVec::new();
    for (sigty, argty) in signature.iter().zip(arguments.iter()) {
        // autoderef applies when calling a function or operator
        let argty = argty.unref();
        if sigty.deref() == argty {
            // direct match, no conversion necessary
            conversion_ranks.push(0);
            continue;
        }

        // check type of argument, if it doesn't work, retry with an implicit conversion
        //eprintln!("chk param sig:{:?} arg:{:?}", &m.types[*sigty], &m.types[*argty]);
        let conversion_rank = match (sigty.deref(), argty) {
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
                    stride: _stride1,
                },
                TK::Matrix {
                    component_type: tsig,
                    rows: r2,
                    columns: c2,
                    stride: _stride2,
                },
            ) if r1 == r2 && c1 == c2 => match (targ, tsig) {
                (a, b) if a == b => 0, // same component type, number of columns and rows, but different layout (stride): treat as rank 0 (same as no conversion necessary)
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
        return None;
    }

    //eprintln!("chksig OK: {:?} against arguments {:?}", signature, arguments);
    Some(conversion_ranks)
}

//--------------------------------------------------------------------------------------------------

/// Represents one match during overload resolution.
#[derive(Clone, Debug)]
struct Match {
    index: usize,
    conversion_ranks: ImplicitConversionRanks,
}

enum RankMatchesError {
    NoMatch,
    Ambiguous(Vec<Match>),
}

/// Returns the best overload match.
fn best_match(mut matches: Vec<Match>) -> Result<Match, RankMatchesError> {
    match &matches[..] {
        &[] => Err(RankMatchesError::NoMatch),
        &[_] => {
            // There's only one match
            Ok(matches.into_iter().next().unwrap())
        }
        _ => {
            // rank candidates
            matches.sort_by(|a, b| compare_conversion_ranks(&a.conversion_ranks, &b.conversion_ranks));

            // if there's a candidate above all others, select it
            if compare_conversion_ranks(&matches[0].conversion_ranks, &matches[1].conversion_ranks) == Ordering::Less {
                Ok(matches.into_iter().next().unwrap())
            } else {
                // number of "equally good" matches that we couldn't choose from
                let n = matches
                    .iter()
                    .position(|m| {
                        compare_conversion_ranks(&m.conversion_ranks, &matches[0].conversion_ranks) == Ordering::Equal
                    })
                    .unwrap();
                matches.truncate(n);
                Err(RankMatchesError::Ambiguous(matches))
            }
        }
    }
}

pub(crate) struct OverloadSelection {
    pub(crate) index: usize,
    pub(crate) signature: FunctionSignature,
}

impl TyBodyLowerCtxt<'_> {
    ///
    pub(crate) fn resolve_overload(
        &mut self,
        expr: ExprAstId,
        func_name: &str,
        functions: &[FunctionId],
        args: &[Type],
    ) -> Option<OverloadSelection> {
        let mut matches = vec![];
        let mut match_ranks = vec![];

        for (i, function) in functions.iter().enumerate() {
            let signature = function.signature(self.db);

            if signature.parameter_types.len() != args.len() {
                /*self.body.diagnostics.push(TyDiagnostic::InvalidNumberOfArguments {
                    expr,
                    expected: args.len() as u32,
                    got: signature.parameter_types.len() as u32,
                });
                return false;*/
                // not a candidate since it doesn't have the correct number of arguments
                continue;
            }

            match check_signature(&signature.parameter_types, args) {
                Some(conversion_ranks) => {
                    // signature matches (possibly with implicit conversions)
                    // add it to the list of candidates
                    matches.push(OverloadSelection {
                        index: i,
                        signature: signature.clone(),
                    });
                    match_ranks.push(Match {
                        index: matches.len() - 1,
                        conversion_ranks,
                    });
                }
                None => {
                    // no match
                }
            }
        }

        // rank candidates
        match best_match(match_ranks) {
            Ok(m) => Some(matches.into_iter().nth(m.index).unwrap()),
            Err(RankMatchesError::Ambiguous(set)) => {
                self.body.diagnostics.push(TyDiagnostic::AmbiguousOverload {
                    expr,
                    func_name: func_name.to_string(),
                    arg_types: args.into(),
                    matches: set.into_iter().map(|i| functions[matches[i.index].index]).collect(),
                });
                None
            }
            Err(RankMatchesError::NoMatch) => {
                self.body.diagnostics.push(TyDiagnostic::NoMatchingOverload {
                    expr,
                    func_name: func_name.to_string(),
                    arg_types: args.into(),
                    candidates: functions.into(),
                });
                None
            }
        }

        /*for (i, overload) in overloads.iter().enumerate() {
            let func = self.compiler.definition(*overload).as_function().unwrap();
            let fty = func.function_type.as_function().unwrap();
            if fty.arg_types.len() != args.len() {
                continue;
            }
            match check_signature(&fty.arg_types, &args) {
                Ok(conversion_ranks) => {
                    let exact_match = conversion_ranks.iter().all(|x| *x == 0);
                    candidates.push(OverloadCandidate {
                        index: i,
                        conversion_ranks,
                        parameter_types: fty.arg_types.clone().into(),
                        result_type: fty.return_type.clone(),
                    });
                    // break early if it is an exact match
                    if exact_match {
                        break;
                    }
                }
                Err(_) => continue,
            }
        }
        best_overload(candidates)*/
    }

    // Maybe refactor so that operators also go through `resolve_overload`.
    // Or maybe refactor so that this only handles operators.
    pub(crate) fn verify_builtin_operation(
        &mut self,
        expr: ExprAstId,
        op: &BuiltinOperation,
        arguments: &[Type],
    ) -> Option<OverloadSelection> {
        let mut match_signatures: Vec<OverloadSelection> = Vec::new();
        let mut match_ranks: Vec<Match> = Vec::new();

        'check_signatures: for (index, sig) in op.signatures.iter().enumerate() {
            // In order to make them more compact, signatures of builtin operations may be generic
            // over vector type ("g"), vector length ("N"), or image classes ("gt"). We call those pseudo-signatures.
            // E.g.
            // * `gvec2` is generic over the vector type (vec2, ivec2, uvec2, ...)
            // * `vecN` is generic over the vector length (vec2, vec3, vec4)
            // * `gtexture` is generic over the image class (texture, itexture, utexture)
            //
            // The following code enumerates all possible combinations of "g", "N" and image classes,
            // and substitutes them in the pseudo-signature.
            let is_vector_generic = sig.parameter_types.iter().any(PseudoType::is_vector_generic);
            let is_image_type_generic = sig.parameter_types.iter().any(PseudoType::is_image_type_generic);
            let max_vec_len = if is_vector_generic { 4 } else { 1 };
            let image_classes = if is_image_type_generic {
                &[ImageClass::F, ImageClass::SI, ImageClass::UI][..]
            } else {
                &[ImageClass::F][..]
            };

            for ic in image_classes {
                for vec_len in 1..=max_vec_len {
                    // substitute generic params in the operation signature
                    let parameter_types: Vec<_> = sig
                        .parameter_types
                        .iter()
                        .map(|ty| ty.to_concrete_type(&self.db.tyctxt().prim_tys, vec_len, *ic))
                        .collect();
                    let result_type = sig
                        .result_type
                        .to_concrete_type(&self.db.tyctxt().prim_tys, vec_len, *ic);
                    // check the concrete signature
                    if let Some(conv) = check_signature(&parameter_types, arguments) {
                        let exact_match = conv.iter().all(|x| *x == 0);
                        match_signatures.push(OverloadSelection {
                            index,
                            signature: FunctionSignature {
                                parameter_types,
                                return_type: result_type,
                            },
                        });
                        match_ranks.push(Match {
                            index: match_signatures.len() - 1,
                            conversion_ranks: conv,
                        });
                        if exact_match {
                            // no need to continue if there's an exact match, since there can be no
                            // ambiguity in this case
                            break 'check_signatures;
                        }
                    }
                }
            }
        }

        match best_match(match_ranks) {
            Ok(m) => Some(match_signatures.into_iter().nth(m.index).unwrap()),
            Err(RankMatchesError::Ambiguous(set)) => {
                // TODO
                /*self.body.diagnostics.push(TyDiagnostic::AmbiguousOverload {
                    expr,
                    func_name: op.name.to_string(),
                    matches: set.into_iter().map(|i| matches[i.index]).collect(),
                });*/
                None
            }
            Err(RankMatchesError::NoMatch) => {
                // TODO
                /*self.body.diagnostics.push(TyDiagnostic::NoMatchingOverload {
                    expr,
                    func_name: func_name.to_string(),
                    arg_types: args.into(),
                    candidates: functions.into(),
                });*/
                None
            }
        }
    }
}
