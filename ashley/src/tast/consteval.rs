use crate::{
    session::{CompilerDb, SourceFileId},
    syntax::{ast, ast::LogicOp},
    tast::{diagnostics::TyDiagnostic, expr::ExprKind, Expr, ExprId, InFile, Scope, Type, TypeKind, TypedBody},
};
use ashley::{
    syntax::ast::BinaryOp,
    tast::{expr::ConversionKind, ScalarType, TypeCheckBodyCtxt},
};
use ordered_float::OrderedFloat;
use rowan::ast::AstPtr;

/*pub(crate) fn try_evaluate_constant_expression(compiler: &dyn CompilerDb, expr: &ast::Expr) -> Option<ConstantValue> {
    match expr {
        ast::Expr::LitExpr(lit) => match lit.kind() {
            ast::LiteralKind::String(_s) => {
                // TODO
                None
            }
            ast::LiteralKind::IntNumber(v) => {
                match v.value() {
                    Ok(i) => {
                        // convert to i32
                        match i.try_into() {
                            Ok(i) => Some(ConstantValue::Int(i)),
                            Err(err) => {
                                compiler
                                    .diag_error(format!("error parsing integer: {err}"))
                                    .primary_label(&v, "")
                                    .emit();
                                return None;
                            }
                        }
                    }
                    Err(err) => {
                        compiler
                            .diag_error(format!("error parsing integer: {err}"))
                            .location(&v)
                            .emit();
                        return None;
                    }
                }
            }
            ast::LiteralKind::FloatNumber(v) => match v.value() {
                Ok(f) => Some(ConstantValue::Float(OrderedFloat::from(f as f32))),
                Err(err) => {
                    compiler
                        .diag_error(format!("error parsing floating-point number: {err}"))
                        .location(v)
                        .emit();
                    return None;
                }
            },
            ast::LiteralKind::Bool(v) => Some(ConstantValue::Bool(v)),
        },
        _ => None,
    }
}*/

/// Errors resulting from constant evaluation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ConstEvalError {
    /// Could not evaluate the expression as a constant.
    CouldNotEvaluateAsConstant { expr: InFile<AstPtr<ast::Expr>> },
    ///
    TypeMismatch {
        expr: InFile<AstPtr<ast::Expr>>,
        expected: Type,
        resolved: Type,
    },
}

struct ConstEvalCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    body: &'a TypedBody,
}

impl<'a> ConstEvalCtxt<'a> {
    fn eval_expr(&self, expr: ExprId) -> Result<ConstantValue, ConstEvalError> {
        const EVAL_TY_MISMATCH: &str = "constant evaluation result is inconsistent with the inferred type";

        let expr_d = &self.body.exprs[expr];
        let expr_ptr = self.body.expr_map_back.get(expr).unwrap().clone();
        match &expr_d.kind {
            ExprKind::Literal { value } => Ok(value.clone()),
            ExprKind::Binary { lhs, op, rhs, .. } => {
                let left_value = self.eval_expr(*lhs)?;
                let right_value = self.eval_expr(*rhs)?;
                match op {
                    BinaryOp::LogicOp(LogicOp::And) => {
                        let left_bool = left_value.to_bool().expect(EVAL_TY_MISMATCH);
                        let right_bool = right_value.to_bool().expect(EVAL_TY_MISMATCH);
                        Ok(ConstantValue::Bool(left_bool && right_bool))
                    }
                    BinaryOp::LogicOp(LogicOp::Or) => {
                        let left_bool = left_value.to_bool().expect(EVAL_TY_MISMATCH);
                        let right_bool = right_value.to_bool().expect(EVAL_TY_MISMATCH);
                        Ok(ConstantValue::Bool(left_bool || right_bool))
                    }
                    BinaryOp::ArithOp(arith_op) => Err(ConstEvalError::CouldNotEvaluateAsConstant { expr: expr_ptr }),
                    BinaryOp::CmpOp(_) => Err(ConstEvalError::CouldNotEvaluateAsConstant { expr: expr_ptr }),
                    BinaryOp::Assignment(_) => Err(ConstEvalError::CouldNotEvaluateAsConstant { expr: expr_ptr }),
                }
            }
            ExprKind::ImplicitConversion { expr, kind } => {
                let value = self.eval_expr(*expr)?;
                match kind {
                    ConversionKind::IntBitcast => {
                        // no-op
                        Ok(value)
                    }
                    ConversionKind::SignedIntToFloat => {
                        // TODO 64-bit consteval
                        Ok(ConstantValue::Float(OrderedFloat::from(
                            value.to_i32().expect(EVAL_TY_MISMATCH) as f32,
                        )))
                    }
                    ConversionKind::UnsignedIntToFloat => {
                        // TODO 64-bit consteval
                        Ok(ConstantValue::Float(OrderedFloat::from(
                            value.to_u32().expect(EVAL_TY_MISMATCH) as f32,
                        )))
                    }
                    ConversionKind::FloatConvert => match value {
                        ConstantValue::Float(v) => Ok(ConstantValue::Double(OrderedFloat::from(v.0 as f64))),
                        _ => panic!("{}", EVAL_TY_MISMATCH),
                    },
                    ConversionKind::FloatToSignedInt => match value {
                        ConstantValue::Float(v) => Ok(ConstantValue::Int((v.0 as i32) as u32)),
                        ConstantValue::Double(v) => Ok(ConstantValue::Int((v.0 as i32) as u32)),
                        // TODO 64-bit consteval
                        _ => panic!("{}", EVAL_TY_MISMATCH),
                    },
                    ConversionKind::FloatToUnsignedInt => match value {
                        ConstantValue::Float(v) => Ok(ConstantValue::Int(v.0 as u32)),
                        ConstantValue::Double(v) => Ok(ConstantValue::Int(v.0 as u32)),
                        // TODO 64-bit consteval
                        _ => panic!("{}", EVAL_TY_MISMATCH),
                    },
                    ConversionKind::Layout => {
                        todo!("consteval layout conversion")
                    }
                }
            }
            ExprKind::BuiltinConstructor { args, ctor } => {
                let args = args
                    .iter()
                    .map(|arg| self.eval_expr(expr))
                    .collect::<Result<Vec<_>, _>>()?;
                match ctor.ty {
                    TypeKind::Scalar(st) => {
                        assert_eq!(args.len(), 1, "consteval: unexpected number of constructor arguments");
                        match st {
                            ScalarType::Int => match args[0] {
                                ConstantValue::Int(v) => Ok(ConstantValue::Int(v)),
                                ConstantValue::Int64(v) => Ok(ConstantValue::Int((v as i32) as u32)),
                                ConstantValue::Float(v) => Ok(ConstantValue::Int((v.0 as i32) as u32)),
                                ConstantValue::Double(v) => Ok(ConstantValue::Int((v.0 as i32) as u32)),
                                _ => panic!("{}", EVAL_TY_MISMATCH),
                            },
                            ScalarType::UnsignedInt => match args[0] {
                                ConstantValue::Int(v) => Ok(ConstantValue::Int(v)),
                                ConstantValue::Int64(v) => Ok(ConstantValue::Int(v as u32)),
                                ConstantValue::Float(v) => Ok(ConstantValue::Int(v.0 as u32)),
                                ConstantValue::Double(v) => Ok(ConstantValue::Int(v.0 as u32)),
                                _ => panic!("{}", EVAL_TY_MISMATCH),
                            },
                            ScalarType::Float => match args[0] {
                                ConstantValue::Int(v) => {
                                    Ok(ConstantValue::Float(OrderedFloat::from((v as i32) as f32)))
                                }
                                ConstantValue::Int64(v) => {
                                    Ok(ConstantValue::Float(OrderedFloat::from((v as i64) as f32)))
                                }
                                ConstantValue::Float(v) => Ok(ConstantValue::Float(v)),
                                ConstantValue::Double(v) => Ok(ConstantValue::Float(OrderedFloat::from(v.0 as f32))),
                                _ => panic!("{}", EVAL_TY_MISMATCH),
                            },
                            // TODO
                            ScalarType::Double => Err(ConstEvalError::CouldNotEvaluateAsConstant { expr: expr_ptr }),
                            ScalarType::Bool => Err(ConstEvalError::CouldNotEvaluateAsConstant { expr: expr_ptr }),
                        }
                    }
                    // TODO
                    _ => Err(ConstEvalError::CouldNotEvaluateAsConstant { expr: expr_ptr }),
                }
            }
            // TODO
            _ => Err(ConstEvalError::CouldNotEvaluateAsConstant { expr: expr_ptr }),
        }
    }
}

/// Try to evaluate a non-specialization constant expression matching the specified type.
///
/// TODO: more detailed error message telling what kind of constant was evaluated (e.g. "array size", "array stride", "uniform location").
pub(crate) fn eval_non_specialization_constant_expr(
    compiler: &dyn CompilerDb,
    source_file: SourceFileId,
    expr: &ast::Expr,
    scopes: &[Scope],
    type_check: impl FnOnce(&dyn CompilerDb, SourceFileId, &ast::Expr, &Type) -> Option<TyDiagnostic>,
    diags: &mut Vec<TyDiagnostic>,
) -> Option<ConstantValue> {
    // type-check the expression
    let mut body = TypedBody::new();
    let mut ctxt = TypeCheckBodyCtxt {
        compiler,
        source_file,
        scopes: scopes.into(), // FIXME: cloning scopes may be expensive
        typed_body: &mut body,
        errors: vec![],
    };
    let expr_id = ctxt.typecheck_expr(expr);

    // don't proceed with evaluation if there are type errors
    if !ctxt.errors.is_empty() {
        diags.extend(ctxt.errors);
        return None;
    }

    // ... or if the type is not the one that we expected
    if let Some(diag) = type_check(compiler, source_file, expr, &expr_id.ty) {
        diags.push(diag);
        return None;
    }

    let mut eval_ctxt = ConstEvalCtxt { compiler, body: &body };
    let result = eval_ctxt.eval_expr(expr_id.id);
    match result {
        Ok(value) => Some(value),
        Err(err) => {
            diags.push(err.into());
            None
        }
    }
}
