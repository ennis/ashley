use crate::{
    ty::body::{diagnostic::ConstEvalDiagnostic, Body, Expr},
    CompilerDb, ConstantValue,
};
use ashley_data_structures::Id;

struct ConstEvalCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    body: &'a Body,
    diagnostics: Vec<ConstEvalDiagnostic>,
}

impl<'a> ConstEvalCtxt<'a> {
    fn new(compiler: &'a dyn CompilerDb, body: &'a Body) -> ConstEvalCtxt<'a> {
        ConstEvalCtxt {
            compiler,
            body,
            diagnostics: vec![],
        }
    }

    /*fn eval_expr(&self, expr: Id<Expr>) -> Result<ConstantValue, ConstEvalDiagnostic> {
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
    }*/
}
