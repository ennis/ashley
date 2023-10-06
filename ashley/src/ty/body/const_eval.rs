use crate::{
    def::ConstExprId,
    syntax::ast::{BinaryOp, LogicOp},
    ty::{
        body::{Body, ConversionKind, Expr, ExprKind},
        ScalarType, TyDiagnostic, TypeKind,
    },
    CompilerDb, ConstantValue,
};
use ashley_data_structures::Id;
use ordered_float::OrderedFloat;

pub(super) struct ConstEvalCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    body: &'a Body,
    diagnostics: Vec<TyDiagnostic>,
}

impl<'a> ConstEvalCtxt<'a> {
    pub(super) fn new(compiler: &'a dyn CompilerDb, body: &'a Body) -> ConstEvalCtxt<'a> {
        ConstEvalCtxt {
            compiler,
            body,
            diagnostics: vec![],
        }
    }

    fn eval_expr(&self, expr: Id<Expr>) -> Result<ConstantValue, TyDiagnostic> {
        use ConstantValue as CV;
        use ConversionKind as CK;
        use ExprKind as EK;
        use ScalarType as ST;
        use TypeKind as TK;

        const EVAL_TY_MISMATCH: &str = "constant evaluation result is inconsistent with the inferred type";

        let expr = &self.body.exprs[expr];
        let ast_id = expr.ast_id;
        //let expr_ptr = self.body.expr_map_back.get(expr).unwrap().clone();

        match expr.kind {
            EK::Literal { ref value } => Ok(value.clone()),
            EK::Binary { lhs, op, rhs, .. } => {
                let left_value = self.eval_expr(lhs)?;
                let right_value = self.eval_expr(rhs)?;
                match op {
                    BinaryOp::LogicOp(LogicOp::And) => {
                        let left_bool = left_value.to_bool().expect(EVAL_TY_MISMATCH);
                        let right_bool = right_value.to_bool().expect(EVAL_TY_MISMATCH);
                        Ok(CV::Bool(left_bool && right_bool))
                    }
                    BinaryOp::LogicOp(LogicOp::Or) => {
                        let left_bool = left_value.to_bool().expect(EVAL_TY_MISMATCH);
                        let right_bool = right_value.to_bool().expect(EVAL_TY_MISMATCH);
                        Ok(CV::Bool(left_bool || right_bool))
                    }
                    BinaryOp::ArithOp(arith_op) => Err(TyDiagnostic::CouldNotEvaluateAsConstant { expr: ast_id }),
                    BinaryOp::CmpOp(_) => Err(TyDiagnostic::CouldNotEvaluateAsConstant { expr: ast_id }),
                    BinaryOp::Assignment(_) => Err(TyDiagnostic::CouldNotEvaluateAsConstant { expr: ast_id }),
                }
            }
            EK::ImplicitConversion { expr, kind } => {
                let value = self.eval_expr(expr)?;
                match kind {
                    CK::IntBitcast => {
                        // no-op
                        Ok(value)
                    }
                    CK::SignedIntToFloat => {
                        // TODO 64-bit consteval
                        Ok(CV::Float(OrderedFloat::from(
                            value.to_i32().expect(EVAL_TY_MISMATCH) as f32
                        )))
                    }
                    CK::UnsignedIntToFloat => {
                        // TODO 64-bit consteval
                        Ok(CV::Float(OrderedFloat::from(
                            value.to_u32().expect(EVAL_TY_MISMATCH) as f32
                        )))
                    }
                    CK::FloatConvert => match value {
                        CV::Float(v) => Ok(CV::Double(OrderedFloat::from(v.0 as f64))),
                        _ => panic!("{}", EVAL_TY_MISMATCH),
                    },
                    CK::FloatToSignedInt => match value {
                        CV::Float(v) => Ok(CV::Int((v.0 as i32) as u32)),
                        CV::Double(v) => Ok(CV::Int((v.0 as i32) as u32)),
                        // TODO 64-bit consteval
                        _ => panic!("{}", EVAL_TY_MISMATCH),
                    },
                    CK::FloatToUnsignedInt => match value {
                        CV::Float(v) => Ok(CV::Int(v.0 as u32)),
                        CV::Double(v) => Ok(CV::Int(v.0 as u32)),
                        // TODO 64-bit consteval
                        _ => panic!("{}", EVAL_TY_MISMATCH),
                    },
                    CK::Layout => {
                        todo!("consteval layout conversion")
                    }
                    CK::Deref => {
                        todo!("consteval autoderef")
                    }
                }
            }
            ExprKind::BuiltinConstructor { ref args, ctor } => {
                let args = args
                    .iter()
                    .map(|arg| self.eval_expr(*arg))
                    .collect::<Result<Vec<_>, _>>()?;
                match ctor.ty {
                    TK::Scalar(st) => {
                        assert_eq!(args.len(), 1, "consteval: unexpected number of constructor arguments");
                        match st {
                            ST::Int => match args[0] {
                                CV::Int(v) => Ok(CV::Int(v)),
                                CV::Int64(v) => Ok(CV::Int((v as i32) as u32)),
                                CV::Float(v) => Ok(CV::Int((v.0 as i32) as u32)),
                                CV::Double(v) => Ok(CV::Int((v.0 as i32) as u32)),
                                _ => panic!("{}", EVAL_TY_MISMATCH),
                            },
                            ST::UnsignedInt => match args[0] {
                                CV::Int(v) => Ok(CV::Int(v)),
                                CV::Int64(v) => Ok(CV::Int(v as u32)),
                                CV::Float(v) => Ok(CV::Int(v.0 as u32)),
                                CV::Double(v) => Ok(CV::Int(v.0 as u32)),
                                _ => panic!("{}", EVAL_TY_MISMATCH),
                            },
                            ST::Float => match args[0] {
                                CV::Int(v) => Ok(CV::Float(OrderedFloat::from((v as i32) as f32))),
                                CV::Int64(v) => Ok(CV::Float(OrderedFloat::from((v as i64) as f32))),
                                CV::Float(v) => Ok(CV::Float(v)),
                                CV::Double(v) => Ok(CV::Float(OrderedFloat::from(v.0 as f32))),
                                _ => panic!("{}", EVAL_TY_MISMATCH),
                            },
                            // TODO
                            ST::Double => Err(TyDiagnostic::CouldNotEvaluateAsConstant { expr: ast_id }),
                            ST::Bool => Err(TyDiagnostic::CouldNotEvaluateAsConstant { expr: ast_id }),
                        }
                    }
                    // TODO
                    _ => Err(TyDiagnostic::CouldNotEvaluateAsConstant { expr: ast_id }),
                }
            }
            // TODO
            _ => Err(TyDiagnostic::CouldNotEvaluateAsConstant { expr: ast_id }),
        }
    }
}

pub(super) fn eval_const_expr_body(
    db: &dyn CompilerDb,
    const_expr_id: ConstExprId,
    body: &Body,
) -> Result<ConstantValue, TyDiagnostic> {
    let root_expr = body.root_expr();
    let mut ctxt = ConstEvalCtxt {
        compiler: db,
        body,
        diagnostics: vec![],
    };

    ctxt.eval_expr(root_expr)
}
