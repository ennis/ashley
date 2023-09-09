use crate::{session::CompilerDb, syntax::ast};
use ordered_float::OrderedFloat;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ConstantValue {
    Int(i32),
    Float(OrderedFloat<f32>),
    Bool(bool),
}

pub(crate) fn try_evaluate_constant_expression(compiler: &dyn CompilerDb, expr: &ast::Expr) -> Option<ConstantValue> {
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
}
