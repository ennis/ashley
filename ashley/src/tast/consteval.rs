use crate::{syntax::ast, tast::TypeCheckCtxt};
use ordered_float::OrderedFloat;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ConstantValue {
    Int(i32),
    Float(OrderedFloat<f32>),
    Bool(bool),
}

impl<'a, 'diag> TypeCheckCtxt<'a, 'diag> {
    pub(crate) fn try_evaluate_constant_expression(&mut self, expr: &ast::Expr) -> Option<ConstantValue> {
        match expr {
            ast::Expr::LitExpr(lit) => match lit.kind() {
                ast::LiteralKind::String(s) => {
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
                                    self.diag
                                        .error(format!("error parsing integer: {err}"))
                                        .primary_label(&v, "")
                                        .emit();
                                    return None;
                                }
                            }
                        }
                        Err(err) => {
                            self.diag
                                .error(format!("error parsing integer: {err}"))
                                .location(&v)
                                .emit();
                            return None;
                        }
                    }
                }
                ast::LiteralKind::FloatNumber(v) => match v.value() {
                    Ok(f) => Some(ConstantValue::Float(OrderedFloat::from(f as f32))),
                    Err(err) => {
                        self.diag
                            .error(format!("error parsing floating-point number: {err}"))
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
}
