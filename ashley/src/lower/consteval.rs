//! Constant evaluation
use crate::{
    diagnostic::{Diagnostic, Diagnostics},
    hir,
    lower::{LowerCtxt, Scope},
    syntax::{ast, ast::AstToken},
};
use ashley::syntax::ast::{Expr, LiteralKind};
use std::num::ParseIntError;
use ordered_float::OrderedFloat;
use thiserror::Error;

//use crate::hir::ConstantData;

#[derive(Debug, Error)]
pub(super) enum ConstEvalError {}

// what do we want as a result of constant eval?
//
// Two steps:
// 1. evaluate the constant -> ConstValue
// 2. coerce to a specific type

// Evaluating an expression -> returns a value or an abstract constant
// -> type checking is done on these "AbstractValue"

pub(super) fn try_evaluate_constant_expression(
    diag: &Diagnostics,
    expr: &ast::Expr,
    scope: &Scope,
    expected_type: Option<hir::Type>,
) -> Option<hir::ConstantData> {
    // only literals for now
    let value = match expr {
        Expr::ArrayExpr(array) => {
            let mut elements = vec![];
            for e in array.elements() {
                elements.push(try_evaluate_constant_expression(diag, &e, scope, None)?);
            }
            // infer the types of the element in the array
            todo!()
        }

        Expr::BinExpr(_) => {
            // TODO const eval binary expr
            None
        }
        Expr::PrefixExpr(_) => {
            // TODO
            None
        }
        Expr::CallExpr(_) => {
            // TODO
            None
        }
        Expr::IndexExpr(_) => {
            // TODO
            None
        }
        Expr::PathExpr(_) => {
            // TODO
            None
        }
        Expr::TupleExpr(_) => {
            // TODO
            None
        }
        Expr::FieldExpr(_) => {
            // TODO
            None
        }
        Expr::ParenExpr(inner) => {
            if let Some(expr) = inner.expr() {
                try_evaluate_constant_expression(diag, &expr, scope, expected_type)
            } else {
                None
            }
        }
        Expr::LitExpr(lit) => match lit.kind() {
            LiteralKind::String(s) => {
                // TODO
                None
            }
            LiteralKind::IntNumber(v) => {
                match v.value() {
                    Ok(i) => {
                        // convert to i32
                        match i.try_into() {
                            Ok(i) => Some(hir::ConstantData::I32(i)),
                            Err(err) => {
                                diag
                                    .error(format!("error parsing integer: {err}"))
                                    .primary_label(&v, "")
                                    .emit();
                                return None
                            }
                        }
                    }
                    Err(err) => {
                        diag.error(format!("error parsing integer: {err}"))
                            .primary_label(&v, "")
                            .emit();
                        return None
                    }
                }
            }
            LiteralKind::FloatNumber(v) => {
                match v.value() {
                    Ok(f) => {
                        Some(hir::ConstantData::F32(OrderedFloat::from(f as f32)))
                    }
                    Err(err) => {
                        diag.error(format!("error parsing floating-point number: {err}"))
                            .primary_label(v, "")
                            .emit();
                        return None
                    }
                }
            }
            LiteralKind::Bool(v) => {
                Some(hir::ConstantData::Bool(v))
            }
        },
    };

    if let Some(expected_type) = expected_type {}

    todo!()
}
