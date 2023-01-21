//! Constant evaluation
use std::num::ParseIntError;
use crate::{
    diagnostic::Diagnostic,
    hir,
    lower::{LowerCtxt, Scope},
    syntax::ast,
};
use ashley::syntax::ast::{Expr, LiteralKind};
use thiserror::Error;
use crate::syntax::ast::AstToken;

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
    ctx: &mut LowerCtxt,
    expr: &ast::Expr,
    scope: &Scope,
    expected_type: Option<hir::Type>,
) -> Option<hir::ConstantData> {
    // only literals for now
    let value = match expr {
        Expr::ArrayExpr(array) => {
            let mut elements = vec![];
            for e in array.elements() {
                elements.push(try_evaluate_constant_expression(ctx, &e, scope, None)?);
            }
            // infer the types of the element in the array

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
                try_evaluate_constant_expression(ctx, &expr, scope, expected_type)
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
                    Ok(v) => { ConstValue::AbstractInt(v) }
                    Err(err) => {
                        let loc = ctx.token_loc(v.syntax());
                        ctx.diag.error(format!("error parsing integer: {err}")).primary_label("", loc).emit();
                        None
                    }
                }
            }
            LiteralKind::FloatNumber(v) => {
                match v.value() {
                    Ok(v) => { ConstValue::AbstractFloat(v) }
                    Err(err) => {
                        let loc = ctx.token_loc(v.syntax());
                        ctx.diag.error(format!("error parsing floating-point number: {err}")).primary_label("", loc).emit();
                        None
                    }
                }
            }
            LiteralKind::Bool(v) => {
                ConstValue::Bool(v)
            }
        },
    };



    if let Some(expected_type) = expected_type {}

    todo!()
}
