//! Body type checker. Lower item bodies to type-checked bodies.

use crate::{
    item,
    ty::{
        body::{diagnostic::TyBodyDiagnostic::*, Body, Expr, ExprKind},
        ScalarType, Type, TypeCtxt, TypeKind,
    },
    CompilerDb,
};
use ashley_data_structures::Id;
use std::{ops::Deref, sync::Arc};

mod overload;
mod swizzle;

type ItemExprId = Id<item::body::Expr>;

pub(super) struct TyBodyLowerCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    body: Body,
    item_body: &'a item::body::Body,
    tyctxt: Arc<TypeCtxt>,
}

pub(crate) struct TypedExprId {
    pub(crate) ty: Type,
    pub(crate) id: Id<Expr>,
}

impl<'a> TyBodyLowerCtxt<'a> {
    pub(crate) fn new(compiler: &'a dyn CompilerDb, item_body: &'a item::body::Body) -> TyBodyLowerCtxt<'a> {
        TyBodyLowerCtxt {
            compiler,
            body: Body {
                stmts: Default::default(),
                exprs: Default::default(),
                local_var: Default::default(),
                blocks: Default::default(),
                params: vec![],
                diagnostics: vec![],
            },
            item_body,
            tyctxt: compiler.tyctxt(),
        }
    }

    fn lower_expr(&mut self, expr: ItemExprId) -> TypedExprId {
        todo!()
    }

    fn error_expr(&mut self) -> Expr {
        todo!()
    }

    fn lower_index_expr(&mut self, full_expr: ItemExprId, array_or_vector: ItemExprId, index: ItemExprId) -> Expr {
        let array_expr = self.lower_expr(array_or_vector);
        let index_expr = self.lower_expr(index);

        let ty = match array_expr.ty.deref() {
            TypeKind::Vector(scalar_type, _) => self.tyctxt.ty(TypeKind::Scalar(*scalar_type)),
            TypeKind::Array { element_type, .. } | TypeKind::RuntimeArray { element_type, .. } => element_type.clone(),
            _ => {
                self.body.diagnostics.push(InvalidIndexing {
                    expr: full_expr,
                    base_ty: array_expr.ty.clone(),
                });
                return self.error_expr();
            }
        };

        match index_expr.ty.deref() {
            // TODO unsigned int?
            TypeKind::Scalar(ScalarType::Int) => {}
            _ => {
                self.body.diagnostics.push(InvalidIndexType {
                    index: index_expr.id,
                    ty: index_expr.ty.clone(),
                });
                return self.error_expr();
            }
        };

        // TODO check for out of bounds access if index is constant
        Expr {
            kind: ExprKind::Index {
                array_or_vector: array_expr.id,
                index: index_expr.id,
            },
            ty,
        }
    }
}
