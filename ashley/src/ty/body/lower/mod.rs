//! Body type checker. Lower item bodies to type-checked bodies.

use crate::{
    def,
    def::{BodyLoc, FunctionId, Resolver, Scope},
    ty::{
        body::{Body, Expr, ExprKind},
        lower_ty, FunctionSignature, ScalarType,
        TyDiagnostic::*,
        Type, TypeCtxt, TypeKind,
    },
    CompilerDb, ModuleId,
};
use ashley::def::body::Statement;
use ashley_data_structures::Id;
use std::{ops::Deref, sync::Arc};

mod overload;
mod swizzle;

type ItemExprId = Id<def::body::Expr>;

pub(super) struct TyBodyLowerCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    body: Body,
    //item_body: &'a def::body::Body,
    resolver: Resolver<'a>,
    //tyctxt: Arc<TypeCtxt>,
}

pub(crate) struct TypedExprId {
    pub(crate) ty: Type,
    pub(crate) id: Id<Expr>,
}

impl<'a> TyBodyLowerCtxt<'a> {
    pub(crate) fn new(compiler: &'a dyn CompilerDb, resolver: Resolver<'a>) -> TyBodyLowerCtxt<'a> {
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
            resolver,
            //tyctxt: compiler.tyctxt(),
        }
    }

    fn lower_body(&mut self, body: &def::body::Body, signature: &FunctionSignature) {}

    fn lower_stmt(&mut self, stmt: &def::body::Statement) {
        match stmt {
            Statement::Select { .. } => {}
            Statement::ForLoop { .. } => {}
            Statement::WhileLoop { .. } => {}
            Statement::Local { .. } => {}
            Statement::Return { .. } => {}
            Statement::ExprStmt { .. } => {}
            Statement::Block { .. } => {}
            Statement::Break => {}
            Statement::Continue => {}
            Statement::Discard => {}
            Statement::Error => {}
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
                    index,
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

pub(crate) fn lower_function_body(compiler: &dyn CompilerDb, function_id: FunctionId) {
    let body = compiler.function_body(function_id);
    let func_loc = function_id.loc(compiler);
    let module_scope = compiler.module_scope(func_loc.module);
    // Resolver for the function body
    let mut resolver = func_loc.module.resolver(compiler);
    //
    let signature = compiler.function_signature(function_id);

    let mut param_scope;

    let ctxt = TyBodyLowerCtxt::new(compiler, resolver);

    if !body.params.is_empty() {
        // add body parameters as local variables
        let mut param_scope = Scope::new();
        for (&param, param_ty) in body.params.iter().zip(signature.parameter_types.iter()) {
            let v = &body.local_vars[param];
            param_scope.add_local_var(&v.name, param);

            // Issue: there's no way to go back to the AST?

            //let param_ty = lower_ty(self.compiler, &resolver, DefL)
        }
        //self.resolver.push_scope(&param_scope);
    }
}
