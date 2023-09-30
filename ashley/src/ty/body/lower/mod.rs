//! Body type checker. Lower item bodies to type-checked bodies.

use crate::{
    builtins, def,
    def::{AstId, BodyLoc, FunctionId, Resolver, Scope},
    syntax::{
        ast,
        ast::{BinaryOp, UnaryOp},
    },
    ty,
    ty::{
        body::{
            lower::swizzle::get_component_indices, Block, Body, ConversionKind, DefExprId, Expr, ExprKind, LocalVar,
            Stmt, StmtKind,
        },
        lower_ty, FunctionSignature, ScalarType,
        TyDiagnostic::*,
        Type, TypeCtxt, TypeKind,
    },
    CompilerDb, ConstantValue, ModuleId,
};
use ashley::{def::ValueRes, ty::body::LocalVarAstId};
use ashley_data_structures::{Id, IndexVec};
use std::{ops::Deref, sync::Arc};

mod overload;
mod swizzle;

pub(super) struct TyBodyLowerCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    body: Body,
    item_body: &'a def::body::Body,
    resolver: Resolver<'a>,
    //tyctxt: Arc<TypeCtxt>,
}

#[derive(Clone)]
pub(crate) struct TypedExprId {
    pub(crate) ty: Type,
    pub(crate) ast_id: ExprAstId,
    pub(crate) id: Id<Expr>,
}

impl TypedExprId {
    fn is_error(&self) -> bool {
        self.ty.is_error()
    }

    fn is_ref(&self) -> bool {
        self.ty.is_ref()
    }
}

type ExprAstId = AstId<ast::Expr>;

impl<'a> TyBodyLowerCtxt<'a> {
    /*pub(crate) fn new(
        compiler: &'a dyn CompilerDb,
        item_body: &'a def::body::Body,
        resolver: Resolver<'a>,
    ) -> TyBodyLowerCtxt<'a> {
        TyBodyLowerCtxt {
            compiler,
            body: Body {
                stmts: Default::default(),
                exprs: Default::default(),
                local_var: Default::default(),
                blocks: Default::default(),
                entry_block: None,
                params: vec![],
                diagnostics: vec![],
            },
            item_body,
            resolver,
            //tyctxt: compiler.tyctxt(),
        }
    }*/

    fn add_expr(&mut self, expr: Expr) -> TypedExprId {
        let ty = expr.ty.clone();
        let ast_id = expr.ast_id;
        let id = self.body.exprs.push(expr);
        TypedExprId { ty, ast_id, id }
    }

    fn create_and_add_expr(&mut self, ast_id: ExprAstId, ty: Type, kind: ExprKind) -> TypedExprId {
        self.add_expr(Expr { ty, ast_id, kind })
    }

    fn error_expr(&mut self, ast_id: ExprAstId) -> TypedExprId {
        self.create_and_add_expr(ast_id, self.resolver.tyctxt().error.clone(), ExprKind::Undef)
    }

    fn apply_autoderef(&mut self, value: TypedExprId) -> TypedExprId {
        use ConversionKind as ICK;
        use ExprKind as EK;
        use TypeKind as TK;

        if let TK::Ref(deref_ty) = value.ty.deref() {
            let deref_expr = self.create_and_add_expr(
                value.ast_id,
                deref_ty.clone(),
                EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::Deref,
                },
            );
            deref_expr
        } else {
            value
        }
    }

    /// Applies implicit conversions and autoderef.
    fn apply_implicit_conversion(&mut self, value: TypedExprId, ty: Type) -> TypedExprId {
        use ConversionKind as ICK;
        use ExprKind as EK;
        use ScalarType as ST;
        use TypeKind as TK;

        let ast_id = self.body.exprs[value.id].ast_id;

        if value.is_error() {
            // don't bother
            return value;
        }

        let value = self.apply_autoderef(value);

        if ty == value.ty {
            // same types, no conversion
            return value;
        }

        // implicit conversions
        let expr_kind = match (value.ty.deref(), ty.deref()) {
            (TK::Scalar(tsrc), TK::Scalar(tdst)) => match (tsrc, tdst) {
                (ST::Int, ST::UnsignedInt) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::IntBitcast,
                },
                (ST::Int, ST::Float | ST::Double) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::SignedIntToFloat,
                },
                (ST::UnsignedInt, ST::Float | ST::Double) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::UnsignedIntToFloat,
                },
                (ST::Float, ST::Double) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::FloatConvert,
                },
                _ => {
                    self.body.diagnostics.push(NoImplicitConversion {
                        expr: ast_id,
                        from: value.ty.clone(),
                        to: ty.clone(),
                    });
                    return self.error_expr(ast_id);
                }
            },
            (TK::Vector(tsrc, n2), TK::Vector(tdst, n1)) if n1 == n2 => match (tsrc, tdst) {
                (ST::Int, ST::UnsignedInt) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::IntBitcast,
                },
                (ST::Int, ST::Float | ST::Double) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::SignedIntToFloat,
                },
                (ST::UnsignedInt, ST::Float | ST::Double) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::UnsignedIntToFloat,
                },
                (ST::Float, ST::Double) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::FloatConvert,
                },
                (_a, _b) => {
                    self.body.diagnostics.push(NoImplicitConversion {
                        expr: ast_id,
                        from: value.ty.clone(),
                        to: ty.clone(),
                    });
                    return self.error_expr(ast_id);
                }
            },
            (
                TK::Matrix {
                    component_type: tsrc,
                    rows: r1,
                    columns: c1,
                    stride: _,
                },
                TK::Matrix {
                    component_type: tdst,
                    rows: r2,
                    columns: c2,
                    stride: _,
                },
            ) if r1 == r2 && c1 == c2 => match (tsrc, tdst) {
                (ST::Float, ST::Float) | (ST::Double, ST::Double) => {
                    // the component type is the same, but the stride may be different
                    EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::Layout,
                    }
                }
                (ST::Float, ST::Double) => EK::ImplicitConversion {
                    expr: value.id,
                    kind: ICK::FloatConvert,
                },

                (_a, _b) => {
                    self.body.diagnostics.push(NoImplicitConversion {
                        expr: ast_id,
                        from: value.ty.clone(),
                        to: ty.clone(),
                    });
                    return self.error_expr(ast_id);
                }
            },
            _ => {
                self.body.diagnostics.push(NoImplicitConversion {
                    expr: ast_id,
                    from: value.ty.clone(),
                    to: ty.clone(),
                });
                return self.error_expr(ast_id);
            }
        };

        self.create_and_add_expr(ast_id, ty, expr_kind)
    }

    fn lower_local_variable(
        &mut self,
        local_var: Id<def::body::LocalVar>,
        initializer: Option<Id<def::body::Expr>>,
    ) -> StmtKind {
        let local_var = &self.item_body.local_vars[local_var];
        let ty = lower_ty(self.compiler, &self.resolver, &local_var.ty, &mut self.body.diagnostics);
        let initializer = if let Some(initializer) = initializer {
            let expr = self.lower_expr(initializer);
            Some(self.apply_implicit_conversion(expr, ty.clone()))
        } else {
            None
        };

        let id = self.body.local_vars.push(LocalVar {
            name: local_var.name.clone(),
            ast_id: LocalVarAstId::LocalVariable(local_var.ast_id),
            ty,
        });

        self.resolver
            .last_mut()
            .expect("expected a scope")
            .add_local_var(&local_var.name, id);
        StmtKind::Local {
            var: id,
            initializer: initializer.map(|e| e.id),
        }
    }

    fn lower_if_stmt(
        &mut self,
        condition: DefExprId,
        true_branch: Id<def::body::Statement>,
        false_branch: Option<Id<def::body::Statement>>,
    ) -> StmtKind {
        let condition = self.lower_expr(condition);
        if condition.ty != self.resolver.tyctxt().prim_tys.bool {
            self.body.diagnostics.push(ExpectedBoolean {
                expr: condition.ast_id,
                ty: condition.ty.clone(),
            });
        }
        let true_branch = self.lower_stmt_in_new_scope(true_branch);
        let false_branch = false_branch.map(|s| self.lower_stmt_in_new_scope(s));

        StmtKind::Select {
            condition: condition.id,
            true_branch,
            false_branch,
        }
    }

    fn lower_expr_stmt(&mut self, expr: Id<def::body::Expr>) -> StmtKind {
        StmtKind::ExprStmt {
            expr: self.lower_expr(expr).id,
        }
    }

    fn lower_return_stmt(&mut self, value: Option<Id<def::body::Expr>>) -> StmtKind {
        StmtKind::Return {
            value: value.map(|v| self.lower_expr(v).id),
        }
    }

    fn lower_block_stmt(&mut self, block: Id<def::body::Block>) -> StmtKind {
        StmtKind::Block {
            block: self.lower_block(block),
        }
    }

    fn lower_for_loop_stmt(
        &mut self,
        initializer: Option<Id<def::body::Statement>>,
        condition: Option<Id<def::body::Expr>>,
        loop_expr: Option<Id<def::body::Expr>>,
        stmt: Id<def::body::Statement>,
    ) -> StmtKind {
        let initializer = initializer.map(|initializer| self.lower_stmt(initializer));
        let condition = condition.map(|condition| self.lower_expr(condition));
        let loop_expr = loop_expr.map(|loop_expr| self.lower_expr(loop_expr));
        let stmt = self.lower_stmt(stmt);

        if let Some(ref condition) = condition {
            if condition.ty != self.resolver.tyctxt().prim_tys.bool {
                self.body.diagnostics.push(ExpectedBoolean {
                    expr: condition.ast_id,
                    ty: condition.ty.clone(),
                });
            }
        }

        StmtKind::ForLoop {
            initializer,
            condition: condition.map(|e| e.id),
            loop_expr: loop_expr.map(|e| e.id),
            stmt,
        }
    }

    fn lower_while_loop_stmt(&mut self, condition: Id<def::body::Expr>, stmt: Id<def::body::Statement>) -> StmtKind {
        let condition = self.lower_expr(condition);
        let stmt = self.lower_stmt(stmt);

        if condition.ty != self.resolver.tyctxt().prim_tys.bool {
            self.body.diagnostics.push(ExpectedBoolean {
                expr: condition.ast_id,
                ty: condition.ty.clone(),
            });
        }

        StmtKind::WhileLoop {
            condition: condition.id,
            stmt,
        }
    }

    fn in_new_scope<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let scope = Scope::new();
        self.resolver.push_scope(scope);
        let r = f(self);
        self.resolver.pop_scope();
        r
    }

    fn lower_stmt_in_new_scope(&mut self, stmt: Id<def::body::Statement>) -> Id<Stmt> {
        self.in_new_scope(|this| this.lower_stmt(stmt))
    }

    fn lower_stmt(&mut self, stmt: Id<def::body::Statement>) -> Id<Stmt> {
        let ast_id = self.item_body[stmt].ast_id;
        let s = match self.item_body[stmt].kind {
            def::body::StmtKind::Select {
                condition,
                true_branch,
                false_branch,
            } => self.lower_if_stmt(condition, true_branch, false_branch),
            def::body::StmtKind::ForLoop {
                initializer,
                condition,
                loop_expr,
                stmt,
            } => self.in_new_scope(|this| this.lower_for_loop_stmt(initializer, condition, loop_expr, stmt)),
            def::body::StmtKind::WhileLoop { condition, stmt } => self.lower_while_loop_stmt(condition, stmt),
            def::body::StmtKind::Local { var, initializer } => self.lower_local_variable(var, initializer),
            def::body::StmtKind::Return { value } => self.lower_return_stmt(value),
            def::body::StmtKind::ExprStmt { expr } => self.lower_expr_stmt(expr),
            def::body::StmtKind::Block { block } => self.lower_block_stmt(block),
            def::body::StmtKind::Break => StmtKind::Break,
            def::body::StmtKind::Continue => StmtKind::Continue,
            def::body::StmtKind::Discard => StmtKind::Discard,
            def::body::StmtKind::Error => StmtKind::Error,
        };

        self.body.stmts.push(Stmt { ast_id, kind: s })
    }

    fn lower_block_inner(&mut self, block: Id<def::body::Block>) -> Id<Block> {
        let mut stmts = Vec::new();
        let block = &self.item_body.blocks[block];
        for stmt in block.statements.iter() {
            stmts.push(self.lower_stmt(*stmt));
        }
        self.body.blocks.push(Block { stmts })
    }

    fn lower_block(&mut self, block: Id<def::body::Block>) -> Id<Block> {
        self.in_new_scope(|this| this.lower_block_inner(block))
    }

    fn lower_expr(&mut self, expr: Id<def::body::Expr>) -> TypedExprId {
        let ast_id = self.item_body[expr].ast_id;
        match self.item_body[expr].kind {
            def::body::ExprKind::Binary { op, lhs, rhs } => self.lower_bin_expr(ast_id, op, lhs, rhs),
            def::body::ExprKind::Prefix { expr, op } => self.lower_unary_expr(ast_id, op, expr),
            def::body::ExprKind::Postfix { expr, op } => self.lower_unary_expr(ast_id, op, expr),
            def::body::ExprKind::Ternary {
                condition,
                true_expr,
                false_expr,
            } => {
                todo!()
            }
            def::body::ExprKind::Call { ref function, ref args } => self.lower_call_expr(ast_id, function, args),
            def::body::ExprKind::Name { ref name } => self.lower_name_expr(ast_id, name),
            def::body::ExprKind::Index { array_or_vector, index } => {
                self.lower_indexing_expr(ast_id, array_or_vector, index)
            }
            def::body::ExprKind::Field { expr, ref name } => self.lower_field_expr(ast_id, expr, name),
            def::body::ExprKind::Constructor { ref ty, ref args } => self.lower_constructor_expr(ast_id, ty, args),
            def::body::ExprKind::Literal { ref value } => self.lower_lit_expr(ast_id, value),
            def::body::ExprKind::Undef => self.error_expr(ast_id),
        }
    }

    fn lower_indexing_expr(&mut self, ast_id: ExprAstId, array_or_vector: DefExprId, index: DefExprId) -> TypedExprId {
        let array_expr = self.lower_expr(array_or_vector);
        let index_expr = self.lower_expr(index);

        let mut ty = match array_expr.ty.unref() {
            TypeKind::Vector(scalar_type, _) => self.resolver.tyctxt().ty(TypeKind::Scalar(*scalar_type)),
            TypeKind::Array { element_type, .. } | TypeKind::RuntimeArray { element_type, .. } => element_type.clone(),
            _ => {
                if !array_expr.is_error() {
                    self.body.diagnostics.push(InvalidIndexing {
                        expr: ast_id,
                        base_ty: array_expr.ty.clone(),
                    });
                }
                return self.error_expr(ast_id);
            }
        };

        if array_expr.ty.is_ref() {
            ty = Type::new(self.compiler, TypeKind::Ref(ty));
        }

        match index_expr.ty.unref() {
            TypeKind::Scalar(ScalarType::Int | ScalarType::UnsignedInt) => {}
            _ => {
                if !index_expr.is_error() {
                    self.body.diagnostics.push(InvalidIndexType {
                        expr: self.body.exprs[index_expr.id].ast_id,
                        ty: index_expr.ty.clone(),
                    });
                }
                return self.error_expr(ast_id);
            }
        };

        let index_expr = self.apply_autoderef(index_expr);

        // TODO check for out of bounds access if index is constant
        self.create_and_add_expr(
            ast_id,
            ty,
            ExprKind::Index {
                array_or_vector: array_expr.id,
                index: index_expr.id,
            },
        )
    }

    fn lower_unary_expr(&mut self, ast_id: ExprAstId, op: UnaryOp, expr: Id<def::body::Expr>) -> TypedExprId {
        let value = self.lower_expr(expr);
        let operation;
        let is_assignment;
        match op {
            UnaryOp::Neg => {
                is_assignment = false;
                operation = &builtins::UnaryMinus;
            }
            UnaryOp::Not => {
                is_assignment = false;
                operation = &builtins::Not;
            }
            UnaryOp::PrefixInc => {
                is_assignment = true;
                operation = &builtins::PrefixInc;
            }
            UnaryOp::PrefixDec => {
                is_assignment = true;
                operation = &builtins::PrefixDec;
            }
            UnaryOp::PostfixInc => {
                is_assignment = true;
                operation = &builtins::PostfixInc;
            }
            UnaryOp::PostfixDec => {
                is_assignment = true;
                operation = &builtins::PostfixDec;
            }
        };

        if is_assignment && !value.is_ref() {
            self.body.diagnostics.push(ExpectedPlace {
                expr: value.ast_id,
                ty: value.ty,
            });
            return self.error_expr(ast_id);
        }

        match self.verify_builtin_operation(ast_id, operation, &[value.ty.clone()]) {
            Some(overload) => {
                let conv_expr = if is_assignment {
                    // there are no "implicit conversions" of the value for `value++` and `value--` (and the prefix variants)
                    value
                } else {
                    self.apply_implicit_conversion(value, overload.signature.parameter_types[0].clone())
                };

                self.create_and_add_expr(
                    ast_id,
                    overload.signature.return_type,
                    ExprKind::Unary {
                        op,
                        signature: operation.signatures[overload.index],
                        expr: conv_expr.id,
                    },
                )
            }
            None => self.error_expr(ast_id),
        }
    }

    fn lower_field_expr(&mut self, ast_id: ExprAstId, base_expr: Id<def::body::Expr>, field_name: &str) -> TypedExprId {
        let base_expr = self.lower_expr(base_expr);

        if base_expr.is_error() {
            return self.error_expr(ast_id);
        }

        match base_expr.ty.unref() {
            // --- Field access ---
            TypeKind::Struct { name: _, def } => {
                let struct_data = def.data(self.compiler);
                let Some(field_index) = struct_data.fields.iter().position(|f| f.name == field_name) else {
                    self.body.diagnostics.push(UnresolvedField {
                        expr: ast_id,
                        name: field_name.to_string(),
                        receiver: base_expr.ty,
                    });
                    return self.error_expr(ast_id);
                };

                // Propagate Ref type (i.e. if the base expression is a place, then the field expr is a place also)
                let mut field_type = def.field_types(self.compiler)[field_index].clone();
                if base_expr.ty.is_ref() {
                    field_type = Type::new(self.compiler, TypeKind::Ref(field_type));
                }

                self.create_and_add_expr(
                    ast_id,
                    field_type,
                    ExprKind::Field {
                        expr: base_expr.id,
                        index: field_index,
                    },
                )
            }
            // --- Vector component access ---
            TypeKind::Vector(scalar_type, size) => match get_component_indices(field_name, *size as usize) {
                Ok(components) => {
                    let mut ty = if components.len() == 1 {
                        Type::new(self.compiler, TypeKind::Scalar(*scalar_type))
                    } else {
                        Type::new(self.compiler, TypeKind::Vector(*scalar_type, components.len() as u8))
                    };

                    if base_expr.ty.is_ref() {
                        ty = Type::new(self.compiler, TypeKind::Ref(ty));
                    }

                    self.create_and_add_expr(
                        ast_id,
                        ty,
                        ExprKind::ComponentAccess {
                            expr: base_expr.id,
                            components,
                        },
                    )
                }
                Err(error) => {
                    self.body.diagnostics.push(InvalidComponentSelection {
                        expr: ast_id,
                        selection: field_name.to_string(),
                        error,
                        receiver: base_expr.ty,
                    });
                    self.error_expr(ast_id)
                }
            },
            _ => {
                self.body.diagnostics.push(ReceiverNotAStructOrVector {
                    expr: ast_id,
                    receiver: base_expr.ty,
                });
                self.error_expr(ast_id)
            }
        }
    }

    fn lower_call_expr(&mut self, ast_id: ExprAstId, func_name: &str, args: &[Id<def::body::Expr>]) -> TypedExprId {
        let args: Vec<_> = args.iter().map(|arg| self.lower_expr(*arg)).collect();
        let arg_types: Vec<_> = args.iter().map(|arg| arg.ty.clone()).collect();

        let function = self.resolver.resolve_value_name(func_name);
        match function {
            Some(ValueRes::Function(func)) => {
                // TODO: we don't support overloads right now for user-defined functions, but
                // since we do for builtins we should allow them at some point.

                // `resolve_overload` already emits diagnostics
                if let Some(signature) = self.resolve_overload(ast_id, func_name, &[func], &arg_types) {
                    let args = args
                        .into_iter()
                        .zip(signature.signature.parameter_types.iter())
                        .map(|(arg, param_ty)| self.apply_implicit_conversion(arg, param_ty.clone()).id)
                        .collect();
                    self.create_and_add_expr(
                        ast_id,
                        signature.signature.return_type,
                        ExprKind::Call { function: func, args },
                    )
                } else {
                    self.error_expr(ast_id)
                }
            }
            Some(ValueRes::BuiltinFunction(op)) => {
                // `verify_builtin_operation` already emits diagnostics
                if let Some(signature) = self.verify_builtin_operation(ast_id, &*op, &arg_types) {
                    let args = args
                        .into_iter()
                        .zip(signature.signature.parameter_types.iter())
                        .map(|(arg, param_ty)| self.apply_implicit_conversion(arg, param_ty.clone()).id)
                        .collect();
                    self.create_and_add_expr(
                        ast_id,
                        signature.signature.return_type,
                        ExprKind::BuiltinCall {
                            signature: op.signatures[signature.index],
                            args,
                        },
                    )
                } else {
                    self.error_expr(ast_id)
                }
            }
            _ => {
                self.body.diagnostics.push(UnresolvedFunction {
                    expr: ast_id,
                    name: func_name.to_string(),
                });
                self.error_expr(ast_id)
            }
        }
    }

    fn lower_name_expr(&mut self, ast_id: ExprAstId, name: &str) -> TypedExprId {
        match self.resolver.resolve_value_name(name) {
            None => {
                self.body.diagnostics.push(UnresolvedPath { expr: ast_id });
                self.error_expr(ast_id)
            }
            Some(value) => match value {
                ValueRes::BuiltinFunction(_) | ValueRes::Function(_) => {
                    self.body.diagnostics.push(ExpectedValue { expr: ast_id });
                    self.error_expr(ast_id)
                }
                ValueRes::Global(global) => self.create_and_add_expr(
                    ast_id,
                    Type::new(self.compiler, TypeKind::Ref(global.ty(self.compiler))),
                    ExprKind::GlobalVar { var: global },
                ),
                ValueRes::Local(local) => self.create_and_add_expr(
                    ast_id,
                    Type::new(self.compiler, TypeKind::Ref(self.body.local_vars[local].ty.clone())),
                    ExprKind::LocalVar { var: local },
                ),
            },
        }
    }

    fn lower_constructor_expr(
        &mut self,
        ast_id: ExprAstId,
        ty: &def::Type,
        args: &[Id<def::body::Expr>],
    ) -> TypedExprId {
        let ty = lower_ty(self.compiler, &self.resolver, ty, &mut self.body.diagnostics);

        let args: Vec<_> = args.iter().map(|arg| self.lower_expr(*arg)).collect();

        /*let mut arg_locations = vec![];
        let mut args = vec![];
        for arg in ast_args.arguments() {
            arg_locations.push(Span::new(self.source_file, arg.syntax().text_range()));
            args.push(self.typecheck_expr(&arg));
        }*/

        use ScalarType as ST;
        use TypeKind as TK;

        let has_scalar_conversion = |from: &TypeKind, to: &TypeKind| match (from, to) {
            (TK::Scalar(from_scalar), TK::Scalar(to_scalar)) => match (from_scalar, to_scalar) {
                (a, b) if a == b => true,
                (ST::UnsignedInt, ST::Int) => true,
                (ST::Bool, ST::Int) => true,
                (ST::Float, ST::Int) => true,
                (ST::Double, ST::Int) => true,
                (ST::Int, ST::UnsignedInt) => true,
                (ST::Bool, ST::UnsignedInt) => true,
                (ST::Float, ST::UnsignedInt) => true,
                (ST::Double, ST::UnsignedInt) => true,
                (ST::Int, ST::Bool) => true,
                (ST::UnsignedInt, ST::Bool) => true,
                (ST::Float, ST::Bool) => true,
                (ST::Double, ST::Bool) => true,
                (ST::Int, ST::Float) => true,
                (ST::UnsignedInt, ST::Float) => true,
                (ST::Bool, ST::Float) => true,
                (ST::Double, ST::Float) => true,
                (ST::Int, ST::Double) => true,
                (ST::UnsignedInt, ST::Double) => true,
                (ST::Bool, ST::Double) => true,
                (ST::Float, ST::Double) => true,
                _ => false,
            },
            _ => false,
        };

        match *ty.deref() {
            TK::Scalar(_) | TK::Vector(_, _) | TK::Matrix { .. } => {
                for &ctor in builtins::CONSTRUCTORS {
                    // Find a constructor signature for the specified type (`ctor.ty == *ty`),
                    // with the correct number of arguments (`ctor.args.len() == args.len()`)
                    // and with matching types: either the argument type is the same as the one in the signature (`*from.ty == *to`)
                    //  or the argument type is a scalar, and is implicitly convertible to the type in the signature (`has_scalar_conversion(&*from.ty, to)`)
                    //
                    // See the GLSL spec:
                    //
                    //       If the basic type (bool, int, float, or double) of a parameter to a constructor
                    //       does not match the basic type of the object being constructed, the scalar construction rules (above)
                    //       are used to convert the parameters.
                    //
                    // Constructor signatures shouldn't be ambiguous.
                    if ctor.ty == *ty
                        && ctor.args.len() == args.len()
                        && args
                            .iter()
                            .zip(ctor.args.iter())
                            .all(|(from, to)| *from.ty == *to || has_scalar_conversion(from.ty.unref(), to))
                    {
                        // Found a matching constructor signature.
                        // Apply implicit conversions on arguments.
                        let conv_args: Vec<_> = args
                            .into_iter()
                            .enumerate()
                            .map(|(i, arg)| {
                                let sig_ty = self.compiler.tyctxt().ty(ctor.args[i].clone());
                                self.apply_implicit_conversion(arg, sig_ty).id
                            })
                            .collect();
                        return self.create_and_add_expr(
                            ast_id,
                            ty,
                            ExprKind::BuiltinConstructor { ctor, args: conv_args },
                        );
                    }
                }

                let arg_tys = args.iter().map(|arg| arg.ty.clone()).collect::<Vec<_>>();
                self.body.diagnostics.push(NoMatchingConstructor {
                    expr: ast_id,
                    ty: ty.clone(),
                    arg_tys,
                });
                self.error_expr(ast_id)
            }
            TK::Array { .. } => {
                todo!("array constructors")
            }
            _ => {
                self.body.diagnostics.push(NoSuchTypeConstructor {
                    expr: ast_id,
                    ty: ty.clone(),
                });
                self.error_expr(ast_id)
            }
        }
    }

    fn lower_lit_expr(&mut self, ast_id: ExprAstId, value: &ConstantValue) -> TypedExprId {
        let ty = match value {
            ConstantValue::Int(_) => self.resolver.tyctxt().prim_tys.int.clone(),
            ConstantValue::Int64(_) => {
                todo!("int64 literal typeck")
            }
            ConstantValue::Float(_) => self.resolver.tyctxt().prim_tys.float.clone(),
            ConstantValue::Double(_) => {
                todo!("double literal typeck")
            }
            ConstantValue::String(_) => {
                todo!("string literal typeck")
            }
            ConstantValue::Bool(_) => self.resolver.tyctxt().prim_tys.bool.clone(),
        };

        self.create_and_add_expr(ast_id, ty, ExprKind::Literal { value: value.clone() })
    }

    fn lower_bin_expr(&mut self, ast_id: ExprAstId, op: BinaryOp, lhs: DefExprId, rhs: DefExprId) -> TypedExprId {
        let conv_arith_op = |arith_op| match arith_op {
            ast::ArithOp::Add => &builtins::Add,
            ast::ArithOp::Mul => &builtins::Mul,
            ast::ArithOp::Sub => &builtins::Sub,
            ast::ArithOp::Div => &builtins::Div,
            ast::ArithOp::Rem => &builtins::Rem,
            ast::ArithOp::Shl => &builtins::Shl,
            ast::ArithOp::Shr => &builtins::Shr,
            ast::ArithOp::BitXor => &builtins::BitXor,
            ast::ArithOp::BitOr => &builtins::BitOr,
            ast::ArithOp::BitAnd => &builtins::BitAnd,
        };

        // TODO typecheck operators as if they were a special kind of function
        // determine if this is an assignment, and the associated operation if there's one
        let is_assignment;
        let operation;
        match op {
            ast::BinaryOp::LogicOp(logic_op) => {
                is_assignment = false;
                operation = match logic_op {
                    ast::LogicOp::And => Some(&builtins::And),
                    ast::LogicOp::Or => Some(&builtins::Or),
                };
            }
            ast::BinaryOp::ArithOp(arith_op) => {
                is_assignment = false;
                operation = Some(conv_arith_op(arith_op));
            }
            ast::BinaryOp::CmpOp(cmp_op) => {
                is_assignment = false;
                operation = match cmp_op {
                    ast::CmpOp::Eq => Some(&builtins::Eq),
                    ast::CmpOp::Ne => Some(&builtins::Ne),
                    ast::CmpOp::Gt => Some(&builtins::Gt),
                    ast::CmpOp::Ge => Some(&builtins::Ge),
                    ast::CmpOp::Lt => Some(&builtins::Lt),
                    ast::CmpOp::Le => Some(&builtins::Le),
                };
            }
            ast::BinaryOp::Assignment(assign_op) => {
                is_assignment = true;
                operation = match assign_op {
                    None => None,
                    Some(arith_op) => Some(conv_arith_op(arith_op)),
                };
            }
        };

        let left = self.lower_expr(lhs);
        let right = self.lower_expr(rhs);

        if is_assignment && !left.is_ref() {
            self.body.diagnostics.push(ExpectedPlace {
                expr: left.ast_id,
                ty: left.ty.clone(),
            });
            //return self.error_expr(ast_id);
        }

        //let lhs_ast_id = self.body.exprs[ty_lhs.id].ast_id;
        //let rhs_ast_id = self.body.exprs[ty_rhs.id].ast_id;

        if let Some(operation) = operation {
            // binary operation, possibly with assignment (e.g `x * y` or `x *= y`)
            match self.verify_builtin_operation(ast_id, operation, &[left.ty.clone(), right.ty.clone()]) {
                Some(selected_overload) => {
                    let coerced_left_ty = selected_overload.signature.parameter_types[0].clone();
                    let coerced_right_ty = selected_overload.signature.parameter_types[1].clone();
                    let result_ty = selected_overload.signature.return_type;
                    let lhs_conv = self.apply_implicit_conversion(left.clone(), coerced_left_ty);
                    let rhs_conv = self.apply_implicit_conversion(right, coerced_right_ty);

                    // First emit tmp = x * y, then the assignment x = tmp, with all the necessary implicit conversions
                    // The temporary 'x * y' expression is synthesized and shares the same ast id as the assign expr
                    let op_result = self.create_and_add_expr(
                        ast_id,
                        result_ty,
                        ExprKind::Binary {
                            op,
                            signature: operation.signatures[selected_overload.index],
                            lhs: lhs_conv.id,
                            rhs: rhs_conv.id,
                        },
                    );

                    if is_assignment {
                        let op_result_conv = self.apply_implicit_conversion(op_result, left.ty.peel_ref());
                        self.create_and_add_expr(
                            ast_id,
                            self.compiler.tyctxt().prim_tys.void.clone(),
                            ExprKind::Assign {
                                lhs: left.id,
                                rhs: op_result_conv.id,
                            },
                        )
                    } else {
                        op_result
                    }
                }
                None => {
                    // don't emit diagnostics if one of the types is already the error type.
                    if !left.is_error() && !right.is_error() {
                        self.body.diagnostics.push(InvalidTypesForBinaryOp {
                            expr: ast_id,
                            left_ty: left.ty.clone(),
                            right_ty: right.ty.clone(),
                        });
                    }
                    return self.error_expr(ast_id);
                }
            }
        } else {
            // assignment only (`x = y`)
            let rhs_conv = self.apply_implicit_conversion(right, left.ty.peel_ref());
            self.create_and_add_expr(
                ast_id,
                self.compiler.tyctxt().prim_tys.void.clone(),
                ExprKind::Assign {
                    lhs: left.id,
                    rhs: rhs_conv.id,
                },
            )
        }
    }
}

pub(crate) fn lower_function_body(db: &dyn CompilerDb, function_id: FunctionId) -> ty::body::Body {
    let body = db.function_body(function_id);
    let func_data = function_id.data(db);
    let signature = function_id.signature(db);

    // Resolver for the function body
    let func_loc = function_id.loc(db);
    let mut resolver = func_loc.module.resolver(db);

    let mut ty_body = Body {
        stmts: IndexVec::with_capacity(body.statements.len()),
        exprs: IndexVec::with_capacity(body.expressions.len()),
        local_vars: IndexVec::with_capacity(body.local_vars.len()),
        blocks: IndexVec::with_capacity(body.blocks.len()),
        entry_block: None,
        params: vec![],
        diagnostics: vec![],
    };

    if !func_data.parameters.is_empty() {
        // add body parameters as local variables
        let mut param_scope = Scope::new();
        assert_eq!(func_data.parameters.len(), signature.parameter_types.len());
        for (param, param_ty) in func_data.parameters.iter().zip(signature.parameter_types.iter()) {
            let param_local_id = ty_body.local_vars.push(LocalVar {
                name: param.name.clone(),
                ast_id: LocalVarAstId::FnParam(param.ast),
                ty: param_ty.clone(),
            });
            param_scope.add_local_var(&param.name, param_local_id);
            ty_body.params.push(param_local_id);
        }

        resolver.push_scope(param_scope);
    }

    if let Some(entry_block) = body.entry_block {
        let mut ctxt = TyBodyLowerCtxt {
            compiler: db,
            body: ty_body,
            item_body: body,
            resolver,
        };

        let entry_block = ctxt.lower_block(entry_block);
        ctxt.body.entry_block = Some(entry_block);
        ctxt.body
    } else {
        panic!("lower_function_body called on function with no entry block");
    }
}
