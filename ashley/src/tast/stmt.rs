use crate::{
    syntax::{ast, ast::AstPtr},
    tast::{
        scope::{Res, Scope},
        Block, BlockId, ExprId, IdentExt, LocalVar, LocalVarId, StmtId, TypeCheckBodyCtxt,
    },
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Stmt {
    pub ast: AstPtr<ast::Stmt>,
    pub kind: StmtKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StmtKind {
    // TODO empty statement?
    Select {
        condition: ExprId,
        true_branch: StmtId,
        false_branch: Option<StmtId>,
    },
    ForLoop {
        initializer: Option<StmtId>,
        condition: Option<ExprId>,
        loop_expr: Option<ExprId>,
        stmt: StmtId,
    },
    Local {
        var: LocalVarId,
        initializer: Option<ExprId>,
    },
    Return {
        value: Option<ExprId>,
    },
    ExprStmt {
        expr: ExprId,
    },
    Block {
        block: BlockId,
    },
    Error,
}

impl<'a> TypeCheckBodyCtxt<'a> {
    /// Typecheck a block and return the id of the block.
    pub fn typecheck_block(&mut self, block: &ast::Block) -> BlockId {
        let mut stmts = Vec::new();
        for stmt in block.stmts() {
            stmts.push(self.typecheck_stmt(&stmt));
        }
        self.typed_body.blocks.push(Block { stmts })
    }

    /*fn error_stmt(&mut self, ast: &ast::Stmt) -> StmtId {
        self.typed_body.stmts.push(Stmt {
            ast: ast.clone(),
            kind: StmtKind::Error,
        })
    }*/

    /*fn add_stmt(&mut self, stmt: Stmt) -> StmtId {
        self.typed_body.stmts.push(stmt)
    }*/

    fn typecheck_if_stmt(&mut self, if_stmt: &ast::IfStmt) -> StmtKind {
        let Some(ast_condition) = if_stmt.condition().and_then(|c| c.expr()) else {
            return StmtKind::Error
        };
        let condition = self.typecheck_expr(&ast_condition);

        let Some(stmt) = if_stmt.stmt() else {
            return StmtKind::Error
        };

        if condition.ty != self.compiler.tyctxt().prim_tys.bool {
            self.compiler
                .diag_error("condition must be a boolean expression")
                .location(&ast_condition)
                .emit();
        }

        let true_branch = self.typecheck_stmt_in_new_scope(&stmt);

        let false_branch = if let Some(else_branch) = if_stmt.else_branch() {
            if let Some(else_stmt) = else_branch.stmt() {
                Some(self.typecheck_stmt_in_new_scope(&else_stmt))
            } else {
                None
            }
        } else {
            None
        };

        let condition = self.add_expr(condition);
        StmtKind::Select {
            condition,
            true_branch,
            false_branch,
        }
    }

    fn typecheck_for_stmt_inner(&mut self, for_stmt: &ast::ForStmt) -> StmtKind {
        let initializer = if let Some(init) = for_stmt.initializer() {
            if let Some(stmt) = init.stmt() {
                Some(self.typecheck_stmt(&stmt))
            } else {
                None
            }
        } else {
            return StmtKind::Error;
        };

        //
        let condition = if let Some(cond) = for_stmt.condition() {
            if let Some(cond_expr) = cond.expr() {
                let c = self.typecheck_expr(&cond_expr);
                Some(self.add_expr(c))
            } else {
                None
            }
        } else {
            return StmtKind::Error;
        };

        let loop_expr = if let Some(loop_expr) = for_stmt.loop_expr() {
            if let Some(e) = loop_expr.expr() {
                let e = self.typecheck_expr(&e);
                Some(self.add_expr(e))
            } else {
                None
            }
        } else {
            return StmtKind::Error;
        };

        let body = if let Some(body) = for_stmt.body() {
            self.typecheck_stmt(&body)
        } else {
            return StmtKind::Error;
        };

        StmtKind::ForLoop {
            initializer,
            condition,
            loop_expr,
            stmt: body,
        }
    }

    fn typecheck_for_stmt(&mut self, for_stmt: &ast::ForStmt) -> StmtKind {
        self.scopes.push(Scope::new());
        let r = self.typecheck_for_stmt_inner(for_stmt);
        self.scopes.pop();
        r
    }

    /// Typechecks an expression statement and return the statement kind.
    fn typecheck_expr_stmt(&mut self, expr_stmt: &ast::ExprStmt) -> StmtKind {
        if let Some(ast_expr) = expr_stmt.expr() {
            let expr = self.typecheck_expr(&ast_expr);
            let id = self.typed_body.exprs.push(expr);
            StmtKind::ExprStmt { expr: id }
        } else {
            StmtKind::Error
        }
    }

    fn typecheck_block_stmt(&mut self, block_stmt: &ast::BlockStmt) -> StmtKind {
        let Some(block) = block_stmt.block() else {
            return StmtKind::Error
        };
        StmtKind::Block {
            block: self.typecheck_block(&block),
        }
    }

    fn typecheck_local_variable_stmt(&mut self, local: &ast::LocalVariable) -> StmtKind {
        let ty = local
            .ty()
            .map(|t| self.convert_type(t))
            .unwrap_or_else(|| self.compiler.tyctxt().error.clone());

        let initializer = if let Some(initializer) = local.initializer() {
            if let Some(initializer) = initializer.expr() {
                let expr = self.typecheck_expr(&initializer);
                Some(self.add_expr(expr))
            } else {
                None
            }
        } else {
            None
        };

        let name = local.name().to_string_opt();
        let local_var_id = self.typed_body.local_vars.push(LocalVar { name: name.clone(), ty });
        self.scopes.last_mut().unwrap().add(name, Res::Local(local_var_id));

        StmtKind::Local {
            var: local_var_id,
            initializer,
        }
    }

    fn typecheck_return_stmt(&mut self, return_stmt: &ast::ReturnStmt) -> StmtKind {
        if let Some(expr) = return_stmt.expr() {
            let return_value = self.typecheck_expr(&expr);
            let return_value = self.add_expr(return_value);
            StmtKind::Return {
                value: Some(return_value),
            }
        } else {
            StmtKind::Return { value: None }
        }
    }

    /// Typechecks a statement and return the statement kind.
    pub fn typecheck_stmt(&mut self, stmt: &ast::Stmt) -> StmtId {
        let kind = match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.typecheck_expr_stmt(expr_stmt),
            ast::Stmt::ReturnStmt(return_stmt) => self.typecheck_return_stmt(return_stmt),
            ast::Stmt::BlockStmt(block) => self.typecheck_block_stmt(block),
            ast::Stmt::WhileStmt(_) => {
                todo!("while stmt")
            }
            ast::Stmt::BreakStmt(_) => {
                todo!("break stmt")
            }
            ast::Stmt::ContinueStmt(_) => {
                todo!("continue stmt")
            }
            ast::Stmt::DiscardStmt(_) => {
                todo!("discard stmt")
            }
            ast::Stmt::LocalVariable(local) => self.typecheck_local_variable_stmt(local),
            ast::Stmt::IfStmt(if_stmt) => self.typecheck_if_stmt(if_stmt),
            ast::Stmt::ForStmt(for_stmt) => self.typecheck_for_stmt(for_stmt),
        };
        self.typed_body.stmts.push(Stmt {
            ast: AstPtr::new(stmt),
            kind,
        })
    }

    pub fn typecheck_stmt_in_new_scope(&mut self, stmt: &ast::Stmt) -> StmtId {
        self.scopes.push(Scope::new());
        let id = self.typecheck_stmt(stmt);
        self.scopes.pop();
        id
    }
}
