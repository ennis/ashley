use crate::{
    syntax::ast,
    tast::{
        scope::Scope, Block, BlockId, Expr, ExprId, StmtId,
        TypeCheckBodyCtxt,
    },
};

pub struct Stmt {
    pub ast: ast::Stmt,
    pub kind: StmtKind,
}

pub enum StmtKind {
    Select {
        condition: ExprId,
        true_branch: StmtId,
        false_branch: Option<StmtId>,
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

impl<'a, 'diag> TypeCheckBodyCtxt<'a, 'diag> {
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

    fn add_stmt(&mut self, stmt: Stmt) -> StmtId {
        self.typed_body.stmts.push(stmt)
    }

    fn typecheck_if_stmt(&mut self, if_stmt: &ast::IfStmt) -> StmtKind {
        let Some(ast_condition) = if_stmt.condition().and_then(|c| c.expr()) else {
            return StmtKind::Error
        };
        let Some(condition) = if_stmt.condition().and_then(|c| c.expr()).map(|c| self.typecheck_expr(&c)) else {
            return StmtKind::Error
        };

        let Some(stmt) = if_stmt.stmt() else {
            return StmtKind::Error
        };

        if condition.ty != self.tyctxt.builtins.bool {
            self.diag
                .error("condition must be a boolean expression")
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

    /// Typechecks an expression statement and return the statement kind.
    fn typecheck_expr_stmt(&mut self, expr_stmt: &ast::ExprStmt) -> StmtKind {
        if let Some(ast_expr) = expr_stmt.expr() {
            let expr = self.typecheck_expr(&ast_expr);
            let id = self.typed_body.exprs.push(Expr {
                ast: Some(ast_expr.clone()),
                ty: expr.ty,
                kind: expr.expr,
            });
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

    /// Typechecks a statement and return the statement kind.
    pub fn typecheck_stmt(&mut self, stmt: &ast::Stmt) -> StmtId {
        let kind = match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.typecheck_expr_stmt(&expr_stmt),
            ast::Stmt::ReturnStmt(_) => {
                todo!("return stmt")
            }
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
            ast::Stmt::LocalVariable(_) => {
                todo!("local variable stmt")
            }
            ast::Stmt::IfStmt(if_stmt) => self.typecheck_if_stmt(if_stmt),
            ast::Stmt::ForStmt(_) => {
                todo!("for stmt")
            }
        };
        self.typed_body.stmts.push(Stmt {
            ast: stmt.clone(),
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
