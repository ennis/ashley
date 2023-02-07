use crate::{
    syntax::ast,
    tast::{
        consteval::ConstantValue, expr::ExprKind, scope::Scope, Block, BlockId, Expr, ExprId, StmtId,
        TypeCheckBodyCtxt, TypedBody,
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
    Error,
}

impl<'a, 'diag> TypeCheckBodyCtxt<'a, 'diag> {
    /// Typecheck a block and return the id of the block.
    pub fn typecheck_block(&mut self, block: &ast::Block) -> BlockId {
        let mut stmts = Vec::new();
        for stmt in block.stmts() {
            let id = self.typed_body.stmts.push(Stmt {
                ast: stmt.clone(),
                kind: self.typecheck_stmt(&stmt),
            });
            stmts.push(id);
        }
        self.typed_body.blocks.push(Block { stmts })
    }

    /*fn error_stmt(&mut self, ast: &ast::Stmt) -> StmtId {
        self.typed_body.stmts.push(Stmt {
            ast: ast.clone(),
            kind: StmtKind::Error,
        })
    }*/

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

        self.scopes.push(Scope::new());
        let true_stmt = self.typecheck_stmt(&stmt);
        self.scopes.pop();

        let false_stmt = if let Some(else_branch) = if_stmt.else_branch() {
            if let Some(stmt) = else_branch.stmt() {
                self.scopes.push(Scope::new());
                let id = self.typecheck_stmt(&stmt);
                self.scopes.pop();
                Some(id)
            } else {
                None
            }
        } else {
            None
        };

        StmtKind::Select {
            condition: condition.id,
            true_branch: true_stmt,
            false_branch: false_stmt,
        }
    }

    /// Typechecks an expression statement and return the statement kind.
    fn typecheck_expr_stmt(&mut self, expr_stmt: &ast::ExprStmt) -> StmtKind {
        if let Some(ast_expr) = expr_stmt.expr() {
            let expr = self.typecheck_expr(&ast_expr);
            let id = self.typed_body.exprs.push(Expr {
                ast: Some(ast_expr.clone()),
                ty: expr.ty,
                kind: expr.kind,
            });
            StmtKind::ExprStmt { expr: id }
        } else {
            StmtKind::Error
        }
    }

    /// Typechecks a statement and return the statement kind.
    pub fn typecheck_stmt(&mut self, stmt: &ast::Stmt) -> StmtKind {
        match stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.typecheck_expr_stmt(&expr_stmt),
            ast::Stmt::ReturnStmt(_) => {}
            ast::Stmt::BlockStmt(_) => {}
            ast::Stmt::WhileStmt(_) => {}
            ast::Stmt::BreakStmt(_) => {}
            ast::Stmt::ContinueStmt(_) => {}
            ast::Stmt::DiscardStmt(_) => {}
            ast::Stmt::LocalVariable(_) => {}
            ast::Stmt::IfStmt(if_stmt) => self.typecheck_if_stmt(if_stmt),
            ast::Stmt::ForStmt(_) => {}
        }
    }
}
