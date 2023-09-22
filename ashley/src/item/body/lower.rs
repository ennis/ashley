use crate::{
    item::body::{Block, Body, BodyMap, Expr, LocalVar, Statement},
    syntax::ast,
    CompilerDb,
};
use ashley_data_structures::Id;
use rowan::ast::{AstNode, AstPtr};

pub(crate) struct BodyLowerCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    map: BodyMap,
    body: Body,
}

impl<'a> BodyLowerCtxt<'a> {
    pub(crate) fn new(compiler: &'a dyn CompilerDb) -> BodyLowerCtxt<'a> {
        BodyLowerCtxt {
            compiler,
            map: BodyMap::new(),
            body: Body::new(),
        }
    }

    fn add_expr(&mut self, source: &ast::Expr, expr: Expr) -> Id<Expr> {
        let ptr = AstPtr::new(source);
        let id = self.body.expressions.push(expr);
        self.map.expr_map.insert(ptr.clone(), id);
        let id2 = self.map.expr_map_back.push(ptr);
        assert_eq!(id, id2);
        id
    }

    fn add_statement(&mut self, source: &ast::Stmt, stmt: Statement) -> Id<Statement> {
        let ptr = AstPtr::new(source);
        let id = self.body.statements.push(stmt);
        self.map.statement_map.insert(ptr.clone(), id);
        let id2 = self.map.statement_map_back.push(ptr);
        assert_eq!(id, id2);
        id
    }

    fn add_block(&mut self, source: &ast::Block, block: Block) -> Id<Block> {
        let ptr = AstPtr::new(source);
        let id = self.body.blocks.push(block);
        self.map.block_map.insert(ptr.clone(), id);
        let id2 = self.map.block_map_back.push(ptr);
        assert_eq!(id, id2);
        id
    }

    fn add_local_var(&mut self, source: &ast::LocalVariable, local_var: LocalVar) -> Id<LocalVar> {
        let ptr = AstPtr::new(source);
        let id = self.body.local_vars.push(local_var);
        self.map.local_var_map.insert(ptr.clone(), id);
        let id2 = self.map.local_var_map_back.push(ptr);
        assert_eq!(id, id2);
        id
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn lower_expr(&mut self, expr: ast::Expr) -> Option<Id<Expr>> {
        todo!()
    }

    fn lower_indexing_expr(&mut self, indexing_expr: ast::IndexExpr) -> Option<Expr> {
        todo!()
    }

    fn lower_call_expr(&mut self, call_expr: ast::CallExpr) -> Option<Expr> {
        todo!()
    }

    fn lower_prefix_expr(&mut self, prefix_expr: ast::PrefixExpr) -> Option<Expr> {
        todo!()
    }

    fn lower_postfix_expr(&mut self, postfix_expr: ast::PostfixExpr) -> Option<Expr> {
        todo!()
    }

    fn lower_path_expr(&mut self, path_expr: ast::PathExpr) -> Option<Expr> {
        todo!()
    }

    fn lower_field_expr(&mut self, field_expr: ast::FieldExpr) -> Option<Expr> {
        todo!()
    }

    fn lower_bin_expr(&mut self, bin_expr: ast::BinExpr) -> Option<Expr> {
        todo!()
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn lower_return_stmt(&mut self, return_stmt: &ast::ReturnStmt) -> Option<Statement> {
        let return_value = if let Some(expr) = return_stmt.expr() {
            Some(self.lower_expr(expr)?)
        } else {
            None
        };

        Some(Statement::Return { value: return_value })
    }

    fn lower_expr_stmt(&mut self, expr_stmt: &ast::ExprStmt) -> Option<Statement> {
        Some(Statement::ExprStmt {
            expr: self.lower_expr(expr_stmt.expr()?)?,
        })
    }

    fn lower_block_stmt(&mut self, block_stmt: &ast::BlockStmt) -> Option<Statement> {
        let block = self.lower_block(block_stmt.block()?);
        Some(Statement::Block { block })
    }

    fn lower_if_stmt(&mut self, if_stmt: &ast::IfStmt) -> Option<Statement> {
        Some(Statement::Select {
            condition: self.lower_expr(if_stmt.condition()?.expr()?)?,
            true_branch: self.lower_statement(if_stmt.stmt()?)?,
            false_branch: if let Some(else_branch) = if_stmt.else_branch() {
                Some(self.lower_statement(else_branch.stmt()?)?)
            } else {
                None
            },
        })
    }

    fn lower_for_stmt(&mut self, for_stmt: &ast::ForStmt) -> Option<Statement> {
        let initializer = if let Some(stmt) = for_stmt.initializer()?.stmt() {
            Some(self.lower_statement(stmt)?)
        } else {
            None
        };

        let condition = if let Some(cond_expr) = for_stmt.condition()?.expr() {
            Some(self.lower_expr(cond_expr)?)
        } else {
            None
        };

        let loop_expr = if let Some(loop_expr) = for_stmt.loop_expr()?.expr() {
            Some(self.lower_expr(loop_expr)?)
        } else {
            None
        };

        let stmt = self.lower_statement(for_stmt.body()?)?;

        Some(Statement::ForLoop {
            initializer,
            condition,
            loop_expr,
            stmt,
        })
    }

    fn lower_statement(&mut self, stmt: ast::Stmt) -> Option<Id<Statement>> {
        let s = match &stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.lower_expr_stmt(expr_stmt),
            ast::Stmt::ReturnStmt(return_stmt) => self.lower_return_stmt(return_stmt),
            ast::Stmt::BlockStmt(block) => self.lower_block_stmt(block),
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
            ast::Stmt::LocalVariable(local) => {
                todo!("local var")
                //self.lower_local_variable_stmt(local)
            }
            ast::Stmt::IfStmt(if_stmt) => self.lower_if_stmt(if_stmt),
            ast::Stmt::ForStmt(for_stmt) => self.lower_for_stmt(for_stmt),
        };

        s.map(|s| self.add_statement(&stmt, s))
    }

    fn lower_block(&mut self, block: ast::Block) -> Id<Block> {
        let mut statements = vec![];
        for stmt in block.stmts() {
            if let Some(s) = self.lower_statement(stmt) {
                statements.push(s);
            }
        }
        self.add_block(&block, Block { statements })
    }

    pub(super) fn lower_body_block_opt(mut self, body_block: Option<ast::Block>) -> (Body, BodyMap) {
        // TODO why Option?
        self.lower_block(body_block.unwrap());
        (self.body, self.map)
    }

    pub(super) fn lower_const_expr(mut self, expr: ast::Expr) -> (Body, BodyMap) {
        let expr = self.lower_expr(expr).unwrap();
        (self.body, self.map)
    }
}
