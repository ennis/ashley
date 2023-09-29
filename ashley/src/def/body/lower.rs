use crate::{
    def::{
        body::{Block, Body, BodyDiagnostic, BodyMap, Expr, ExprKind, LocalVar, Statement, StmtKind},
        lower::{lower_type, lower_type_opt, ItemLowerCtxt},
        InFile,
    },
    syntax::{ast, ast::AstToken},
    CompilerDb, ConstantValue, SourceFileId,
};
use ashley_data_structures::Id;
use ordered_float::OrderedFloat;
use rowan::ast::{AstNode, AstPtr};

pub(crate) struct BodyLowerCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    map: BodyMap,
    body: Body,
    source_file: SourceFileId,
}

impl<'a> BodyLowerCtxt<'a> {
    pub(crate) fn new(compiler: &'a dyn CompilerDb, source_file: SourceFileId) -> BodyLowerCtxt<'a> {
        BodyLowerCtxt {
            compiler,
            map: BodyMap::new(),
            body: Body::new(),
            source_file,
        }
    }

    fn add_expr(&mut self, source: &ast::Expr, expr: Expr) -> Id<Expr> {
        let ptr = AstPtr::new(source);
        let id = self.body.expressions.push(expr);
        self.map.expr_map.insert(ptr.clone(), id);
        //let id2 = self.map.expr_map_back.push(ptr);
        //assert_eq!(id, id2);
        id
    }

    fn add_statement(&mut self, source: &ast::Stmt, stmt: Statement) -> Id<Statement> {
        let ptr = AstPtr::new(source);
        let id = self.body.statements.push(stmt);
        self.map.statement_map.insert(ptr.clone(), id);
        //let id2 = self.map.statement_map_back.push(ptr);
        //assert_eq!(id, id2);
        id
    }

    fn add_block(&mut self, source: &ast::Block, block: Block) -> Id<Block> {
        let ptr = AstPtr::new(source);
        let id = self.body.blocks.push(block);
        self.map.block_map.insert(ptr.clone(), id);
        //let id2 = self.map.block_map_back.push(ptr);
        //assert_eq!(id, id2);
        id
    }

    fn add_local_var(&mut self, source: &ast::LocalVariable, local_var: LocalVar) -> Id<LocalVar> {
        let ptr = AstPtr::new(source);
        let id = self.body.local_vars.push(local_var);
        self.map.local_var_map.insert(ptr.clone(), id);
        //let id2 = self.map.local_var_map_back.push(ptr);
        //assert_eq!(id, id2);
        id
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn lower_expr(&mut self, expr: ast::Expr) -> Option<Id<Expr>> {
        let expr_data = match expr {
            ast::Expr::BinExpr(ref bin_expr) => self.lower_bin_expr(bin_expr),
            ast::Expr::PrefixExpr(ref prefix_expr) => self.lower_prefix_expr(prefix_expr),
            ast::Expr::PostfixExpr(ref postfix_expr) => self.lower_postfix_expr(postfix_expr),
            ast::Expr::CallExpr(ref call_expr) => self.lower_call_expr(call_expr),
            ast::Expr::IndexExpr(ref index_expr) => self.lower_indexing_expr(index_expr),
            ast::Expr::ParenExpr(ref paren_expr) => {
                return self.lower_expr(paren_expr.expr()?);
            }
            ast::Expr::LitExpr(ref lit_expr) => self.lower_lit_expr(lit_expr),
            ast::Expr::PathExpr(ref path_expr) => self.lower_path_expr(path_expr),
            ast::Expr::TupleExpr(_) => {
                todo!("tuples")
            }
            ast::Expr::ArrayExpr(ref array_expr) => {
                todo!("array exprs")
            }
            ast::Expr::FieldExpr(ref field_expr) => self.lower_field_expr(field_expr),
            ast::Expr::TernaryExpr(ref ternary_expr) => self.lower_ternary_expr(ternary_expr),
            ast::Expr::ConstructorExpr(ref ctor_expr) => self.lower_ctor_expr(ctor_expr),
        };

        let Some(expr_data) = expr_data else {
            trace!("failed to lower expression `{}`", expr.syntax().text().to_string());
            return None;
        };

        let ast_id = self.map.def_map.push(&expr);
        let id = self.add_expr(
            &expr,
            Expr {
                ast_id,
                kind: expr_data,
            },
        );
        Some(id)
    }

    fn lower_indexing_expr(&mut self, indexing_expr: &ast::IndexExpr) -> Option<ExprKind> {
        let array = self.lower_expr(indexing_expr.array()?)?;
        let index = self.lower_expr(indexing_expr.index()?)?;
        Some(ExprKind::Index {
            array_or_vector: array,
            index,
        })
    }

    fn lower_call_expr(&mut self, call_expr: &ast::CallExpr) -> Option<ExprKind> {
        let callee = call_expr.callee()?;
        let function = match callee {
            ast::Expr::PathExpr(ref func_path) => func_path.name()?.text(),
            _ => {
                self.body.diagnostics.push(BodyDiagnostic::ExpectedFunctionName {
                    callee: InFile::new_ast_ptr(self.source_file, &callee),
                });
                return None;
            }
        };

        let mut args = vec![];
        for arg in call_expr.arg_list()?.arguments() {
            let Some(arg_expr) = self.lower_expr(arg) else { continue };
            args.push(arg_expr);
        }
        Some(ExprKind::Call { function, args })
    }

    fn lower_ctor_expr(&mut self, ctor_expr: &ast::ConstructorExpr) -> Option<ExprKind> {
        let ty = lower_type_opt(self.compiler, &mut self.map.def_map, &ctor_expr.ty());
        let mut args = vec![];
        for arg in ctor_expr.arg_list()?.arguments() {
            let Some(arg_expr) = self.lower_expr(arg) else { continue };
            args.push(arg_expr);
        }
        Some(ExprKind::Constructor { ty, args })
    }

    fn lower_prefix_expr(&mut self, prefix_expr: &ast::PrefixExpr) -> Option<ExprKind> {
        Some(ExprKind::Prefix {
            op: prefix_expr.op_details()?.1,
            expr: self.lower_expr(prefix_expr.expr()?)?,
        })
    }

    fn lower_postfix_expr(&mut self, postfix_expr: &ast::PostfixExpr) -> Option<ExprKind> {
        Some(ExprKind::Postfix {
            op: postfix_expr.op_details()?.1,
            expr: self.lower_expr(postfix_expr.expr()?)?,
        })
    }

    fn lower_path_expr(&mut self, path_expr: &ast::PathExpr) -> Option<ExprKind> {
        Some(ExprKind::Name {
            name: path_expr.name()?.text(),
        })
    }

    fn lower_field_expr(&mut self, field_expr: &ast::FieldExpr) -> Option<ExprKind> {
        Some(ExprKind::Field {
            expr: self.lower_expr(field_expr.expr()?)?,
            name: field_expr.field()?.text(),
        })
    }

    fn lower_bin_expr(&mut self, bin_expr: &ast::BinExpr) -> Option<ExprKind> {
        Some(ExprKind::Binary {
            op: bin_expr.op_details()?.1,
            lhs: self.lower_expr(bin_expr.lhs()?)?,
            rhs: self.lower_expr(bin_expr.rhs()?)?,
        })
    }

    fn lower_ternary_expr(&mut self, ternary_expr: &ast::TernaryExpr) -> Option<ExprKind> {
        Some(ExprKind::Ternary {
            condition: self.lower_expr(ternary_expr.condition()?.expr()?)?,
            true_expr: self.lower_expr(ternary_expr.true_alt()?)?,
            false_expr: self.lower_expr(ternary_expr.false_alt()?)?,
        })
    }

    fn lower_lit_expr(&mut self, lit_expr: &ast::LitExpr) -> Option<ExprKind> {
        let value = match lit_expr.kind() {
            ast::LiteralKind::String(str) => ConstantValue::String(str.text().to_string()),
            ast::LiteralKind::IntNumber(v) => match v.value_i32() {
                Ok(v) => ConstantValue::Int(v as u32),
                Err(err) => {
                    self.body.diagnostics.push(BodyDiagnostic::ParseIntError {
                        lit_expr: InFile::new_ast_ptr(self.source_file, &lit_expr),
                    });
                    return Some(ExprKind::Undef);
                }
            },
            ast::LiteralKind::FloatNumber(v) => {
                match v.value_f32() {
                    Ok(v) => {
                        // TODO warn about non-representable floats
                        ConstantValue::Float(OrderedFloat::from(v))
                    }
                    Err(err) => {
                        self.body.diagnostics.push(BodyDiagnostic::ParseFloatError {
                            lit_expr: InFile::new_ast_ptr(self.source_file, &lit_expr),
                        });
                        return Some(ExprKind::Undef);
                    }
                }
            }
            ast::LiteralKind::Bool(v) => ConstantValue::Bool(v),
        };
        Some(ExprKind::Literal { value })
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////

    fn lower_return_stmt(&mut self, return_stmt: &ast::ReturnStmt) -> Option<StmtKind> {
        let return_value = if let Some(expr) = return_stmt.expr() {
            Some(self.lower_expr(expr)?)
        } else {
            None
        };

        Some(StmtKind::Return { value: return_value })
    }

    fn lower_expr_stmt(&mut self, expr_stmt: &ast::ExprStmt) -> Option<StmtKind> {
        Some(StmtKind::ExprStmt {
            expr: self.lower_expr(expr_stmt.expr()?)?,
        })
    }

    fn lower_block_stmt(&mut self, block_stmt: &ast::BlockStmt) -> Option<StmtKind> {
        let block = self.lower_block(block_stmt.block()?);
        Some(StmtKind::Block { block })
    }

    fn lower_if_stmt(&mut self, if_stmt: &ast::IfStmt) -> Option<StmtKind> {
        Some(StmtKind::Select {
            condition: self.lower_expr(if_stmt.condition()?.expr()?)?,
            true_branch: self.lower_statement(if_stmt.stmt()?)?,
            false_branch: if let Some(else_branch) = if_stmt.else_branch() {
                Some(self.lower_statement(else_branch.stmt()?)?)
            } else {
                None
            },
        })
    }

    fn lower_for_stmt(&mut self, for_stmt: &ast::ForStmt) -> Option<StmtKind> {
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

        Some(StmtKind::ForLoop {
            initializer,
            condition,
            loop_expr,
            stmt,
        })
    }

    fn lower_while_stmt(&mut self, while_stmt: &ast::WhileStmt) -> Option<StmtKind> {
        let condition = self.lower_expr(while_stmt.condition()?.expr()?)?;

        let stmt = self.lower_statement(while_stmt.stmt()?)?;

        Some(StmtKind::WhileLoop { condition, stmt })
    }

    fn lower_break_stmt(&mut self, _break: ast::BreakStmt) -> Option<StmtKind> {
        Some(StmtKind::Error)
    }

    fn lower_local_variable_stmt(&mut self, local_var_stmt: &ast::LocalVariable) -> Option<StmtKind> {
        let ty = lower_type_opt(self.compiler, &mut self.map.def_map, &local_var_stmt.ty());
        let name = local_var_stmt.name()?.text();
        let initializer = if let Some(initializer) = local_var_stmt.initializer() {
            Some(self.lower_expr(initializer.expr()?)?)
        } else {
            None
        };
        let ast_id = self.map.def_map.push(local_var_stmt);
        let var = self.add_local_var(&local_var_stmt, LocalVar { ast_id, ty, name });
        Some(StmtKind::Local { var, initializer })
    }

    fn lower_statement(&mut self, stmt: ast::Stmt) -> Option<Id<Statement>> {
        let s = match &stmt {
            ast::Stmt::ExprStmt(expr_stmt) => self.lower_expr_stmt(expr_stmt),
            ast::Stmt::ReturnStmt(return_stmt) => self.lower_return_stmt(return_stmt),
            ast::Stmt::BlockStmt(block) => self.lower_block_stmt(block),
            ast::Stmt::WhileStmt(while_stmt) => {
                todo!("while stmt")
            }
            ast::Stmt::BreakStmt(_break_stmt) => Some(StmtKind::Break),
            ast::Stmt::ContinueStmt(_continue_stmt) => Some(StmtKind::Continue),
            ast::Stmt::DiscardStmt(_discard_stmt) => Some(StmtKind::Discard),
            ast::Stmt::LocalVariable(local_stmt) => self.lower_local_variable_stmt(local_stmt),
            ast::Stmt::IfStmt(if_stmt) => self.lower_if_stmt(if_stmt),
            ast::Stmt::ForStmt(for_stmt) => self.lower_for_stmt(for_stmt),
        };

        let Some(s) = s else {
            trace!("failed to lower statement `{}`", stmt.syntax().text().to_string());
            return None;
        };

        let ast_id = self.map.def_map.push(&stmt);
        Some(self.add_statement(&stmt, Statement { ast_id, kind: s }))
    }

    fn lower_block(&mut self, block: ast::Block) -> Id<Block> {
        let mut statements = vec![];
        for stmt in block.stmts() {
            if let Some(s) = self.lower_statement(stmt) {
                statements.push(s);
            }
        }
        let ast_id = self.map.def_map.push(&block);
        self.add_block(&block, Block { ast_id, statements })
    }

    pub(super) fn lower_body_block(mut self, body_block: ast::Block) -> (Body, BodyMap) {
        let block = self.lower_block(body_block);
        self.body.entry_block = Some(block);
        (self.body, self.map)
    }

    pub(super) fn lower_const_expr(mut self, expr: ast::Expr) -> (Body, BodyMap) {
        let expr = self.lower_expr(expr).unwrap();
        (self.body, self.map)
    }
}
