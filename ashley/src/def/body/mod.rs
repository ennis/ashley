mod diagnostic;
mod lower;

pub use self::diagnostic::BodyDiagnostic;

use crate::{
    def::{
        body::lower::BodyLowerCtxt, diagnostic::ItemDiagnostic, BodyId, BodyKind, BodyOwnerId, DefLoc, DefMap,
        FunctionLoc, HasSource, Type,
    },
    syntax::ast,
    CompilerDb, ConstantValue,
};
use ashley::def::{BodyOwnerLoc, FunctionId};
use ashley_data_structures::{Id, IndexVec};
use rowan::ast::AstPtr;
use std::{collections::HashMap, ops::Index, sync::Arc};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalVar {
    pub ty: Type,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    // TODO empty statement?
    Select {
        condition: Id<Expr>,
        true_branch: Id<Statement>,
        false_branch: Option<Id<Statement>>,
    },
    ForLoop {
        initializer: Option<Id<Statement>>,
        condition: Option<Id<Expr>>,
        loop_expr: Option<Id<Expr>>,
        stmt: Id<Statement>,
    },
    WhileLoop {
        condition: Id<Expr>,
        stmt: Id<Statement>,
    },
    Local {
        var: Id<LocalVar>,
        initializer: Option<Id<Expr>>,
    },
    Return {
        value: Option<Id<Expr>>,
    },
    ExprStmt {
        expr: Id<Expr>,
    },
    Block {
        block: Id<Block>,
    },
    Break,
    Continue,
    Discard,
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Id<Statement>>,
}

impl Block {
    pub fn new() -> Block {
        Block { statements: Vec::new() }
    }
}

/// Expressions
// TODO: we could resolve some names here, but only global variables
// (fields would need the type of the expression, and functions need the argument type for overload resolution)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    // Including BinaryAssign and Assign
    Binary {
        op: ast::BinaryOp,
        lhs: Id<Expr>,
        rhs: Id<Expr>,
    },
    Prefix {
        op: ast::UnaryOp,
        expr: Id<Expr>,
    },
    Postfix {
        op: ast::UnaryOp,
        expr: Id<Expr>,
    },
    Ternary {
        condition: Id<Expr>,
        true_expr: Id<Expr>,
        false_expr: Id<Expr>,
    },
    Call {
        function: String,
        args: Vec<Id<Expr>>,
    },
    Name {
        name: String,
    },
    Index {
        array_or_vector: Id<Expr>,
        index: Id<Expr>,
    },
    Field {
        expr: Id<Expr>,
        name: String,
    },
    Constructor {
        ty: Type,
        args: Vec<Id<Expr>>,
    },
    Literal {
        value: ConstantValue,
    },
    Undef,
}

#[derive(Clone, PartialEq)]
pub struct BodyMap {
    expr_map: HashMap<AstPtr<ast::Expr>, Id<Expr>>,
    expr_map_back: IndexVec<AstPtr<ast::Expr>, Id<Expr>>,

    statement_map: HashMap<AstPtr<ast::Stmt>, Id<Statement>>,
    statement_map_back: IndexVec<AstPtr<ast::Stmt>, Id<Statement>>,

    block_map: HashMap<AstPtr<ast::Block>, Id<Block>>,
    block_map_back: IndexVec<AstPtr<ast::Block>, Id<Block>>,

    local_var_map: HashMap<AstPtr<ast::LocalVariable>, Id<LocalVar>>,
    local_var_map_back: IndexVec<AstPtr<ast::LocalVariable>, Id<LocalVar>>,

    /// Used for AstIds in def::Type
    def_map: DefMap,
}

impl Index<Id<Expr>> for BodyMap {
    type Output = AstPtr<ast::Expr>;
    fn index(&self, index: Id<Expr>) -> &Self::Output {
        &self.expr_map_back[index]
    }
}
impl Index<Id<Statement>> for BodyMap {
    type Output = AstPtr<ast::Stmt>;
    fn index(&self, index: Id<Statement>) -> &Self::Output {
        &self.statement_map_back[index]
    }
}
impl Index<Id<Block>> for BodyMap {
    type Output = AstPtr<ast::Block>;
    fn index(&self, index: Id<Block>) -> &Self::Output {
        &self.block_map_back[index]
    }
}
impl Index<Id<LocalVar>> for BodyMap {
    type Output = AstPtr<ast::LocalVariable>;
    fn index(&self, index: Id<LocalVar>) -> &Self::Output {
        &self.local_var_map_back[index]
    }
}

impl BodyMap {
    fn new() -> BodyMap {
        BodyMap {
            expr_map: Default::default(),
            expr_map_back: Default::default(),
            statement_map: Default::default(),
            statement_map_back: Default::default(),
            block_map: Default::default(),
            block_map_back: Default::default(),
            local_var_map: Default::default(),
            local_var_map_back: Default::default(),
            def_map: DefMap::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Body {
    pub statements: IndexVec<Statement>,
    pub expressions: IndexVec<Expr>,
    pub local_vars: IndexVec<LocalVar>,
    pub blocks: IndexVec<Block>,
    pub params: Vec<Id<LocalVar>>,
    pub diagnostics: Vec<BodyDiagnostic>,
    pub entry_block: Option<Id<Block>>,
}

impl Body {
    fn new() -> Body {
        Body {
            statements: Default::default(),
            expressions: Default::default(),
            local_vars: Default::default(),
            blocks: Default::default(),
            params: vec![],
            diagnostics: vec![],
            entry_block: None,
        }
    }

    pub fn root_expr(&self) -> Option<Id<Expr>> {
        if self.entry_block.is_some() {
            None
        } else {
            if self.expressions.is_empty() {
                None
            } else {
                Some(Id::from_index(0))
            }
        }
    }
}

pub(crate) fn function_body_query(db: &dyn CompilerDb, function_id: FunctionId) {
    let _span = trace_span!("function_body_query", ?function_id).entered();
    let func_data = db.function_data(function_id);
    let Some(body) = func_data.body else {
        return;
    };
    let body_src = function_id.loc(db).node_from_id(db, body);

    let ctxt = BodyLowerCtxt::new(db, body_src.file);
    let (body, body_map) = ctxt.lower_body_block(body_src.data);

    db.set_function_body(function_id, body);
    db.set_function_body_map(function_id, body_map);
}

/*pub(crate) fn body_and_map_query(db: &dyn CompilerDb, body_id: BodyId) {
    let _span = trace_span!("body_and_map_query", ?body_id).entered();
    let ctxt = BodyLowerCtxt::new(db);
    let body_loc = db.lookup_body_id(body_id);

    let module = match body_loc.owner {
        BodyOwnerLoc::Def(def_loc) => def_loc.module(),
        BodyOwnerLoc::FunctionBody(function) => {}
    };

    /*let (body, body_map) = match body_loc.kind {
        BodyKind::Block(ref block) => ctxt.lower_body_block_opt(Some(block.to_node(db, module))),
        BodyKind::Expr(ref expr) => ctxt.lower_const_expr(expr.to_node(db, module)),
    };*/

    //db.set_body(body_id, body);
    //db.set_body_map(body_id, body_map);

    todo!()
}
*/
