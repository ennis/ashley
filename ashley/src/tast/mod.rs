//! Typed AST representation

mod ty;
mod decl;
mod item;

use std::hash::{Hash, Hasher};
use std::sync::Arc;
use crate::hir;
use crate::hir::types::ScalarType;
use crate::lower::swizzle::ComponentIndices;
use crate::lower::typecheck::BuiltinOperation;
use crate::syntax::ast;

// TODO: separate the items / types from the bodies of functions and initializers

macro_rules! ast_identity {
    ($t:ident) => {
        impl PartialEq for $t {
            fn eq(&self, other: &Self) -> bool {
                self.ast.eq(&other.ast)
            }
        }

        impl Eq for $t {}

        impl Hash for $t {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.ast.hash(state)
            }
        }
    };
}

use ast_identity;


pub struct ExprStmt {
    pub ast: ast::ExprStmt,
    pub expr: Expr,
}

struct Expr {
    pub ast: ast::Expr,
    pub ty: Type,
    pub kind: ExprKind
}

pub enum Operation {
    Builtin(BuiltinOperation),
    FunctionCall(Arc<FunctionDef>),
}

#[derive(Clone,Debug)]
pub struct OpExpr {
    pub operation: Operation,
    pub arguments: Vec<Expr>
}

#[derive(Clone,Debug)]
pub struct IndexExpr {
    pub ast: IndexExpr,
    pub array: Box<Expr>,
    pub index: Box<Expr>,
}

pub struct FieldExpr {
    pub ast: FieldExpr,
    pub def: Arc<StructDef>,
    pub index: usize,
}

pub struct ComponentAccessExpr {
    pub ast: FieldExpr,
    pub components: ComponentIndices,
}


pub struct CallExpr {
    pub ast: ast::CallExpr,
    pub function:
}

enum ExprKind {
    Operation(OpExpr),
    Call(CallExpr),
    Index(IndexExpr),
    Field(FieldExpr),
    ComponentAccess(ComponentAccessExpr),
    Error,
}


enum Statement {

}

struct Module {

}