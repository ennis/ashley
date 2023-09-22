use crate::{
    item::{AstId, ConstExpr},
    syntax::ast,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Type {
    Name {
        name: String,
    },
    Array {
        element: Box<Type>,
        size: AstId<ast::Expr>,
        stride: Option<AstId<ast::Expr>>,
    },
    /// Runtime array type. Array without a known length.
    RuntimeArray {
        element: Box<Type>,
        stride: Option<AstId<ast::Expr>>,
    },
    Error,
}

impl Type {
    pub fn array_size(&self) -> Option<AstId<ast::Expr>> {
        match self {
            Type::Array { size, .. } => Some(*size),
            _ => None,
        }
    }

    pub fn array_stride(&self) -> Option<AstId<ast::Expr>> {
        match self {
            Type::Array { stride, .. } | Type::RuntimeArray { stride, .. } => *stride,
            _ => None,
        }
    }
}
