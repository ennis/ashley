use crate::{
    def::{AstId, ConstExpr, ConstExprId},
    syntax::ast,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Type {
    pub ast_id: AstId<ast::Type>,
    pub kind: TypeKind,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeKind {
    Name {
        name: String,
    },
    Array {
        element: Box<Type>,
        size: ConstExprId,
        stride: Option<ConstExprId>,
    },
    /// Runtime array type. Array without a known length.
    RuntimeArray {
        element: Box<Type>,
        stride: Option<ConstExprId>,
    },
    Error,
}

impl Type {
    pub fn error(ast_id: AstId<ast::Type>) -> Type {
        Type {
            ast_id,
            kind: TypeKind::Error,
        }
    }

    pub fn array_size(&self) -> Option<ConstExprId> {
        match self.kind {
            TypeKind::Array { size, .. } => Some(size),
            _ => None,
        }
    }

    pub fn array_stride(&self) -> Option<ConstExprId> {
        match self.kind {
            TypeKind::Array { stride, .. } | TypeKind::RuntimeArray { stride, .. } => stride,
            _ => None,
        }
    }
}
