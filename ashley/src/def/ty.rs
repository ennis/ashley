use crate::{
    def::{AstId, BodyId, ConstExpr},
    syntax::ast,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Type {
    pub ast_id: Option<AstId<ast::Type>>,
    pub kind: TypeKind,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeKind {
    Name {
        name: String,
    },
    Array {
        element: Box<Type>,
        size: BodyId,
        stride: Option<BodyId>,
    },
    /// Runtime array type. Array without a known length.
    RuntimeArray {
        element: Box<Type>,
        // This means that struct_type (or any other type resolution) will depend on the module_map (or body_map)
        stride: Option<BodyId>,
    },
    Error,
}

impl Type {
    pub fn error() -> Type {
        Type {
            ast_id: None,
            kind: TypeKind::Error,
        }
    }

    pub fn array_size(&self) -> Option<BodyId> {
        match self.kind {
            TypeKind::Array { size, .. } => Some(size),
            _ => None,
        }
    }

    pub fn array_stride(&self) -> Option<BodyId> {
        match self.kind {
            TypeKind::Array { stride, .. } | TypeKind::RuntimeArray { stride, .. } => stride,
            _ => None,
        }
    }
}
