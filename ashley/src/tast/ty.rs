use crate::{hir, syntax::ast, tast::DefId};
use std::{
    hash::{Hash, Hasher},
    ops::Deref,
    ptr,
    sync::Arc,
};

pub use hir::types::{ImageSampling, ImageType};
pub type ScalarType = hir::types::ScalarType;

// TODO: arcs are probably not necessary here, we could replace them with a straight reference, but then
// we'd need an arena for the types
#[derive(Clone, Debug)]
pub struct Type(pub(crate) Arc<TypeKind>);

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Type {}

impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(Arc::as_ptr(&self.0), state);
    }
}

impl Deref for Type {
    type Target = TypeKind;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Function or closure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionType {
    pub return_type: Type,
    pub arg_types: Vec<Type>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeKind {
    /// Void (or unit) type.
    Unit,
    /// Scalar type.
    Scalar(ScalarType),
    /// Vector type (element type + size).
    Vector(ScalarType, u8),
    /// Matrix type (element type + row count + column count).
    Matrix {
        component_type: ScalarType,
        columns: u8,
        rows: u8,
    },
    /// Array type (element type + size).
    Array(Type, u32),
    /// Runtime array type. Array without a known length.
    RuntimeArray(Type),
    /// Structure type (array of (offset, type) tuples).
    Struct(DefId),
    /// Unsampled image type.
    Image(hir::types::ImageType),
    /// Pointer.
    Pointer {
        pointee_type: Type,
        storage_class: spirv::StorageClass,
    },
    /// Function or closure type.
    Function(FunctionType),
    /// Sampler.
    Sampler,
    SamplerShadow,
    /// Strings.
    String,
    Unknown,
    Error,
}

impl From<ScalarType> for TypeKind {
    fn from(s: ScalarType) -> Self {
        TypeKind::Scalar(s)
    }
}

impl From<FunctionType> for TypeKind {
    fn from(fty: FunctionType) -> Self {
        TypeKind::Function(fty)
    }
}

impl TypeKind {
    pub fn as_function(&self) -> Option<&FunctionType> {
        match self {
            TypeKind::Function(fty) => Some(fty),
            _ => None,
        }
    }
}

/// A type as it appears in the AST, and its resolved form.
pub struct TypeSpec {
    pub ast: ast::Type,
    pub ty: Type,
}
