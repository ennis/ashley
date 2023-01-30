use std::sync::Arc;
use crate::hir;
use crate::syntax::ast;
use crate::tast::ast_identity;
use crate::tast::decl::StructDef;

pub type ScalarType = crate::hir::types::ScalarType;

#[derive(Clone)]
pub struct Type(Arc<TypeKind>);

/// Function or closure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionType {
    pub return_type: Type,
    pub arg_types: Vec<Type>,
}

#[derive(Clone,Eq,PartialEq)]
enum TypeKind {
    /// Void (or unit) type.
    Unit,
    /// Scalar type.
    Scalar(ScalarType),
    /// Vector type (element type + size).
    Vector(ScalarType, u8),
    /// Matrix type (element type + row count + column count).
    Matrix { component_type: ScalarType, columns: u8, rows: u8 },
    /// Array type (element type + size).
    Array(Type, u32),
    /// Runtime array type. Array without a known length.
    RuntimeArray(Type),
    /// Structure type (array of (offset, type) tuples).
    Struct(Arc<StructDef>),
    /// Sampled image type (e.g. `texture2D`).
    SampledImage(hir::types::ImageType),
    /// Unsampled image type (e.g. `image2D`).
    Image(hir::types::ImageType),
    /// Pointer.
    Pointer {
        pointee_type: Arc<Type>,
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
}

/// A type as it appears in the AST, and its resolved form.
pub struct TypeSpec {
    pub ast: ast::Type,
    pub ty: Type,
}
ast_identity!(TypeSpec);