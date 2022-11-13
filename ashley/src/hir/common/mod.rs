//! Common types & operations.

use crate::hir::ir::Type;
use crate::impl_arena_alloc;

/// Unknown type.
///
/// Used in places where the type is not known, not yet inferred, or invalid.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct UnknownType;
impl_arena_alloc!(UnknownType);

/// Scalar type.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ScalarTypeKind {
    Int,
    UnsignedInt,
    Float,
    Double,
    Bool,
}

impl ScalarTypeKind {
    pub fn display(&self) -> &'static str {
        match *self {
            ScalarTypeKind::Int => "int",
            ScalarTypeKind::UnsignedInt => "uint",
            ScalarTypeKind::Float => "float",
            ScalarTypeKind::Double => "double",
            ScalarTypeKind::Bool => "bool",
        }
    }
}

/// Scalar type.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ScalarType(pub ScalarTypeKind);
impl_arena_alloc!(ScalarType);

/// Vector type (element type + size).
pub struct VectorType(pub ScalarTypeKind, u8);
impl_arena_alloc!(VectorType);

/// Array type (element type + size).
pub struct ArrayType<'hir>(Type<'hir>, u8);
impl_arena_alloc!(ArrayType<'hir>);

/// Field of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Field<'hir> {
    pub ty: Type<'hir>,
    pub name: &'hir str,
}
impl_arena_alloc!(Field<'hir>);

/// Structure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructType<'hir> {
    pub name: &'hir str,
    pub fields: &'hir [Field<'hir>],
}
impl_arena_alloc!(StructType<'hir>);

impl<'hir> StructType<'hir> {
    /// Finds a field by name.
    pub fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.name == name)
    }

    /// Finds a field by name.
    pub fn field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.name == name)
    }
}



