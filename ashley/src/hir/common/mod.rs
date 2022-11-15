//! Common types & operations in shaders.

use crate::hir::ir::{Type, TypeBase};
use crate::impl_arena_any;


/// Unknown type.
///
/// Used in places where the type is not known, not yet inferred, or invalid.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct UnknownType;
impl_arena_any!(UnknownType);

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
impl_arena_any!(ScalarType);
impl<'hir> TypeBase<'hir> for ScalarType {}

/// Vector type (element type + size).
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct VectorType(pub ScalarTypeKind, u8);
impl_arena_any!(VectorType);
impl<'hir> TypeBase<'hir> for VectorType {}

/// Array type (element type + size).
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ArrayType<'hir>(Type<'hir>, u8);
impl_arena_any!(ArrayType<'hir>);
impl<'hir> TypeBase<'hir> for ArrayType<'hir> {}

/// Field of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Field<'hir> {
    pub ty: Type<'hir>,
    pub name: &'hir str,
}
//impl_arena_any!(Field<'hir>);

/// Structure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructType<'hir> {
    pub name: &'hir str,
    pub fields: &'hir [Field<'hir>],
}
impl_arena_any!(StructType<'hir>);
impl<'hir> TypeBase<'hir> for StructType<'hir> {}

impl<'hir> StructType<'hir> {
    /// Finds a field by name.
    pub fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.name == name)
    }

    /// Finds a field by name.
    pub fn field(&self, name: &str) -> Option<&Field<'hir>> {
        self.fields.iter().find(|f| f.name == name)
    }
}


#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ImageDimension {
    Dim1D,
    Dim2D,
    Dim3D,
    DimCube,
    Dim1DArray,
    Dim2DArray,
}

impl ImageDimension {
    pub fn display(&self) -> &'static str {
        match self {
            ImageDimension::Dim1D => "1D",
            ImageDimension::Dim2D => "2D",
            ImageDimension::Dim3D => "3D",
            ImageDimension::DimCube => "cube map",
            ImageDimension::Dim1DArray => "1D array",
            ImageDimension::Dim2DArray => "2D array",
        }
    }

    /*// TODO move this out of ast
    pub fn display_glsl_image_suffix(&self) -> &'static str {
        match self {
            ImageDimension::Dim1D => "1D",
            ImageDimension::Dim2D => "2D",
            ImageDimension::Dim3D => "3D",
            ImageDimension::DimCube => "Cube",
            ImageDimension::Dim1DArray => "1DArray",
            ImageDimension::Dim2DArray => "2DArray",
        }
    }*/
}

/// Sampled image type
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct SampledImageType {
    pub sampled_ty: ScalarType,
    pub dim: ImageDimension,
    pub ms: bool,
}
impl_arena_any!(SampledImageType);
impl<'hir> TypeBase<'hir> for SampledImageType {}

/*
impl SampledImageType {
    pub fn display_glsl(&self) -> impl fmt::Display + '_ {
        SampledImageTypeDisplayGlsl(self)
    }
}

struct SampledImageTypeDisplayGlsl<'a>(&'a SampledImageType);

impl<'a> fmt::Display for SampledImageTypeDisplayGlsl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}texture{}",
            self.0.sampled_ty.display_glsl_prefix(),
            self.0.dim.display_glsl_image_suffix()
        )?;
        if self.0.ms {
            write!(f, "MS")?
        }
        Ok(())
    }
}*/

/// Unsampled image type
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ImageType {
    pub element_ty: ScalarType,
    pub dim: ImageDimension,
    pub ms: bool,
}
impl_arena_any!(ImageType);
impl<'hir> TypeBase<'hir> for ImageType {}
