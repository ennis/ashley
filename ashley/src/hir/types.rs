use crate::hir::Type;
use rspirv::{
    spirv,
    spirv::{AccessQualifier, ImageFormat},
};
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

/// Scalar type kind.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ScalarType {
    Int,
    UnsignedInt,
    Float,
    Double,
    Bool,
}

impl ScalarType {
    pub fn display(&self) -> &'static str {
        match *self {
            ScalarType::Int => "int",
            ScalarType::UnsignedInt => "uint",
            ScalarType::Float => "float",
            ScalarType::Double => "double",
            ScalarType::Bool => "bool",
        }
    }
}

/// Field of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Field<'a> {
    pub ty: Type,
    pub name: Option<Cow<'a, str>>,
}

impl<'a> Field<'a> {
    pub fn into_static(self) -> Field<'static> {
        Field {
            ty: self.ty,
            name: self.name.map(|name| Cow::Owned(name.into_owned())),
        }
    }
}

/// Structure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructType<'a> {
    pub name: Option<Cow<'a, str>>,
    pub fields: Cow<'a, [Field<'a>]>,
}

impl<'a> StructType<'a> {
    pub fn into_static(self) -> StructType<'static> {
        let mut fields: Vec<_> = self.fields.into_owned().into_iter().map(Field::into_static).collect();
        StructType {
            name: self.name.map(|name| Cow::Owned(name.into_owned())),
            fields: Cow::Owned(fields),
        }
    }
}

impl<'a> StructType<'a> {
    /// Finds a field by name.
    pub fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.name.as_deref() == Some(name))
    }

    /// Finds a field by name.
    pub fn field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.name.as_deref() == Some(name))
    }
}

//--------------------------------------------------------------------------------------------------

/// Function or closure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionType<'a> {
    pub return_type: Type,
    pub arg_types: Cow<'a, [Type]>,
}

impl<'a> FunctionType<'a> {
    pub fn into_static(self) -> FunctionType<'static> {
        FunctionType {
            return_type: self.return_type,
            arg_types: Cow::Owned(self.arg_types.into_owned()),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ImageSampling {
    Unknown,
    Sampled,
    ReadWrite,
}

/// Image type
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ImageType {
    pub sampled_type: ScalarType,
    pub dim: spirv::Dim,
    pub arrayed: bool,
    pub depth: Option<bool>,
    pub ms: bool,
    pub sampled: ImageSampling,
    pub image_format: ImageFormat,
    pub access: Option<AccessQualifier>,
}

/// Describes the data type of a value.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeData<'a> {
    /// Void (or unit) type.
    Unit,
    /// Scalar type.
    Scalar(ScalarType),
    /// Vector type (element type + size).
    Vector(ScalarType, u8),
    /// Matrix type (element type + row count + column count).
    Matrix(ScalarType, u8, u8),
    /// Array type (element type + size).
    Array(Type, u32),
    /// Runtime array type. Array without a known length.
    RuntimeArray(Type),
    /// Structure type (array of (offset, type) tuples).
    Struct(StructType<'a>),
    /// Sampled image type (e.g. `texture2D`).
    SampledImage(ImageType),
    /// Unsampled image type (e.g. `image2D`).
    Image(ImageType),
    /// Pointer.
    Pointer {
        pointee_type: Type,
        storage_class: spirv::StorageClass,
    },
    /// Function or closure type.
    Function(FunctionType<'a>),
    /// Sampler.
    Sampler,
    SamplerShadow,
    /// Strings.
    String,
    Unknown,
}

impl<'a> TypeData<'a> {
    pub fn into_static(self) -> TypeData<'static> {
        match self {
            TypeData::Struct(struct_type) => TypeData::Struct(struct_type.into_static()),
            TypeData::Function(func) => TypeData::Function(func.into_static()),
            TypeData::Unit => TypeData::Unit,
            TypeData::Scalar(t) => TypeData::Scalar(t),
            TypeData::Vector(t, n) => TypeData::Vector(t, n),
            TypeData::Matrix(t, r, c) => TypeData::Matrix(t, r, c),
            TypeData::Array(t, n) => TypeData::Array(t, n),
            TypeData::RuntimeArray(t) => TypeData::RuntimeArray(t),
            TypeData::SampledImage(i) => TypeData::SampledImage(i),
            TypeData::Image(i) => TypeData::Image(i),
            TypeData::Pointer {
                pointee_type,
                storage_class,
            } => TypeData::Pointer {
                pointee_type,
                storage_class,
            },
            TypeData::Sampler => TypeData::Sampler,
            TypeData::String => TypeData::String,
            TypeData::Unknown => TypeData::Unknown,
            TypeData::SamplerShadow => TypeData::SamplerShadow,
        }
    }
}
