use crate::hir::Type;
use rspirv::{
    spirv,
    spirv::{AccessQualifier, ImageFormat},
};
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, sync::Arc};

/// Scalar type kind.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
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
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
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
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StructType<'a> {
    pub name: Option<Cow<'a, str>>,
    pub fields: Cow<'a, [Field<'a>]>,
}

impl<'a> StructType<'a> {
    pub fn into_static(self) -> StructType<'static> {
        StructType {
            name: self.name.map(|name| Cow::Owned(name.into_owned())),
            fields: Cow::Owned(self.fields.iter().map(|f| f.to_static()).collect::<Box<[Field]>>()),
        }
    }
}

impl StructType {
    /// Finds a field by name.
    pub fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.name == name)
    }

    /// Finds a field by name.
    pub fn field(&self, name: &str) -> Option<&Field> {
        self.fields.iter().find(|f| f.name == name)
    }
}

//--------------------------------------------------------------------------------------------------

/// Function or closure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum ImageSampling {
    Unknown,
    Sampled,
    ReadWrite,
}

/// Image type
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
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
#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
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
        }
    }
}

/*impl TypeDesc {
    pub const VOID: TypeDesc = TypeDesc::Void;
    pub const BOOL: TypeDesc = TypeDesc::Primitive(PrimitiveType::Bool);
    pub const INT: TypeDesc = TypeDesc::Primitive(PrimitiveType::Int);
    pub const UNSIGNED_INT: TypeDesc = TypeDesc::Primitive(PrimitiveType::UnsignedInt);
    pub const FLOAT: TypeDesc = TypeDesc::Primitive(PrimitiveType::Float);
    pub const DOUBLE: TypeDesc = TypeDesc::Primitive(PrimitiveType::Double);
    pub const SAMPLER: TypeDesc = TypeDesc::Sampler;
    pub const SAMPLER_SHADOW: TypeDesc = TypeDesc::ShadowSampler;
    pub const VEC2: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Float,
        len: 2,
    };
    pub const VEC3: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Float,
        len: 3,
    };
    pub const VEC4: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Float,
        len: 4,
    };
    pub const DVEC2: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Double,
        len: 2,
    };
    pub const DVEC3: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Double,
        len: 3,
    };
    pub const DVEC4: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Double,
        len: 4,
    };
    pub const IVEC2: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Int,
        len: 2,
    };
    pub const IVEC3: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Int,
        len: 3,
    };
    pub const IVEC4: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::Int,
        len: 4,
    };
    pub const UVEC2: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::UnsignedInt,
        len: 2,
    };
    pub const UVEC3: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::UnsignedInt,
        len: 3,
    };
    pub const UVEC4: TypeDesc = TypeDesc::Vector {
        elem_ty: PrimitiveType::UnsignedInt,
        len: 4,
    };
    pub const MAT2: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 2,
        columns: 2,
    };
    pub const MAT3: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 3,
        columns: 3,
    };
    pub const MAT4: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 4,
        columns: 4,
    };
    pub const MAT2X3: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 3,
        columns: 2,
    };
    pub const MAT2X4: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 4,
        columns: 5,
    };
    pub const MAT3X2: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 2,
        columns: 3,
    };
    pub const MAT3X4: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 4,
        columns: 3,
    };
    pub const MAT4X2: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 2,
        columns: 4,
    };
    pub const MAT4X3: TypeDesc = TypeDesc::Matrix {
        elem_ty: PrimitiveType::Float,
        rows: 3,
        columns: 4,
    };

    /// Returns whether the described type is usable in a shader.
    pub fn is_shader_representable(&self) -> bool {
        match self {
            TypeDesc::String => false,
            _ => true,
        }
    }

    /// Returns whether instances of the described type can't be stored in a buffer.
    pub fn is_opaque(&self) -> bool {
        match self {
            TypeDesc::Void => true,
            TypeDesc::Primitive(_) | TypeDesc::Vector { .. } | TypeDesc::Matrix { .. } => false,
            TypeDesc::Array { elem_ty, .. } => elem_ty.is_opaque(),
            TypeDesc::RuntimeArray(_) => true,
            TypeDesc::Struct(_) => false,
            TypeDesc::SampledImage(_) => true,
            TypeDesc::Image(_) => true,
            TypeDesc::Pointer(_) => true, // ??
            TypeDesc::Sampler => true,
            TypeDesc::ShadowSampler => true,
            TypeDesc::String => true,
            TypeDesc::Unknown => true,
        }
    }
}

struct TypeDescGlslDisplay<'a>(&'a TypeDesc);

impl<'a> fmt::Display for TypeDescGlslDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // most of the impl here was inferred by copilot
        match self.0 {
            TypeDesc::Void => {
                write!(f, "void")
            }
            TypeDesc::Primitive(primitive_ty) => {
                write!(f, "{}", primitive_ty.display_glsl())
            }
            TypeDesc::Vector { elem_ty, len } => match elem_ty {
                PrimitiveType::Int => write!(f, "ivec{}", len),
                PrimitiveType::UnsignedInt => write!(f, "uvec{}", len),
                PrimitiveType::Float => write!(f, "vec{}", len),
                PrimitiveType::Double => write!(f, "dvec{}", len),
                PrimitiveType::Bool => write!(f, "bvec{}", len),
            },
            TypeDesc::Matrix { elem_ty, rows, columns } => {
                match elem_ty {
                    PrimitiveType::Float => write!(f, "mat{}{}", rows, columns),
                    PrimitiveType::Double => write!(f, "dmat{}{}", rows, columns),
                    // those are not valid GLSL, but whatever
                    PrimitiveType::Int => write!(f, "imat{}{}", rows, columns),
                    PrimitiveType::UnsignedInt => write!(f, "umat{}{}", rows, columns),
                    PrimitiveType::Bool => write!(f, "bmat{}{}", rows, columns),
                }
            }
            TypeDesc::Array { elem_ty, len } => {
                write!(f, "{}[{}]", elem_ty.display_glsl(), len)
            }
            TypeDesc::RuntimeArray(ty) => {
                write!(f, "{}[]", ty.display_glsl())
            }
            TypeDesc::Struct(struct_ty) => {
                write!(f, "{}", struct_ty.display_glsl())
            }
            TypeDesc::SampledImage(sampled_image_ty) => {
                write!(f, "{}", sampled_image_ty.display_glsl())
            }
            TypeDesc::Image(image_ty) => {
                write!(f, "{}", image_ty.display_glsl())
            }
            TypeDesc::Pointer(ptr) => {
                // not valid GLSL
                write!(f, "{}*", ptr.display_glsl())
            }
            TypeDesc::Sampler => {
                write!(f, "sampler")
            }
            TypeDesc::ShadowSampler => {
                write!(f, "samplerShadow")
            }
            TypeDesc::String => {
                // not valid GLSL
                write!(f, "string")
            }
            TypeDesc::Unknown => {
                write!(f, "unknown")
            }
        }
    }
}

impl TypeDesc {
    pub fn display_glsl(&self) -> impl fmt::Display + '_ {
        TypeDescGlslDisplay(self)
    }
}
*/
