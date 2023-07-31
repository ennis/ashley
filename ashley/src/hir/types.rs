use crate::hir::{Module, Type};
use ashley::diagnostic::SourceLocation;
use rspirv::{
    spirv,
    spirv::{AccessQualifier, ImageFormat},
};
use spirv::Dim;
use std::{borrow::Cow, fmt};

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
    pub offset: Option<usize>,
}

impl<'a> Field<'a> {
    pub fn into_static(self) -> Field<'static> {
        Field {
            ty: self.ty,
            name: self.name.map(|name| Cow::Owned(name.into_owned())),
            offset: self.offset,
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
        let fields: Vec<_> = self.fields.into_owned().into_iter().map(Field::into_static).collect();
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
            TypeData::Matrix {
                component_type,
                columns,
                rows,
            } => TypeData::Matrix {
                component_type,
                columns,
                rows,
            },
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

    /// Returns whether this is a storage or sampled image type.
    pub fn is_image(&self) -> bool {
        match self {
            TypeData::Image(_) | TypeData::SampledImage(_) => true,
            _ => false,
        }
    }

    /// Returns whether this is a storage image type.
    pub fn is_storage_image(&self) -> bool {
        match self {
            TypeData::Image(_) => true,
            _ => false,
        }
    }

    /// Returns whether this is a sampled image type.
    pub fn is_sampled_image(&self) -> bool {
        match self {
            TypeData::SampledImage(_) => true,
            _ => false,
        }
    }

    pub fn is_sampler(&self) -> bool {
        match self {
            TypeData::Sampler => true,
            _ => false,
        }
    }
}

// implemented as a macro to avoid borrowing woes
macro_rules! write_list {
    ($w:expr, $i:ident in $coll:expr => $b:block) => {
        let mut first = true;
        for $i in $coll {
            if !first {
                write!($w, ",").unwrap();
            }
            $b
            first = false;
        }
    };
}

fn print_image_type(img: &ImageType, f: &mut fmt::Formatter, is_sampled: bool) -> fmt::Result {
    let prefix = match img.sampled_type {
        ScalarType::Int => "i",
        ScalarType::UnsignedInt => "u",
        ScalarType::Double => "d",
        ScalarType::Bool => "b",
        _ => panic!("invalid image sampled type"),
    };
    let image = if is_sampled { "texture" } else { "image" };
    let dim = match img.dim {
        Dim::Dim1D => "1D",
        Dim::Dim2D => "2D",
        Dim::Dim3D => "3D",
        Dim::DimCube => "Cube",
        Dim::DimRect => "Rect",
        Dim::DimBuffer => "Buffer",
        Dim::DimSubpassData => "SubpassData",
    };

    let array = if img.arrayed { "Array" } else { "" };
    let ms = if img.ms { "MS" } else { "" };

    write!(f, "{prefix}{image}{dim}{array}{ms}")?;
    Ok(())
}

fn print_struct_type(m: &Module, ty: &StructType, f: &mut fmt::Formatter) -> fmt::Result {
    let name = if let Some(ref name) = ty.name {
        name.as_ref()
    } else {
        ""
    };
    write!(f, "struct {name}<")?;
    write_list!(f, field in ty.fields.iter() => {
        write!(f, "{:?}", m.debug_type(field.ty))?;
    });
    write!(f, ">")?;
    Ok(())
}

fn print_function_type(m: &Module, ty: &FunctionType, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "function (")?;
    write_list!(f, arg in ty.arg_types.iter() => {
        write!(f, "{:?}", m.debug_type(*arg))?;
    });
    write!(f, ") -> {:?}", m.debug_type(ty.return_type))?;
    Ok(())
}

pub struct TypeDataDebug<'a>(pub(crate) &'a Module, pub(crate) &'a TypeData<'static>);

impl<'a> fmt::Debug for TypeDataDebug<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.1 {
            TypeData::Unit => {
                write!(f, "void")?;
            }
            TypeData::Scalar(s) => {
                write!(f, "{}", s.display())?;
            }
            TypeData::Vector(s, n) => {
                let prefix = match s {
                    ScalarType::Int => "ivec",
                    ScalarType::UnsignedInt => "uvec",
                    ScalarType::Float => "vec",
                    ScalarType::Double => "dvec",
                    ScalarType::Bool => "bvec",
                };
                write!(f, "{prefix}{n}")?;
            }
            TypeData::Matrix {
                rows,
                columns,
                component_type,
            } => {
                let prefix = match component_type {
                    ScalarType::Float => "mat",
                    ScalarType::Double => "dmat",
                    _ => panic!("invalid matrix type"),
                };
                write!(f, "{prefix}{columns}x{rows}")?;
            }
            TypeData::Array(ty, n) => {
                let td = self.0.debug_type(*ty);
                write!(f, "{td:?}[{n}]")?;
            }
            TypeData::RuntimeArray(rt) => {
                let td = self.0.debug_type(*rt);
                write!(f, "{td:?}[]")?;
            }
            TypeData::Struct(s) => {
                print_struct_type(self.0, s, f)?;
            }
            TypeData::SampledImage(img) => {
                print_image_type(img, f, true)?;
            }
            TypeData::Image(img) => {
                print_image_type(img, f, false)?;
            }
            TypeData::Pointer {
                pointee_type,
                storage_class,
            } => {
                let pointee = self.0.debug_type(*pointee_type);
                write!(f, "pointer ({storage_class:?}) to {pointee:?}")?;
            }
            TypeData::Function(fty) => {
                print_function_type(self.0, fty, f)?;
            }
            TypeData::Sampler => {
                write!(f, "sampler")?;
            }
            TypeData::SamplerShadow => {
                write!(f, "samplerShadow")?;
            }
            TypeData::String => {
                write!(f, "string")?;
            }
            TypeData::Unknown => {
                write!(f, "unknown")?;
            }
        }
        Ok(())
    }
}
