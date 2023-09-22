use crate::{
    ir::{Module, StructLayout, Type},
    utils::write_list,
};
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
    // TODO 64-bit scalars
    // TODO 16-bit scalars? 8-bit? do we need them? does SPIR-V + vulkan support them? Do we gain anything by exposing these?
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

/*bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct FieldFlags: u32 {
        const NO_PERSPECTIVE = 0b00000001;
        const FLAT = 0b00000010;
        const C = 0b00000100;
        const ABC = Self::A.bits() | Self::B.bits() | Self::C.bits();
    }
}*/

#[derive(Default, Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum InterpolationKind {
    #[default]
    Flat,
    NoPerspective,
    Smooth,
}

#[derive(Default, Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum InterpolationSampling {
    #[default]
    Center,
    Centroid,
    Sample,
}

#[derive(Default, Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Interpolation {
    pub kind: InterpolationKind,
    pub sampling: InterpolationSampling,
}

/// Field of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Field<'a> {
    pub ty: Type,
    pub name: Option<Cow<'a, str>>,
    //pub offset: Option<usize>,
    /// For fields of interface blocks, defines the interpolation parameters of the field.
    pub interpolation: Option<Interpolation>,
}

impl<'a> Field<'a> {
    pub fn into_static(self) -> Field<'static> {
        Field {
            ty: self.ty,
            name: self.name.map(|name| Cow::Owned(name.into_owned())),
            interpolation: self.interpolation,
        }
    }
}

/// Layout of memory interface structs.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MemoryInterfaceLayout {
    /// std140 layout
    Std140,
    /// std430 layout
    Std430,
    /// Explicitly-specified layout
    Explicit(StructLayout),
}

/*
/// Special kinds of structs.
#[derive(Clone, Debug)]
pub enum StructTypeKind {
    /// This is a regular struct, not used in interfaces.
    NoInterface,
    /// The struct is used as an interface type between stages.
    StageInterface,
    /// The struct describes a memory interface between the host and the shader.
    MemoryInterface(MemoryInterfaceLayout),
}*/

/// Structure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructType<'a> {
    pub name: Option<Cow<'a, str>>,
    pub fields: Cow<'a, [Field<'a>]>,
    pub layout: Option<StructLayout>,
    /// Whether this structure type is a memory interface block (`Block` decoration in SPIR-V).
    pub block: bool,
}

impl<'a> StructType<'a> {
    pub fn into_static(self) -> StructType<'static> {
        let fields: Vec<_> = self.fields.into_owned().into_iter().map(Field::into_static).collect();
        StructType {
            name: self.name.map(|name| Cow::Owned(name.into_owned())),
            fields: Cow::Owned(fields),
            layout: self.layout.clone(),
            block: self.block,
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

    pub fn is_opaque(&self) -> bool {
        self.layout.is_none()
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
        stride: u32,
    },
    /// Array type (element type + size).
    Array {
        element_type: Type,
        size: u32,
        stride: Option<u32>,
    },
    /// Runtime array type. Array without a known length.
    RuntimeArray {
        element_type: Type,
        stride: Option<u32>,
    },
    /// Structure type (array of (offset, type) tuples).
    Struct(StructType<'a>),
    /// Image type (e.g. `image2D`, `texture2D`).
    Image(ImageType),
    /// Combined image-sampler image type (e.g. `sampler2D`).
    ///
    /// The inner type must be an `Image`.
    SampledImage(Type),
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
                stride,
            } => TypeData::Matrix {
                component_type,
                columns,
                rows,
                stride,
            },
            TypeData::Array {
                element_type,
                size,
                stride,
            } => TypeData::Array {
                element_type,
                size,
                stride,
            },
            TypeData::RuntimeArray { element_type, stride } => TypeData::RuntimeArray { element_type, stride },
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

    ///
    pub fn as_image(&self) -> Option<&ImageType> {
        match self {
            TypeData::Image(img) => Some(img),
            _ => None,
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

    pub fn is_opaque(&self) -> bool {
        match self {
            TypeData::Image(_)
            | TypeData::String
            | TypeData::Sampler
            | TypeData::SamplerShadow
            | TypeData::Unknown
            | TypeData::Function(_) => true,
            TypeData::Struct(ty) => ty.is_opaque(),
            _ => false,
        }
    }
}

fn print_image_type(img: &ImageType, f: &mut fmt::Formatter, is_combined_image_sampler: bool) -> fmt::Result {
    let prefix = match img.sampled_type {
        ScalarType::Int => "i",
        ScalarType::UnsignedInt => "u",
        ScalarType::Double => "d",
        ScalarType::Bool => "b",
        _ => panic!("invalid image sampled type"),
    };
    let image = if is_combined_image_sampler {
        "combinedImageSampler"
    } else {
        match img.sampled {
            ImageSampling::Unknown => "texture",
            ImageSampling::Sampled => "texture",
            ImageSampling::ReadWrite => "image",
        }
    };
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
                stride,
            } => {
                let prefix = match component_type {
                    ScalarType::Float => "mat",
                    ScalarType::Double => "dmat",
                    _ => panic!("invalid matrix type"),
                };
                write!(f, "{prefix}{columns}x{rows} stride({stride})")?;
            }
            TypeData::Array {
                element_type,
                size,
                stride,
            } => {
                let td = self.0.debug_type(*element_type);
                if let Some(stride) = *stride {
                    write!(f, "{td:?}[{size}] stride({stride})")?;
                } else {
                    write!(f, "{td:?}[{size}]")?;
                }
            }
            TypeData::RuntimeArray { element_type, stride } => {
                let td = self.0.debug_type(*element_type);
                if let Some(stride) = *stride {
                    write!(f, "{td:?}[] stride({stride}) ")?;
                } else {
                    write!(f, "{td:?}[]")?;
                }
            }
            TypeData::Struct(s) => {
                print_struct_type(self.0, s, f)?;
            }
            TypeData::SampledImage(img) => {
                if let Some(img) = self.0.types[*img].as_image() {
                    print_image_type(img, f, true)?;
                } else {
                    write!(f, "<malformed SampledImage>")?;
                }
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
