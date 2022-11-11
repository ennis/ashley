use crate::hir::Arena;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::ptr;

/// Primitive value types.
///
/// Scalar values of integral and floating-point types.
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
pub struct Field<'hir> {
    pub ty: Type<'hir>,
    pub name: &'hir str,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructType<'hir> {
    pub name: &'hir str,
    pub fields: &'hir [Field<'hir>],
}

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

/*impl StructType {
    pub fn display_glsl(&self) -> impl fmt::Display + '_ {
        StructTypeDisplayGlsl(self)
    }
}

struct StructTypeDisplayGlsl<'a>(&'a StructType);

impl<'a> fmt::Display for StructTypeDisplayGlsl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "struct {} {{", self.0.name)?;
        for (i, field) in self.0.fields.iter().enumerate() {
            write!(f, "{} {};", field.ty.display_glsl(), field.name)?;
        }
        write!(f, "}};")
    }
}*/

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

/*
struct ImageTypeDisplayGlsl<'a>(&'a ImageType);

impl<'a> fmt::Display for ImageTypeDisplayGlsl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}image{}",
            self.0.element_ty.display_glsl_prefix(),
            self.0.dim.display_glsl_image_suffix()
        )?;
        if self.0.ms {
            write!(f, "MS")?
        }
        Ok(())
    }
}

impl ImageType {
    pub fn display_glsl(&self) -> impl fmt::Display + '_ {
        ImageTypeDisplayGlsl(self)
    }
}*/

// Change in place:
// * Rewrite declarations
// * Add/insert expressions => statement list in Vec
// * Change expression types =>

// Since changing a declaration (e.g. changing the params of a function or the type of a global) affects all use sites,
// it's preferable to make declarations immutable, so as to not affect existing uses (uses will have to be rewritten anyway).

/// Describes the data type of a value.
///
/// This enum is modeled after SPIR-V (and GLSL) data types, so it is suited to describe the types
/// of a shader interface. However, it also contains types not directly usable in a shader, such as
/// strings.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeKind<'hir> {
    Void,
    /// Primitive type.
    Scalar(ScalarType),
    /// Vector type (ty,size).
    Vector(ScalarType, u8),
    /// Matrix type. Element type, columns, rows, in this order.
    Matrix(ScalarType, u8, u8),
    /// Array type. Element type, length.
    Array(Type<'hir>, u32),
    /// Runtime array type. Array without a known length.
    RuntimeArray(Type<'hir>),
    /// Structure type (array of (offset, type) tuples).
    Struct(StructType<'hir>),
    /// Sampled image type (e.g. `texture2D`).
    SampledImage(SampledImageType),
    /// Unsampled image type (e.g. `image2D`).
    Image(ImageType),
    /// Pointer to data.
    Pointer(Type<'hir>),
    /// Sampler.
    Sampler,
    /// Shadow sampler (`samplerShadow`)
    ShadowSampler,
    /// Strings.
    String,
    /// Function (return type, arguments)
    Function(Type<'hir>, &'hir [Type<'hir>]),
    Unknown,
    Error,
}

#[derive(Copy, Clone, Debug)]
pub struct Type<'hir>(pub(crate) &'hir TypeKind<'hir>);

impl<'hir> PartialEq for Type<'hir> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl<'hir> Eq for Type<'hir> {}

impl<'hir> PartialOrd for Type<'hir> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'hir> Ord for Type<'hir> {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&(self.0 as *const _), &(other.0 as *const _))
    }
}

impl<'hir> Hash for Type<'hir> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&(self.0 as *const _), state);
    }
}


/*pub struct TypeDisplay<'a> {
    ty: Type<'a>,
}

impl<'a> fmt::Display for TypeDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // most of the impl here was inferred by copilot
        match *self.ty {
            TypeKind::Void => {
                write!(f, "void")
            }
            TypeKind::Scalar(primitive_ty) => {
                write!(f, "{}", primitive_ty.display())
            }
            TypeKind::Vector(elem_ty, len) => match elem_ty {
                ScalarType::Int => write!(f, "ivec{len}"),
                ScalarType::UnsignedInt => write!(f, "uvec{len}"),
                ScalarType::Float => write!(f, "vec{len}"),
                ScalarType::Double => write!(f, "dvec{len}"),
                ScalarType::Bool => write!(f, "bvec{len}"),
            },
            TypeKind::Matrix(elem_ty, columns, rows) => {
                match elem_ty {
                    ScalarType::Float => write!(f, "mat{columns}{rows}"),
                    ScalarType::Double => write!(f, "dmat{columns}{rows}"),
                    // those are not valid GLSL, but whatever
                    ScalarType::Int => write!(f, "imat{columns}{rows}"),
                    ScalarType::UnsignedInt => write!(f, "umat{columns}{rows}"),
                    ScalarType::Bool => write!(f, "bmat{columns}{rows}"),
                }
            }
            TypeKind::Array(elem_ty, len) => {
                let elem_ty = elem_ty.display();
                write!(f, "{elem_ty}[{len}]")
            }
            TypeKind::RuntimeArray(elem_ty) => {
                let elem_ty = self.module.display_type(elem_ty);
                write!(f, "{elem_ty}[]")
            }
            TypeKind::Struct(ref struct_ty) => {
                write!(f, "struct {}", struct_ty.name)
            }
            TypeKind::SampledImage(ref sampled_image_ty) => {
                write!(
                    f,
                    "{}{} {} texture",
                    if sampled_image_ty.ms {
                        "multisampled "
                    } else {
                        ""
                    },
                    sampled_image_ty.dim.display(),
                    sampled_image_ty.sampled_ty.display(),
                )
            }
            TypeKind::Image(ref image_ty) => {
                write!(
                    f,
                    "{}{} {} image",
                    if image_ty.ms { "multisampled " } else { "" },
                    image_ty.dim.display(),
                    image_ty.element_ty.display(),
                )
            }
            TypeKind::Pointer(ptr) => {
                // not valid GLSL
                let pointee = self.module.display_type(ptr);
                write!(f, "pointer to {}", pointee)
            }
            TypeKind::Sampler => {
                write!(f, "sampler")
            }
            TypeKind::ShadowSampler => {
                write!(f, "samplerShadow")
            }
            TypeKind::String => {
                // not valid GLSL
                write!(f, "string")
            }
            TypeKind::Unknown => {
                write!(f, "unknown")
            }
            TypeKind::Function(return_type, ref arguments) => {
                write!(f, "function(")?;
                for &arg in arguments.iter() {
                    let arg = self.module.display_type(arg);
                    write!(f, "{arg}")?;
                }
                write!(f, ")")?;
                if return_type != self.module.void_type {
                    let return_type = self.module.display_type(return_type);
                    write!(f, " -> {return_type}")?;
                }
                Ok(())
            }
            TypeKind::Error => {
                write!(f, "(error type)")
            }
        }
    }
}*/
