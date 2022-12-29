use std::fmt;
use std::num::NonZeroU32;
use std::sync::Arc;
//use crate::hir::MatchCtxt;
use crate::{
    hir::{HirCtxt, IRPrintable, IRPrinter, IRSyntaxElem, IRVisitable},
    write_ir,
};
use crate::hir::{AttrConstraint, Type};

/// Scalar type kind.
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
impl<'a> IRPrintable<'a> for ScalarType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        match self.0 {
            ScalarTypeKind::Int => printer.token("int"),
            ScalarTypeKind::UnsignedInt => printer.token("uint"),
            ScalarTypeKind::Float => printer.token("float"),
            ScalarTypeKind::Double => printer.token("double"),
            ScalarTypeKind::Bool => printer.token("bool"),
        }
    }
}

/// Vector type (element type + size).
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct VectorType(pub ScalarTypeKind, pub u8);
impl<'a> IRPrintable<'a> for VectorType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        let n = self.1;
        match self.0 {
            ScalarTypeKind::Int => printer.token(&format!("ivec{n}")),
            ScalarTypeKind::UnsignedInt => printer.token(&format!("uvec{n}")),
            ScalarTypeKind::Float => printer.token(&format!("vec{n}")),
            ScalarTypeKind::Double => printer.token(&format!("dvec{n}")),
            ScalarTypeKind::Bool => printer.token(&format!("bvec{n}")),
        }
    }
}

/// Matrix type (element type + row count + column count).
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct MatrixType(pub ScalarTypeKind, pub u8, pub u8);
impl<'a> IRPrintable<'a> for MatrixType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        let (r, c) = (self.1, self.2);
        match self.0 {
            ScalarTypeKind::Int => printer.token(&format!("imat{r}x{c}")),
            ScalarTypeKind::UnsignedInt => printer.token(&format!("umat{r}x{c}")),
            ScalarTypeKind::Float => printer.token(&format!("mat{r}x{c}")),
            ScalarTypeKind::Double => printer.token(&format!("dmat{r}x{c}")),
            ScalarTypeKind::Bool => printer.token(&format!("bmat{r}x{c}")),
        }
    }
}

/// Array type (element type + size).
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct ArrayType(pub Type, pub u32);

impl<'a> IRPrintable<'a> for ArrayType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "array<", self.0, ",", self.1, ">");
    }
}

/// Field of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct Field {
    pub ty: Type,
    pub name: String,
}

/// Structure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<Field>,
}

impl<'a> IRPrintable<'a> for StructType {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "struct<", self.name);
        for f in self.fields {
            write_ir!(printer, f.name, ":", f.ty, ",");
        }
        write_ir!(printer, ">");
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

/// Tuple type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct TupleType(pub Vec<Type>);
impl<'a> IRPrintable<'a> for TupleType {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "tuple<");
        for ty in self.0 {
            write_ir!(printer, *ty, ",");
        }
        write_ir!(printer, ">");
    }
}

//--------------------------------------------------------------------------------------------------
/// Function type
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionType {
    pub return_ty: Type,
    pub arg_types: Vec<Type>,
}

impl<'a> IRPrintable<'a> for FunctionType {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "fn<(");
        let mut first = true;
        for arg in self.arg_types {
            if !first {
                write_ir!(printer, ",");
            }
            write_ir!(printer, *arg);
            first = false;
        }
        write_ir!(printer, ") -> ", self.return_ty, ">");
    }
}


/// Dimensions of an image.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ImageDimension {
    /// 1D image
    Dim1D,
    /// 2D image
    Dim2D,
    /// 3D image
    Dim3D,
    /// Cube map image: 6 2D images of the same size
    DimCube,
    /// Array of 1D images
    Dim1DArray,
    /// Array of 2D images
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
}


/// Sampled image type
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct SampledImageType {
    pub sampled_ty: ScalarType,
    pub dim: ImageDimension,
    pub ms: bool,
}

impl SampledImageType {
    pub fn display_glsl(&self) -> impl fmt::Display + '_ {
        SampledImageTypeDisplayGlsl(self)
    }
}

struct SampledImageTypeDisplayGlsl<'a>(&'a SampledImageType);

impl<'a> fmt::Display for SampledImageTypeDisplayGlsl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.sampled_ty {
            PrimitiveType::Int => write!(f, "i")?,
            PrimitiveType::UnsignedInt => write!(f, "u")?,
            PrimitiveType::Double => write!(f, "d")?,
            PrimitiveType::Bool => write!(f, "b")?,
            _ => {}
        }

        write!(f, "texture")?;

        match self.0.dim {
            ImageDimension::Dim1D => write!(f, "1D")?,
            ImageDimension::Dim2D => write!(f, "2D")?,
            ImageDimension::Dim3D => write!(f, "3D")?,
            ImageDimension::DimCube => write!(f, "Cube")?,
            ImageDimension::Dim1DArray => write!(f, "1DArray")?,
            ImageDimension::Dim2DArray => write!(f, "2DArray")?,
        }

        if self.0.ms {
            write!(f, "MS")?
        }

        Ok(())
    }
}

/// Unsampled image type
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ImageType {
    pub element_ty: ScalarType,
    pub dim: ImageDimension,
    pub ms: bool,
}

struct ImageTypeDisplayGlsl<'a>(&'a ImageType);

impl<'a> fmt::Display for ImageTypeDisplayGlsl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.element_ty {
            PrimitiveType::Int => write!(f, "i")?,
            PrimitiveType::UnsignedInt => write!(f, "u")?,
            PrimitiveType::Double => write!(f, "d")?,
            PrimitiveType::Bool => write!(f, "b")?,
            _ => {}
        }

        write!(f, "image")?;

        match self.0.dim {
            ImageDimension::Dim1D => write!(f, "1D")?,
            ImageDimension::Dim2D => write!(f, "2D")?,
            ImageDimension::Dim3D => write!(f, "3D")?,
            ImageDimension::DimCube => write!(f, "Cube")?,
            ImageDimension::Dim1DArray => write!(f, "1DArray")?,
            ImageDimension::Dim2DArray => write!(f, "2DArray")?,
        }

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
}

/// Describes the data type of a value.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeImpl {
    /// Void (or unit) type.
    Void,
    /// Scalar type.
    Scalar(ScalarType),
    /// Vector type (ty,size).
    Vector(VectorType),
    /// Matrix type (ty,rows,cols).
    Matrix(MatrixType),
    /// Array type. (typedesc + length + stride)
    Array(ArrayType),
    /// Runtime array type. Array without a known length.
    RuntimeArray(Type),
    /// Structure type (array of (offset, type) tuples).
    Struct(Arc<StructType>),
    /// Sampled image type (e.g. `texture2D`).
    SampledImage(Arc<SampledImageType>),
    /// Unsampled image type (e.g. `image2D`).
    Image(Arc<ImageType>),
    /// Pointer to data.
    Pointer(Type),
    /// Sampler.
    Sampler,
    /// Shadow sampler (`samplerShadow`)
    ShadowSampler,
    /// Strings.
    String,
    Unknown,
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