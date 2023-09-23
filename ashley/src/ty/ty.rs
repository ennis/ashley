use crate::{
    db::CompilerDb,
    ir,
    ir::{Interpolation, Layout},
    item::{InFile, StructId},
    layout::{std_scalar_type_layout, std_vector_type_layout},
    syntax::{ast, ast::TypeQualifier},
    utils::round_up,
    SourceFileId,
};
pub use ir::types::{ImageSampling, ImageType};
use rowan::ast::AstPtr;
use spirv::Dim;
use std::{
    fmt,
    fmt::Display,
    hash::{Hash, Hasher},
    ops::Deref,
    ptr,
    sync::Arc,
};

////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
pub type ScalarType = ir::types::ScalarType;

fn scalar_type_prefix(ty: ScalarType) -> &'static str {
    match ty {
        ScalarType::Bool => "b",
        ScalarType::Int => "i",
        ScalarType::UnsignedInt => "u",
        ScalarType::Float => "",
        ScalarType::Double => "d",
    }
}

fn scalar_type_to_string(ty: ScalarType) -> &'static str {
    match ty {
        ScalarType::Bool => "bool",
        ScalarType::Int => "int",
        ScalarType::UnsignedInt => "uint",
        ScalarType::Float => "float",
        ScalarType::Double => "double",
    }
}

struct ImageTypeDisplay<'a>(&'a ImageType);

impl<'a> fmt::Display for ImageTypeDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ImageType {
            sampled_type,
            dim,
            arrayed,
            depth: _,
            ms,
            sampled,
            image_format: _,
            access: _,
        } = self.0;
        let base_type = match sampled {
            ImageSampling::Sampled => "texture",
            ImageSampling::ReadWrite => "image",
            ImageSampling::Unknown => "image",
        };

        write!(f, "{}{}", scalar_type_prefix(*sampled_type), base_type)?;
        match dim {
            Dim::Dim1D => write!(f, "1D")?,
            Dim::Dim2D => write!(f, "2D")?,
            Dim::Dim3D => write!(f, "3D")?,
            Dim::DimCube => write!(f, "Cube")?,
            Dim::DimRect => write!(f, "Rect")?,
            Dim::DimBuffer => write!(f, "Buffer")?,
            Dim::DimSubpassData => write!(f, "SubpassData")?,
        }
        if *ms {
            write!(f, "MS")?;
        }
        if *arrayed {
            write!(f, "Array")?;
        }
        // TODO this is incomplete
        Ok(())
    }
}

// TODO: arcs are probably not necessary here, we could replace them with a straight reference, but then
// we'd need an arena for the types
#[derive(Clone, Debug)]
pub struct Type(pub(crate) Arc<TypeKind>);

// impl display for Type forwards to TypeKind
impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

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

/// User-provided struct layout
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ExplicitStructLayout {
    /// Field offsets
    pub offsets: Vec<u32>,
}

/// Structure field
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructField {
    /// Name of the field.
    pub name: String,
    /// Type of the field.
    pub ty: Type,
    /// Syntax node.
    pub syntax: Option<InFile<AstPtr<ast::StructField>>>,
    pub location: Option<u32>,
    pub interpolation: Option<Interpolation>,
    pub builtin: Option<spirv::BuiltIn>,
    pub offset: Option<u32>,
    pub align: Option<u32>,
}

impl StructField {
    pub fn new(name: String, ty: Type, syntax: Option<InFile<AstPtr<ast::StructField>>>) -> StructField {
        StructField {
            name,
            ty,
            syntax,
            location: None,
            interpolation: None,
            builtin: None,
            offset: None,
            align: None,
        }
    }
}

/*/// Row-major or column-major order for matrices.
#[derive(Default, Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum MatrixOrder {
    #[default]
    ColumnMajor,
    RowMajor,
}*/
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructLayout {
    pub offsets: Vec<u32>,
    pub layout: Layout,
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
        /// Matrix stride.
        stride: u32,
    },
    /// Array type (element type + size).
    Array {
        element_type: Type,
        size: u32,
        /// Array stride. None if the element type is opaque.
        stride: Option<u32>,
    },
    /// Runtime array type. Array without a known length.
    RuntimeArray {
        element_type: Type,
        /// Array stride. None if the element type is opaque.
        stride: Option<u32>,
    },
    /// Structure type (array of (offset, type) tuples).
    Struct {
        /// Name of the struct, only used for display purposes. Can be empty, or can be different from the name in the def.
        name: String,
        fields: Vec<StructField>,
        def: Option<StructId>,
        /// Field offsets
        layout: Option<StructLayout>,
    },
    /// Unsampled image type.
    Image(ir::types::ImageType),
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
    /*/// Built-in type for `modf` results (struct with `fract` and `whole` fields)
    ModfResultFloat(ScalarType),
    /// Built-in type for `modf` results (struct with `fract` and `whole` fields)
    ModfResultVecN(ScalarType, u8),
    /// Built-in type for `frexp` results (struct with `fract` and `exp` fields)
    FrexpResultFloat(ScalarType),
    /// Built-in type for `frexp` results (struct with `fract` and `exp` fields)
    FrexpResultVecN(ScalarType, u8),*/
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::Unit => write!(f, "void"),
            TypeKind::Scalar(s) => write!(f, "{}", scalar_type_to_string(*s)),
            TypeKind::Vector(s, size) => write!(f, "{}vec{}", scalar_type_prefix(*s), size),
            TypeKind::Matrix {
                component_type,
                columns,
                rows,
                ..
            } => {
                write!(f, "{}mat{}x{}", scalar_type_prefix(*component_type), columns, rows,)
            }
            TypeKind::Array {
                element_type,
                size,
                stride: _,
            } => {
                // TODO display layout
                write!(f, "{}[{}]", &element_type.0, size)
            }
            TypeKind::RuntimeArray { element_type, .. } => write!(f, "{}[]", element_type),
            TypeKind::Struct { fields, name, .. } => {
                // TODO display layout
                if !name.is_empty() {
                    return write!(f, "{}", name);
                } else {
                    write!(f, "struct {{ ")?;
                    for (i, StructField { name, ty, .. }) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", name, ty)?;
                    }
                    write!(f, " }}")
                }
            }
            TypeKind::Image(ty) => write!(f, "{}", ImageTypeDisplay(ty)),
            TypeKind::Pointer {
                pointee_type,
                storage_class,
            } => {
                // TODO: display storage class
                write!(f, "pointer({storage_class:?}) to {pointee_type}")
            }
            TypeKind::Function(fty) => {
                // TODO use C syntax
                write!(f, "fn(")?;
                for (i, arg) in fty.arg_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") -> {}", fty.return_type)
            }
            TypeKind::Sampler => write!(f, "sampler"),
            TypeKind::SamplerShadow => write!(f, "samplerShadow"),
            TypeKind::String => write!(f, "string"),
            TypeKind::Unknown => write!(f, "unknown"),
            TypeKind::Error => write!(f, "error"),
        }
    }
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

    /// Returns the typekind with the same shape but with a different scalar type.
    pub fn with_scalar_type(&self, scalar_type: ScalarType) -> Self {
        match self {
            TypeKind::Scalar(_) => TypeKind::Scalar(scalar_type),
            TypeKind::Vector(_, size) => TypeKind::Vector(scalar_type, *size),
            TypeKind::Matrix {
                component_type: _,
                columns,
                rows,
                stride,
            } => TypeKind::Matrix {
                component_type: scalar_type,
                columns: *columns,
                rows: *rows,
                stride: *stride,
            },
            _ => self.clone(),
        }
    }

    /// Returns whether this type is a scalar or vector.
    pub fn is_scalar_or_vector(&self) -> bool {
        match self {
            TypeKind::Scalar(_) | TypeKind::Vector(_, _) => true,
            _ => false,
        }
    }

    pub fn is_scalar(&self) -> bool {
        match self {
            TypeKind::Scalar(_) => true,
            _ => false,
        }
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, TypeKind::Unit)
    }

    pub fn is_struct(&self) -> bool {
        matches!(self, TypeKind::Struct { .. })
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, TypeKind::Pointer { .. })
    }

    pub fn storage_class(&self) -> Option<spirv::StorageClass> {
        if let TypeKind::Pointer { storage_class, .. } = self {
            Some(*storage_class)
        } else {
            None
        }
    }

    pub fn is_opaque(&self) -> bool {
        match self {
            TypeKind::Image(_)
            | TypeKind::String
            | TypeKind::Sampler
            | TypeKind::SamplerShadow
            | TypeKind::Unknown
            | TypeKind::Error
            | TypeKind::Function(_) => true,
            TypeKind::Struct { fields, .. } => fields.iter().any(|f| f.ty.is_opaque()),
            _ => false,
        }
    }

    pub fn num_components(&self) -> Option<u8> {
        match self {
            TypeKind::Scalar(_) => Some(1),
            TypeKind::Vector(_, size) => Some(*size),
            TypeKind::Matrix { columns, rows, .. } => Some(columns * rows),
            _ => None,
        }
    }

    /// Returns the layout of a type.
    pub fn layout(&self) -> Option<Layout> {
        let layout = match *self {
            TypeKind::Unit => Layout { align: 1, size: 0 },
            TypeKind::Scalar(s) => std_scalar_type_layout(s),
            TypeKind::Vector(s, l) => std_vector_type_layout(s, l),
            TypeKind::Matrix { stride, columns, .. } => Layout {
                align: stride,
                size: stride * (columns as u32),
            },
            TypeKind::Array { stride, size, .. } => {
                if let Some(stride) = stride {
                    Layout {
                        align: stride,
                        size: stride * size,
                    }
                } else {
                    return None;
                }
            }
            TypeKind::Struct { ref layout, .. } => {
                if let Some(layout) = layout {
                    layout.layout
                } else {
                    return None;
                }
            }
            TypeKind::RuntimeArray { .. }
            | TypeKind::Image(_)
            | TypeKind::Pointer { .. }
            | TypeKind::Function(_)
            | TypeKind::Sampler
            | TypeKind::SamplerShadow
            | TypeKind::String
            | TypeKind::Unknown
            | TypeKind::Error => {
                return None;
            }
        };
        Some(layout)
    }
}
