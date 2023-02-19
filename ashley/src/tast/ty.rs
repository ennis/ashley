use crate::{hir, syntax::ast, tast::DefId, Session};
use ashley::tast::TypeCtxt;
use spirv::Dim;
use std::{
    fmt,
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
    ops::Deref,
    ptr,
    sync::Arc,
};

use crate::{
    diagnostic::Diagnostics,
    tast::{
        consteval::{try_evaluate_constant_expression, ConstantValue},
        def::DefKind,
        scope::{resolve_name, Res, Scope},
        Module,
    },
};
pub use hir::types::{ImageSampling, ImageType};

pub type ScalarType = hir::types::ScalarType;

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
            depth,
            ms,
            sampled,
            image_format,
            access,
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
    },
    /// Array type (element type + size).
    Array(Type, u32),
    /// Runtime array type. Array without a known length.
    RuntimeArray(Type),
    /// Structure type (array of (offset, type) tuples).
    Struct {
        /// Name of the struct, only used for display purposes. Can be empty, or can be different from the name in the def.
        name: String,
        fields: Vec<(String, Type)>,
        def: Option<DefId>,
    },
    /// Unsampled image type.
    Image(hir::types::ImageType),
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
            } => write!(f, "{}mat{}x{}", scalar_type_prefix(*component_type), columns, rows),
            TypeKind::Array(ty, size) => write!(f, "{}[{}]", &ty.0, size),
            TypeKind::RuntimeArray(ty) => write!(f, "{}[]", ty),
            TypeKind::Struct { fields, name, .. } => {
                if !name.is_empty() {
                    return write!(f, "{}", name);
                } else {
                    write!(f, "struct {{ ")?;
                    for (i, (name, ty)) in fields.iter().enumerate() {
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
                // TODO: incomplete
                write!(f, "ptr to {}", pointee_type)
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
            } => TypeKind::Matrix {
                component_type: scalar_type,
                columns: *columns,
                rows: *rows,
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

    pub fn num_components(&self) -> Option<u8> {
        match self {
            TypeKind::Scalar(_) => Some(1),
            TypeKind::Vector(_, size) => Some(*size),
            TypeKind::Matrix { columns, rows, .. } => Some(columns * rows),
            _ => None,
        }
    }
}

/// A type as it appears in the AST, and its resolved form.
pub struct TypeSpec {
    pub ast: ast::Type,
    pub ty: Type,
}

/// Converts an AST type to a TAST type.
pub(crate) fn convert_type(
    tyctxt: &mut TypeCtxt,
    ty: ast::Type,
    module: &Module,
    scopes: &[Scope],
    diag: &mut Diagnostics,
) -> Type {
    match ty {
        ast::Type::TypeRef(tyref) => {
            let Some(ident) = tyref.ident() else { return tyctxt.error.clone(); };
            if let Some(res) = resolve_name(tyctxt, ident.text(), scopes) {
                match res {
                    Res::Global(def_id) => {
                        let def = module.def(def_id);
                        match def.kind {
                            DefKind::Struct(ref struct_def) => struct_def.ty.clone(),
                            _ => {
                                diag.error("expected a type").location(ident).emit();
                                tyctxt.error.clone()
                            }
                        }
                    }
                    Res::PrimTy(ty) => ty,
                    _ => {
                        diag.error("expected a type").location(ident).emit();
                        tyctxt.error.clone()
                    }
                }
            } else {
                // TODO better error message
                diag.error("expected a type").location(ident).emit();
                tyctxt.error.clone()
            }
        }
        ast::Type::TupleType(_tuple_ty) => {
            todo!("tuple types");
            /*let fields: Vec<_> = tuple_ty
                .fields()
                .enumerate()
                .map(|(i,f)| StructField {
                    ast: None,
                    name: format!("field{}", i),
                    ty: self.convert_type(f),
                })
                .collect();
            self.module.ty(TypeKind::Struct(Arc::new(StructDef {
                ast: None,
                name: "".to_string(), // TODO generate a name for tuples (or remove tuples entirely)
                fields,
                ty: (),
            })))*/
        }
        ast::Type::ArrayType(array_type) => {
            let element_type = array_type
                .element_type()
                .map(|ty| convert_type(tyctxt, ty, module, scopes, diag))
                .unwrap_or_else(|| tyctxt.error.clone());

            if let Some(expr) = array_type.length() {
                if let Some(len) = try_evaluate_constant_expression(&expr, module, diag) {
                    match len {
                        ConstantValue::Int(len) => {
                            let len = len as u32;
                            tyctxt.ty(TypeKind::Array(element_type, len))
                        }
                        ConstantValue::Float(_) => {
                            diag.error("array length must be an integer").location(expr).emit();
                            tyctxt.error.clone()
                        }
                        ConstantValue::Bool(_) => {
                            diag.error("array length must be an integer").location(expr).emit();
                            tyctxt.error.clone()
                        }
                    }
                } else {
                    // TODO better error message
                    diag.error("array length must be a constant expression")
                        .location(expr)
                        .emit();
                    tyctxt.error.clone()
                }
            } else {
                // no length
                tyctxt.ty(TypeKind::RuntimeArray(element_type))
            }
        }
        ast::Type::ClosureType(_closure_type) => {
            todo!("closure types")
        }
    }
}
