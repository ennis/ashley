use crate::{hir, syntax::ast, tast::DefId};
use ashley::tast::TypeCtxt;
use std::{
    hash::{Hash, Hasher},
    ops::Deref,
    ptr,
    sync::Arc,
};

use crate::{
    diagnostic::Diagnostics,
    tast::{
        consteval::ConstantValue,
        def::DefKind,
        scope::{Res, Scope},
        Module,
    },
};
pub use hir::types::{ImageSampling, ImageType};

pub type ScalarType = hir::types::ScalarType;

// TODO: arcs are probably not necessary here, we could replace them with a straight reference, but then
// we'd need an arena for the types
#[derive(Clone, Debug)]
pub struct Type(pub(crate) Arc<TypeKind>);

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

impl TypeCtxt {
    /// Converts an AST type to a TAST type.
    pub(crate) fn convert_type(
        &mut self,
        ty: ast::Type,
        module: &Module,
        scopes: &[Scope],
        diag: &mut Diagnostics,
    ) -> Type {
        match ty {
            ast::Type::TypeRef(tyref) => {
                let Some(ident) = tyref.ident() else { return self.error.clone(); };
                if let Some(res) = self.resolve_name(ident.text(), scopes) {
                    match res {
                        Res::Global(def_id) => {
                            let def = module.def(def_id);
                            match def.kind {
                                DefKind::Struct(ref struct_def) => struct_def.ty.clone(),
                                _ => {
                                    diag.error("expected a type").location(ident).emit();
                                    self.error.clone()
                                }
                            }
                        }
                        Res::PrimTy(ty) => ty,
                        _ => {
                            diag.error("expected a type").location(ident).emit();
                            self.error.clone()
                        }
                    }
                } else {
                    // TODO better error message
                    diag.error("expected a type").location(ident).emit();
                    self.error.clone()
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
                    .map(|ty| self.convert_type(ty, module, scopes, diag))
                    .unwrap_or_else(|| self.error.clone());

                if let Some(expr) = array_type.length() {
                    if let Some(len) = self.try_evaluate_constant_expression(&expr, module, diag) {
                        match len {
                            ConstantValue::Int(len) => {
                                let len = len as u32;
                                self.ty(TypeKind::Array(element_type, len))
                            }
                            ConstantValue::Float(_) => {
                                diag.error("array length must be an integer").location(expr).emit();
                                self.error.clone()
                            }
                            ConstantValue::Bool(_) => {
                                diag.error("array length must be an integer").location(expr).emit();
                                self.error.clone()
                            }
                        }
                    } else {
                        // TODO better error message
                        diag.error("array length must be a constant expression")
                            .location(expr)
                            .emit();
                        self.error.clone()
                    }
                } else {
                    // no length
                    self.ty(TypeKind::RuntimeArray(element_type))
                }
            }
            ast::Type::ClosureType(_closure_type) => {
                todo!("closure types")
            }
        }
    }
}
