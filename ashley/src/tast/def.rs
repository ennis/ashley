use crate::{
    builtins::BuiltinSignature,
    diagnostic::{Diagnostics, SourceLocation},
    hir::Interpolation,
    syntax::{ast, SyntaxNodePtr},
    tast::{
        attributes::{KnownAttribute, KnownAttributeKind},
        ty::Type,
    },
};
use rowan::ast::AstPtr;
use std::fmt;

/// Describes the kind of a global program variable.
/// TODO remove this and use spirv::StorageClass directly?
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Qualifier {
    Buffer,
    Shared,
    Uniform,
    Const,
    In,
    Out,
}

/*impl Qualifier {
    fn from_token(token: &syntax::SyntaxToken) -> Option<Qualifier> {
        match token.kind() {
            SyntaxKind::UNIFORM_KW => Some(Qualifier::Uniform),
            SyntaxKind::BUFFER_KW => Some(Qualifier::Buffer),
            SyntaxKind::SHARED_KW => Some(Qualifier::Shared),
            SyntaxKind::IN_KW => Some(Qualifier::In),
            SyntaxKind::OUT_KW => Some(Qualifier::Out),
            SyntaxKind::CONST_KW => Some(Qualifier::Const),
            _ => None,
        }
    }
}*/

/// display impl for qualifier
impl fmt::Display for Qualifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Qualifier::Buffer => write!(f, "buffer"),
            Qualifier::Uniform => write!(f, "uniform"),
            Qualifier::Shared => write!(f, "shared"),
            Qualifier::Const => write!(f, "const"),
            Qualifier::In => write!(f, "in"),
            Qualifier::Out => write!(f, "out"),
        }
    }
}

#[derive(Debug)]
pub struct StructDef {
    pub ast: Option<AstPtr<ast::StructDef>>,
    pub ty: Type,
}

#[derive(Debug)]
pub struct FunctionParam {
    pub ast: Option<AstPtr<ast::FnParam>>,
    pub ty: Type,
    /// Can be empty.
    pub name: String,
}

/// Qualifiers on functions.
#[derive(Default, Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionQualifiers {
    pub execution_model: Option<spirv::ExecutionModel>,
}

impl FunctionQualifiers {
    pub(crate) fn from_attributes(
        attrs: &[KnownAttribute],
        srcloc: Option<SourceLocation>,
        diag: &mut Diagnostics,
    ) -> FunctionQualifiers {
        let mut execution_model = None;
        let mut specified_execution_model_more_than_once = false;

        for attr in attrs {
            match attr.kind {
                KnownAttributeKind::Vertex => {
                    if execution_model.is_some() {
                        specified_execution_model_more_than_once = true;
                    }
                    execution_model = Some(spirv::ExecutionModel::Vertex);
                }
                KnownAttributeKind::Fragment => {
                    if execution_model.is_some() {
                        specified_execution_model_more_than_once = true;
                    }
                    execution_model = Some(spirv::ExecutionModel::Fragment);
                }
                _ => {
                    //
                    diag.error(format!("invalid attribute"))
                        .primary_label_opt(srcloc, "")
                        .emit();
                }
            }
        }

        if specified_execution_model_more_than_once {
            // TODO more precise locations
            diag.error(format!("execution model specified more than once"))
                .primary_label_opt(srcloc, "")
                .emit();
        }

        FunctionQualifiers { execution_model }
    }
}

/// A function definition or declaration.
#[derive(Debug)]
pub struct FunctionDef {
    pub ast: Option<ast::AstPtr<ast::FnDef>>,
    pub has_body: bool,
    pub linkage: Option<spirv::LinkageType>,
    pub function_control: spirv::FunctionControl,
    pub function_type: Type,
    pub parameters: Vec<FunctionParam>,
    pub builtin: Option<&'static BuiltinSignature>,
    pub execution_model: Option<spirv::ExecutionModel>,
}

#[derive(Debug)]
pub struct GlobalDef {
    pub ast: Option<ast::AstPtr<ast::Global>>,
    pub linkage: Option<spirv::LinkageType>,
    pub ty: Type,
    pub qualifier: Option<Qualifier>,
    pub location: Option<u32>,
    pub interpolation: Option<Interpolation>,
    pub builtin: Option<spirv::BuiltIn>,
}

#[derive(Debug)]
pub enum DefKind {
    Function(FunctionDef),
    Global(GlobalDef),
    Struct(StructDef),
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug)]
pub struct Def {
    // The package containing the definition.
    //
    // `None` if defined in the current package.
    //pub package: Option<PackageImportId>,
    /// Source location, if available.
    pub location: Option<SourceLocation>,
    pub builtin: bool,
    /// Name of the definition.
    pub name: String,
    pub visibility: Visibility,
    // TODO: to type-check this, we need to remember the set of visible items before this definition.
    pub kind: DefKind,
}

impl Def {
    pub fn as_struct(&self) -> Option<&StructDef> {
        match &self.kind {
            DefKind::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&FunctionDef> {
        match &self.kind {
            DefKind::Function(f) => Some(f),
            _ => None,
        }
    }

    /// Returns a wrapper object implementing `Display` that can be used to print the declaration of
    /// the definition.
    ///
    /// * Functions: printed as `int foo(int, float)`
    /// * Globals: printed as `uniform int foo`
    /// * Structs: printed as `struct Foo`
    pub fn display_declaration(&self) -> impl fmt::Display + '_ {
        pub struct DefDisplay<'a>(&'a Def);

        impl<'a> fmt::Display for DefDisplay<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.0.kind {
                    DefKind::Function(ref func) => {
                        let func_ty = func.function_type.as_function().unwrap();
                        write!(f, "{} {}(", func_ty.return_type, self.0.name)?;
                        for (i, arg) in func_ty.arg_types.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", arg)?;
                        }
                        write!(f, ")")?;
                        Ok(())
                    }
                    DefKind::Global(ref global) => {
                        if let Some(qual) = global.qualifier {
                            write!(f, "{} ", qual)?;
                        }
                        write!(f, "{} {}", global.ty, self.0.name)
                    }
                    DefKind::Struct(_) => {
                        write!(f, "struct {}", self.0.name)
                    }
                }
            }
        }

        DefDisplay(self)
    }
}
