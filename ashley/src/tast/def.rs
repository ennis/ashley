use crate::{
    builtins::BuiltinSignature,
    diagnostic::SourceLocation,
    syntax,
    syntax::{ast, SyntaxKind},
    tast::{ty::Type, PackageImportId},
};

/// Describes the kind of a global program variable.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Qualifier {
    Uniform,
    Const,
    In,
    Out,
}

impl Qualifier {
    fn from_token(token: &syntax::SyntaxToken) -> Option<Qualifier> {
        match token.kind() {
            SyntaxKind::UNIFORM_KW => Some(Qualifier::Uniform),
            SyntaxKind::IN_KW => Some(Qualifier::In),
            SyntaxKind::OUT_KW => Some(Qualifier::Out),
            SyntaxKind::CONST_KW => Some(Qualifier::Const),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct StructDef {
    pub ast: Option<ast::StructDef>,
    pub ty: Type,
}

#[derive(Debug)]
pub struct FunctionParam {
    pub ast: Option<ast::FnParam>,
    pub ty: Type,
    pub name: String,
}

/// A function definition or declaration.
#[derive(Debug)]
pub struct FunctionDef {
    pub ast: Option<ast::FnDef>,
    pub linkage: Option<spirv::LinkageType>,
    pub function_control: spirv::FunctionControl,
    pub function_type: Type,
    pub parameters: Vec<FunctionParam>,
    pub builtin: Option<&'static BuiltinSignature>,
}

#[derive(Debug)]
pub struct GlobalDef {
    pub ast: Option<ast::Global>,
    pub linkage: Option<spirv::LinkageType>,
    pub ty: Type,
    pub qualifier: Option<Qualifier>,
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
    /// The package containing the definition.
    ///
    /// `None` if defined in the current package.
    pub package: Option<PackageImportId>,
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
}
