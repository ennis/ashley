use crate::syntax::SyntaxKind;
use crate::syntax;
use crate::syntax::ast;
use crate::tast::ty::{Type, TypeSpec};
use crate::tast::ast_identity;

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

pub struct StructField {
    pub ast: ast::StructField,
    pub ty: Type,
    pub name: ast::Ident,
}
ast_identity!(StructField);

pub struct StructDef {
    pub ast: ast::StructDef,
    pub fields: Vec<StructField>,
}
ast_identity!(StructDef);

pub struct FunctionParam {
    pub ast: ast::FnParam,
    pub ty: Type,
    pub name: ast::Ident,
}
ast_identity!(FunctionDef);

pub struct FunctionDef {
    pub ast: ast::FnDef,
    pub extern_: bool,
    pub return_type: Type,
    pub parameters: Vec<FunctionParam>,
}
ast_identity!(FunctionDef);

pub struct GlobalDef {
    pub ast: ast::Global,
    pub extern_: bool,
    pub ty: Type,
    pub qualifier: Qualifier,
    pub initializer
}
ast_identity!(GlobalDef);