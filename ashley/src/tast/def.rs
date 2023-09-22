use crate::{
    builtins::BuiltinSignature,
    diagnostic::Span,
    ir::Interpolation,
    syntax::ast,
    tast::{ty::Type, InFile},
};
use rowan::ast::AstPtr;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub ast: Option<InFile<AstPtr<ast::StructDef>>>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParam {
    pub ast: Option<InFile<AstPtr<ast::FnParam>>>,
    pub ty: Type,
    /// Can be empty.
    pub name: String,
}

/// A function definition or declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionDef {
    pub ast: Option<InFile<ast::AstPtr<ast::FnDef>>>,
    pub has_body: bool,
    pub linkage: Option<spirv::LinkageType>,
    pub function_control: spirv::FunctionControl,
    //pub function_type: Type,
    pub parameters: Vec<FunctionParam>,
    //pub builtin: Option<&'static BuiltinSignature>,
    //pub execution_model: Option<spirv::ExecutionModel>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GlobalDef {
    pub ast: Option<InFile<ast::AstPtr<ast::Global>>>,
    pub linkage: Option<spirv::LinkageType>,
    pub ty: Type,
    pub qualifier: Option<Qualifier>,
    pub location: Option<u32>,
    pub interpolation: Option<Interpolation>,
    pub builtin: Option<spirv::BuiltIn>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Def {
    pub item: ItemId,
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
