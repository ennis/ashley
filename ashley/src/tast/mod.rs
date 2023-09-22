//! Typed AST representation
mod attributes;
mod body;
mod builtins;
mod consteval;
mod def;
mod diagnostics;
mod expr;
mod item;
mod layout;
mod lower;
mod overload;
mod scope;
mod stmt;
mod swizzle;
pub mod ty;

pub use attributes::{AttrParseError, Attribute, AttributeMultiplicity, AttributeTarget};
pub(crate) use attributes::{AttributeChecker, AttributeCheckerImpl};
pub use def::{Def, DefKind, FunctionDef, Qualifier, Visibility};
pub use expr::Expr;
pub use lower::lower_to_hir;
pub(crate) use scope::Scope;
pub use ty::{FunctionType, ScalarType, Type, TypeKind};

use crate::{
    builtins::PrimitiveTypes,
    diagnostic::{DiagnosticSpan, Span, Spanned},
    session::{CompilerDb, DefId, ModuleId, SourceFileId},
    syntax::{ast, ast::Name, SyntaxNode, SyntaxNodePtr, SyntaxToken},
    tast::{diagnostics::TyDiagnostic, scope::Res, stmt::Stmt, ty::TypeLoweringCtxt},
    utils::{Id, IndexVec},
};
use ashley::syntax::TextRange;
use ashley_data_structures::IndexVecMap;
use ashley_db::new_key_type;
use rowan::ast::{AstNode, AstPtr};
use std::{
    collections::{HashMap, HashSet},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Utility trait to parse a value with access to the diagnostics output.
pub trait ParseFrom<T>: Sized {
    fn parse_from(value: T, compiler: &dyn CompilerDb) -> Result<Self, ()>;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

pub type ExprId = Id<Expr>;
pub type StmtId = Id<Stmt>;
pub type BlockId = Id<Block>;
pub type LocalVarId = Id<LocalVar>;

/*#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct DefId {
    pub package: PackageId,
    pub local_def: LocalDefId,
}*/

/*impl DefId {
    pub fn new(package: PackageId, local_def_id: LocalDefId) -> DefId {
        DefId {
            package,
            local_def: local_def_id,
        }
    }
}*/

pub(crate) trait NameExt {
    // TODO rename this to to_unique_name
    fn to_string_opt(&self) -> String;
}

impl NameExt for Option<Name> {
    fn to_string_opt(&self) -> String {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        self.as_ref()
            .map(|ident| ident.text().to_string())
            .unwrap_or_else(|| format!("__anon_{}", COUNTER.fetch_add(1, Ordering::Relaxed)))
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {}
