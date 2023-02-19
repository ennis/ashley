use thiserror::Error;
use crate::diagnostic::SourceLocation;
use crate::{
    tast::{Def, LocalDefId},
    utils::TypedVec,
};
use crate::tast::{DefId, FunctionDef, Module, Scope};

mod resolver;
mod filesystem_resolver;

#[derive(Debug)]
pub struct Package {
    pub(crate) defs: TypedVec<Def, LocalDefId>,
}

impl Package {
    /// Imports all the definitions of this package into the given scope.
    pub(crate) fn import(&self, scope: &mut Scope) {
        for (id, def) in self.defs.iter_full() {
            scope.add_def(DefId::from(id), def);
        }
    }
}
