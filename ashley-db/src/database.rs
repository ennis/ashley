use crate::{DbIndex, Revision, Runtime};

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Operations on a database type.
pub trait Database: Send + 'static {
    ///
    fn maybe_changed_after(&self, dbindex: DbIndex, after: Revision) -> bool;

    ///
    fn on_new_revision(&mut self, revision: Revision);

    /// Returns the `Runtime` instance.
    fn runtime(&self) -> &Runtime;

    /// Returns a mutable reference to the `Runtime`.
    fn runtime_mut(&mut self) -> &mut Runtime;

    /// Returns the current revision.
    ///
    /// Short-hand for `self.runtime().current_revision()`.
    fn current_revision(&self) -> Revision {
        self.runtime().current_revision()
    }
}

/// Extension methods on databases.
pub trait DatabaseExt: Database {
    /// Increments the revision number, then executes the closure with the new revision index.
    ///
    /// Use the returned revision as an input to `set()` methods of input tables.
    fn with_new_revision<R>(&mut self, f: impl FnOnce(&mut Self, Revision) -> R) -> R {
        let next_rev = self.runtime_mut().next_revision();
        let r = f(self, next_rev);
        self.on_new_revision(next_rev);
        r
    }
}

impl<DB: Database + ?Sized> DatabaseExt for DB {}

////////////////////////////////////////////////////////////////////////////////////////////////////
