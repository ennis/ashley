//! Query system

use crate::{
    new_index,
    session::PackageId,
    syntax::ast,
    tast::DefId,
    utils::{IndexVec, IndexVecMap},
};
use std::{
    any::{Any, TypeId},
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    hash::Hash,
};

mod database;
pub mod derived;
pub mod input;
pub mod interned;

pub use database::{Database, DatabaseImpl, Runtime};

new_index! { pub struct DepNodeId; }

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct DbKey {
    pub table_index: usize,
    pub index: usize,
}

impl DbKey {
    pub fn new(table_index: usize, index: usize) -> DbKey {
        DbKey { table_index, index }
    }
}

pub type Revision = usize;

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Generic operations on query data.
///
/// # Type arguments
/// * `DB` the database type
pub trait TableOps<DB: Database> {
    fn table_index(&self) -> usize;
    fn maybe_changed_after(&self, db: &DB, index: usize, rev: Revision) -> bool;
    fn new_revision(&mut self) {}
}

////////////////////////////////////////////////////////////////////////////////////////////////////

// Should the query system be extensible?
// Clients other than the compiler would be able to add their own queries and derived computations.
// Could be useful for analyses.

// Q: when should nodes be removed?
// Example: a bunch of lines are removed from the source, resulting in removed definitions
// But the old definitions themselves remain in the query cache (some DefIds may be reused though).
// -> note that affected definitions will be marked "red", so at some point we can "cull" red cache entries

// Should query results be re-evaluated eagerly?
// -> not sure

// Algorithm:
// - each DepNode has a revision index
// - when some data is queried, traverse the inputs of the queries; if any input DepNode has a more recent rev index,
//   then the query must be recomputed (do that recursively). The rev index of the dep node is updated with the max
//   rev index of the inputs

// On input change:
// - update the rev index of the input (we have one global rev counter, increase it and set the input rev to that value)
// -

// How are DefIds produced?
// First, package sources are provided as input.
// Then, the `definitions(package_id)` query is invoked.
// The query scans the AST and produces a complete list of definitions in an `IndexVec<DefId, Definition>`.
// This vec is the output value of the query, and is cached for future reference.
//
// This invalidates any previous queries that depend on `definitions`. Notably, "projection queries" that
// return a definition from a DefId are all invalidated. However, when re-running projection queries, we can
// compare the result with the previous value. If it's the same we don't need to re-run the queries that depend on the projection
// (see https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html#the-projection-query-pattern).
//
// The main issue is that the data of the definitions is effectively duplicated: once in the output value of
// the `definitions` query (the IndexVec), and once in the outputs of each projection queries over the definitions.
// This is wasteful, as we could have only one IndexVec containing the definitions.
//
// Alternative: some queries can produce more than one value.
//
