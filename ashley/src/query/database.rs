use crate::query::{DbKey, Revision, TableOps};
use memo_map::MemoMap;
use std::{
    any::{Any, TypeId},
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    marker::PhantomData,
};

struct ActiveQuery {
    dependencies: HashSet<DbKey>,
}

pub struct Runtime {
    query_stack: RefCell<Vec<ActiveQuery>>,
    active_query: RefCell<Option<ActiveQuery>>,
}

impl Runtime {
    pub fn enter_query(&self, key: DbKey) {
        let prev_active = self.active_query.replace(Some(ActiveQuery {
            dependencies: Default::default(),
        }));
        if let Some(q) = prev_active {
            self.query_stack.borrow_mut().push(q);
        }
    }

    /// Exits the current query and returns its dependencies.
    pub fn exit_query(&self) -> HashSet<DbKey> {
        let q = self.query_stack.borrow_mut().pop();
        self.active_query.replace(q).unwrap().dependencies
    }

    /// Registers a dependency to the current executing query.
    pub fn add_dependency(&self, key: DbKey) {
        let mut active_query = self.active_query.borrow_mut();
        if let Some(active_query) = active_query.as_mut() {
            active_query.dependencies.insert(key);
        }
    }

    fn new() -> Runtime {
        Runtime {
            query_stack: RefCell::new(vec![]),
            active_query: RefCell::new(None),
        }
    }
}

pub struct DatabaseImpl {
    /// Current input revision index.
    input_rev: Cell<Revision>,
    /// Table index counter.
    next_table_index: usize,
    runtime: Runtime,
}

impl DatabaseImpl {
    /// Creates a new, empty database.
    pub fn new() -> DatabaseImpl {
        DatabaseImpl {
            input_rev: Cell::new(0),
            next_table_index: 0,
            runtime: Runtime::new(),
        }
    }

    /// Returns the current revision index.
    pub fn current_rev(&self) -> Revision {
        self.input_rev.get()
    }

    /// Increments the revision counter and returns the new value.
    pub fn next_rev(&self) -> Revision {
        let mut r = self.input_rev.get();
        r += 1;
        self.input_rev.set(r);
        r
    }

    pub fn next_table_index(&mut self) -> usize {
        let i = self.next_table_index;
        self.next_table_index += 1;
        i
    }
}

pub trait Database {
    /// Increments the table index and returns the new value.
    fn next_table_index(&mut self) -> usize;

    /// Returns the current revision index.
    fn current_rev(&self) -> Revision;

    /// Increments the revision counter and returns the new value.
    fn next_rev(&mut self) -> Revision;

    ///
    fn maybe_changed_after(&self, dep_index: DbKey, after: Revision) -> bool;

    /// Returns the current query state
    fn runtime(&self) -> &Runtime;
}

impl Database for DatabaseImpl {
    fn next_table_index(&mut self) -> usize {
        DatabaseImpl::next_table_index(self)
    }

    fn current_rev(&self) -> Revision {
        DatabaseImpl::current_rev(self)
    }

    fn next_rev(&mut self) -> Revision {
        DatabaseImpl::next_rev(self)
    }

    fn maybe_changed_after(&self, dep_index: DbKey, after: Revision) -> bool {
        panic!("unknown table index")
    }

    fn runtime(&self) -> &Runtime {
        &self.runtime
    }
}
