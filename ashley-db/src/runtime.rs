use crate::{DbIndex, Revision};
use std::{cell::RefCell, collections::HashSet};

/// An entry in the query stack.
struct ActiveQuery {
    label: String,
    index: DbIndex,
    dependencies: HashSet<DbIndex>,
}

/// Tracks the current state of the database: current revision number, whether we are executing a query, etc.
pub struct Runtime {
    /// All active queries.
    query_stack: RefCell<Vec<ActiveQuery>>,
    rev: Revision,
}

impl Runtime {
    /// Creates a new `Runtime`.
    pub fn new() -> Runtime {
        Runtime {
            query_stack: RefCell::new(vec![]),
            rev: Revision(0),
        }
    }

    /// Called when running the query for the specified `index`.
    ///
    /// This pushes an entry into the query stack. There must be a matching call to `pop_query`
    /// when the query has finished.
    pub fn push_query(&self, index: DbIndex) {
        self.query_stack.borrow_mut().push(ActiveQuery {
            label: "".to_string(),
            index,
            dependencies: Default::default(),
        });
    }

    /// Sets a debug label for the topmost active query.
    pub fn set_query_label(&self, label: impl Into<String>) {
        self.query_stack
            .borrow_mut()
            .last_mut()
            .expect("`set_query_label` must be called inside a query")
            .label = label.into();
    }

    /// Pops the last entry on the active query stack and returns the collected dependencies of the query.
    pub fn pop_query(&self) -> HashSet<DbIndex> {
        let q = self.query_stack.borrow_mut().pop();
        let q = q.expect("unbalanced calls to `Runtime::enter_query`/`Runtime::exit_query`");
        q.dependencies
    }

    /// Registers a dependency to the current executing query.
    pub fn add_dependency(&self, key: DbIndex) {
        let mut stack = self.query_stack.borrow_mut();
        // it is not an error to call this outside of an active query
        // (it just means that we're at the top level and we don't need to track dependencies).
        if let Some(active) = stack.last_mut() {
            active.dependencies.insert(key);
        }
    }

    /// Returns the dependencies of the currently executing query.
    pub fn current_dependencies(&self) -> HashSet<DbIndex> {
        self.query_stack
            .borrow_mut()
            .last_mut()
            .expect("`current_dependencies` must be called inside a query")
            .dependencies
            .clone()
    }

    /// Returns the database index of the topmost active query in the stack.
    ///
    /// Panics if no query is active.
    pub fn active_query_index(&self) -> DbIndex {
        self.query_stack
            .borrow()
            .last()
            .expect("`active_query` must be called inside a query")
            .index
    }

    /// Returns the labels of all active queries, starting with the latest started.
    pub fn active_query_labels(&self) -> Vec<String> {
        let mut labels = vec![];
        let query_stack = self.query_stack.borrow_mut();
        for q in query_stack.iter().rev() {
            labels.push(q.label.clone());
        }
        labels
    }

    /// Increases the revision number and returns the current revision.
    pub fn next_revision(&mut self) -> Revision {
        self.rev.0 += 1;
        self.rev
    }

    /// Returns the current revision index.
    pub fn current_revision(&self) -> Revision {
        self.rev
    }
}
