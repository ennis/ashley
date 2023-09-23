//! Derived queries.
//!
//! This is heavily inspired by salsa.
use crate::{database::Database, table::TableIndex, AsIndex, DbIndex, Idx, Index, Revision, TableOps};
use memo_map::MemoMap;
use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
};
use tracing::trace;

struct Memo<K, V> {
    key: K,
    value: V,
    changed_at: Cell<Revision>,
    verified: Cell<Revision>,
    /// List of dependencies.
    dependencies: RefCell<HashSet<DbIndex>>,
}

impl<K, V> Memo<K, V> {
    fn new(key: K, value: V, rev: Revision, dependencies: HashSet<DbIndex>) -> Memo<K, V> {
        Memo {
            key,
            value,
            changed_at: Cell::new(rev),
            verified: Cell::new(rev),
            dependencies: RefCell::new(dependencies),
        }
    }
}

enum QueryFunction<K, V, DB: ?Sized> {
    Direct(fn(&DB, K) -> V),
    Indirect(fn(&DB, K)),
}

pub struct DerivedQueryTable<K, V, DB: ?Sized> {
    table_index: TableIndex,
    memos: MemoMap<K, Memo<K, V>>,
    /// MemoMap for values that changed in the current evaluation cycle,
    /// because we can't update them in place.
    new_memos: MemoMap<K, Memo<K, V>>,
    query_fn: QueryFunction<K, V, DB>, //fn(&DB, K) -> V,
}

impl<K, V, DB> DerivedQueryTable<K, V, DB>
where
    K: AsIndex,
    V: PartialEq + Clone,
    DB: ?Sized + Database,
{
    pub fn new(table_index: TableIndex, query_fn: fn(&DB, K) -> V) -> DerivedQueryTable<K, V, DB> {
        DerivedQueryTable {
            table_index,
            memos: Default::default(),
            new_memos: Default::default(),
            query_fn: QueryFunction::Direct(query_fn),
        }
    }

    pub fn new_indirect(table_index: TableIndex, indirect_query_fn: fn(&DB, K)) -> DerivedQueryTable<K, V, DB> {
        DerivedQueryTable {
            table_index,
            memos: Default::default(),
            new_memos: Default::default(),
            query_fn: QueryFunction::Indirect(indirect_query_fn),
        }
    }

    fn get_memo(&self, key: K) -> Option<&Memo<K, V>> {
        // check temp memos first
        self.new_memos.get(&key).or_else(|| self.memos.get(&key))
    }

    fn clear_temp_memos(&mut self) {
        // TODO drain
        let keys = self.new_memos.keys().cloned().collect::<Vec<_>>();
        for k in keys {
            self.memos.replace(k, self.new_memos.remove(&k).unwrap());
        }
    }

    fn compute<'a>(&'a self, db: &DB, key: K, prev_memo: Option<&'a Memo<K, V>>) -> (&'a V, Revision) {
        // Evaluate the query function inside push_query/pop_query, and collect dependencies
        let rt = db.runtime();
        match self.query_fn {
            QueryFunction::Direct(query_fn) => {
                rt.push_query(DbIndex::new(self.table_index, key.index()));
                let value = (query_fn)(db, key.clone());
                let deps = rt.pop_query();
                // Store the new value
                self.store(db, key, value, deps, prev_memo)
            }
            QueryFunction::Indirect(indirect_query_fn) => {
                rt.push_query(DbIndex::new(self.table_index, key.index()));
                (indirect_query_fn)(db, key.clone());
                let _ = rt.pop_query();
                let memo = self.get_memo(key).expect("value not set after query");
                (&memo.value, memo.changed_at.get())
            }
        }
    }

    fn store<'a>(
        &'a self,
        db: &DB,
        key: K,
        value: V,
        dependencies: HashSet<DbIndex>,
        prev_memo: Option<&'a Memo<K, V>>,
    ) -> (&'a V, Revision) {
        let current_rev = db.current_revision();
        if let Some(memo) = prev_memo {
            if memo.value == value {
                trace!("value did not change");
                // value hasn't changed, just update the current rev
                // the dependencies might have changed though
                memo.verified.set(current_rev);
                memo.dependencies.replace(dependencies);
                // changed_at hasn't changed
                (&memo.value, memo.changed_at.get())
            } else {
                // value has changed, create a new memo
                let inserted = self
                    .new_memos
                    .insert(key, Memo::new(key, value, current_rev, dependencies));
                if !inserted {
                    // TODO more actionable error message
                    eprintln!(
                        "A memo for a value has already been inserted but the value has changed in the same revision"
                    );
                }
                let new_memo = self.new_memos.get(&key).unwrap();
                (&new_memo.value, current_rev)
            }
        } else {
            // create a new memo directly in the main map
            let inserted = self.memos.insert(key, Memo::new(key, value, current_rev, dependencies));
            assert!(inserted);
            let memo = self.memos.get(&key).unwrap();
            (&memo.value, current_rev)
        }
    }

    pub fn fetch(&self, db: &DB, key: K) -> &V {
        db.runtime().add_dependency(DbIndex::new(self.table_index, key.index()));

        let memo = self.get_memo(key);
        let rev = db.current_revision();

        // Do we have a memoized value for the key?
        if let Some(memo) = memo {
            if memo.verified.get() == rev {
                &memo.value
            } else {
                // same logic as maybe_changed_after
                let mut has_changed = false;
                {
                    let deps = memo.dependencies.borrow();
                    for dep in deps.iter() {
                        has_changed |= db.maybe_changed_after(*dep, memo.verified.get());
                    }
                }
                memo.verified.set(rev);

                if !has_changed {
                    // inputs haven't changed since last verification,
                    // the value is still valid
                    &memo.value
                } else {
                    // inputs have changed, the value of this query might have as well
                    let (new_value, _) = self.compute(db, key, Some(memo));
                    new_value
                }
            }
        } else {
            let (new_value, _) = self.compute(db, key, None);
            new_value
        }
    }

    /// Explicitly specifies the value of the query for the given key.
    pub fn specify(&self, db: &DB, key: K, value: V) -> &V {
        // the value will depend on the current query
        let deps = db.runtime().current_dependencies();
        let (value, _) = self.store(db, key, value, deps, self.get_memo(key));
        value
    }
}

impl<K, V, DB> TableOps<DB> for DerivedQueryTable<K, V, DB>
where
    K: AsIndex,
    V: PartialEq + Clone,
    DB: ?Sized + Database,
{
    fn maybe_changed_after(&self, db: &DB, index: Index, rev: Revision) -> bool {
        let cur_rev = db.current_revision();
        let key = K::from_index(index);

        // get the last change revision
        let changed_at = if let Some(memo) = self.get_memo(key) {
            // we have a memo
            if memo.verified.get() == cur_rev {
                // memo verified at current revision
                memo.changed_at.get()
            } else {
                // verify memo
                let mut has_changed = false;

                // introduce a scope so that we don't borrow the deps for too long
                {
                    let deps = memo.dependencies.borrow();
                    for dep in deps.iter() {
                        has_changed |= db.maybe_changed_after(*dep, memo.verified.get());
                    }
                    memo.verified.set(rev);
                }

                if !has_changed {
                    // inputs haven't changed since last verification,
                    // the value is still valid
                    memo.changed_at.get()
                } else {
                    // inputs have changed, the value of this query might have as well
                    let (_, changed_at) = self.compute(db, key, Some(memo));
                    changed_at
                }
            }
        } else {
            // No memo, compute the value
            let (_, changed_at) = self.compute(db, key, None);
            changed_at
        };

        changed_at > rev
    }

    fn on_new_revision(&mut self, _revision: Revision) {
        self.clear_temp_memos()
    }
}
