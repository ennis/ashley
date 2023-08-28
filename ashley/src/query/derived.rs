//! Derived queries.
//!
//! This is heavily inspired by salsa.
use crate::{
    query::{database::Database, DatabaseImpl, DbKey, Revision, TableOps},
    utils::Idx,
};
use dashmap::DashMap;
use memo_map::MemoMap;
use std::{
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
};

pub struct Memo<K, V> {
    key: K,
    value: V,
    changed_at: Cell<Revision>,
    verified: Cell<Revision>,
    /// List of dependencies.
    dependencies: RefCell<HashSet<DbKey>>,
}

impl<K, V> Memo<K, V> {
    fn new(key: K, value: V, rev: Revision, dependencies: HashSet<DbKey>) -> Memo<K, V> {
        Memo {
            key,
            value,
            changed_at: Cell::new(rev),
            verified: Cell::new(rev),
            dependencies: RefCell::new(dependencies),
        }
    }
}

pub struct DerivedQueryTable<K, V, DB> {
    table_index: usize,
    memos: MemoMap<usize, Memo<K, V>>,
    /// Temporary MemoMap for values that changed in the current evaluation cycle,
    /// because we can't update them in place.
    temp_memos: MemoMap<usize, Memo<K, V>>,
    query_fn: fn(&DB, &K) -> V,
}

impl<K, V, DB> DerivedQueryTable<K, V, DB>
where
    K: Idx,
    V: PartialEq,
    DB: Database,
{
    pub fn new(db: &mut dyn Database, query_fn: fn(&DB, &K) -> V) -> DerivedQueryTable<K, V, DB> {
        DerivedQueryTable {
            table_index: db.next_table_index(),
            memos: Default::default(),
            temp_memos: Default::default(),
            query_fn,
        }
    }

    fn get_memo(&self, index: usize) -> Option<&Memo<K, V>> {
        // check temp memos first
        self.temp_memos.get(&index).or_else(|| self.memos.get(&index))
    }

    fn clear_temp_memos(&mut self) {
        // TODO drain
        let keys = self.temp_memos.keys().cloned().collect::<Vec<_>>();
        for k in keys {
            self.memos.replace(k, self.temp_memos.remove(&k).unwrap());
        }
    }

    fn compute<'a>(&'a self, db: &DB, index: usize, prev_memo: Option<&'a Memo<K, V>>) -> (&'a V, Revision) {
        let current_rev = db.current_rev();
        let key = K::from_index(index);

        db.runtime().enter_query(DbKey::new(self.table_index, index));
        let value = (self.query_fn)(db, &key);
        let dependencies = db.runtime().exit_query();

        if let Some(memo) = prev_memo {
            if memo.value == value {
                // value hasn't changed, just update the current rev
                // the dependencies might have changed though
                memo.verified.set(current_rev);
                memo.dependencies.replace(dependencies);
                // changed_at hasn't changed
                (&memo.value, memo.changed_at.get())
            } else {
                // value has changed, create a new memo
                let inserted = self
                    .temp_memos
                    .insert(index, Memo::new(key, value, current_rev, dependencies));
                assert!(inserted);
                let new_memo = self.temp_memos.get(&index).unwrap();
                (&new_memo.value, current_rev)
            }
        } else {
            // create a new memo directly in the main map
            let inserted = self
                .temp_memos
                .insert(index, Memo::new(key, value, current_rev, dependencies));
            assert!(inserted);
            let memo = self.temp_memos.get(&index).unwrap();
            (&memo.value, current_rev)
        }
    }

    pub fn fetch(&self, db: &DB, key: K) -> &V {
        db.runtime().add_dependency(DbKey::new(self.table_index, key.index()));
        let index = key.index();
        let memo = self.get_memo(index);
        let rev = db.current_rev();

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
                    let (new_value, _) = self.compute(db, index, Some(memo));
                    new_value
                }
            }
        } else {
            let (new_value, _) = self.compute(db, index, None);
            new_value
        }
    }
}

impl<K, V, DB: Database> TableOps<DB> for DerivedQueryTable<K, V, DB>
where
    K: Idx,
    V: PartialEq,
{
    fn table_index(&self) -> usize {
        self.table_index
    }

    fn maybe_changed_after(&self, db: &DB, index: usize, rev: Revision) -> bool {
        let current_rev = db.current_rev();

        // get the last change revision
        let changed_at = if let Some(memo) = self.get_memo(index) {
            // we have a memo
            if memo.verified.get() == current_rev {
                // memo verified at current revision
                memo.changed_at.get()
            } else {
                // verify memo
                let mut has_changed = false;
                let deps = memo.dependencies.borrow();
                for dep in deps.iter() {
                    has_changed |= db.maybe_changed_after(*dep, memo.verified.get());
                }
                memo.verified.set(rev);

                if !has_changed {
                    // inputs haven't changed since last verification,
                    // the value is still valid
                    memo.changed_at.get()
                } else {
                    // inputs have changed, the value of this query might have as well
                    let (_, changed_at) = self.compute(db, index, Some(memo));
                    changed_at
                }
            }
        } else {
            // No memo, compute the value
            let (_, changed_at) = self.compute(db, index, None);
            changed_at
        };

        changed_at > rev
    }

    fn new_revision(&mut self) {
        self.clear_temp_memos()
    }
}

// Issue: going from DbKey.index to Q::Key
//
// This assumes that Q::Key is basically an index, but it might not be the case.
// E.g. some queries operate on DefId, which is PackageId+local index
//
// In salsa: keys that are not Ids are interned -> special query type
// db.def_id(package_id, index) -> DefId
// DefId becomes a regular ID
//
// Alternative:
//
//
// Example: recomputing definitions
// Arguments: Package ID
// Depedencies: Package Source (input)
//
// Iterate over AST items and create definitions.
// Evict old DefIds: delete all DefIds with the current package ID and an index > the number of AST items.
// For each created definition, update the `Definition` table:
// - remove all defs

// Compared to kyute-state: dep tracking is fine-grained. Query results depend on a precise set of
// rows in input tables, instead of being defined as joins, filters and maps on a set of input tables.
//
//

// Dependency keys
//
// Right now it's annoying that we have to maintain a map from Keys to global indices
// -> Require that keys are basically indices
// There can be a separate map from keys to indices elsewhere. E.g. the key can be a string, but it
// should be interned into an ID before being used as a query input.
//
// Q: what should be used as a key for definitions?
// 1. It should be stable across changes to the source code if the definition is not affected.
// 2. It must be unique for the definition (so we can't use only the name).
//
// A: (PackageId + Kind + Name) should be stable. There cannot be two definitions with the same name and type in the same program.

// Problem: when re-evaluating a query, we need to overwrite the result in the memo entry, but
// we don't have mut access at that point.
// Facts:
// - we can't create another memo since it would have another index/key
// - we can't overwrite the memo since there might be outstanding references to the value inside (ensured by MemoMap)
//
// Solutions:
// (1) create a new memo in a side-map that takes precedence over the main memomap. When mut access is regained, this map is merged with the main map, and cleared.
