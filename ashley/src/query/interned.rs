//! Interned keys
use crate::{
    query::{Database, Revision, TableOps},
    utils::{Idx, IndexVec},
};
use dashmap::DashMap;
use memo_map::MemoMap;
use std::{
    collections::HashMap,
    hash::Hash,
    sync::atomic::{AtomicUsize, Ordering},
};

/// Q::Output must be Idx.
pub struct InternTable<K, I> {
    table_index: usize,
    indexes: MemoMap<K, I>,
    index_to_key: DashMap<usize, K>,
    counter: AtomicUsize,
}

impl<K, I> InternTable<K, I>
where
    K: Clone + Eq + Hash,
    I: Idx,
{
    pub fn new(db: &mut dyn Database) -> InternTable<K, I> {
        InternTable {
            table_index: db.next_table_index(),
            indexes: MemoMap::new(),
            index_to_key: DashMap::new(),
            counter: Default::default(),
        }
    }

    pub fn intern(&self, key: K) -> I {
        self.indexes
            .get_or_insert(&key, || I::from_index(self.counter.fetch_add(1, Ordering::Relaxed)))
            .clone()
    }
}

impl<K, I, DB> TableOps<DB> for InternTable<K, I>
where
    DB: Database,
{
    fn table_index(&self) -> usize {
        self.table_index
    }

    fn maybe_changed_after(&self, _db: &DB, _index: usize, _rev: Revision) -> bool {
        // interned values never change
        false
    }
}
