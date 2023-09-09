//! Interned keys
use crate::{AsIndex, Database, Idx, Index, Revision, TableIndex, TableOps};
use dashmap::DashMap;
use memo_map::MemoMap;
use std::{
    hash::Hash,
    sync::atomic::{AtomicU32, AtomicUsize, Ordering},
};

/// Q::Output must be Idx.
pub struct InternTable<K, I> {
    table_index: TableIndex,
    indexes: MemoMap<K, I>,
    index_to_key: DashMap<I, K>,
    counter: AtomicU32,
}

impl<K, I> InternTable<K, I>
where
    K: Clone + Eq + Hash,
    I: AsIndex,
{
    pub fn new(table_index: TableIndex) -> InternTable<K, I> {
        InternTable {
            table_index,
            indexes: MemoMap::new(),
            index_to_key: DashMap::new(),
            counter: Default::default(),
        }
    }

    pub fn intern(&self, key: K) -> I {
        self.indexes
            .get_or_insert(&key, || {
                let index = I::from_index(Index::from_u32(self.counter.fetch_add(1, Ordering::Relaxed)));
                self.index_to_key.insert(index, key.clone());
                index
            })
            .clone()
    }

    pub fn fetch(&self, index: I) -> K {
        self.index_to_key.get(&index).unwrap().clone()
    }
}

impl<K, I, DB> TableOps<DB> for InternTable<K, I>
where
    DB: ?Sized + Database,
{
    fn maybe_changed_after(&self, _db: &DB, _index: Index, _rev: Revision) -> bool {
        // interned values never change
        false
    }
}
