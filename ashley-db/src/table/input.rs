use crate::{AsIndex, Database, DbIndex, Idx, Index, Revision, TableIndex, TableOps};
use ashley_data_structures::IndexVec;
use std::{cell::Cell, marker::PhantomData, mem};

/// Represents an input to the query system.
///
/// The purpose of this struct is just to generate unique IDs for the inputs.
/// It doesn't contain any input data, which is stored in secondary tables using the generated IDs as keys.
pub struct InputIndex<I> {
    table_index: TableIndex,
    counter: Cell<usize>,
    _phantom: PhantomData<fn() -> I>,
}

impl<I> InputIndex<I>
where
    I: Idx,
{
    pub fn new(table_index: TableIndex) -> InputIndex<I> {
        InputIndex {
            table_index,
            counter: Cell::new(0),
            _phantom: Default::default(),
        }
    }

    // TODO rename this (`generate_id`? `next_index`?).
    pub fn new_input(&self) -> I {
        let mut r = self.counter.get();
        r += 1;
        self.counter.set(r);
        I::from_usize(r)
    }
}

impl<I, DB> TableOps<DB> for InputIndex<I>
where
    DB: ?Sized + Database,
{
    fn maybe_changed_after(&self, _db: &DB, _index: Index, _rev: Revision) -> bool {
        false
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Default)]
struct InputValue<T> {
    changed_at: Revision,
    value: T,
}

/// Stores inputs to the query system.
pub struct InputTable<K, V> {
    table_index: TableIndex,
    data: IndexVec<InputValue<V>, Index>,
    _phantom: PhantomData<fn() -> K>,
}

impl<K, V> InputTable<K, V>
where
    K: AsIndex,
    V: Eq + Default + Clone,
{
    pub fn new(table_index: TableIndex) -> InputTable<K, V> {
        InputTable {
            table_index,
            data: IndexVec::new(),
            _phantom: PhantomData,
        }
    }
}

impl<K, V> InputTable<K, V>
where
    K: AsIndex,
    V: Eq + Default + Clone,
{
    pub fn set(&mut self, rev: Revision, key: K, value: V) {
        // ensure that there's enough space in the vector,
        // initializing with the default value of the query result type
        let index = key.index();
        self.data.resize(index.to_usize() + 1, Default::default());
        if self.data[index].value != value {
            // value has changed, update changed_at
            self.data[index].value = value;
            self.data[index].changed_at = rev;
        }
    }

    pub fn update(&mut self, rev: Revision, key: K, f: impl FnOnce(&mut V)) {
        let index = key.index();
        self.data.resize(index.to_usize() + 1, Default::default());
        let d = &mut self.data[index];
        let prev_value = d.value.clone();
        f(&mut d.value);
        if d.value != prev_value {
            d.changed_at = rev;
        }
    }

    pub fn fetch<DB: ?Sized + Database>(&self, db: &DB, key: K) -> &V {
        db.runtime().add_dependency(DbIndex::new(self.table_index, key.index()));
        &self.data[key.index()].value
    }
}

impl<K, V, DB> TableOps<DB> for InputTable<K, V>
where
    K: AsIndex,
    DB: ?Sized + Database,
{
    fn maybe_changed_after(&self, _db: &DB, index: Index, rev: Revision) -> bool {
        self.data[index].changed_at > rev
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
