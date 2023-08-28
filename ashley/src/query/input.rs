use crate::{
    query::{
        database::{Database, DatabaseImpl},
        DbKey, Revision,
    },
    utils::{Idx, IndexMap, IndexVec},
};
use ashley::query::TableOps;
use std::{cell::Cell, collections::HashSet, marker::PhantomData};

/*/// Operations on input queries
pub trait InputQueryStorageOps<Q: Query> {
    /// Creates a new "input row" with the specified value.
    ///
    /// It's common that input data has multiple "fields" to set. In this case,
    /// you should have one input query on which you'd call `new_input` (the "primary" query),
    /// which will produce the input key. You'd then set the other fields on "secondary" input queries,
    /// using `set` with the key returned by the primary input query.
    fn new_input(&self, value: Q::Output) -> Q::Key;

    /// Sets the value of an existing "input row".
    fn set(&self, key: Q::Key, value: Q::Output);
}*/

/// Represents an input to the query system.
///
/// The purpose of this struct is just to generate unique IDs for the inputs.
/// It doesn't contain any input data, which is stored in secondary tables using the generated IDs as keys.
pub struct InputIndex<I> {
    table_index: usize,
    counter: Cell<usize>,
    _phantom: PhantomData<fn() -> (I)>,
}

impl<I> InputIndex<I>
where
    I: Idx,
{
    pub fn new(db: &mut dyn Database) -> InputIndex<I> {
        InputIndex {
            table_index: db.next_table_index(),
            counter: Cell::new(0),
            _phantom: Default::default(),
        }
    }

    pub fn new_input(&self) -> I {
        let mut r = self.counter.get();
        r += 1;
        self.counter.set(r);
        I::from_index(r)
    }
}

impl<I, DB: Database> TableOps<DB> for InputIndex<I> {
    fn table_index(&self) -> usize {
        self.table_index
    }

    fn maybe_changed_after(&self, _db: &DB, _index: usize, _rev: Revision) -> bool {
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
    table_index: usize,
    data: IndexVec<InputValue<V>, K>,
}

impl<K, V> InputTable<K, V>
where
    K: Idx,
    V: Eq + Default + Clone,
{
    pub fn new(db: &mut dyn Database) -> InputTable<K, V> {
        InputTable {
            table_index: db.next_table_index(),
            data: IndexVec::new(),
        }
    }
}

impl<K, V> InputTable<K, V>
where
    K: Idx,
    V: Eq + Default + Clone,
{
    pub fn set(&mut self, rev: Revision, key: K, value: V) {
        // ensure that there's enough space in the vector,
        // initializing with the default value of the query result type
        let index = key.index();
        self.data.resize(index + 1, Default::default());

        let same_value = self.data[key].value == value;

        if same_value {
            // same value as last time, don't update changed_at
        } else {
            // value has changed, update
            self.data[key].value = value;
            self.data[key].changed_at = rev;
        }
    }

    pub fn fetch<DB: Database>(&self, db: &DB, key: K) -> &V {
        db.runtime().add_dependency(DbKey::new(self.table_index, key.index()));
        &self.data[key].value
    }
}

impl<K, V, DB> TableOps<DB> for InputTable<K, V>
where
    K: Idx,
    DB: Database,
{
    fn table_index(&self) -> usize {
        self.table_index
    }

    fn maybe_changed_after(&self, _db: &DB, index: usize, rev: Revision) -> bool {
        self.data[K::from_index(index)].changed_at > rev
    }

    fn new_revision(&mut self) {}
}

////////////////////////////////////////////////////////////////////////////////////////////////////
