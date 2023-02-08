use indexmap::{IndexMap, IndexSet};
use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

/// A trait for a type that can be used to index a TypedVec.
pub trait TypedIndex: Copy + Clone + Eq + PartialEq + Hash + Ord + PartialOrd {
    fn index(&self) -> usize;
    fn from_index(index: usize) -> Self;
}

/// Strongly-typed ID.
#[repr(transparent)]
pub struct Id<T>(pub(crate) NonZeroU32, PhantomData<fn() -> T>);

impl<T> Id<T> {
    pub fn index(&self) -> usize {
        (self.0.get() - 1) as usize
    }

    pub fn dummy() -> Id<T> {
        unsafe { Id(NonZeroU32::new_unchecked(u32::MAX), PhantomData) }
    }

    pub fn from_index(index: usize) -> Id<T> {
        unsafe { Id(NonZeroU32::new_unchecked((index + 1) as u32), PhantomData) }
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id(self.0, PhantomData)
    }
}

impl<T> Copy for Id<T> {}

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T> TypedIndex for Id<T> {
    fn index(&self) -> usize {
        self.index()
    }

    fn from_index(index: usize) -> Self {
        Self::from_index(index)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Vector with strongly-typed indices.
#[derive(Debug)]
pub struct TypedVec<T, I = Id<T>> {
    items: Vec<T>,
    _phantom: PhantomData<fn() -> I>,
}

impl<T, I> Default for TypedVec<T, I> {
    fn default() -> Self {
        TypedVec::new()
    }
}

impl<T, I> TypedVec<T, I> {
    /// Creates a new empty vector.
    pub fn new() -> TypedVec<T, I> {
        TypedVec {
            items: vec![],
            _phantom: PhantomData,
        }
    }
}

impl<T, I: TypedIndex> TypedVec<T, I> {
    /// Inserts an item and returns its ID.
    pub fn push(&mut self, item: T) -> I {
        self.items.push(item);
        unsafe { I::from_index(self.items.len() - 1) }
    }

    /// Returns the index of the next item to be inserted.
    pub fn next_id(&self) -> I {
        unsafe { I::from_index(self.items.len()) }
    }

    /// Returns the last item in the vector.
    pub fn last(&self) -> Option<&T> {
        self.items.last()
    }

    /// Returns an iterator over the items.
    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.items.iter()
    }

    /// Returns an iterator over the items and their IDs.
    pub fn iter_full(&self) -> impl Iterator<Item = (I, &T)> + '_ {
        self.items.iter().enumerate().map(|(i, v)| (I::from_index(i), v))
    }

    /// Length of the vector.
    pub fn len(&self) -> usize {
        self.items.len()
    }
}

impl<T, I: TypedIndex> Index<I> for TypedVec<T> {
    type Output = T;

    fn index(&self, id: I) -> &Self::Output {
        &self.items[id.index()]
    }
}

impl<T, I: TypedIndex> IndexMut<I> for TypedVec<T> {
    fn index_mut(&mut self, id: I) -> &mut Self::Output {
        &mut self.items[id.index()]
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

///
#[derive(Debug)]
pub struct TypedIndexSet<T> {
    set: IndexSet<T>,
}

impl<T: Hash + Eq> TypedIndexSet<T> {
    pub fn new() -> TypedIndexSet<T> {
        TypedIndexSet { set: IndexSet::new() }
    }

    pub fn add(&mut self, item: T) -> Id<T> {
        let index = self.set.insert_full(item).0;
        Id::from_index(index)
    }
}

impl<T> Index<Id<T>> for TypedIndexSet<T> {
    type Output = T;

    fn index(&self, id: Id<T>) -> &Self::Output {
        &self.set[id.index()]
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

///
#[derive(Debug)]
pub struct TypedIndexMap<K, V> {
    map: IndexMap<K, V>,
}

impl<K, V> TypedIndexMap<K, V> {
    pub fn new() -> TypedIndexMap<K, V> {
        TypedIndexMap { map: IndexMap::new() }
    }
}

impl<K, V> TypedIndexMap<K, V>
where
    K: Hash + Eq,
{
    pub fn insert_full(&mut self, key: K, item: V) -> (Id<V>, Option<V>) {
        let (index, prev) = self.map.insert_full(key, item);
        (Id::from_index(index), prev)
    }
}

/// Indexing with the key
impl<K, V, Q> Index<&Q> for TypedIndexMap<K, V>
where
    K: Borrow<Q> + Eq + Hash,
    Q: Eq + Hash + ?Sized,
{
    type Output = V;

    fn index(&self, key: &Q) -> &V {
        self.map.get(key).expect("IndexMap: key not found")
    }
}

/// Indexing with the key
impl<K, V, Q> IndexMut<&Q> for TypedIndexMap<K, V>
where
    K: Borrow<Q> + Eq + Hash,
    Q: Eq + Hash + ?Sized,
{
    fn index_mut(&mut self, key: &Q) -> &mut V {
        self.map.get_mut(key).expect("IndexMap: key not found")
    }
}

/// Indexing with the index
impl<K, V> Index<Id<V>> for TypedIndexMap<K, V> {
    type Output = V;

    fn index(&self, id: Id<V>) -> &V {
        self.map.get_index(id.index()).unwrap().1
    }
}

/// Indexing with the index
impl<K, V> IndexMut<Id<V>> for TypedIndexMap<K, V> {
    fn index_mut(&mut self, id: Id<V>) -> &mut V {
        self.map.get_index_mut(id.index()).unwrap().1
    }
}
