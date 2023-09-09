use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};

/// A trait for a type that is equivalent to an index.
pub trait Idx: Copy + Clone + Eq + PartialEq + Hash + Ord + PartialOrd {
    fn to_usize(&self) -> usize;
    fn from_usize(index: usize) -> Self;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

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

impl<T> Idx for Id<T> {
    fn to_usize(&self) -> usize {
        self.index()
    }

    fn from_usize(index: usize) -> Self {
        Self::from_index(index)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Creates a new index type (u32-sized) that implements `Idx`,
/// and is usable as an index for `IndexVec`.
///
/// It also derives `From<u32>` and `From<usize>` implementations.
/// It also implements `Default`, which returns index 0.
///
/// # Example
///```
///new_index! {
///    pub struct MyIndex;    // Implements Idx
///}
///```
#[macro_export]
macro_rules! new_index {
    ($(#[$m:meta])* $v:vis struct $name:ident;) => {
        #[derive(Default, Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
        #[repr(transparent)]
        $(#[$m])* $v struct $name(pub u32);

        impl $crate::Idx for $name {
            fn to_usize(&self) -> usize {
                self.0 as usize
            }
            fn from_usize(index: usize) -> Self {
                $name(index as u32)
            }
        }

        impl From<usize> for $name {
            fn from(v: usize) -> $name {
                <$name as $crate::Idx>::from_usize(v)
            }
        }

        impl From<u32> for $name {
            fn from(v: u32) -> $name {
                <$name as $crate::Idx>::from_usize(v as usize)
            }
        }
    };
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Vector with strongly-typed indices.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IndexVec<T, I = Id<T>> {
    items: Vec<T>,
    _phantom: PhantomData<fn() -> I>,
}

impl<T, I> Default for IndexVec<T, I> {
    fn default() -> Self {
        IndexVec::new()
    }
}

impl<T, I> IndexVec<T, I> {
    /// Creates a new empty vector.
    pub fn new() -> IndexVec<T, I> {
        IndexVec {
            items: vec![],
            _phantom: PhantomData,
        }
    }
}

impl<T, I: Idx> IndexVec<T, I> {
    /// Inserts an item and returns its ID.
    pub fn push(&mut self, item: T) -> I {
        self.items.push(item);
        I::from_usize(self.items.len() - 1)
    }

    /// Resizes the vector.
    pub fn resize(&mut self, new_len: usize, value: T)
    where
        T: Clone,
    {
        self.items.resize(new_len, value);
    }

    /// Returns the index of the next item to be inserted.
    pub fn next_id(&self) -> I {
        I::from_usize(self.items.len())
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
        self.items.iter().enumerate().map(|(i, v)| (I::from_usize(i), v))
    }

    /// Length of the vector.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Returns the element at the given index.
    pub fn get(&self, id: I) -> Option<&T> {
        self.items.get(id.to_usize())
    }

    /// Returns a mutable reference to the element at the given index.
    pub fn get_mut(&mut self, id: I) -> Option<&mut T> {
        self.items.get_mut(id.to_usize())
    }
}

impl<T, I: Idx> Index<I> for IndexVec<T, I> {
    type Output = T;

    fn index(&self, id: I) -> &Self::Output {
        &self.items[id.to_usize()]
    }
}

impl<T, I: Idx> IndexMut<I> for IndexVec<T, I> {
    fn index_mut(&mut self, id: I) -> &mut Self::Output {
        &mut self.items[id.to_usize()]
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Flat map using strongly-typed indices as keys.
///
/// The map is backed by a vector.
pub struct IndexVecMap<I, V> {
    items: Vec<Option<V>>,
    _phantom: PhantomData<fn() -> I>,
}

impl<I, V> IndexVecMap<I, V> {
    pub fn new() -> IndexVecMap<I, V> {
        IndexVecMap {
            items: vec![],
            _phantom: PhantomData,
        }
    }
}

impl<I: Idx, V> IndexVecMap<I, V> {
    /// Inserts an element in the map.
    ///
    /// Returns the previous value if any.
    pub fn insert(&mut self, id: I, item: V) -> Option<V> {
        let index = id.to_usize();
        if index >= self.items.len() {
            self.items.resize_with(index + 1, || None);
        }
        mem::replace(&mut self.items[index], Some(item))
    }

    /// Removes an element from the map.
    pub fn remove(&mut self, id: I) -> Option<V> {
        let index = id.to_usize();
        if index < self.items.len() {
            self.items[index].take()
        } else {
            None
        }
    }

    /// Returns the element at the given index.
    pub fn get(&self, id: I) -> Option<&V> {
        let index = id.to_usize();
        if index < self.items.len() {
            self.items[index].as_ref()
        } else {
            None
        }
    }

    /// Returns a mutable reference to the element at the given index.
    pub fn get_mut(&mut self, id: I) -> Option<&mut V> {
        let index = id.to_usize();
        if index < self.items.len() {
            self.items[index].as_mut()
        } else {
            None
        }
    }

    /// Returns an iterator over the items.
    pub fn iter(&self) -> impl Iterator<Item = (I, &V)> + '_ {
        self.items
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|v| (I::from_usize(i), v)))
    }

    /// Returns a mutable iterator over the items.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (I, &mut V)> + '_ {
        self.items
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| v.as_mut().map(|v| (I::from_usize(i), v)))
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/*
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
}*/

////////////////////////////////////////////////////////////////////////////////////////////////////

/// A wrapper over an `indexmap::IndexMap`, except with a strongly-typed index type `I`.
#[derive(Debug)]
pub struct IndexMap<K, V, I: Idx> {
    map: indexmap::IndexMap<K, V>,
    _phantom: PhantomData<fn() -> I>,
}

impl<K, V, I: Idx> IndexMap<K, V, I> {
    pub fn new() -> IndexMap<K, V, I> {
        IndexMap {
            map: indexmap::IndexMap::new(),
            _phantom: Default::default(),
        }
    }
}

impl<K, V, I> IndexMap<K, V, I>
where
    I: Idx,
    K: Hash + Eq,
{
    pub fn insert_full(&mut self, key: K, item: V) -> (Id<V>, Option<V>) {
        let (index, prev) = self.map.insert_full(key, item);
        (Id::from_index(index), prev)
    }
}

impl<K, V, I: Idx> IndexMap<K, V, I> {
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        Q: Eq + Hash + ?Sized,
        K: Borrow<Q> + Eq + Hash,
    {
        self.map.get(key)
    }

    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        Q: Eq + Hash + ?Sized,
        K: Borrow<Q> + Eq + Hash,
    {
        self.map.get_mut(key)
    }

    pub fn get_full<Q>(&self, key: &Q) -> Option<(I, &K, &V)>
    where
        K: Borrow<Q> + Eq + Hash,
        Q: Eq + Hash + ?Sized,
    {
        let (i, k, v) = self.map.get_full(key)?;
        Some((I::from_usize(i), k, v))
    }

    pub fn get_full_mut<Q>(&mut self, key: &Q) -> Option<(I, &K, &mut V)>
    where
        K: Borrow<Q> + Eq + Hash,
        Q: Eq + Hash + ?Sized,
    {
        let (i, k, v) = self.map.get_full_mut(key)?;
        Some((I::from_usize(i), k, v))
    }
}

/*/// Indexing with the key
impl<K, V, Q, I> Index<&Q> for IndexMap<K, V, I>
where
    I: Idx,
    K: Borrow<Q> + Eq + Hash,
    Q: Eq + Hash + ?Sized,
{
    type Output = V;

    fn index(&self, key: &Q) -> &V {
        self.map.get(key).expect("IndexMap: key not found")
    }
}

/// Indexing with the key
impl<K, V, Q, I> IndexMut<&Q> for IndexMap<K, V, I>
where
    I: Idx,
    K: Borrow<Q> + Eq + Hash,
    Q: Eq + Hash + ?Sized,
{
    fn index_mut(&mut self, key: &Q) -> &mut V {
        self.map.get_mut(key).expect("IndexMap: key not found")
    }
}*/

/// Indexing with the index
impl<K, V, I: Idx> Index<I> for IndexMap<K, V, I> {
    type Output = V;

    fn index(&self, id: I) -> &V {
        self.map.get_index(id.to_usize()).unwrap().1
    }
}

/// Indexing with the index
impl<K, V, I: Idx> IndexMut<I> for IndexMap<K, V, I> {
    fn index_mut(&mut self, id: I) -> &mut V {
        self.map.get_index_mut(id.to_usize()).unwrap().1
    }
}
