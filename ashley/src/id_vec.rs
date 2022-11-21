/*use indexmap::IndexSet;
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    num::NonZeroU32,
    ops::{Index, IndexMut},
};*/

/*
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Vector with strongly-typed indices.
#[derive(Debug)]
pub struct IdVec<I,T> {
    pub items: Vec<T>,
}

impl<T> IdVec<T> {
    pub fn new() -> IdVec<T> {
        IdVec { items: vec![] }
    }

    pub fn push(&mut self, item: T) -> Id<T> {
        let id = self.next_id();
        self.items.push(item);
        id
    }

    pub fn last(&self) -> Option<&T> {
        self.items.last()
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.items.iter()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn next_id(&self) -> Id<T> {
        unsafe {
            Id(
                NonZeroU32::new_unchecked(self.items.len() as u32 + 1),
                PhantomData,
            )
        }
    }
}

impl<T> Index<Id<T>> for IdVec<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        &self.items[(index.0.get() - 1) as usize]
    }
}

impl<T> IndexMut<Id<T>> for IdVec<T> {
    fn index_mut(&mut self, index: Id<T>) -> &mut Self::Output {
        &mut self.items[(index.0.get() - 1) as usize]
    }
}

#[derive(Debug)]
pub struct UniqueIdVec<T> {
    set: IndexSet<T>,
}

impl<T: Hash + Eq> UniqueIdVec<T> {
    pub fn new() -> UniqueIdVec<T> {
        UniqueIdVec {
            set: IndexSet::new(),
        }
    }

    pub fn add(&mut self, item: T) -> Id<T> {
        let index = self.set.insert_full(item).0;
        unsafe { Id(NonZeroU32::new_unchecked((index + 1) as u32), PhantomData) }
    }
}

impl<T> Index<Id<T>> for UniqueIdVec<T> {
    type Output = T;

    fn index(&self, index: Id<T>) -> &Self::Output {
        &self.set[(index.0.get() - 1) as usize]
    }
}


/*
#[repr(transparent)]
pub struct Id<T>(NonZeroU32, PhantomData<fn() -> T>);

impl<T> Id<T> {
    pub fn index(&self) -> usize {
        (self.0.get() - 1) as usize
    }

    pub fn dummy() -> Id<T> {
        unsafe { Id(NonZeroU32::new_unchecked(u32::MAX), PhantomData) }
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
}*/

////////////////////////////////////////////////////////////////////////////////////////////////////

/*
pub struct IdRange<T>(Id<T>, Id<T>);

impl<T> IdRange<T> {
    pub fn range(&self) -> Range<usize> {
        self.0.index()..self.1.index()
    }
}

impl<T> Clone for IdRange<T> {
    fn clone(&self) -> Self {
        IdRange(self.0, self.1)
    }
}

impl<T> Copy for IdRange<T> {}

impl<T> fmt::Debug for IdRange<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.0 .0, self.1 .0)
    }
}

impl<T> PartialEq for IdRange<T> {
    fn eq(&self, other: &Self) -> bool {
        (self.0 .0, self.1 .0) == (other.0 .0, other.1 .0)
    }
}

impl<T> Eq for IdRange<T> {}

impl<T> PartialOrd for IdRange<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.0 .0, self.1 .0).partial_cmp(&(other.0 .0, other.1 .0))
    }
}

impl<T> Ord for IdRange<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.0 .0, self.1 .0).cmp(&(other.0 .0, other.1 .0))
    }
}

impl<T> Hash for IdRange<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}*/
*/
