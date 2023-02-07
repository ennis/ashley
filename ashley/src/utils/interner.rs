use id_arena::ArenaBehavior;
use indexmap::IndexSet;
use std::{borrow::Borrow, hash::Hash, marker::PhantomData, ops::Index};

/// Associates unique handles to equal values.
pub struct UniqueArena<T, Id> {
    items: IndexSet<T>,
    _phantom: PhantomData<fn() -> Id>,
}

impl<T: Eq + Hash, Id: Clone + ArenaBehavior> UniqueArena<T, Id> {
    pub fn new() -> UniqueArena<T, Id> {
        UniqueArena {
            items: Default::default(),
            _phantom: PhantomData,
        }
    }

    /*/// Returns whether this interner contains the specified value.
    pub fn get_handle(&self, value: &T) -> Option<H> {
        self.items.borrow().get(value).cloned()
    }*/

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id::Id, &'_ T)> {
        self.items.iter().enumerate().map(|(i, e)| (Id::new_id(0, i), e))
    }

    pub fn get_index_of<U>(&self, value: &U) -> Option<Id::Id>
    where
        T: Borrow<U>,
        U: Eq + Hash + ?Sized,
    {
        self.items.get_index_of(value).map(|e| Id::new_id(0, e))
    }

    pub fn insert(&mut self, value: T) -> (Id::Id, bool) {
        let (index, inserted) = self.items.insert_full(value);
        (Id::new_id(0, index), inserted)
    }
}

impl<T, Id: ArenaBehavior> Index<Id::Id> for UniqueArena<T, Id> {
    type Output = T;

    fn index(&self, index: Id::Id) -> &Self::Output {
        self.items.get_index(Id::index(index)).unwrap()
    }
}

#[cfg(test)]
mod tests {}
