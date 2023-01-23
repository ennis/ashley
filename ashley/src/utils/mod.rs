use id_arena::ArenaBehavior;
use std::{
    marker::PhantomData,
    ops::{Index, IndexMut},
};

pub mod interner;

/// A "side-map" for items allocated in an IdArena.
pub struct IdMap<Id, T> {
    map: Vec<T>,
    _phantom: PhantomData<fn() -> Id>,
}

impl<Id, T> IdMap<Id, T> {
    pub fn new() -> IdMap<Id, T> {
        IdMap {
            map: vec![],
            _phantom: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> IdMap<Id, T> {
        IdMap {
            map: Vec::with_capacity(capacity),
            _phantom: PhantomData,
        }
    }
}

impl<Id: ArenaBehavior, T: Default + Clone> IdMap<Id, T> {
    fn ensure(&mut self, index: usize) {
        if self.map.len() < index + 1 {
            self.map.resize(index + 1, T::default());
        }
    }
}

impl<Id: ArenaBehavior, T> Index<Id::Id> for IdMap<Id, T> {
    type Output = T;

    fn index(&self, index: Id::Id) -> &Self::Output {
        &self.map[Id::index(index)]
    }
}

impl<Id: ArenaBehavior, T: Default + Clone> IndexMut<Id::Id> for IdMap<Id, T> {
    fn index_mut(&mut self, index: Id::Id) -> &mut Self::Output {
        let index = Id::index(index);
        self.ensure(index);
        &mut self.map[index]
    }
}
