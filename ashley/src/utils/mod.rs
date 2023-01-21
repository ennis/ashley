use id_arena::ArenaBehavior;
use std::ops::{Index, IndexMut};

pub mod interner;

/// A "side-map" for items allocated in an IdArena.
pub struct IdMap<Id, T> {
    map: Vec<T>,
}

impl<Id, T> IdMap<Id, T> {
    pub fn new() -> IdMap<Id, T> {
        IdMap { map: vec![] }
    }

    pub fn with_capacity(capacity: usize) -> IdMap<Id, T> {
        IdMap {
            map: Vec::with_capacity(capacity),
        }
    }
}

impl<Id: ArenaBehavior, T: Default> IdMap<Id, T> {
    fn ensure(&mut self, index: usize) {
        if self.map.len() < index + 1 {
            self.map.resize(index + 1, T::default());
        }
    }
}

impl<Id: ArenaBehavior, T> Index<Id> for IdMap<Id, T> {
    type Output = T;

    fn index(&self, index: Id) -> &Self::Output {
        &self.map[ArenaBehavior::index(index)]
    }
}

impl<Id: ArenaBehavior, T: Default> IndexMut<Id> for IdMap<Id, T> {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        let index = ArenaBehavior::index(index);
        self.ensure(index);
        &mut self.map[index]
    }
}
