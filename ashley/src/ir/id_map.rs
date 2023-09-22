use id_arena::ArenaBehavior;
use std::{marker::PhantomData, ops::Index};

/// A "side-map" for items allocated in an IdArena.
// TODO: the API is shit, needs T: Default+Clone, only used once, needs mut access for reading
pub struct IdMap<Id, T> {
    map: Vec<Option<T>>,
    _phantom: PhantomData<fn() -> Id>,
}

impl<Id: ArenaBehavior, T: Default + Clone> IdMap<Id, T> {
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

    pub fn get(&self, index: Id::Id) -> Option<&T> {
        self.map.get(Id::index(index))?.as_ref()
    }

    pub fn insert(&mut self, index: Id::Id, value: T) {
        let index = Id::index(index);
        self.ensure(index);
        self.map[index] = Some(value);
    }
}

impl<Id: ArenaBehavior, T: Default + Clone> IdMap<Id, T> {
    fn ensure(&mut self, index: usize) {
        if self.map.len() < index + 1 {
            self.map.resize(index + 1, Default::default());
        }
    }
}

impl<Id: ArenaBehavior, T> Index<Id::Id> for IdMap<Id, T> {
    type Output = T;

    fn index(&self, index: Id::Id) -> &Self::Output {
        self.map[Id::index(index)].as_ref().unwrap()
    }
}
