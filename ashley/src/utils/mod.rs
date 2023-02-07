use id_arena::ArenaBehavior;
use std::{marker::PhantomData, ops::Index};

pub mod interner;
mod typed_vec;
pub use typed_vec::{Id, TypedIndexMap, TypedIndexSet, TypedVec};

/// Defines arena ID types.
macro_rules! id_types {
    ($($(#[$m:meta])* $v:vis struct $n:ident;)*) => {
        $(
        $(#[$m])*
        #[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
        #[repr(transparent)]
        $v struct $n(NonZeroU32);

        impl $n {
            $v fn index(&self) -> usize {
                (self.0.get() - 1) as usize
            }

            $v fn from_index(index: usize) -> $n {
                $n(unsafe { NonZeroU32::new_unchecked((index+1) as u32) })
            }
        }

        impl id_arena::ArenaBehavior for $n {
            type Id = Self;

            fn new_id(_arena_id: u32, index: usize) -> Self::Id {
                $n(unsafe { NonZeroU32::new_unchecked((index+1) as u32) })
            }

            fn index(id: Self::Id) -> usize {
                (id.0.get() - 1) as usize
            }

            fn arena_id(_: Self::Id) -> u32 {
                0
            }

            fn new_arena_id() -> u32 {
                0
            }
        }
        )*
    };
}

pub(crate) use id_types;

/// A "side-map" for items allocated in an IdArena.
/// TODO: the API is shit, needs T: Default+Clone, only used once, needs mut access for reading
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
