use id_arena::ArenaBehavior;
use std::{fmt, marker::PhantomData, ops::Index};

pub mod interner;
mod memory_layout;
mod typed_vec;

pub use ashley_derive::MemoryLayout;
pub use memory_layout::{MemoryLayout, Std140Float, Std140IVec4, Std140Int, Std140Vec4};
//pub(crate) use interner::UniqueArena;
pub(crate) use typed_vec::{Id, TypedIndexMap, TypedIndexSet, TypedVec, TypedVecMap};

///
pub(crate) fn round_up(value: u32, multiple: u32) -> u32 {
    if multiple == 0 {
        return value;
    }
    let remainder = value % multiple;
    if remainder == 0 {
        return value;
    }
    value + multiple - remainder
}

/// Helper macro to write a comma-separated list.
// implemented as a macro to avoid borrowing woes
macro_rules! write_list {
    ($w:expr, $i:ident in $coll:expr => $b:block) => {
        let mut first = true;
        for $i in $coll {
            if !first {
                write!($w, ",").unwrap();
            }
            $b
            first = false;
        }
    };
}

pub(crate) use write_list;

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
// TODO: the API is shit, needs T: Default+Clone, only used once, needs mut access for reading
// TODO: don't make it public?
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

pub struct DisplayCommaSeparated<'a, T>(pub &'a [T]);

// fmt::Display for DisplayCommaSeparated<'a, T>
impl<'a, T: fmt::Display> fmt::Display for DisplayCommaSeparated<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }
        Ok(())
    }
}
