//! Interner that works with `ArenaAny` instances.

use crate::utils::ArenaAny;
use bumpalo::Bump;
use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
    ptr,
};
use std::borrow::Borrow;

pub trait DynEq<'a> {
    fn any_eq(&self, other: &dyn ArenaAny<'a>) -> bool;
}

pub trait DynHash {
    fn hash(&self, state: &mut dyn Hasher);
}

impl<'a, T> DynEq<'a> for T
where
    T: ArenaAny<'a> + Eq,
{
    fn any_eq(&self, other: &dyn ArenaAny<'a>) -> bool {
        if let Some(other) = other.cast::<T>() {
            self == other
        } else {
            false
        }
    }
}

impl<'a, T> DynHash for T
where
    T: Hash,
{
    fn hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state)
    }
}

trait Internable<'a>: ArenaAny<'a> + DynEq<'a> + DynHash {}
impl<'a, T> Internable<'a> for T where T: ArenaAny<'a> + DynEq<'a> + DynHash {}

impl<'a> PartialEq for dyn Internable<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.any_eq(other.as_any())
    }
}

impl<'a> Eq for dyn Internable<'a> {}

impl<'a> Hash for dyn Internable<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        DynHash::hash(self, state)
    }
}

//--------------------------------------------------------------------------------------------------

struct InternKey<'a>(&'a dyn Internable<'a>);

impl<'a> PartialEq for InternKey<'a> {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.0, &other.0)
    }
}

impl<'a> Eq for InternKey<'a> {}

impl<'a> Hash for InternKey<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.0, state)
    }
}

pub struct Interner<'a> {
    items: HashSet<InternKey<'a>>,
}

impl<'a> Borrow<dyn Internable<'a>> for InternKey<'a> {
    fn borrow(&self) -> &dyn Internable<'a> {
        self.0
    }
}

impl<'a> Interner<'a> {
    pub fn new() -> Interner<'a> {
        Interner {
            items: Default::default(),
        }
    }

    pub fn contains<T>(&mut self, value: &T) -> bool
    where
        T: ArenaAny<'a> + Eq + Hash
    {
        self.items.contains(value as &dyn Internable<'a>)
    }

    pub fn intern<T>(&mut self, arena: &'a Bump, value: T) -> &'a T
        where
            T: ArenaAny<'a> + Eq + Hash
    {
        if let Some(value) = self.items.get(&value as &dyn Internable<'a>) {
            return value.0.as_any().cast::<T>().unwrap();
        }

        let wrap = arena.alloc((0u8, value));
        self.items.insert(InternKey(&wrap.1));
        &wrap.1
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::ptr;
    use crate::utils::intern::{Interner};
    use bumpalo::Bump;
    use crate::impl_arena_any;
    use crate::utils::ArenaAny;

    trait Type<'a>: ArenaAny<'a> + Debug {}

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub struct OpaqueType<'a>(&'a str);
    impl_arena_any!(OpaqueType<'a>);
    impl<'a> Type<'a> for OpaqueType<'a> {}

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub struct IntegerType(u32);
    impl_arena_any!(IntegerType);
    impl<'a> Type<'a> for IntegerType {}

    #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
    pub struct MatrixType(u8, u8, u8);
    impl_arena_any!(MatrixType);
    impl<'a> Type<'a> for MatrixType {}

    #[derive(Copy, Clone, Debug)]
    pub struct ArrayType<'a>(&'a dyn Type<'a>, usize);
    impl_arena_any!(ArrayType<'a>);
    impl<'a> Type<'a> for ArrayType<'a> {}


    #[test]
    fn interner() {
        let arena = Bump::new();
        let mut interner = Interner::new();

        let a1 = interner.intern(&arena, OpaqueType("hello"));
        let a2 = interner.intern(&arena, OpaqueType("hello"));
        let a3 = interner.intern(&arena, IntegerType(16));
        let a4 = interner.intern(&arena, IntegerType(32));

        assert!(ptr::eq(a1, a2));
        assert!(!ptr::eq(a3, a4));

        // upcast
        let a1 : &dyn Type = a1;
    }
}
