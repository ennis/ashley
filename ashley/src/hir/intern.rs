//! Interner for objects allocated in a `HirArena`.
use crate::{hir::HirArena, utils::ArenaAny};
use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
};
use indexmap::IndexSet;

/// Utility trait for object-safe equality comparison between `ArenaAny` objects.
pub trait DynEq<'a> {
    fn any_eq(&self, other: &dyn ArenaAny<'a>) -> bool;
}

/// Utility trait for object-safe hashing.
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

/// Objects that can be interned with `Interner`.
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

/// Interns values of arbitrary types allocated on an arena.
pub struct Interner<'a> {
    items: HashSet<&'a dyn Internable<'a>>,
}

impl<'a> Interner<'a> {
    pub fn new() -> Interner<'a> {
        Interner {
            items: Default::default(),
        }
    }

    /// Returns whether this interner contains the specified value.
    pub fn contains<T>(&mut self, value: &T) -> bool
    where
        T: ArenaAny<'a> + Eq + Hash,
    {
        self.items.contains(value as &dyn Internable<'a>)
    }

    /// Interns the specified value.
    ///
    /// Returns a reference to the interned object.
    ///
    /// # Arguments
    /// * arena the arena in which to allocate the object if necessary
    /// * value the value to intern
    ///
    /// # Note
    /// The `arena` must be the same for all calls to `intern` on this object.
    pub fn intern<T>(&mut self, arena: &'a HirArena, value: T) -> (&'a T, bool)
    where
        T: ArenaAny<'a> + Eq + Hash,
    {
        if let Some(value) = self.items.get(&value as &dyn Internable<'a>) {
            return (value.as_any().cast::<T>().unwrap(), false);
        }

        // the (u8, value) tuple is there to ensure that each interned value has a unique address, even
        // in the presence of ZSTs.
        let wrap = arena.0.alloc((0u8, value));
        self.items.insert(&wrap.1);
        (&wrap.1, true)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        hir::{intern::Interner, HirArena},
        impl_arena_any,
        utils::ArenaAny,
    };
    use std::{fmt::Debug, ptr};

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
        let arena = HirArena::new();
        let mut interner = Interner::new();

        let (a1,_) = interner.intern(&arena, OpaqueType("hello"));
        let (a2,_) = interner.intern(&arena, OpaqueType("hello"));
        let (a3,_) = interner.intern(&arena, IntegerType(16));
        let (a4,_) = interner.intern(&arena, IntegerType(32));

        assert!(ptr::eq(a1, a2));
        assert!(!ptr::eq(a3, a4));

        // upcast
        let a1: &dyn Type = a1;
    }
}
