use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    ffi::c_void,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

/// A trait similar to `Any` but with an added lifetime bound, suitable for allocation of type-erased objects in
/// an arena.
///
/// The `'a` lifetime bound represents the borrow of the memory arena in which the object is allocated.
/// It can be safely implemented for types with at most one lifetime parameter.
/// Use `impl_arena_any` or the `ArenaAny` derive macro to implement this trait safely.
pub unsafe trait ArenaAny<'a>: 'a {
    /// Static function to return the static TypeId of this type.
    ///
    /// See `ArenaAny::type_id`
    fn static_type_id() -> std::any::TypeId
    where
        Self: Sized;

    /// Returns the static TypeId of the value.
    ///
    /// The "static TypeId" of a type is its TypeId with all lifetime parameters bound to `'static`.
    /// E.g. `TypeId::of::<Foo>` for a type with no generics, and `TypeId::of::<Foo<'static>>` for `Foo<'a>` for all `'a`.
    fn type_id(&self) -> std::any::TypeId;

    /// Returns a reference to self as a `dyn ArenaAny<'a>` trait object.
    fn as_any(&self) -> &dyn ArenaAny<'a>;
}

// XXX what about generics? given a type Attr<T>, and Inner<'a>(Cell<&'a i32>)
//
// Attr<Inner<'a>> where Inner<'a>: 'hir
// thus we can insert a Inner<'a>, with 'a: 'hir, cast it to Inner<'static>
//
// Attr<T> does not impl ArenaAny<'a> for all a
// impl<'a, T: 'a> ArenaAny<'a> for Attr<T> ?
// -> no: we have impl ArenaAny<'static> for Attr<T> where T: 'static, so
//
// U = Attr<Inner<'static>>: ArenaAny<'a>, because Inner<'static>: 'a for all 'a
// -> unsound
// -> can downcast to &Attr<Inner<'static>>, and get a &'static reference to something that is not static at all
//
// => Wouldn't be able to extract a static type anyway
// BUT: could add the ArenaAny<'a> bound on all types
//
// Idea: ArenaAny<'a> for Attr<T> where T: ArenaAny<'a> + WithoutLifetime, T::Static: <T bounds>
// problem: generating a static typeid for that
// may be an associated type T::Static
//
// Refactor:
// -> use Attribute IDs instead? But then can't store &str and such in the arena
// -> Arc<dyn Attribute>?


impl<'a> dyn ArenaAny<'a> {
    /// Tries to downcast a reference to a `dyn ArenaAny` object to a reference to a concrete type `T`.
    pub fn cast<T>(&self) -> Option<&T>
    where
        T: ArenaAny<'a>,
    {
        if self.type_id() == T::static_type_id() {
            unsafe { Some(&*(self as *const dyn ArenaAny as *const T)) }
        } else {
            None
        }
    }
}

/// Utility trait for downcasting `&dyn ArenaAny` values.
pub trait DowncastExt<'a> {
    /// Tries to downcast `self` to a reference to a concrete type `U`.
    ///
    /// Returns `None` if the object is not of the correct type.
    fn cast<'b, U>(&'b self) -> Option<&'b U>
    where
        U: ArenaAny<'a>,
        'a: 'b;
}

impl<'a, T> DowncastExt<'a> for T
where
    T: ?Sized + ArenaAny<'a>,
{
    fn cast<'b, U>(&'b self) -> Option<&'b U>
    where
        U: ArenaAny<'a>,
        'a: 'b,
    {
        self.as_any().cast()
    }
}

/// Macro to implement `ArenaAny` safely on a type.
///
/// It expects inputs of the form `Foo`, for types with no generics, or `Foo<'a>`, for a type with one lifetime.
/// Only types with at most one lifetime and no type- or const-generics are supported.
///
/// Alternatively, you can also use the `ArenaAny` proc-macro derive, which supports type and const generics.
#[macro_export]
macro_rules! impl_arena_any {
    // single lifetime: OK
    ($n:ident < $lt:lifetime >) => {
        unsafe impl<$lt> $crate::utils::ArenaAny<$lt> for $n<$lt> {
            fn static_type_id() -> ::std::any::TypeId {
                ::std::any::TypeId::of::<$n<'static>>()
            }

            fn type_id(&self) -> ::std::any::TypeId {
                ::std::any::TypeId::of::<$n<'static>>()
            }

            fn as_any(&self) -> &dyn $crate::utils::ArenaAny<$lt> {
                self
            }
        }
    };
    // no lifetime: OK
    ($n:ident) => {
        unsafe impl<'a> $crate::utils::ArenaAny<'a> for $n {
            fn static_type_id() -> ::std::any::TypeId {
                ::std::any::TypeId::of::<$n>()
            }

            fn type_id(&self) -> ::std::any::TypeId {
                ::std::any::TypeId::of::<$n>()
            }

            fn as_any(&self) -> &dyn $crate::utils::ArenaAny<'a> {
                self
            }
        }
    };
}

pub use impl_arena_any;

#[cfg(test)]
mod tests {
    use crate::utils::{ArenaAny, DowncastExt};
    use bumpalo::Bump;
    use std::{any::TypeId, cell::Cell, fmt::Debug, sync::Arc};

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
    fn test_arena() {
        let arena = Bump::new();

        let opaque_ty = arena.alloc(OpaqueType("sampler")) as &dyn Type;
        let i16_ty = arena.alloc(IntegerType(16)) as &dyn Type;
        let i32_ty = arena.alloc(IntegerType(32)) as &dyn Type;
        let mat4x4_ty = arena.alloc(MatrixType(1, 4, 4)) as &dyn Type;
        let i32_array_ty = arena.alloc(ArrayType(i32_ty, 64)) as &dyn Type;

        let _ = opaque_ty.cast::<OpaqueType>().unwrap();
        let _ = i32_array_ty.cast::<ArrayType>().unwrap();
    }

    /*#[test]
    fn interner() {
        let arena = Arena::new();
        let mut interner = Interner::new();
        let i16_ty = interner.intern(&arena, IntegerType(16));
        let same_i16_ty = interner.intern(&arena, IntegerType(16));
        let other_i16_ty = interner.intern(&arena, IntegerType(32));
        let opaque_type_a = interner.intern(&arena, OpaqueType("test"));
        let opaque_type_b = interner.intern(&arena, OpaqueType("test2"));
        let opaque_type_c = interner.intern(&arena, OpaqueType("test2"));

        assert_eq!(i16_ty.opaque_ptr(), same_i16_ty.opaque_ptr());
        assert_ne!(i16_ty.opaque_ptr(), other_i16_ty.opaque_ptr());
        assert_ne!(i16_ty.opaque_ptr(), opaque_type_a.opaque_ptr());
        assert_ne!(opaque_type_a.opaque_ptr(), opaque_type_b.opaque_ptr());
        assert_eq!(opaque_type_b.opaque_ptr(), opaque_type_c.opaque_ptr());
    }*/

    /*#[test]
    fn smuggler() {

        pub struct Smuggler<'a>(Arc<Cell<&'a str>>);
        unsafe impl<'a> ArenaAny<'a> for Smuggler<'a> {
            fn static_type_id() -> TypeId {
                TypeId::of::<Smuggler<'static>>()
            }
        }

        // cell with &'static str
        let string = String::from("hey");
        let peekaboo /*: Arc<Cell<&'static str>>*/ = Arc::new(Cell::new(string.as_str()));

        {
            let arena = Arena::new();
            let smuggler = arena.allocate_any(Smuggler(peekaboo.clone()));
            let original = smuggler.cast::<Smuggler>().unwrap();
            let tmp_str = arena.alloc_str("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
            original.0.set(tmp_str);
        }

        // observe invalid value
        dbg!(peekaboo.get());

    }*/

    /*#[test]
    fn smuggler2() {

        let expired = {
            let string = String::from("heyaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
            let arena = Arena::new();
            let smuggler = arena.allocate_any(Wtf("hey", &string));
            let original = smuggler.cast::<Wtf<'_, 'static>>().unwrap();
            //let tmp_str = arena.alloc_str("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
            original.1
        };

        // observe invalid value
        dbg!(expired);
    }*/
}
