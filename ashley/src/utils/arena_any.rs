use bumpalo::Bump;
use fxhash::FxHasher64;
use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    ffi::c_void,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

/// A trait similar to `Any` but with an added lifetime bound.
pub unsafe trait ArenaAny<'a>: 'a {
    fn static_type_id() -> std::any::TypeId
        where
            Self: Sized;
    fn type_id(&self) -> std::any::TypeId;
    fn as_any(&self) -> &dyn ArenaAny<'a>;
}

impl<'a> dyn ArenaAny<'a> {
    pub fn cast<T>(&self) -> Option<&T> where T: ArenaAny<'a> {
        if self.type_id() == T::static_type_id() {
            unsafe {
                Some(&*(self as *const dyn ArenaAny as *const T))
            }
        } else {
            None
        }
    }
}

pub trait DowncastExt<'a> {
    fn cast<'b, U>(&'b self) -> Option<&'b U> where U: ArenaAny<'a>, 'a: 'b;
}

impl<'a, T> DowncastExt<'a> for T where T: ?Sized + ArenaAny<'a> {
    fn cast<'b, U>(&'b self) -> Option<&'b U> where U: ArenaAny<'a>, 'a: 'b {
        self.as_any().cast()
    }
}


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
    use std::{any::TypeId, cell::Cell, sync::Arc};
    use std::fmt::Debug;
    use bumpalo::Bump;
    use crate::utils::{ArenaAny, DowncastExt};

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

        let _  = opaque_ty.cast::<OpaqueType>().unwrap();
        let _  = i32_array_ty.cast::<ArrayType>().unwrap();
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
