use bumpalo::Bump;
use fxhash::FxHasher64;
use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    ffi::c_void,
    hash::{Hash, Hasher},
    marker::PhantomData,
};

/// Object with associated type ID stored in a dropless arena.
#[repr(C)]
struct ArenaObject<T> {
    static_type_id: std::any::TypeId,
    payload: T,
}

/// Opaque pointer to a type-erased object stored in an arena.
#[derive(Copy, Clone)]
pub struct ArenaAny<'a>(*const std::any::TypeId, PhantomData<&'a ()>);

/// Arena
pub struct Arena {
    pub(crate) bump: Bump,
    //pub(crate) type_kinds: typed_arena::Arena<TypeKind<'hir>>,
}

impl Arena {
    pub fn new() -> Arena {
        Arena {
            bump: Default::default(),
            //type_kinds: Default::default(),
        }
    }

    pub fn alloc_str(&self, s: &str) -> &mut str {
        self.bump.alloc_str(s)
    }

    pub fn allocate_any<'a, T>(&'a self, value: T) -> ArenaAny<'a>
    where
        T: ArenaAlloc<'a>,
    {
        let obj = self.bump.alloc(ArenaObject {
            static_type_id: T::static_type_id(),
            payload: value,
        });
        ArenaAny(obj as *const ArenaObject<T> as *const std::any::TypeId, PhantomData)
    }
}

impl<'a> ArenaAny<'a> {
    pub fn type_id(&self) -> std::any::TypeId {
        // SAFETY: ArenaAny instances can only be produced by Arena, which we (logically) borrow via the 'a lifetime.
        // So the arena is guaranteed to be still alive, and the pointer still valid.
        unsafe { self.0.read() }
    }

    /// Compares two `ArenaAny` via pointer equality.
    pub fn ptr_eq(&self, other: &ArenaAny) -> bool {
        self.0 == other.0
    }

    /// Returns an opaque pointer to the underlying object.
    pub fn opaque_ptr(&self) -> *const c_void {
        self.0 as *const c_void
    }

    /// Returns a pointer to the payload.
    ///
    /// # Safety
    ///
    /// `T` must be the payload type of the `ArenaAny` object.
    pub fn cast<T: ArenaAlloc<'a>>(&self) -> Option<&'a T> {
        // all impls of ArenaAlloc should be of one of these two forms:
        // * `impl<'a> ArenaAlloc<'a> for T<'a>`
        // * `impl<'a> ArenaAlloc<'a> for T`
        //
        // Crucially, if T has lifetime parameters, there MUST NOT be an impl of this form:
        // * `impl<'a, 'b> ArenaAlloc<'a> for T<'b>`
        // because the compiler can choose whatever it wants for 'b

        if self.type_id() == T::static_type_id() {
            unsafe { Some(&(*(self.0.cast::<ArenaObject<T>>())).payload) }
        } else {
            None
        }
    }
}

pub unsafe trait ArenaAlloc<'a>: 'a {
    fn static_type_id() -> std::any::TypeId
    where
        Self: Sized;
    fn type_id(&self) -> std::any::TypeId;
    //fn cast(any: ArenaAny<'a>) -> Option<&'a Self>;
}

#[macro_export]
macro_rules! impl_arena_alloc {
    // single lifetime: OK
    ($n:ident < $lt:lifetime >) => {
        unsafe impl<$lt> $crate::hir::ir::ArenaAlloc<$lt> for $n<$lt> {
            fn static_type_id() -> ::std::any::TypeId {
                ::std::any::TypeId::of::<$n<'static>>()
            }

            fn type_id(&self) -> ::std::any::TypeId {
                ::std::any::TypeId::of::<$n<'static>>()
            }
        }
    };
    // no lifetime: OK
    ($n:ident) => {
        unsafe impl<'a> $crate::hir::ir::ArenaAlloc<'a> for $n {
            fn static_type_id() -> ::std::any::TypeId {
                ::std::any::TypeId::of::<$n>()
            }

            fn type_id(&self) -> ::std::any::TypeId {
                ::std::any::TypeId::of::<$n>()
            }
        }
    };
}

pub use impl_arena_alloc;

//--------------------------------------------------------------------------------------------------

trait Internable<'a>: ArenaAlloc<'a> {
    fn any_eq(&self, other: &dyn Internable<'a>) -> bool;
    fn hash(&self, hasher: &mut FxHasher64);
}

impl<'a> PartialEq for dyn Internable<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.any_eq(other)
    }
}

impl<'a> Eq for dyn Internable<'a> {}

impl<'a> Hash for dyn Internable<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut fxhasher = FxHasher64::default();
        Internable::hash(self, &mut fxhasher);
        fxhasher.finish().hash(state);
    }
}

impl<'a, T> Internable<'a> for T
where
    T: Eq + Hash + ArenaAlloc<'a>,
{
    fn any_eq(&self, other: &dyn Internable<'a>) -> bool {
        (Self::static_type_id() == ArenaAlloc::type_id(other)) && {
            // SAFETY: same type ID guarantees that T is the correct type.
            let other = unsafe { &*(self as *const dyn Internable as *const T) };
            self.eq(other)
        }
    }

    fn hash(&self, hasher: &mut FxHasher64) {
        self.hash(hasher);
    }
}

struct InternKey<'a>(&'a dyn Internable<'a>);

impl<'a> Borrow<dyn Internable<'a>> for InternKey<'a> {
    fn borrow(&self) -> &dyn Internable<'a> {
        self.0
    }
}

impl<'a> PartialEq for InternKey<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0.any_eq(other.0)
    }
}

impl<'a> Eq for InternKey<'a> {}

impl<'a> Hash for InternKey<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(self.0, state);
    }
}

pub struct Interner<'a> {
    values: HashMap<InternKey<'a>, ArenaAny<'a>>,
}

impl<'a> Interner<'a> {
    pub fn new() -> Interner<'a> {
        Interner {
            values: Default::default(),
        }
    }

    pub fn intern<T>(&mut self, arena: &'a Arena, value: T) -> ArenaAny<'a>
    where
        T: ArenaAlloc<'a> + Eq + Hash,
    {
        if let Some(value) = self.values.get(&value as &dyn Internable) {
            return *value;
        }

        let any = arena.allocate_any(value);
        let internable = any.cast::<T>().unwrap();
        self.values.insert(InternKey(internable), any);
        any
    }
}

#[cfg(test)]
mod tests {
    use crate::hir::ir::arena::{Arena, ArenaAlloc, ArenaAny};
    use std::{any::TypeId, cell::Cell, sync::Arc};
    use crate::hir::ir::Interner;

    #[derive(Copy, Clone, PartialEq, Eq, Hash)]
    pub struct OpaqueType<'a>(&'a str);
    impl_arena_alloc!(OpaqueType<'a>);

    pub struct Wtf<'a, 'b>(&'a str, &'b str);
    //impl_arena_any!(OpaqueType<'a>);

    #[derive(Copy, Clone, PartialEq, Eq, Hash)]
    pub struct IntegerType(u32);
    impl_arena_alloc!(IntegerType);

    #[derive(Copy, Clone, PartialEq, Eq, Hash)]
    pub struct MatrixType(u8, u8, u8);
    impl_arena_alloc!(MatrixType);

    //#[derive(Copy, Clone, PartialEq, Eq, Hash)]
    pub struct ArrayType<'a>(ArenaAny<'a>, usize);
    impl_arena_alloc!(ArrayType<'a>);

    #[test]
    fn test_arena() {
        let arena = Arena::new();
        let opaque_ty = arena.allocate_any(OpaqueType("sampler"));
        let i16_ty = arena.allocate_any(IntegerType(16));
        let i32_ty = arena.allocate_any(IntegerType(32));
        let mat4x4_ty = arena.allocate_any(MatrixType(1, 4, 4));
        let i32_array_ty = arena.allocate_any(ArrayType(i32_ty, 64));

        let _ = opaque_ty.cast::<OpaqueType>().unwrap();
        let _ = i32_array_ty.cast::<ArrayType>().unwrap();
    }

    #[test]
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
    }

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
