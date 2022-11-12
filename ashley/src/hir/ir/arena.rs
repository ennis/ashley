use std::marker::PhantomData;
use bumpalo::Bump;

pub struct Arena<'hir> {
    pub(crate) dropless: Bump,
    _phantom: PhantomData<&'hir ()>
    //pub(crate) type_kinds: typed_arena::Arena<TypeKind<'hir>>,
}

impl<'hir> Arena<'hir> {
    pub fn new() -> Arena<'hir> {
        Arena {
            dropless: Default::default(),
            _phantom: PhantomData
            //type_kinds: Default::default(),
        }
    }
}

/// Object with associated type ID stored in a dropless arena.
#[repr(C)]
struct ArenaObject<T> {
    static_typeid: std::any::TypeId,
    payload: T,
}

/// Opaque pointer to a type-erased object stored in an arena.
pub struct ArenaAny<'a>(*const u32, PhantomData<&'a ()>);

pub unsafe trait ArenaAlloc<'a> {
    fn cast(any: ArenaAny<'a>) -> &'a Self;
}
