use crate::hir::ir::{
    arena::{ArenaAlloc, ArenaAny},
    HirCtxt, Interner,
};
use fxhash::{FxHashMap, FxHashSet, FxHasher64};
use indexmap::Equivalent;
use std::{
    cell::Cell,
    cmp::Ordering,
    collections::HashSet,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem, ptr,
};

/// Represents an interned type.
#[derive(Copy, Clone)]
pub struct Type<'hir>(ArenaAny<'hir>);

// `Type` instances are interned, so we can compare equality by comparing the pointers
impl<'hir> PartialEq for Type<'hir> {
    fn eq(&self, other: &Self) -> bool {
        self.0.opaque_ptr().eq(&other.0.opaque_ptr())
    }
}

impl<'hir> Eq for Type<'hir> {}

impl<'hir> PartialOrd for Type<'hir> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'hir> Ord for Type<'hir> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.opaque_ptr().cmp(&other.0.opaque_ptr())
    }
}

impl<'hir> Hash for Type<'hir> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&self.0.opaque_ptr(), state);
    }
}

impl<'hir> Type<'hir> {
    pub fn cast<T>(&self) -> Option<&'hir T>
    where
        T: ArenaAlloc<'hir>,
    {
        self.0.cast()
    }
}

pub(crate) struct TypeCtxt<'hir> {
    interner: Interner<'hir>,
}

impl<'hir> TypeCtxt<'hir> {
    pub(crate) fn new() -> TypeCtxt<'hir> {
        TypeCtxt {
            interner: Interner::new(),
        }
    }
}

impl<'hir> HirCtxt<'hir> {
    pub fn intern_type<T>(&mut self, ty: T) -> Type<'hir>
    where
        T: ArenaAlloc<'hir> + Eq + Hash,
    {
        Type(self.ty_ctxt.interner.intern(self.arena, ty))
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    /*#[test]
    fn test_lifetimes() {

        let expired_str = {
            let str = String::from("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
            let arena = Arena::new();
            let mut ctxt = HirCtxt::new(&arena);

            //let sampler_str = ctxt.arena.dropless.alloc_str("sampler");
            let sampler_ty = ctxt.intern_type(OpaqueType(&str));
            let int_ty = ctxt.intern_type(OpaqueType("int"));
            let float_ty = ctxt.intern_type(OpaqueType("float"));
            let int_array_ty = ctxt.intern_type(ArrayType {
                elem_ty: int_ty,
                len: 16,
            });

            assert!(
                int_array_ty.cast::<ArrayType>().unwrap()
                    == &ArrayType {
                    elem_ty: int_ty,
                    len: 16
                }
            );

            //let what = String::from("dddddddd");

            // issue: can request whatever lifetime here, even a longer one!
            //
            // fundamentally unsound; however, can probably implement a cast method on the side of the derived type
            // with the correct signature
            let sampler_ty_cast = sampler_ty.cast::<OpaqueType<'static>>().unwrap();
            sampler_ty_cast.0
        };



        // Can observe an expired value => cannot be safe
        dbg!(expired_str);
    }*/
}
