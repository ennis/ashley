use crate::hir::ir::HirCtxt;
use fxhash::{FxHashMap, FxHashSet, FxHasher64};
use indexmap::Equivalent;
use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::{mem, ptr};

#[repr(C)]
struct PayloadWithTypeId<T> {
    id: u32,
    payload: T,
}

/// Represents an interned type.
#[derive(Copy, Clone)]
pub struct Type<'hir>(*const u32, PhantomData<&'hir ()>);

impl<'hir> PartialEq for Type<'hir> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
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
        Ord::cmp(&(self.0 as *const _), &(other.0 as *const _))
    }
}

impl<'hir> Hash for Type<'hir> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&(self.0 as *const _), state);
    }
}

impl<'hir> Type<'hir> {
    // -> need to ensure that the lifetimes inside T **do not** outlive hir
    // How?
    //
    // T is T<'a>, ensure 'hir: 'a
    //
    //
    fn cast<T: HasTypeId>(&self) -> Option<&'hir T>  {
        unsafe {
            if ptr::read(self.0) == T::ID {
                Some(&(*(self.0.cast::<PayloadWithTypeId<T>>())).payload)
            } else {
                None
            }
        }
    }
}

/// Associates an unique ID to a type.
pub unsafe trait HasTypeId {
    const ID: u32;
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct OpaqueType<'a>(&'a str);
unsafe impl<'a> HasTypeId for OpaqueType<'a> {
    const ID: u32 = 0;
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct ArrayType<'a> {
    elem_ty: Type<'a>,
    len: usize,
}
unsafe impl<'a> HasTypeId for ArrayType<'a> {
    const ID: u32 = 1;
}

pub trait Something<'hir> {
    type Target<'a>;

    /*fn cast(ty: Type<'hir>) -> Self::Target<'hir> {

    }*/
}

/*pub unsafe trait TypeDowncast<'hir> {
    fn cast(ty: Type<'hir>) -> Self {

    }
}

unsafe impl<'a> TypeDowncast<'a> for ArrayType<'a> {
    pub fn cast(ty: Type<>)
}*/


/*#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Tricky<'a> {
    text: Cell<&'a str>,
}
unsafe impl<'a> HasTypeId for Tricky<'a> {
    const ID: u32 = 1;
}*/

pub(crate) struct TypeCtxt<'hir> {
    type_map: FxHashMap<u64, Type<'hir>>,
}

impl<'hir> TypeCtxt<'hir> {
    pub(crate) fn new() -> TypeCtxt<'hir> {
        TypeCtxt {
            type_map: Default::default(),
        }
    }
}

impl<'hir> HirCtxt<'hir> {
    pub fn intern_type<T>(&mut self, ty: T) -> Type<'hir>
    where
        T: HasTypeId + Copy + Eq + Hash + 'hir,
    {
        let mut hasher = FxHasher64::default();
        T::ID.hash(&mut hasher);
        ty.hash(&mut hasher);
        let hash = hasher.finish();
        let ty_clone = ty.clone();

        let new_ty = self.ty_ctxt.type_map.entry(hash).or_insert_with(|| {
            let ty = self.arena.dropless.alloc(PayloadWithTypeId {
                id: T::ID,
                payload: ty_clone,
            });
            Type(ty as *const PayloadWithTypeId<T> as *const u32, PhantomData)
        });

        let new_ty_downcast = new_ty
            .cast::<T>()
            .expect("unexpected interned type - possible hash collision?");
        assert!(
            new_ty_downcast == &ty,
            "unexpected interned type value - possible hash collision?"
        );

        *new_ty
    }
}

#[cfg(test)]
mod tests {
    use crate::hir::ir::ty::{ArrayType, OpaqueType};
    use crate::hir::ir::{Arena, HirCtxt};
    use std::cell::Cell;

    #[test]
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
    }
}
