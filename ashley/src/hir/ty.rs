use crate::utils::{ArenaAny, DowncastExt};
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ptr,
};
use crate::hir::{IRPrintable, IRVisitable};

/// Trait implemented by types.
pub trait TypeBase<'hir>: fmt::Debug + ArenaAny<'hir> + IRPrintable<'hir> {}

/// Represents an interned type.
#[derive(Copy, Clone, Debug)]
pub struct Type<'hir>(pub(crate) &'hir dyn TypeBase<'hir>);

impl<'hir> PartialEq for Type<'hir> {
    fn eq(&self, other: &Self) -> bool {
        // `Type` instances are interned, so we can compare equality by comparing the pointers.
        // However, do so via `as_any` because the pointer metadata (vtable for `Type`) might be different
        // even for the same objects (see docs of `std::ptr::eq`).
        ptr::eq(self.0.as_any(), other.0.as_any())
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
        Ord::cmp(&(self.0.as_any() as *const _), &(other.0.as_any() as *const _))
    }
}

impl<'hir> Hash for Type<'hir> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // hash the pointer value
        Hash::hash(&(self.0.as_any() as *const _), state);
    }
}

impl<'hir> Type<'hir> {

    /// Returns whether the type is of the specified concrete type.
    pub fn is<T>(&self) -> bool
        where
            T: TypeBase<'hir>,
    {
        self.0.cast::<T>().is_some()
    }

    /// Casts to a concrete type.
    pub fn cast<T>(&self) -> Option<&'hir T>
    where
        T: TypeBase<'hir>,
    {
        self.0.cast::<T>()
    }
}

#[cfg(test)]
mod tests {}
