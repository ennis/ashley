//! IR attributes
use crate::{
    hir::{IRPrintable, Type},
    utils::{ArenaAny, DowncastExt},
    write_ir,
};
use ashley::hir::IRPrinter;
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::Range,
    ptr,
};

/// Trait implemented by types.
pub trait AttributeBase<'hir>: fmt::Debug + ArenaAny<'hir> + IRPrintable<'hir> {}

/// Represents an interned attribute.
#[derive(Copy, Clone, Debug)]
pub struct Attribute<'hir>(pub(crate) &'hir dyn AttributeBase<'hir>);

impl<'hir> PartialEq for Attribute<'hir> {
    fn eq(&self, other: &Self) -> bool {
        // `Type` instances are interned, so we can compare equality by comparing the pointers.
        // However, do so via `as_any` because the pointer metadata (vtable for `Type`) might be different
        // even for the same objects (see docs of `std::ptr::eq`).
        ptr::eq(self.0.as_any(), other.0.as_any())
    }
}

impl<'hir> Eq for Attribute<'hir> {}

impl<'hir> PartialOrd for Attribute<'hir> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'hir> Ord for Attribute<'hir> {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&(self.0.as_any() as *const _), &(other.0.as_any() as *const _))
    }
}

impl<'hir> Hash for Attribute<'hir> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // hash the pointer value
        Hash::hash(&(self.0.as_any() as *const _), state);
    }
}

impl<'hir> Attribute<'hir> {
    /// Casts to a concrete attribute type.
    pub fn cast<T>(&self) -> Option<&'hir T>
    where
        T: AttributeBase<'hir>,
    {
        self.0.cast::<T>()
    }
}

/// Attribute containing a type reference.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct TypeAttr<'hir>(pub Type<'hir>);
impl<'hir> IRPrintable<'hir> for TypeAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, "!", self.0);
    }
}

impl<'hir> AttributeBase<'hir> for TypeAttr<'hir> {}

/// String attribute.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct StringAttr<'hir>(pub &'hir str);
impl<'hir> IRPrintable<'hir> for StringAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        todo!()
    }
}
impl<'hir> AttributeBase<'hir> for StringAttr<'hir> {}

/// Source location attribute.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct LineColumnLocationAttr<'hir> {
    /// Path to the source file. Not necessarily a filesystem path.
    pub file: &'hir StringAttr<'hir>,
    pub line: u32,
    pub column: u32,
}

impl<'hir> IRPrintable<'hir> for LineColumnLocationAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(
            printer,
            "loc(",
            Attribute(self.file),
            " at ",
            self.line,
            ":",
            self.column
        );
    }
}
impl<'hir> AttributeBase<'hir> for LineColumnLocationAttr<'hir> {}

/// Byte-span source location attribute.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct ByteSpanLocationAttr<'hir> {
    pub file: &'hir StringAttr<'hir>,
    pub byte_range: Range<u32>,
}
impl<'hir> IRPrintable<'hir> for ByteSpanLocationAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(
            printer,
            "byteloc(",
            Attribute(self.file),
            "@",
            self.byte_range.start,
            "..",
            self.byte_range.end
        );
    }
}
impl<'hir> AttributeBase<'hir> for ByteSpanLocationAttr<'hir> {}
