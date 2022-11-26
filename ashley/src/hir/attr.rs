//! IR attributes
use crate::{
    diagnostic::SourceLocation,
    hir::{IRPrintable},
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
use std::ffi::c_void;

/// Trait implemented by types.
pub trait AttributeBase<'hir>: fmt::Debug + ArenaAny<'hir> + IRPrintable<'hir> {}

/// Represents an interned attribute.
#[derive(Debug)]
pub struct Attribute<'hir, T: ?Sized + AttributeBase<'hir> = dyn AttributeBase<'hir>>(pub(crate) &'hir T);

impl<'hir, T: AttributeBase<'hir>> Attribute<'hir,T> {
    pub fn upcast(&self) -> Attribute<'hir> {
        Attribute(self.0)
    }
}

impl<'hir, T: ?Sized + AttributeBase<'hir>> Copy for Attribute<'hir,T> {}

impl<'hir, T: ?Sized + AttributeBase<'hir>> Clone for Attribute<'hir,T> {
    fn clone(&self) -> Self {
        Attribute(self.0)
    }
}

impl<'hir> PartialEq for Attribute<'hir> {
    fn eq(&self, other: &Self) -> bool {
        // `Type` instances are interned, so we can compare equality by comparing the pointers.
        // Cast it to `*const c_void` first so that we don't compare the vtable (it may be different across instances).
        ptr::eq(self.0 as *const _ as *const c_void, other.0 as *const _ as *const c_void)
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

/*
/// Attribute containing a type reference.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct TypeAttr<'hir>(pub Type<'hir>);
impl<'hir> IRPrintable<'hir> for TypeAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, "!", self.0);
    }
}

impl<'hir> AttributeBase<'hir> for TypeAttr<'hir> {}
*/

/// String attribute.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct StringAttr<'hir>(pub &'hir str);
impl<'hir> IRPrintable<'hir> for StringAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, self.0);
    }
}
impl<'hir> AttributeBase<'hir> for StringAttr<'hir> {}


/// Integer attribute.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct IntegerAttr(pub i128);
impl<'hir> IRPrintable<'hir> for IntegerAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, format!("{}", self.0));
    }
}
impl<'hir> AttributeBase<'hir> for IntegerAttr<'hir> {}

/// Floating-point value.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct FloatAttr(pub f64);
impl<'hir> IRPrintable<'hir> for FloatAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, format!("{}", self.0));
    }
}
impl<'hir> AttributeBase<'hir> for FloatAttr<'hir> {}

/// Boolean value.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct BooleanAttr(pub bool);
impl<'hir> IRPrintable<'hir> for BooleanAttr<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, format!("{}", self.0));
    }
}
impl<'hir> AttributeBase<'hir> for BooleanAttr<'hir> {}

/*/// Source location attribute.
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
impl<'hir> AttributeBase<'hir> for LineColumnLocationAttr<'hir> {}*/

/// Byte-span source location attribute.
// TODO: right now all attributes are interned, but it's a bit wasteful for locations since they're probably all gonna be different
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub enum Location {
    /// An actual location in source code.
    Source(SourceLocation),
    Unknown,
}

impl Location {
    pub fn to_source_location(&self) -> Option<SourceLocation> {
        match self {
            Location::Source(loc) => {Some(*loc)}
            Location::Unknown => {None}
        }
    }
}

impl Default for Location {
    fn default() -> Self {
        Location::Unknown
    }
}

impl<'hir> IRPrintable<'hir> for Location {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        match self {
            Location::Source(src_loc) => {
                write_ir!(printer, "loc(", *src_loc, ")");
            }
            Location::Unknown => {
                write_ir!(printer, "loc(?)");
            }
        }
    }
}
impl<'hir> AttributeBase<'hir> for Location {}

