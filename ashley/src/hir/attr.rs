//! IR attributes
use crate::{
    diagnostic::SourceLocation,
    hir::{HirArena, IRPrintable},
    utils::{ArenaAny, DowncastExt},
    write_ir,
};
use ashley::hir::IRPrinter;
use ordered_float::OrderedFloat;
use std::{
    cmp::Ordering,
    collections::HashSet,
    ffi::c_void,
    fmt,
    hash::{Hash, Hasher},
    ops::Range,
    ptr,
};
use std::cell::RefCell;
use crate::diagnostic::DiagnosticBuilder;
use crate::hir::{AttrConstraint, Builder, HirCtxt, MatchCtxt};

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

/// Trait implemented by attribute value types.
pub trait AttributeBase<'a>: fmt::Debug + ArenaAny<'a> + IRPrintable<'a> + DynEq<'a> + DynHash {}

impl<'a, T> AttributeBase<'a> for T where T: fmt::Debug + ArenaAny<'a> + IRPrintable<'a> + DynEq<'a> + DynHash {}

/// Represents an interned attribute.
#[derive(Debug)]
pub struct Attr<'a, T: ?Sized + AttributeBase<'a> = dyn AttributeBase<'a>>(pub(crate) &'a T);

impl <'ir> Attr<'ir> {
    pub fn pmatch<'a, U>(&self, ctxt: &HirCtxt<'ir>) -> Option<U> where U: AttrConstraint<'ir> {
        let mcx = MatchCtxt::new(ctxt);
        U::try_match(&mcx, *self)
    }
}

impl<'a, T: AttributeBase<'a>> Attr<'a, T> {
    pub fn upcast(&self) -> Attr<'a> {
        Attr(self.0)
    }
}

impl<'a, T: ?Sized + AttributeBase<'a>> Copy for Attr<'a, T> {}

impl<'a, T: ?Sized + AttributeBase<'a>> Clone for Attr<'a, T> {
    fn clone(&self) -> Self {
        Attr(self.0)
    }
}

impl<'a> PartialEq for Attr<'a> {
    fn eq(&self, other: &Self) -> bool {
        // `Type` instances are interned, so we can compare equality by comparing the pointers.
        // Cast it to `*const c_void` first so that we don't compare the vtable (it may be different across instances).
        ptr::eq(
            self.0 as *const _ as *const c_void,
            other.0 as *const _ as *const c_void,
        )
    }
}

impl<'a> Eq for Attr<'a> {}

impl<'a> PartialOrd for Attr<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> Ord for Attr<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&(self.0.as_any() as *const _), &(other.0.as_any() as *const _))
    }
}

impl<'a> Hash for Attr<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // hash the pointer value
        Hash::hash(&(self.0.as_any() as *const _), state);
    }
}

impl<'a> Attr<'a> {
    /// Casts to a concrete attribute type.
    pub fn cast<T>(&self) -> Option<&'a T>
    where
        T: AttributeBase<'a>,
    {
        self.0.cast::<T>()
    }
}

//--------------------------------------------------------------------------------------------------

/*
/// Attribute containing a type reference.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct TypeAttr<'a>(pub Type<'a>);
impl<'a> IRPrintable<'a> for TypeAttr<'a> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "!", self.0);
    }
}

impl<'a> AttributeBase<'a> for TypeAttr<'a> {}
*/

/// String attribute.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct StringAttr<'a>(pub &'a str);
impl<'a> IRPrintable<'a> for StringAttr<'a> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, self.0);
    }
}

/// Integer attribute.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct IntegerAttr(pub i128);
impl<'a> IRPrintable<'a> for IntegerAttr {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, format!("{}", self.0));
    }
}

/// Floating-point value.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct FloatAttr(pub OrderedFloat<f64>);
impl<'a> IRPrintable<'a> for FloatAttr {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, format!("{}", self.0));
    }
}

/// Boolean value.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct BooleanAttr(pub bool);
impl<'a> IRPrintable<'a> for BooleanAttr {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, format!("{}", self.0));
    }
}

/*/// Source location attribute.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct LineColumnLocationAttr<'a> {
    /// Path to the source file. Not necessarily a filesystem path.
    pub file: &'a StringAttr<'a>,
    pub line: u32,
    pub column: u32,
}

impl<'a> IRPrintable<'a> for LineColumnLocationAttr<'a> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
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
impl<'a> AttributeBase<'a> for LineColumnLocationAttr<'a> {}*/

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
            Location::Source(loc) => Some(*loc),
            Location::Unknown => None,
        }
    }
}

impl Default for Location {
    fn default() -> Self {
        Location::Unknown
    }
}

impl<'a> IRPrintable<'a> for Location {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
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

//--------------------------------------------------------------------------------------------------

impl<'a> PartialEq for dyn AttributeBase<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.any_eq(other.as_any())
    }
}

impl<'a> Eq for dyn AttributeBase<'a> {}

impl<'a> Hash for dyn AttributeBase<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        DynHash::hash(self, state)
    }
}

//--------------------------------------------------------------------------------------------------

/// Interns values of arbitrary types allocated on an arena.
pub struct AttrInterner<'a> {
    items: RefCell<HashSet<&'a dyn AttributeBase<'a>>>,
}

impl<'a> AttrInterner<'a> {
    pub fn new() -> AttrInterner<'a> {
        AttrInterner {
            items: Default::default(),
        }
    }

    /// Returns whether this interner contains the specified value.
    pub fn contains<T>(&self, value: &T) -> bool
    where
        T: AttributeBase<'a>,
    {
        self.items.borrow().contains(value as &dyn AttributeBase<'a>)
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
    pub fn intern<T>(&self, arena: &'a HirArena, value: T) -> (&'a T, bool)
    where
        T: AttributeBase<'a>,
    {
        let mut items = self.items.borrow_mut();
        if let Some(value) = items.get(&value as &dyn AttributeBase<'a>) {
            return (value.as_any().cast::<T>().unwrap(), false);
        }

        // the (u8, value) tuple is there to ensure that each interned value has a unique address, even
        // in the presence of ZSTs.
        // FIXME: not sure what guarantees are made about the address of `value` if it is a ZST. Maybe wrap it in a repr(C) struct before?
        let wrap = arena.0.alloc((0u8, value));
        items.insert(&wrap.1);
        (&wrap.1, true)
    }
}

#[cfg(test)]
mod tests {
    /*use std::cell::{Cell, UnsafeCell};
    use std::hash::{Hash, Hasher};
    use std::sync::Mutex;
    use ashley::hir::IRPrinter;
    use ashley_derive::ArenaAny;
    use crate::hir::{HirArena, HirCtxt, IRPrintable};

    #[derive(Copy,Clone,Debug,ArenaAny)]
    struct MyAttr<'a>(&'a Mutex<&'a str>);
    impl<'a> PartialEq for MyAttr<'a> {
        fn eq(&self, other: &Self) -> bool {
            *self.0.lock().unwrap() == *other.0.lock().unwrap()
        }
    }
    impl<'a> Eq for MyAttr<'a> {}
    impl<'a> Hash for MyAttr<'a> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.0.lock().unwrap().hash(state)
        }
    }
    impl<'a> IRPrintable<'a> for MyAttr<'a> {
        fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
            todo!()
        }
    }

    #[test]
    fn test_attr_lifetime() {

        static S: Mutex<&'static str> = Mutex::new("hello");

        let arena = HirArena::new();
        let mut ctxt = HirCtxt::new(&arena);

        let a = MyAttr(&S);
        let a = ctxt.intern_attr(a);

    }*/
}