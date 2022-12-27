use ashley::hir::MatchCtxt;
use crate::{
    hir::{Attr, AttributeBase, HirCtxt, IRPrintable, IRPrinter, IRSyntaxElem, IRVisitable},
    utils::ArenaAny,
    write_ir,
};
use crate::hir::AttrConstraint;

/// Unknown type.
///
/// Used in places where the type is not known, not yet inferred, or invalid.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct UnknownType;
impl<'a> IRPrintable<'a> for UnknownType {
    fn is_inline(&self) -> bool {
        true
    }
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "unknown");
    }
}

/// Unit type.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct UnitType;
impl<'a> IRPrintable<'a> for UnitType {
    fn is_inline(&self) -> bool {
        true
    }
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "()");
    }
}

/// Scalar type kind.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ScalarTypeKind {
    Int,
    UnsignedInt,
    Float,
    Double,
    Bool,
}

impl ScalarTypeKind {
    pub fn display(&self) -> &'static str {
        match *self {
            ScalarTypeKind::Int => "int",
            ScalarTypeKind::UnsignedInt => "uint",
            ScalarTypeKind::Float => "float",
            ScalarTypeKind::Double => "double",
            ScalarTypeKind::Bool => "bool",
        }
    }
}

/// Scalar type.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct ScalarType(pub ScalarTypeKind);
impl<'a> IRPrintable<'a> for ScalarType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        match self.0 {
            ScalarTypeKind::Int => printer.token("int"),
            ScalarTypeKind::UnsignedInt => printer.token("uint"),
            ScalarTypeKind::Float => printer.token("float"),
            ScalarTypeKind::Double => printer.token("double"),
            ScalarTypeKind::Bool => printer.token("bool"),
        }
    }
}

/// Vector type (element type + size).
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct VectorType(pub ScalarTypeKind, pub u8);
impl<'a> IRPrintable<'a> for VectorType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        let n = self.1;
        match self.0 {
            ScalarTypeKind::Int => printer.token(&format!("ivec{n}")),
            ScalarTypeKind::UnsignedInt => printer.token(&format!("uvec{n}")),
            ScalarTypeKind::Float => printer.token(&format!("vec{n}")),
            ScalarTypeKind::Double => printer.token(&format!("dvec{n}")),
            ScalarTypeKind::Bool => printer.token(&format!("bvec{n}")),
        }
    }
}

/// Matrix type (element type + row count + column count).
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct MatrixType(pub ScalarTypeKind, pub u8, pub u8);
impl<'a> IRPrintable<'a> for MatrixType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        let (r, c) = (self.1, self.2);
        match self.0 {
            ScalarTypeKind::Int => printer.token(&format!("imat{r}x{c}")),
            ScalarTypeKind::UnsignedInt => printer.token(&format!("umat{r}x{c}")),
            ScalarTypeKind::Float => printer.token(&format!("mat{r}x{c}")),
            ScalarTypeKind::Double => printer.token(&format!("dmat{r}x{c}")),
            ScalarTypeKind::Bool => printer.token(&format!("bmat{r}x{c}")),
        }
    }
}

/// Array type (element type + size).
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct ArrayType<'a>(pub Attr<'a>, pub u32);
impl<'a> IRPrintable<'a> for ArrayType<'a> {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "array<", self.0, ",", self.1, ">");
    }
}

/// Field of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct Field<'a> {
    pub ty: Attr<'a>,
    pub name: &'a str,
}

/// Structure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct StructType<'a> {
    pub name: &'a str,
    pub fields: &'a [Field<'a>],
}

impl<'a> IRPrintable<'a> for StructType<'a> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "struct<", self.name);
        for f in self.fields {
            write_ir!(printer, f.name, ":", f.ty, ",");
        }
        write_ir!(printer, ">");
    }
}

impl<'a> StructType<'a> {
    /// Finds a field by name.
    pub fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.name == name)
    }

    /// Finds a field by name.
    pub fn field(&self, name: &str) -> Option<&Field<'a>> {
        self.fields.iter().find(|f| f.name == name)
    }
}

/// Tuple type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct TupleType<'a>(pub &'a [Attr<'a>]);
impl<'a> IRPrintable<'a> for TupleType<'a> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "tuple<");
        for ty in self.0 {
            write_ir!(printer, *ty, ",");
        }
        write_ir!(printer, ">");
    }
}

//--------------------------------------------------------------------------------------------------
/// Function type
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct FunctionType<'a> {
    pub return_ty: Attr<'a>,
    pub arg_types: &'a [Attr<'a>],
}

impl<'a> IRPrintable<'a> for FunctionType<'a> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        write_ir!(printer, "fn<(");
        let mut first = true;
        for arg in self.arg_types {
            if !first {
                write_ir!(printer, ",");
            }
            write_ir!(printer, *arg);
            first = false;
        }
        write_ir!(printer, ") -> ", self.return_ty, ">");
    }
}

//--------------------------------------------------------------------------------------------------
// Extension methods on attributes
pub trait BaseDialectAttrExt {
    fn is_scalar_type(&self) -> bool;
    fn is_unit_type(&self) -> bool;
    fn is_vector_type(&self) -> bool;
    fn is_bool(&self) -> bool;
}

impl<'a> BaseDialectAttrExt for Attr<'a> {
    fn is_scalar_type(&self) -> bool {
        self.cast::<ScalarType>().is_some()
    }

    fn is_unit_type(&self) -> bool {
        self.cast::<UnitType>().is_some()
    }

    fn is_vector_type(&self) -> bool {
        self.cast::<VectorType>().is_some()
    }

    fn is_bool(&self) -> bool {
        self.cast::<ScalarType>() == Some(&ScalarType(ScalarTypeKind::Bool))
    }
}
