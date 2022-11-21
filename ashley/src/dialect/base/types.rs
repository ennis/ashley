use crate::{
    hir::{Type, TypeBase},
    utils::ArenaAny,
    write_ir,
};
use ashley::hir::{IRPrintable, IRSyntaxElem, IRPrinter, IRVisitable};

/// Unknown type.
///
/// Used in places where the type is not known, not yet inferred, or invalid.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct UnknownType;
impl<'hir> IRPrintable<'hir> for UnknownType {
    fn is_inline(&self) -> bool {
        true
    }
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, "unknown");
    }
}
impl<'hir> TypeBase<'hir> for UnknownType {}

/// Unit type.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct UnitType;
impl<'hir> IRPrintable<'hir> for UnitType {
    fn is_inline(&self) -> bool {
        true
    }
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, "()");
    }
}
impl<'hir> TypeBase<'hir> for UnitType {}

/// Scalar type.
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
impl<'hir> IRPrintable<'hir> for ScalarType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        match self.0 {
            ScalarTypeKind::Int => printer.token("int"),
            ScalarTypeKind::UnsignedInt => printer.token("uint"),
            ScalarTypeKind::Float => printer.token("float"),
            ScalarTypeKind::Double => printer.token("double"),
            ScalarTypeKind::Bool => printer.token("bool"),
        }
    }
}

impl<'hir> TypeBase<'hir> for ScalarType {}

/// Vector type (element type + size).
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct VectorType(pub ScalarTypeKind, u8);
impl<'hir> IRPrintable<'hir> for VectorType {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
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
impl<'hir> TypeBase<'hir> for VectorType {}

/// Array type (element type + size).
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct ArrayType<'hir>(Type<'hir>, u32);
impl<'hir> IRPrintable<'hir> for ArrayType<'hir> {
    fn is_inline(&self) -> bool {
        true
    }

    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, "array<", self.0, ",", self.1, ">");
    }
}
impl<'hir> TypeBase<'hir> for ArrayType<'hir> {}

/// Field of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct Field<'hir> {
    pub ty: Type<'hir>,
    pub name: &'hir str,
}

/// Structure type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct StructType<'hir> {
    pub name: &'hir str,
    pub fields: &'hir [Field<'hir>],
}
impl<'hir> IRPrintable<'hir> for StructType<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, "struct<", self.name);
        for f in self.fields {
            write_ir!(printer, f.name, ":", f.ty, ",");
        }
        write_ir!(printer, ">");
    }
}
impl<'hir> TypeBase<'hir> for StructType<'hir> {}

impl<'hir> StructType<'hir> {
    /// Finds a field by name.
    pub fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.name == name)
    }

    /// Finds a field by name.
    pub fn field(&self, name: &str) -> Option<&Field<'hir>> {
        self.fields.iter().find(|f| f.name == name)
    }
}

/// Tuple type.
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct TupleType<'hir>(pub &'hir [Type<'hir>]);
impl<'hir> IRPrintable<'hir> for TupleType<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        write_ir!(printer, "tuple<");
        for ty in self.0 {
            write_ir!(printer, *ty, ",");
        }
        write_ir!(printer, ">");
    }
}
impl<'hir> TypeBase<'hir> for TupleType<'hir> {}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ImageDimension {
    Dim1D,
    Dim2D,
    Dim3D,
    DimCube,
    Dim1DArray,
    Dim2DArray,
}

impl ImageDimension {
    pub fn display(&self) -> &'static str {
        match self {
            ImageDimension::Dim1D => "1D",
            ImageDimension::Dim2D => "2D",
            ImageDimension::Dim3D => "3D",
            ImageDimension::DimCube => "cube map",
            ImageDimension::Dim1DArray => "1D array",
            ImageDimension::Dim2DArray => "2D array",
        }
    }

    /*// TODO move this out of ast
    pub fn display_glsl_image_suffix(&self) -> &'static str {
        match self {
            ImageDimension::Dim1D => "1D",
            ImageDimension::Dim2D => "2D",
            ImageDimension::Dim3D => "3D",
            ImageDimension::DimCube => "Cube",
            ImageDimension::Dim1DArray => "1DArray",
            ImageDimension::Dim2DArray => "2DArray",
        }
    }*/
}

/// Sampled image type
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct SampledImageType {
    pub sampled_ty: ScalarType,
    pub dim: ImageDimension,
    pub ms: bool,
}

impl<'hir> IRPrintable<'hir> for SampledImageType {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        todo!()
    }
}
impl<'hir> TypeBase<'hir> for SampledImageType {}

/*
impl SampledImageType {
    pub fn display_glsl(&self) -> impl fmt::Display + '_ {
        SampledImageTypeDisplayGlsl(self)
    }
}

struct SampledImageTypeDisplayGlsl<'a>(&'a SampledImageType);

impl<'a> fmt::Display for SampledImageTypeDisplayGlsl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}texture{}",
            self.0.sampled_ty.display_glsl_prefix(),
            self.0.dim.display_glsl_image_suffix()
        )?;
        if self.0.ms {
            write!(f, "MS")?
        }
        Ok(())
    }
}*/

/// Unsampled image type
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct ImageType {
    pub element_ty: ScalarType,
    pub dim: ImageDimension,
    pub ms: bool,
}
impl<'hir> IRPrintable<'hir> for ImageType {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
        todo!()
    }
}
impl<'hir> TypeBase<'hir> for ImageType {}

/// Function type
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct FunctionType<'hir> {
    pub return_ty: Type<'hir>,
    pub arg_types: &'hir [Type<'hir>],
}

impl<'hir> IRPrintable<'hir> for FunctionType<'hir> {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>) {
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
impl<'hir> TypeBase<'hir> for FunctionType<'hir> {}
