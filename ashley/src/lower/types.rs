//! Type system during lowering

use crate::hir;

enum AbstractScalarType {
    AbstractInt,
    AbstractFloat,
    Float,
    Double,
    Int,
    UnsignedInt,
    Bool
}

#[derive(Copy,Clone,Debug,Eq,PartialEq,Hash)]
enum AbstractType {
    Void,
    /// Concrete scalar
    Scalar(AbstractScalarType),
    /// Concrete vector
    Vector(AbstractScalarType, u32),
    Matrix(AbstractScalarType, u32, u32),
    Array(Box<AbstractType>, hir::Constant),
    // All other concrete types not directly known to the language
    Type(hir::Type),
}
