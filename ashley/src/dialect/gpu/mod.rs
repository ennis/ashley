mod ops;

use std::hash::{Hash, Hasher};
use ashley::hir::IRPrinter;
use crate::dialect::base::ScalarType;
use crate::hir::{AttributeBase, IRPrintable};
use crate::utils::ArenaAny;
use crate::write_ir;

/// Specifies a viewport for a rendering operation.
#[derive(Copy,Clone,Debug,ArenaAny)]
pub struct Viewport {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub min_depth: f32,
    pub max_depth: f32,
}

impl PartialEq for Viewport {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Eq for Viewport {}

impl Hash for Viewport {
    fn hash<H: Hasher>(&self, state: &mut H) {
        todo!()
    }
}

impl<'a> IRPrintable<'a> for Viewport {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        todo!()
    }
}

/// Precision of a derivative operation.
#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash,ArenaAny)]
pub enum DerivativePrecision {
    /// Equivalent to dFdxCoarse.
    Coarse,
    /// Equivalent to dFdyCoarse.
    Fine
}

impl<'a> IRPrintable<'a> for DerivativePrecision {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        todo!()
    }
}

/// The type of a vertex attribute.
#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash,ArenaAny)]
pub enum AttributeType {
    Float,
    Vec2,
    Vec3,
    Vec4,
    IVec2,
    IVec3,
    IVec4
}

impl<'a> IRPrintable<'a> for AttributeType {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        todo!()
    }
}


/// Specifies a request for a vertex attribute.
#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash,ArenaAny)]
pub struct VertexAttribute<'a> {
    pub name: &'a str,
    pub ty: AttributeType,
}

/// Specifies a set of required vertex attributes.
#[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash,ArenaAny)]
pub struct VertexAttributeSet<'a>(pub &'a [VertexAttribute<'a>]);


/// Sampled image type
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct SampledImageType {
    pub sampled_ty: ScalarType,
    pub dim: ImageDimension,
    pub ms: bool,
}

impl<'a> IRPrintable<'a> for SampledImageType {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        todo!()
    }
}

/// Unsampled image type
#[derive(Clone, Debug, Eq, PartialEq, Hash, ArenaAny)]
pub struct ImageType {
    pub element_ty: ScalarType,
    pub dim: ImageDimension,
    pub ms: bool,
}
impl<'a> IRPrintable<'a> for ImageType {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        todo!()
    }
}

