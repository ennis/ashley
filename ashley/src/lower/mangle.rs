//! Type mangling
use std::fmt;
use crate::hir;
use crate::hir::TypeData;
use crate::hir::types::ScalarType;

/*///
/// # Examples:
///
/// `vec3[]` => FV3R
/// `vec3[4]` => FV3A4
/// `struct Stroke[]` => S6StrokeR
/// `mat4x3` => M43
///
fn generate_mangled_type_name(module: &hir::Module, ty: hir::Type, out: &mut dyn fmt::Write) -> fmt::Result {
    match module.types[ty] {
        TypeData::Unit => write!(out, "Z")?,
        TypeData::Scalar(s) => match s {
           ScalarType::Float => write!(out, "F")? ,
           ScalarType::Int => write!(out, "I")? ,
           ScalarType::UnsignedInt => write!(out, "U")? ,
           ScalarType::Bool => write!(out, "B")? ,
           ScalarType::Double => write!(out, "D")? ,
        },
        TypeData::Vector(comp_ty, len) => {
            generate_mangled_type_name(out, &TypeData::Primitive(*elem_ty))?;
            write!(out, "V{}", len)?;
        }
        TypeData::Matrix { elem_ty, rows, columns } => {
            generate_mangled_type_name(out, &TypeData::Primitive(*elem_ty))?;
            write!(out, "M{}{}", rows, columns)?;
        }
        TypeData::Array { elem_ty, len } => {
            generate_mangled_type_name(out, elem_ty)?;
            write!(out, "A{}", len)?;
        }
        TypeData::RuntimeArray(elem_ty) => {
            generate_mangled_type_name(out, elem_ty)?;
            write!(out, "R")?;
        }
        TypeData::Struct(s) => {
            write!(out, "T{}{}", s.name.len(), s.name)?;
        }
        TypeData::SampledImage(image) => {
            match image.ms {
                true => write!(out, "Gm")?,
                false => write!(out, "Gs")?,
            };
            generate_mangled_type_name(out, &TypeData::Primitive(image.sampled_ty))?;
            match image.dim {
                ImageDimension::Dim1D => write!(out, "1")?,
                ImageDimension::Dim2D => write!(out, "2")?,
                ImageDimension::Dim3D => write!(out, "3")?,
                ImageDimension::DimCube => write!(out, "C")?,
                ImageDimension::Dim1DArray => write!(out, "A")?,
                ImageDimension::Dim2DArray => write!(out, "B")?,
            }
        }
        TypeData::Image(image) => {
            match image.ms {
                true => write!(out, "Hm")?,
                false => write!(out, "Hs")?,
            };
            generate_mangled_type_name(out, &TypeData::Primitive(image.element_ty))?;
            match image.dim {
                ImageDimension::Dim1D => write!(out, "1")?,
                ImageDimension::Dim2D => write!(out, "2")?,
                ImageDimension::Dim3D => write!(out, "3")?,
                ImageDimension::DimCube => write!(out, "C")?,
                ImageDimension::Dim1DArray => write!(out, "A")?,
                ImageDimension::Dim2DArray => write!(out, "B")?,
            }
        }
        TypeData::Pointer(ty) => {
            generate_mangled_type_name(out, ty);
            write!(out, "P")?
        }
        TypeData::String => write!(out, "Y")?,
        TypeData::Sampler => write!(out, "S")?,
        TypeData::ShadowSampler => write!(out, "W")?,
        TypeData::Unknown => write!(out, "?")?,
    };

    Ok(())
}*/