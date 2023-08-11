//! Utilities for computing the memory layout of types.
use crate::{
    diagnostic::{AsSourceLocation, Diagnostics},
    hir::{types::ScalarType, Layout},
    syntax::ast,
    tast::{layout::StdLayoutRules::Std140, Type, TypeKind},
    utils::round_up,
};

/// Error during type layout calculation.
#[derive(Debug, thiserror::Error)]
pub enum LayoutError {
    #[error("encountered and opaque or unrepresentable type")]
    OpaqueType,
    #[error("incompatible layout")]
    IncompatibleLayout,
}

/// Standard layout rules.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum StdLayoutRules {
    /// Components are laid-out according to GLSL std140 rules
    Std140,
    /// Components are laid-out according to GLSL std430 rules
    Std430,
}

/// Returns the size and alignment of a scalar type according to std140 (and std430) GLSL layout rules.
pub(crate) fn std_scalar_type_layout(s: ScalarType) -> Layout {
    match s {
        ScalarType::Int | ScalarType::UnsignedInt | ScalarType::Float | ScalarType::Double => {
            Layout { size: 4, align: 4 }
        }
        _ => unimplemented!("scalar type layout"),
    }
}

/// Returns the size and alignment of a vector type according to std140 (and std430) GLSL layout rules.
pub(crate) fn std_vector_type_layout(prim_ty: ScalarType, len: u8) -> Layout {
    let Layout { size: n, .. } = std_scalar_type_layout(prim_ty);
    match len {
        2 => Layout {
            align: 2 * n,
            size: 2 * n,
        },
        3 => Layout {
            align: 4 * n,
            size: 3 * n,
        },
        4 => Layout {
            align: 4 * n,
            size: 4 * n,
        },
        _ => panic!("unsupported vector size"),
    }
}

/// Computes the stride of an array type.
///
/// If an explicit stride is provided in `stride`, use that.
/// Otherwise, compute it from the element type.
///
/// `layout_rule` doesn't affect the calculated stride, but the function emits an error diagnostic
/// if the calculated stride doesn't conform to the specified layout rule.
pub(crate) fn std_array_stride(
    element_type: &Type,
    stride: Option<u32>,
    layout_rule: StdLayoutRules,
    syntax: Option<ast::Type>,
    diag: &mut Diagnostics,
) -> Result<u32, LayoutError> {
    // get syntax node of the element type
    let elem_syntax = if let Some(ref syntax) = syntax {
        let ast::Type::ArrayType(array_type) = syntax else {
            panic!("invalid type syntax")
        };
        Some(array_type.element_type().expect("invalid type syntax"))
    } else {
        None
    };
    let loc = syntax.as_ref().map(|s| s.source_location());

    let element_type_layout = std_type_layout(&element_type, layout_rule, elem_syntax, diag)?;
    let stride = if let Some(stride) = stride {
        // stride must not be less than the element type size
        let elem_ty_size = element_type_layout.size;
        if stride < elem_ty_size {
            diag.error("invalid array stride")
                .primary_label_opt(loc, "")
                .note(format!(
                    "array stride ({stride}) is less than the element type size ({elem_ty_size})"
                ))
                .emit();
            // ignore user-specified stride if it is invalid
            elem_ty_size
        } else {
            stride
        }
    } else {
        round_up(element_type_layout.size, element_type_layout.align)
    };

    // check for valid std140 stride
    // stride must be a multiple of 16
    if layout_rule == Std140 && stride % 16 != 0 {
        diag.error("invalid array layout")
            .primary_label_opt(loc, "")
            .note(
                "this type appears within a std140 layout struct, consider adding the `stride(16)` qualifier to the type",
            )
            .emit();
    }
    Ok(stride)
}

/// Returns the size and base alignment of the specified type when used as a member of a structure type.
pub(crate) fn std_type_layout(
    ty: &Type,
    layout_rule: StdLayoutRules,
    syntax: Option<ast::Type>,
    diag: &mut Diagnostics,
) -> Result<Layout, LayoutError> {
    let loc = syntax.as_ref().map(|s| s.source_location());

    match *ty.0 {
        TypeKind::Unit => Ok(Layout { align: 0, size: 0 }),
        TypeKind::Scalar(scalar_type) => Ok(std_scalar_type_layout(scalar_type)),
        TypeKind::Vector(scalar_type, len) => Ok(std_vector_type_layout(scalar_type, len)),
        TypeKind::Matrix {
            component_type,
            columns,
            rows,
            stride,
        } => {
            // if std140, check that the stride is a multiple of vec4 size
            if layout_rule == Std140 && stride % 16 != 0 {
                diag.error("invalid matrix layout")
                    .primary_label_opt(loc, "")
                    .note(
                        "this type appears within a std140 layout struct, consider using the `std140` variant of the type",
                    )
                    .emit();
            }

            Ok(Layout {
                align: stride,
                size: stride * (columns as u32),
            })
        }
        TypeKind::Array {
            ref element_type,
            size,
            stride,
        } => {
            // will emit a diagnostic if the calculated layouts are invalid w.r.t layout_rule
            std_array_stride(element_type, stride, layout_rule, None, diag)?;

            if let Some(stride) = stride {
                Ok(Layout {
                    align: stride,
                    size: stride * size,
                })
            } else {
                // No stride = Opaque element type
                Err(LayoutError::OpaqueType)
            }
        }
        TypeKind::RuntimeArray {
            ref element_type,
            stride,
        } => {
            // will emit a diagnostic if the calculated layouts are invalid w.r.t layout_rule
            std_array_stride(element_type, stride, layout_rule, None, diag)?;
            if let Some(stride) = stride {
                Ok(Layout { align: stride, size: 0 })
            } else {
                Err(LayoutError::OpaqueType)
            }
        }
        TypeKind::Struct {
            ref name,
            ref fields,
            def,
            ref offsets,
        } => {
            if let Some(offsets) = offsets {
                let (layout, new_offsets) = std_struct_layout(&fields, layout_rule, diag)?;
                // Compare the offsets computed with the current layout rule to the offsets computed for the standalone type.
                // They should be the same.
                if offsets != &new_offsets {
                    //
                    diag.error(format!(
                        "structure `{name}` layout is incompatible with {layout_rule:?} layout rules"
                    ))
                    .primary_label_opt(loc, "type used here")
                    .emit();
                    Err(LayoutError::IncompatibleLayout)
                    // TODO: show where the offsets differ from the std layout rule
                    // TODO: source location of the offending struct
                    // TODO: chain of uses leading to the struct
                } else {
                    Ok(layout)
                }
            } else {
                Err(LayoutError::OpaqueType)
            }
        }
        TypeKind::Pointer { .. } => {
            todo!("pointer type layout")
        }
        TypeKind::Image(_)
        | TypeKind::Function(_)
        | TypeKind::Sampler
        | TypeKind::SamplerShadow
        | TypeKind::String
        | TypeKind::Unknown
        | TypeKind::Error => Err(LayoutError::OpaqueType),
    }
}

///
/// # Arguments
///
/// * std_layout the layout rule used to compute member offsets
/// * required_layout the layout rule that the offsets must conform to (due to this struct being used in another).
///   This can be different from `std_layout` as long as they produce the same result.
pub(crate) fn std_struct_layout(
    fields: &[crate::tast::ty::StructField],
    std_layout: StdLayoutRules,
    diag: &mut Diagnostics,
) -> Result<(Layout, Vec<u32>), LayoutError> {
    let mut next_offset = 0;
    let mut offsets = Vec::with_capacity(fields.len());
    let mut max_align = 0;

    for field in fields.iter() {
        let member_layout = std_type_layout(&field.ty, std_layout, None, diag)?;

        // compute actual alignment
        let mut actual_alignment = member_layout.align;
        if std_layout == Std140 {
            // round up the alignment to vec4 if layout rule is std140
            actual_alignment = round_up(actual_alignment, 16)
        }
        if let Some(explicit_alignment) = field.align {
            // take into account explicit alignment if any
            // check that it's a power of two
            if !explicit_alignment.is_power_of_two() {
                let name = &field.name;
                // FIXME: source location
                diag.error(format!("alignment of member `{name}` must be a power of two"))
                    .emit();
            }
            actual_alignment = actual_alignment.max(explicit_alignment);
        }

        // compute actual offset
        let mut actual_offset = if let Some(explicit_offset) = field.offset {
            // check that it doesn't overlap with the previous member
            if explicit_offset < next_offset {
                let name = &field.name;
                // FIXME: source location
                diag.error(format!(
                    "member `{name}` (at offset {explicit_offset}) overlaps previous members (of size {next_offset})"
                ))
                //.primary_label_opt(field.ast.clone(), "")
                .emit();
            }
            explicit_offset
        } else {
            next_offset
        };

        // round up to alignment
        actual_offset = round_up(actual_offset, actual_alignment);

        // update offset, next offset and max align
        offsets.push(actual_offset);
        next_offset = actual_offset + member_layout.size;
        max_align = max_align.max(actual_alignment);

        // In addition, if the member is a struct, round up the next offset to the multiple of the alignment.
        // See OpenGL 4.6 spec - 7.6.2.2:
        //
        //         The structure may have padding at the end;
        //         the base offset of the member following the sub-structure is rounded up to
        //         the next multiple of the base alignment of the structure.
        //
        if field.ty.is_struct() {
            next_offset = round_up(next_offset, actual_alignment);
        }
    }

    if std_layout == Std140 {
        // OpenGL 4.6 spec - 7.6.2.2 Standard Uniform Block Layout:
        //
        //      If the member is a structure, the base alignment of the structure is N , where
        //      N is the largest base alignment value of any of its members, and rounded
        //      up to the base alignment of a vec4.
        max_align = round_up(max_align, 16);
    }

    Ok((
        Layout {
            align: max_align,
            size: next_offset,
        },
        offsets,
    ))
}
