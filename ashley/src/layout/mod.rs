//! Utilities for computing the memory layout of types.
mod diagnostic;

use crate::{
    diagnostic::Span,
    ir::{types::ScalarType, Layout},
    item::{InFile, StructId},
    layout::diagnostic::BlockLayoutDiagnostic,
    session::CompilerDb,
    syntax::{ast, ast::Name},
    utils::round_up,
};
use rowan::ast::AstPtr;
use std::sync::Arc;

/// Error during type layout calculation.
#[derive(Debug, thiserror::Error)]
pub enum LayoutError {
    #[error("encountered and opaque or unrepresentable type")]
    OpaqueType,
    #[error("incompatible layout")]
    IncompatibleLayout,
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

/// Standard layout rules.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum StdLayoutRules {
    /// Components are laid-out according to GLSL std140 rules
    Std140,
    /// Components are laid-out according to GLSL std430 rules
    Std430,
}

fn warn_opaque_type() {
    warn!("opaque type during block layout check");
}

pub type StructFieldPtr = InFile<AstPtr<ast::StructField>>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MemberInfo {
    pub member: Option<StructFieldPtr>,
    pub struct_name: String,
    pub member_name: String,
    pub member_index: usize,
}

pub(crate) struct BlockLayoutCheck<'a> {
    compiler: &'a dyn CompilerDb,
    ref_chain: Vec<MemberInfo>,
    diags: &'a mut Vec<BlockLayoutDiagnostic>,
    layout_rule: StdLayoutRules,
}

impl<'a> BlockLayoutCheck<'a> {
    pub(crate) fn new(
        compiler: &'a dyn CompilerDb,
        layout_rule: StdLayoutRules,
        diags: &'a mut Vec<BlockLayoutDiagnostic>,
    ) -> BlockLayoutCheck<'a> {
        BlockLayoutCheck {
            compiler,
            ref_chain: vec![],
            diags,
            layout_rule,
        }
    }

    /*
    /// Computes the stride of an array type.
    ///
    /// If an explicit stride is provided in `stride`, use that.
    /// Otherwise, compute it from the element type.
    ///
    /// `layout_rule` doesn't affect the calculated stride, but the function emits an error diagnostic
    /// if the calculated stride doesn't conform to the specified layout rule.
    pub(crate) fn check_array_stride(&mut self, ty: &Type, element_type: &Type, stride: Option<u32>) -> u32 {
        let element_type_layout = self.check_type_layout(&element_type);
        let stride = if let Some(stride) = stride {
            // stride must not be less than the element type size
            let elem_ty_size = element_type_layout.size;
            if stride < elem_ty_size {
                // ignore user-specified stride if it is invalid
                // the error should have been reported in `convert_type`
                elem_ty_size
            } else {
                stride
            }
        } else {
            round_up(element_type_layout.size, element_type_layout.align)
        };

        // check for valid std140 stride
        // stride must be a multiple of 16
        if self.layout_rule == Std140 && stride % 16 != 0 {
            self.diags.push(
                BlockLayoutDiagnostic::InvalidStd140ArrayStride {
                    ref_chain: self.ref_chain.clone(),
                    ty: ty.clone(),
                    stride,
                }
                .into(),
            );
            /*compiler.diag_error("invalid array layout")
            .primary_label_opt(member_span, "in this member")
            .note(
                "this type appears within a std140 layout struct, consider adding the `stride(16)` qualifier to the type",
            )
            .emit();*/

            //
        }
        stride
    }

    /// Returns the size and base alignment of the specified type when used as a member of a structure type.
    fn check_type_layout(&mut self, ty: &Type) -> Layout {
        match *ty.0 {
            TypeKind::Unit => Layout { align: 0, size: 0 },
            TypeKind::Scalar(scalar_type) => std_scalar_type_layout(scalar_type),
            TypeKind::Vector(scalar_type, len) => std_vector_type_layout(scalar_type, len),
            TypeKind::Matrix {
                component_type: _,
                columns,
                rows: _,
                stride,
            } => {
                // if std140, check that the stride is a multiple of vec4 size
                if self.layout_rule == Std140 && stride % 16 != 0 {
                    self.diags.push(
                        BlockLayoutDiagnostic::InvalidStd140MatrixLayout {
                            ref_chain: self.ref_chain.clone(),
                            ty: ty.clone(),
                        }
                        .into(),
                    );
                }

                Layout {
                    align: stride,
                    size: stride * (columns as u32),
                }
            }
            TypeKind::Array {
                ref element_type,
                size,
                stride,
            } => {
                // will emit a diagnostic if the calculated layouts are invalid w.r.t layout_rule
                self.check_array_stride(ty, element_type, stride);

                if let Some(stride) = stride {
                    Layout {
                        align: stride,
                        size: stride * size,
                    }
                } else {
                    // opaque type, this is an error but it should have been reported elsewhere
                    warn_opaque_type();
                    // return a dummy layout so that compilation can proceed
                    Layout { align: 1, size: 1 }
                }
            }
            TypeKind::RuntimeArray {
                ref element_type,
                stride,
            } => {
                // will emit a diagnostic if the calculated layouts are invalid w.r.t layout_rule
                self.check_array_stride(ty, element_type, stride);
                if let Some(stride) = stride {
                    Layout { align: stride, size: 0 }
                } else {
                    warn_opaque_type();
                    Layout { align: 1, size: 1 }
                }
            }
            TypeKind::Struct {
                ref name,
                ref fields,
                ref layout,
                ..
            } => {
                if let Some(layout) = layout {
                    // check the provided layout agains the current layout rule
                    let new_layout = self.check_struct_layout(name, &fields, Some(&layout.offsets));
                    // Always return new_layout which is computed under the current layout rules.
                    // This way we don't get cascading errors if the layout mismatch
                    // happens several layers deep.
                    new_layout.layout
                } else {
                    warn_opaque_type();
                    Layout { align: 1, size: 1 }
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
            | TypeKind::Error => {
                warn_opaque_type();
                Layout { align: 1, size: 1 }
            }
        }
    }

    ///
    /// # Arguments
    ///
    /// * std_layout the layout rule used to compute member offsets
    /// * required_layout the layout rule that the offsets must conform to (due to this struct being used in another).
    ///   This can be different from `std_layout` as long as they produce the same result.
    pub(crate) fn check_struct_layout(
        &mut self,
        struct_name: &str,
        fields: &[crate::tast::ty::StructField],
        expected_offsets: Option<&[u32]>,
    ) -> StructLayout {
        let mut next_offset = 0;
        let mut offsets = Vec::with_capacity(fields.len());
        let mut max_align = 0;

        if let Some(expected_offsets) = expected_offsets {
            assert_eq!(expected_offsets.len(), fields.len());
        }

        for (i_field, field) in fields.iter().enumerate() {
            let member_info = MemberInfo {
                member: field.syntax.clone(),
                struct_name: struct_name.to_string(),
                member_name: field.name.clone(),
                member_index: i_field,
            };
            self.ref_chain.push(member_info);

            let member_layout = self.check_type_layout(&field.ty);

            // compute actual alignment
            let mut actual_alignment = member_layout.align;
            if self.layout_rule == Std140 {
                // round up the alignment to vec4 if layout rule is std140
                actual_alignment = round_up(actual_alignment, 16)
            }
            if let Some(explicit_alignment) = field.align {
                // take into account explicit alignment if any
                // check that it's a power of two
                // TODO this should be done when parsing the attribute

                if explicit_alignment.is_power_of_two() {
                    actual_alignment = actual_alignment.max(explicit_alignment);
                } else {
                    // ignore explicit alignment if it's invalid (not a power of two)
                    // it should have been reported before
                    /*errors.push(LayoutDiagnostic::AlignmentNotPowerOfTwo {
                        member: field.syntax.clone(),
                        member_name: field.name.clone(),
                        member_index: i_field,
                        explicit_alignment,
                    });*/
                }
            }

            // compute actual offset
            let mut actual_offset = if let Some(explicit_offset) = field.offset {
                // check that it doesn't overlap with the previous member
                if explicit_offset < next_offset {
                    self.diags.push(
                        BlockLayoutDiagnostic::MemberOverlap {
                            member: self.ref_chain.last().unwrap().clone(),
                            explicit_offset,
                            computed_next_offset: next_offset,
                        }
                        .into(),
                    )
                }
                // ... and that it is correctly aligned
                if (explicit_offset % actual_alignment) != 0 {
                    self.diags.push(
                        BlockLayoutDiagnostic::MisalignedOffset {
                            ref_chain: self.ref_chain.clone(),
                            explicit_offset,
                            required_alignment: actual_alignment,
                        }
                        .into(),
                    )
                }
                explicit_offset
            } else {
                // compute by rounding up next_offset to alignment of the member
                round_up(next_offset, actual_alignment)
            };

            // check offset with what is expected
            if let Some(expected_offsets) = expected_offsets {
                if expected_offsets[i_field] != actual_offset {
                    self.diags.push(
                        BlockLayoutDiagnostic::LayoutMismatch {
                            ref_chain: self.ref_chain.clone(),
                            computed_offset: actual_offset,
                            expected_offset: expected_offsets[i_field],
                        }
                        .into(),
                    )
                }
            }

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

        if self.layout_rule == Std140 {
            // OpenGL 4.6 spec - 7.6.2.2 Standard Uniform Block Layout:
            //
            //      If the member is a structure, the base alignment of the structure is N , where
            //      N is the largest base alignment value of any of its members, and rounded
            //      up to the base alignment of a vec4.
            max_align = round_up(max_align, 16);
        }

        StructLayout {
            offsets,
            layout: Layout {
                align: max_align,
                size: next_offset,
            },
        }
    }*/
}

/*
pub(crate) fn check_struct_layout(
    compiler: &dyn CompilerDb,
    struct_name: &str,
    fields: &[crate::tast::ty::StructField],
    layout_rule: StdLayoutRules,
    expected_offsets: Option<&[u32]>,
    diags: &mut Vec<TyDiagnostic>,
) -> StructLayout {
    let mut ctx = BlockLayoutCheck::new(compiler, layout_rule, diags);
    ctx.check_struct_layout(struct_name, fields, expected_offsets)
}
*/

#[derive(Clone, PartialEq, Eq)]
pub struct LayoutInfo {
    //pub layout: StructLayout,
    pub diagnostics: Vec<BlockLayoutDiagnostic>,
}

pub(crate) fn layout_query(compiler: &dyn CompilerDb, strukt: StructId) -> LayoutInfo {
    todo!()
}
