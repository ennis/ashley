//! Utilities to compute the std140 GLSL layout of types.
use crate::hir::{types::ScalarType, Module, Type, TypeData};
use thiserror::Error;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Layout
////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Error)]
pub enum LayoutError {
    #[error("encountered and opaque or unrepresentable type")]
    OpaqueType,
}

fn round_up(value: u32, multiple: u32) -> u32 {
    if multiple == 0 {
        return value;
    }
    let remainder = value % multiple;
    if remainder == 0 {
        return value;
    }
    value + multiple - remainder
}

/// Contains information about the layout of a type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Layout {
    /// Alignment
    pub align: u32,
    /// Byte size
    pub size: u32,
    /// Layout of the contents of the type, for array or structs
    pub inner: Option<Box<InnerLayout>>,
}

impl Layout {
    /// Creates a new layout for a scalar element with the specified size and alignment.
    pub const fn with_size_align(size: u32, align: u32) -> Layout {
        Layout {
            align,
            size,
            inner: None,
        }
    }
}

/// Layout of the fields of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructLayout {
    /// Offsets of each field.
    pub offsets: Vec<u32>,
    /// Individual layout information of each field.
    pub layouts: Vec<Layout>,
}

impl StructLayout {
    pub fn std140<'a>(
        m: &Module,
        fields: impl Iterator<Item = &'a TypeData<'static>>,
    ) -> Result<StructLayout, LayoutError> {
        let (_size, _align, layout) = std140_struct_layout(m, fields)?;
        Ok(layout)
    }
}

/// Layout of the array elements of a array type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ArrayLayout {
    /// Layout of individual array elements.
    pub elem_layout: Layout,
    /// Number of bytes between consecutive array elements.
    pub stride: u32,
}

/// Layout information for arrays or structs.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum InnerLayout {
    Array(ArrayLayout),
    Struct(StructLayout),
}

fn std140_array_layout(m: &Module, elem_ty: &TypeData, arraylen: u32) -> Result<(u32, u32, ArrayLayout), LayoutError> {
    let elem_layout = std140_layout(m, elem_ty)?;
    // alignment = column type align rounded up to vec4 align (16 bytes)
    let base_align = round_up(elem_layout.align, 16);
    let stride = round_up(elem_layout.size, elem_layout.align);
    // total array size = num columns * stride, rounded up to the next multiple of the base alignment.
    // actually the spec says nothing about the 'size' of an element, only about the alignment
    // of the next element in the structure.
    let array_size = round_up(arraylen * stride, base_align);

    Ok((array_size, base_align, ArrayLayout { elem_layout, stride }))
}

fn std140_struct_layout<'a>(
    m: &Module,
    fields: impl Iterator<Item = &'a TypeData<'static>>,
) -> Result<(u32, u32, StructLayout), LayoutError> {
    /* If the member is a structure, the base alignment of the structure is N,
    where N is the largest base alignment value of any of its members,
    and rounded up to the base alignment of a vec4.
    The individual members of this sub-structure are then assigned offsets by applying this set of rules recursively,
    where the base offset of the first member of the sub-structure is equal to the aligned offset of the structure.
    The structure may have padding at the end;
    the base offset of the member following the sub-structure is rounded up to the next multiple of the base alignment of the structure.
    */
    // TODO: zero-sized structures?

    let layouts = fields
        .map(|field| std140_layout(m, field))
        .collect::<Result<Vec<_>, _>>()?;
    let n = layouts.iter().map(|l| l.align).max().unwrap_or(0);
    if n == 0 {
        // skip, no members
        return Ok((
            0,
            0,
            StructLayout {
                offsets: vec![],
                layouts: vec![],
            },
        ));
    }

    // round up to base alignment of vec4
    let n = round_up(n, 16);

    // compute field offsets
    let mut offsets = vec![0; layouts.len()];
    let mut off = 0;
    for i in 0..layouts.len() {
        offsets[i] = off;
        off += layouts[i].size;
    }

    // round up total size to base align
    let size = round_up(off, n);

    Ok((size, n, StructLayout { layouts, offsets }))
}

fn std140_scalar_layout(s: ScalarType) -> Layout {
    match s {
        ScalarType::Int | ScalarType::UnsignedInt | ScalarType::Float => Layout {
            size: 4,
            align: 4,
            inner: None,
        },
        _ => unimplemented!("scalar type layout"),
    }
}

fn std140_vector_layout(prim_ty: ScalarType, len: u8) -> Layout {
    let Layout { size: n, .. } = std140_scalar_layout(prim_ty);
    match len {
        2 => Layout {
            align: 2 * n,
            size: 2 * n,
            inner: None,
        },
        3 => Layout {
            align: 4 * n,
            size: 3 * n,
            inner: None,
        },
        4 => Layout {
            align: 4 * n,
            size: 4 * n,
            inner: None,
        },
        _ => panic!("unsupported vector size"),
    }
}

pub(crate) fn std140_align_member(m: &Module, ty: &TypeData, current_offset: &mut u32) -> Result<u32, LayoutError> {
    let layout = std140_layout(m, ty)?;
    *current_offset = round_up(*current_offset, layout.align);
    let offset = *current_offset;
    *current_offset += layout.size;
    Ok(offset)
}

/// Computes the layout of a TypeData, using std140 rules.
pub(crate) fn std140_layout(m: &Module, ty: &TypeData) -> Result<Layout, LayoutError> {
    match *ty {
        TypeData::Scalar(s) => Ok(std140_scalar_layout(s)),
        TypeData::Vector(elem_ty, len) => Ok(std140_vector_layout(elem_ty, len)),
        TypeData::Matrix {
            component_type,
            columns,
            rows,
        } => {
            let (size, align, layout) =
                std140_array_layout(m, &TypeData::Vector(component_type, rows), columns as u32)?;
            Ok(Layout {
                size,
                align,
                inner: Some(Box::new(InnerLayout::Array(layout))),
            })
        }
        TypeData::Array(elem_ty, len) => {
            let tydata = m.type_data(elem_ty);
            match tydata {
                TypeData::Scalar(_) | TypeData::Vector { .. } | TypeData::Struct { .. } => {
                    let (size, align, layout) = std140_array_layout(m, tydata, len)?;
                    Ok(Layout {
                        size,
                        align,
                        inner: Some(Box::new(InnerLayout::Array(layout))),
                    })
                }
                ty => panic!("unsupported array element type: {:?}", ty),
            }
        }
        TypeData::Struct(ref ty) => {
            let (size, align, layout) = std140_struct_layout(m, ty.fields.iter().map(|f| &m.types[f.ty]))?;
            Ok(Layout {
                size,
                align,
                inner: Some(Box::new(InnerLayout::Struct(layout))),
            })
        }
        ref ty => Err(LayoutError::OpaqueType),
    }
}
