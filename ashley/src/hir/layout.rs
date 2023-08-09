//! Utilities to compute the std140 GLSL layout of types.
use crate::{
    hir::{types::ScalarType, Module, Type, TypeData},
    utils::round_up,
};
use thiserror::Error;

////////////////////////////////////////////////////////////////////////////////////////////////////
// Layout
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Contains information about the layout of a type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Layout {
    /// Alignment
    pub align: u32,
    /// Byte size
    pub size: u32,
}

impl Layout {
    /// Creates a new layout for a scalar element with the specified size and alignment.
    pub const fn new(size: u32, align: u32) -> Layout {
        Layout { align, size }
    }
}

/// Layout of the fields of a struct type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructLayout {
    /// Offsets of each field.
    pub offsets: Vec<u32>,
    /// Individual layout information of each field.
    /// TODO: not sure this is necessary: it can be derived easily for scalar types and should be specified explicitly for nested structs and arrays
    pub layouts: Vec<Layout>,
}

/*impl StructLayout {
    pub fn std140<'a>(
        m: &Module,
        fields: impl Iterator<Item = &'a TypeData<'static>>,
    ) -> Result<StructLayout, LayoutError> {
        let (_size, _align, layout) = std140_struct_layout(m, fields)?;
        Ok(layout)
    }
}*/

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

/*fn std140_array_layout(m: &Module, elem_ty: &TypeData, arraylen: u32) -> Result<(u32, u32, ArrayLayout), LayoutError> {
    let elem_layout = std140_layout(m, elem_ty)?;
    // alignment = column type align rounded up to vec4 align (16 bytes)
    let base_align = round_up(elem_layout.align, 16);
    let stride = round_up(elem_layout.size, elem_layout.align);
    // total array size = num columns * stride, rounded up to the next multiple of the base alignment.
    // actually the spec says nothing about the 'size' of an element, only about the alignment
    // of the next element in the structure.
    let array_size = round_up(arraylen * stride, base_align);

    Ok((array_size, base_align, ArrayLayout { elem_layout, stride }))
}*/

/*/// Returns the size and alignment of a structure-type member.
fn std_struct_member_layout<'a>(
    m: &Module,
    fields: impl Iterator<Item = &'a TypeData<'static>>,
) -> Result<(u32, u32), LayoutError> {
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
        .map(|field| compute_layout(m, field))
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

    Ok((size, n))
}*/

/*pub(crate) fn std140_align_member(m: &Module, ty: &TypeData, current_offset: &mut u32) -> Result<u32, LayoutError> {
    let layout = std140_layout(m, ty)?;
    *current_offset = round_up(*current_offset, layout.align);
    let offset = *current_offset;
    *current_offset += layout.size;
    Ok(offset)
}*/

/*/// Computes the layout of a TypeData, using std140 rules.
pub(crate) fn std140_layout(m: &Module, ty: &TypeData) -> Result<Layout, LayoutError> {
    match *ty {
        TypeData::Scalar(s) => Ok(std140_scalar_layout(s)),
        TypeData::Vector(elem_ty, len) => Ok(std140_vector_layout(elem_ty, len)),
        TypeData::Matrix {
            component_type,
            columns,
            rows,
            stride,
        } => {
            stride * columns * std
            /*let (size, align, layout) =
                std140_array_layout(m, &TypeData::Vector(component_type, rows), columns as u32)?;
            Ok(Layout {
                size,
                align,
                inner: Some(Box::new(InnerLayout::Array(layout))),
            })*/
        }
        TypeData::Array {
            element_type,
            size,
            stride,
        } => {
            if let Some(stride) = stride {
                todo!()
            } else {
                let tydata = m.type_data(element_type);
                match tydata {
                    TypeData::Scalar(_) | TypeData::Vector { .. } | TypeData::Struct { .. } => {
                        let (size, align, layout) = std140_array_layout(m, tydata, size)?;
                        Ok(Layout {
                            size,
                            align,
                            inner: Some(Box::new(InnerLayout::Array(layout))),
                        })
                    }
                    ty => panic!("unsupported array element type: {:?}", ty),
                }
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
}*/

/*/// Calculates the size and alignment of the specified type.
pub(crate) fn compute_layout(m: &Module, ty: &TypeData) -> Result<Layout, LayoutError> {
    match *ty {
        TypeData::Scalar(s) => Ok(std_scalar_type_layout(s)),
        TypeData::Vector(elem_ty, len) => Ok(std_vector_type_layout(elem_ty, len)),
        TypeData::Matrix {
            component_type,
            columns,
            rows: _,
            stride,
        } => {
            let size = std_scalar_type_layout(component_type).size * stride * columns;
            let align = stride;
            Ok(Layout { size, align })
        }
        TypeData::Array {
            element_type: _,
            size,
            stride,
        } => {
            let stride = if let Some(stride) = stride {
                stride
            } else {
                return Err(LayoutError::OpaqueType);
            };

            Ok(Layout {
                align: stride,
                size: size * stride,
            })
        }
        TypeData::Struct(ref ty) => {
            let (size, align, layout) = std140_struct_layout(m, ty.fields.iter().map(|f| &m.types[f.ty]))?;
            Ok(Layout { size, align })
        }
        ref ty => Err(LayoutError::OpaqueType),
    }
}
*/
