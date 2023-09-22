use crate::{
    ir,
    ir::{types::ScalarType, Module, Type, TypeData},
};
use std::mem;

/// Trait implemented by types that can be shared with shaders.
pub trait MemoryLayout: Sized + 'static {
    fn hir_type(hir: &mut ir::Module) -> ir::Type;
}

impl MemoryLayout for f32 {
    fn hir_type(hir: &mut Module) -> Type {
        hir.ty_float()
    }
}

impl MemoryLayout for i32 {
    fn hir_type(hir: &mut Module) -> Type {
        hir.ty_int()
    }
}

impl MemoryLayout for u32 {
    fn hir_type(hir: &mut Module) -> Type {
        hir.ty_uint()
    }
}

impl<T: MemoryLayout, const N: usize> MemoryLayout for [T; N] {
    fn hir_type(hir: &mut Module) -> Type {
        let element_type = T::hir_type(hir);
        hir.define_type(TypeData::Array {
            element_type,
            size: N as u32,
            stride: Some(mem::size_of::<T>() as u32), // FIXME: this is not guaranteed: https://rust-lang.github.io/unsafe-code-guidelines/layout/arrays-and-slices.html
        })
    }
}

macro_rules! std140_aligned_types {
    ( $($(#[$m:meta])* $name:ident($t:ty) => $hir:expr;)* ) => {
        $(
            $(#[$m])*
            #[repr(C, align(16))]
            pub struct $name(pub $t);

            impl From<$t> for $name {
                fn from(v: $t) -> $name {
                    $name(v)
                }
            }

            impl MemoryLayout for $name {
                fn hir_type(hir: &mut Module) -> Type {
                    hir.define_type($hir)
                }
            }
        )*
    };
}

std140_aligned_types!(
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    Std140Int(i32) => TypeData::Scalar(ScalarType::Int);

    #[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
    Std140Float(f32) => TypeData::Scalar(ScalarType::Float);
);

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub struct Std140Vec4(pub [f32; 4]);

impl MemoryLayout for Std140Vec4 {
    fn hir_type(hir: &mut Module) -> Type {
        hir.ty_vec4()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Std140IVec4(pub [i32; 4]);

impl MemoryLayout for Std140IVec4 {
    fn hir_type(hir: &mut Module) -> Type {
        hir.ty_ivec4()
    }
}
