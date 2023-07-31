use crate::{
    hir,
    hir::{Module, Type, TypeData},
};

/// Trait implemented by types which can be represented in HIR.
pub trait HirType: 'static {
    fn hir_repr(hir: &mut hir::Module) -> hir::Type;
}

impl HirType for f32 {
    fn hir_repr(hir: &mut Module) -> Type {
        hir.ty_float()
    }
}

impl HirType for i32 {
    fn hir_repr(hir: &mut Module) -> Type {
        hir.ty_int()
    }
}

impl<T: HirType, const N: usize> HirType for [T; N] {
    fn hir_repr(hir: &mut Module) -> Type {
        let elem_ty = T::hir_repr(hir);
        hir.define_type(TypeData::Array(elem_ty, N as u32))
    }
}
