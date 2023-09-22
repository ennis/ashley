use crate::ty::{TyOwnerId, Type};

pub enum TyDiagnostic {
    // ------ Type resolution ------
    UnresolvedType {
        type_owner: TyOwnerId,
    },
    InvalidArrayStride {
        type_owner: TyOwnerId,
        elem_ty_size: u32,
        stride: u32,
    },
    ArrayStrideOnOpaqueType {
        type_owner: TyOwnerId,
        element_type: Type,
    },
    Unimplemented {
        type_owner: TyOwnerId,
    },
}
