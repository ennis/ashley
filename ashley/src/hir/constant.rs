use crate::hir::{Constant, Type};
use ordered_float::OrderedFloat;

type F32Ord = OrderedFloat<f32>;
type F64Ord = OrderedFloat<f64>;

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ConstantData {
    F32(F32Ord),
    F64(F64Ord),

    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Bool(bool),
    Composite { ty: Type, constituents: Vec<Constant> },
}
