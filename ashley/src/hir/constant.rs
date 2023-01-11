use ordered_float::OrderedFloat;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ConstantData {
    /// A floating-point constant.
    F64(OrderedFloat<f64>),
    /// Integer
    I64(i64),
    /// Boolean
    Boolean(bool),
}
