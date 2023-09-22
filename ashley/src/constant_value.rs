use ordered_float::OrderedFloat;

/// Represents a result of constant evaluation.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ConstantValue {
    /// 32-bit integer. Can be interpreted as signed or unsigned depending on the expected type,
    /// or the inferred type of the constant expression that produced the value.
    Int(u32),
    /// 64-bit integer.
    Int64(u64),
    /// 32-bit float.
    Float(OrderedFloat<f32>),
    /// 64-bit float.
    Double(OrderedFloat<f64>),
    /// String constant.
    String(String),
    /// Boolean constant.
    Bool(bool),
}

impl ConstantValue {
    ///
    pub fn to_i32(&self) -> Option<i32> {
        match self {
            ConstantValue::Int(v) => Some(*v as i32),
            _ => None,
        }
    }

    pub fn to_u32(&self) -> Option<u32> {
        match self {
            ConstantValue::Int(v) => Some(*v),
            _ => None,
        }
    }

    pub fn to_float(&self) -> Option<f32> {
        match self {
            ConstantValue::Float(v) => Some(v.0),
            _ => None,
        }
    }
    pub fn to_double(&self) -> Option<f64> {
        match self {
            ConstantValue::Float(v) => Some(v.0 as f64),
            ConstantValue::Double(v) => Some(v.0),
            _ => None,
        }
    }

    pub fn to_bool(&self) -> Option<bool> {
        match self {
            ConstantValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn to_string(&self) -> Option<&str> {
        match self {
            ConstantValue::String(b) => Some(b),
            _ => None,
        }
    }
}

impl TryFrom<ConstantValue> for u32 {
    type Error = ();

    fn try_from(value: ConstantValue) -> Result<Self, ()> {
        value.to_u32().ok_or(())
    }
}
impl TryFrom<ConstantValue> for bool {
    type Error = ();

    fn try_from(value: ConstantValue) -> Result<bool, ()> {
        value.to_bool().ok_or(())
    }
}
