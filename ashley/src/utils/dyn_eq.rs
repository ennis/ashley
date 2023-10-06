use std::any::Any;

/// Utility trait for object-safe equality comparison between `Any` objects.
pub trait DynEq {
    fn any_eq(&self, other: &dyn Any) -> bool;
}

impl<T> DynEq for T
where
    T: Any + Eq,
{
    fn any_eq(&self, other: &dyn Any) -> bool {
        if let Some(other) = other.downcast_ref::<T>() {
            self == other
        } else {
            false
        }
    }
}
