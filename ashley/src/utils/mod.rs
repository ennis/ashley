mod counter;
mod interner;
mod memory_layout;

pub use ashley_derive::MemoryLayout;
pub use counter::Counter;
pub use memory_layout::{MemoryLayout, Std140Float, Std140IVec4, Std140Int, Std140Vec4};
use std::fmt;

pub(crate) use ashley_data_structures::{Id, IndexVec};
pub(crate) use interner::UniqueArena;

/// Helper macro to write a separated list.
///
/// # Example
///
/// ```rust
/// let mut vs = vec![0,1,2,3];
/// let mut string = String::new();
/// write_list!(&mut string, ",", i in vs => { write!(&mut string, "{i}"); } );       
/// assert_eq!(string, "0,1,2,3");
/// ```
// implemented as a macro to avoid borrowing woes
macro_rules! write_list {
    ($w:expr, $sep:literal, $i:ident in $coll:expr => $b:block) => {
        let mut first = true;
        for $i in $coll {
            if !first {
                write!($w, "{}", $sep).unwrap();
            }
            $b
            first = false;
        }
    };
}

pub(crate) use write_list;

/// Utility for displaying a comma-separated list of `Display`-able elements.
pub(crate) struct CommaSeparated<'a, T>(pub(crate) &'a [T]);

// fmt::Display for DisplayCommaSeparated<'a, T>
impl<'a, T: fmt::Display> fmt::Display for CommaSeparated<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                if f.alternate() {
                    write!(f, ", ")?;
                } else {
                    write!(f, ",")?;
                }
            }
            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

// fmt::Display for DisplayCommaSeparated<'a, T>
impl<'a, T: fmt::Debug> fmt::Debug for CommaSeparated<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, item) in self.0.iter().enumerate() {
            if i > 0 {
                if f.alternate() {
                    write!(f, ", ")?;
                } else {
                    write!(f, ",")?;
                }
            }
            write!(f, "{:?}", item)?;
        }
        Ok(())
    }
}

/// Rounds up the value to the specified multiple.
pub(crate) fn round_up(value: u32, multiple: u32) -> u32 {
    if multiple == 0 {
        return value;
    }
    let remainder = value % multiple;
    if remainder == 0 {
        return value;
    }
    value + multiple - remainder
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_write_list() {
        let mut vs = vec![0, 1, 2, 3];
        let mut string = String::new();
        write_list!(string, i in vs => { write!(string, "{i}"); } );
        assert_eq!(string, "0,1,2,3");
    }
}
