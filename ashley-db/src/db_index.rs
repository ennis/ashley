use crate::{Index, TableIndex};

/// A global database index that identifies a value in a database, across all tables.
///
/// It's a combination of a `TableIndex` that identifies the table, and a `u32` index that identifies the value within the table.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct DbIndex {
    /// Identifies the table.
    pub table: TableIndex,
    /// Identifies the value within the table.
    pub value: Index,
}

impl DbIndex {
    pub const fn new(table: TableIndex, value: Index) -> DbIndex {
        DbIndex { table, value }
    }
}
