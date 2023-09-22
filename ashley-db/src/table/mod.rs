mod derived;
mod input;
mod interned;

pub use self::{derived::DerivedQueryTable, input::InputTable, interned::InternTable};

use crate::{Database, Revision};
use ashley_data_structures::{new_index, Idx};
use std::{fmt::Debug, hash::Hash};

new_index! {
    /// Identifies a table in a database.
    pub struct TableIndex;
}

/// Identifies a value in a table.
///
/// Internally it's just a newtype for a u32 index.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct Index(u32);

impl Index {
    pub const fn from_u32(x: u32) -> Self {
        Index(x)
    }
    pub const fn as_u32(self) -> u32 {
        self.0
    }
}

impl Idx for Index {
    fn to_usize(&self) -> usize {
        self.0 as usize
    }
    fn from_usize(index: usize) -> Self {
        Index(index as u32)
    }
}

/// Trait implemented by key types that can be converted to `Index`es.
pub trait AsIndex: Copy + Clone + Eq + PartialEq + Hash + Ord + PartialOrd {
    fn from_index(id: Index) -> Self;
    fn index(self) -> Index;
}

impl AsIndex for () {
    fn from_index(id: Index) -> Self {
        assert_eq!(id.as_u32(), 0);
        ()
    }

    fn index(self) -> Index {
        Index::from_u32(0)
    }
}

/// Defines a new key type for tables.
///
/// The type implements `AsIndex`.
#[macro_export]
macro_rules! new_key_type {
    ($(#[$m:meta])* $v:vis struct $name:ident;) => {
        #[derive(Default, Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
        #[repr(transparent)]
        $(#[$m])* $v struct $name(pub u32);

        impl ::ashley_data_structures::Idx for $name {
            fn to_usize(&self) -> usize {
                self.0 as usize
            }
            fn from_usize(index: usize) -> Self {
                $name(index as u32)
            }
        }

        impl $crate::AsIndex for $name {
            fn from_index(id: $crate::Index) -> Self {
                $name(id.as_u32())
            }

            fn index(self) -> $crate::Index {
                $crate::Index::from_u32(self.0)
            }
        }
    };
}

/// Generic operations on tables.
///
/// # Type arguments
/// * `DB` the database type
pub trait TableOps<DB: Database + ?Sized> {
    /// Returns whether the value at the specified index may have changed since the given revision.
    fn maybe_changed_after(&self, db: &DB, index: Index, revision: Revision) -> bool;

    /// Called after each new revision.
    fn on_new_revision(&mut self, _revision: Revision) {}
}

/// Trait implemented by databases that holds
pub trait HasTables<Tables>: Database {
    fn tables(&self) -> &Tables;
    fn tables_mut(&mut self) -> &mut Tables;
}
