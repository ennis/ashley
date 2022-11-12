//! Common types & operations.

use crate::hir::ir::HasTypeId;

/// Unknown type.
///
/// Used in places where the type is not known, not yet inferred, or invalid.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct UnknownType;
unsafe impl HasTypeId for UnknownType { const ID: u32 = 0; }
