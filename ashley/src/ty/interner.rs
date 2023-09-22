use crate::ty::{Type, TypeKind};
use std::{
    collections::HashSet,
    sync::{Arc, Mutex},
};

/// Types interner.
pub struct Interner {
    types: Mutex<HashSet<Arc<TypeKind>>>,
}

impl Interner {
    pub fn new() -> Self {
        Interner {
            types: Mutex::new(HashSet::new()),
        }
    }

    /// Interns a type.
    pub fn intern(&self, kind: impl Into<TypeKind>) -> Type {
        // TODO replace with get_or_insert_owned once stabilized
        let kind = kind.into();
        let mut types = self.types.lock().unwrap();
        if let Some(ty) = types.get(&kind) {
            Type(ty.clone())
        } else {
            let ty = Arc::new(kind);
            types.insert(ty.clone());
            Type(ty)
        }
    }
}
