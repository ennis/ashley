use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
};

/// Associates unique handles to equal values.
pub struct Interner<T, H> {
    items: RefCell<HashMap<T, H>>,
}

impl<T: Eq + Hash, H: Clone> Interner<T, H> {
    pub fn new() -> Interner<T, H> {
        Interner {
            items: Default::default(),
        }
    }

    /// Returns whether this interner contains the specified value.
    pub fn get_handle(&self, value: &T) -> Option<H> {
        self.items.borrow().get(value)
    }

    /// Interns the specified value.
    ///
    /// Returns a reference to the interned object.
    pub fn intern(&self, value: T, make_handle: impl FnOnce() -> H) -> (H, bool) {
        let mut items = self.items.borrow_mut();
        let mut entry = items.entry(value);
        let new_entry = matches!(entry, Entry::Vacant(_));
        (entry.or_insert_with(make_handle).clone(), new_entry)
    }
}

#[cfg(test)]
mod tests {}
