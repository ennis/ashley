use std::{fmt, marker::PhantomData};

/// Insertion point in a linked list.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Cursor<I> {
    /// Points before the specified node.
    Before(I),
    /// Points to the beginning of the list.
    Beginning,
    /// Points after the specified node.
    After(I),
}

/// Linked list node.
#[derive(Debug)]
pub struct ListNode<I, T> {
    pub prev: Option<I>,
    pub next: Option<I>,
    pub data: T,
}

impl<I, T> ListNode<I, T> {
    pub fn new(data: T) -> ListNode<I, T> {
        ListNode {
            prev: None,
            next: None,
            data,
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct ListFirstLast<I> {
    first: I,
    last: I,
}

/// The two ends of a linked list stored in an id_arena.
pub struct List<I, T>(Option<ListFirstLast<I>>, PhantomData<fn() -> T>);

impl<I: Copy, T> Copy for List<I, T> {}

impl<I: Clone, T> Clone for List<I, T> {
    fn clone(&self) -> Self {
        List(self.0.clone(), PhantomData)
    }
}

impl<I: fmt::Debug, T> fmt::Debug for List<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("List").field(&self.0).finish()
    }
}

impl<I, T> Default for List<I, T> {
    fn default() -> Self {
        List(None, PhantomData)
    }
}

impl<I, T> List<I, T>
where
    I: id_arena::ArenaBehavior<Id = I> + Copy,
{
    /// Returns the index of the first element in the list.
    pub fn first(&self) -> Option<I> {
        self.0.map(|x| x.first)
    }

    /// Returns the index of the last element in the list.
    pub fn last(&self) -> Option<I> {
        self.0.map(|x| x.last)
    }

    /// Inserts the specified element before `next`.
    pub fn insert_before(&mut self, this: I, next: I, storage: &mut id_arena::Arena<ListNode<I, T>, I>) {
        if let Some(ref mut ends) = self.0 {
            let prev = storage[next].prev;
            storage[this].prev = prev;
            storage[this].next = Some(next);
            storage[next].prev = Some(this);
            if let Some(prev) = prev {
                storage[prev].next = Some(this);
            } else {
                ends.first = this;
            }
        } else {
            panic!()
        }
    }

    /// Inserts the specified element after `prev`.
    pub fn insert_after(&mut self, this: I, prev: I, storage: &mut id_arena::Arena<ListNode<I, T>, I>) {
        if let Some(ref mut ends) = self.0 {
            let next = storage[prev].next;
            storage[this].prev = Some(prev);
            storage[this].next = next;
            storage[prev].next = Some(this);
            if let Some(next) = next {
                storage[next].prev = Some(this);
            } else {
                ends.last = this;
            }
        } else {
            panic!()
        }
    }

    /// Appends the specified element at the end of the list.
    pub fn append(&mut self, this: I, storage: &mut id_arena::Arena<ListNode<I, T>, I>) {
        if let Some(ref mut ends) = self.0 {
            let prev = ends.last;
            storage[this].prev = Some(prev);
            storage[this].next = None;
            storage[prev].next = Some(this);
            ends.last = this;
        } else {
            self.0 = Some(ListFirstLast {
                first: this,
                last: this,
            });
        }
    }
}
