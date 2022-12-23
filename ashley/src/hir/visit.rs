use crate::hir::{Attr};

pub trait Visit<'hir> {
    fn visit_attribute(&self, attr: Attr<'hir>);
}

pub trait IRVisitable<'hir> {
    fn accept(&self, v: &dyn Visit<'hir>);
}