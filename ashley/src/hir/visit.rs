use crate::hir::{Attribute};

pub trait Visit<'hir> {
    fn visit_attribute(&self, attr: Attribute<'hir>);
}

pub trait IRVisitable<'hir> {
    fn accept(&self, v: &dyn Visit<'hir>);
}