pub trait Visit<'hir> {
}

pub trait IRVisitable<'hir> {
    fn accept(&self, v: &dyn Visit<'hir>);
}