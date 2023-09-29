use crate::{
    db::ModuleId,
    def::{DefLoc, FunctionLoc, Scope, StructLoc, TypeRes, ValueRes},
    ty::TypeCtxt,
};
use ashley::CompilerDb;
use std::{borrow::Cow, sync::Arc};

#[derive(Clone)]
pub(crate) struct Resolver<'db> {
    scopes: Vec<Cow<'db, Scope>>,
    tyctxt: Arc<TypeCtxt>,
}

impl<'db> Resolver<'db> {
    pub(crate) fn new(tyctxt: Arc<TypeCtxt>) -> Resolver<'db> {
        Resolver { scopes: vec![], tyctxt }
    }

    pub(crate) fn resolve_type_name(&self, name: &str) -> Option<TypeRes> {
        //dump_scope_stack(scopes);
        //trace!("resolving type `{name}`");
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.resolve_type_name(name) {
                return Some(var.clone());
            }
        }
        if let Some(ty) = self.tyctxt.prim_tys.resolve(name) {
            Some(TypeRes::Primitive(ty))
        } else {
            None
        }
    }

    pub(crate) fn resolve_value_name(&self, name: &str) -> Option<ValueRes> {
        //trace!("resolving value `{name}`");
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.resolve_value_name(name) {
                return Some(var.clone());
            }
        }
        None
    }

    pub(crate) fn tyctxt(&self) -> &TypeCtxt {
        &self.tyctxt
    }

    pub(crate) fn push_scope_ref(&mut self, scope: &'db Scope) {
        self.scopes.push(Cow::Borrowed(scope));
    }

    pub(crate) fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(Cow::Owned(scope));
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub(crate) fn last_mut(&mut self) -> Option<&mut Scope> {
        Some(self.scopes.last_mut()?.to_mut())
    }
}

impl ModuleId {
    pub(crate) fn resolver<'db>(&self, compiler: &'db dyn CompilerDb) -> Resolver<'db> {
        let tyctxt = compiler.tyctxt();
        let mut resolver = Resolver::new(tyctxt);
        resolver.push_scope_ref(compiler.builtin_scope(()));
        resolver.push_scope_ref(compiler.module_scope(*self));
        resolver
    }
}

/*
impl DefId {
    pub(crate) fn resolver(&self, compiler: &dyn CompilerDb) -> Resolver {
        self.module().resolver(compiler)
    }
}
*/
