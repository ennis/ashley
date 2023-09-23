use crate::{
    db::ModuleId,
    item::{DefId, FunctionId, Scope, StructId, TypeRes, ValueRes},
    ty::TypeCtxt,
};
use ashley::CompilerDb;
use std::sync::Arc;

#[derive(Clone)]
pub(crate) struct Resolver {
    scopes: Vec<Arc<Scope>>,
    tyctxt: Arc<TypeCtxt>,
}

impl Resolver {
    pub(crate) fn new(tyctxt: Arc<TypeCtxt>) -> Resolver {
        Resolver { scopes: vec![], tyctxt }
    }

    pub(crate) fn resolve_type_name(&self, name: &str) -> Option<TypeRes> {
        //dump_scope_stack(scopes);
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
        //dump_scope_stack(scopes);
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.resolve_value_name(name) {
                return Some(var.clone());
            }
        }
        None
    }

    pub(crate) fn push_scope(&mut self, scope: Arc<Scope>) {
        self.scopes.push(scope);
    }
}

impl ModuleId {
    pub(crate) fn resolver(&self, compiler: &dyn CompilerDb) -> Resolver {
        let tyctxt = compiler.tyctxt();
        let mut resolver = Resolver::new(tyctxt);
        //resolver.push_scope(compiler.builtin_scope());
        todo!("resolver module scope");
        //resolver.push_scope(compiler.module_scope(*self));

        resolver
    }
}

impl DefId {
    pub(crate) fn resolver(&self, compiler: &dyn CompilerDb) -> Resolver {
        self.module().resolver(compiler)
    }
}
