use crate::tast::{def::DefKind, Def, DefId, LocalVarId, Type, TypeCheckBodyCtxt, TypeCtxt};
use std::{collections::HashMap, sync::Arc};

/// Result of scope resolution.
#[derive(Clone, Debug)]
pub(crate) enum Res {
    /// The name resolves to a function overload set.
    OverloadSet(Arc<Vec<DefId>>),
    /// The name resolves to a global variable.
    Global(DefId),
    /// The name resolves to a local variable in the current scope.
    Local(LocalVarId),
    /// The name resolves to a primitive type.
    PrimTy(Type),
}

///
#[derive(Clone)]
pub(crate) struct Scope {
    items: HashMap<String, Res>,
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new()
    }
}

impl Scope {
    pub(crate) fn new() -> Scope {
        Scope {
            items: Default::default(),
        }
    }

    /// Associates a name to a resolution in this scope.
    ///
    /// For functions, use `add_function_overload` instead to merge with existing overload sets.
    pub(crate) fn add(&mut self, name: String, resolution: Res) -> Option<Res> {
        self.items.insert(name, resolution)
    }

    pub(crate) fn add_def(&mut self, def_id: DefId, def: &Def) -> Option<Res> {
        match def.kind {
            DefKind::Function(_) => self.add_function_overload(def.name.clone(), def_id),
            DefKind::Global(_) | DefKind::Struct(_) => self.add(def.name.clone(), Res::Global(def_id)),
        }
    }

    /// Defines a function, or if there's already an overload set with the specified name in this scope, appends the definition to the overload set.
    pub(crate) fn add_function_overload(&mut self, name: String, def_id: DefId) -> Option<Res> {
        if let Some(Res::OverloadSet(ref mut overloads)) = self.items.get_mut(&name) {
            // append to overload set
            Arc::make_mut(overloads).push(def_id);
            return None;
        }
        // this always returns None, otherwise we would append to the overload
        self.items.insert(name, Res::OverloadSet(Arc::new(vec![def_id])))
    }
}

impl TypeCheckBodyCtxt<'_> {
    /// Resolves a name in the current scope
    pub(crate) fn resolve_name(&self, name: &str) -> Option<Res> {
        resolve_name(&self.compiler.tyctxt(), name, &self.scopes)
    }
}

#[allow(dead_code)]
fn dump_scope_stack(scopes: &[Scope]) {
    eprintln!("====== dumping scopes ======");
    for (i, s) in scopes.iter().rev().enumerate() {
        eprintln!("Scope {i}:");
        for (k, res) in s.items.iter() {
            match res {
                Res::OverloadSet(_) => {
                    eprintln!("\t{k} -> overload set");
                }
                Res::Global(_) => {
                    eprintln!("\t{k} -> global");
                }
                Res::Local(_) => {
                    eprintln!("\t{k} -> local");
                }
                Res::PrimTy(_) => {
                    eprintln!("\t{k} -> primitive ty");
                }
            }
        }
    }
}

pub(crate) fn resolve_name(tyctxt: &TypeCtxt, name: &str, scopes: &[Scope]) -> Option<Res> {
    //dump_scope_stack(scopes);
    for scope in scopes.iter().rev() {
        if let Some(var) = scope.items.get(name) {
            return Some(var.clone());
        }
    }
    if let Some(ty) = tyctxt.prim_tys.resolve(name) {
        Some(Res::PrimTy(ty))
    } else {
        None
    }
}
