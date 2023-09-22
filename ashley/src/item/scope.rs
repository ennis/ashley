use crate::{
    builtins::BuiltinOperationPtr,
    item,
    item::{AstId, AttributeOwnerId, DefId, FunctionId, GlobalId, StructId},
    session::ModuleId,
    syntax::ast,
    ty,
    ty::Type,
    CompilerDb,
};
use ashley::item::BodyId;
use ashley_data_structures::Id;
use std::{collections::HashMap, sync::Arc};

////////////////////////////////////////////////////////////////////////////////////////////////////

// Scopes are the set of definitions visible to some piece of code.
//
// They are important for incremental computation because if the scope
// of an item (a "body") hasn't changed, then we don't need to recheck it.

///
#[derive(Clone, Eq, PartialEq)]
pub struct Scope {
    values: HashMap<String, ValueRes>,
    types: HashMap<String, TypeRes>,
}

pub(crate) struct OverloadSet {
    functions: Vec<FunctionId>,
}

/// Resolution of names in the value namespace.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum ValueRes {
    /// The name resolves to a function overload set.
    Function(FunctionId),
    /// The name resolves to a global variable.
    Global(GlobalId),
    /// Builtin function.
    BuiltinFunction(BuiltinOperationPtr),
    // The name resolves to a local variable in the current scope.
    //Local(LocalVarId),
}

/// Resolution of names in the type namespace.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TypeRes {
    ///
    Struct(StructId),
    Primitive(ty::Type),
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new()
    }
}

impl Scope {
    pub(crate) fn new() -> Scope {
        Scope {
            values: Default::default(),
            types: Default::default(),
        }
    }

    pub(crate) fn resolve_type_name(&self, name: &str) -> Option<TypeRes> {
        self.types.get(name).cloned()
    }

    pub(crate) fn resolve_value_name(&self, name: &str) -> Option<ValueRes> {
        self.values.get(name).cloned()
    }

    /*/// Associates a name to a resolution in this scope.
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
    }*/
}

/*
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
*/

/*
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
}*/

impl Scope {
    pub(crate) fn add_builtins(&mut self) {
        for b in crate::builtins::OPERATION_SIGNATURES {
            self.values.insert(b.name.to_string(), ValueRes::BuiltinFunction(*b));
        }
    }
}

pub(crate) fn scope_for_body_query(compiler: &dyn CompilerDb, body: BodyId) -> Scope {
    todo!()
}

pub(crate) fn scope_for_definition_query(compiler: &dyn CompilerDb, def: DefId) -> Scope {
    todo!()
}

pub(crate) fn module_scope_query(compiler: &dyn CompilerDb, module: ModuleId) -> Scope {
    // this query will re-run when the module items have changed
    let items = compiler.module_items(module);

    let mut scope = Scope::new();
    //scope.add_builtin_functions();

    for (_import_id, _import) in items.imports.iter_full() {
        // TODO resolve imports
        todo!("import resolution")
    }
    for (struct_id, struct_data) in items.structs.iter_full() {
        scope.types.insert(
            struct_data.name.clone(),
            TypeRes::Struct(StructId {
                module,
                strukt: struct_id,
            }),
        );
    }
    for (function_id, function_data) in items.functions.iter_full() {
        scope.values.insert(
            function_data.name.clone(),
            ValueRes::Function(FunctionId {
                module,
                function: function_id,
            }),
        );
    }
    for (global_id, global_data) in items.globals.iter_full() {
        scope.values.insert(
            global_data.name.clone(),
            ValueRes::Global(GlobalId {
                module,
                global: global_id,
            }),
        );
    }

    scope
}
