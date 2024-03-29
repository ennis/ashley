// TODO: move this (along with Resolver) out of "def" and into "ty", we don't do name resolution at this stage anyway
use crate::{
    builtins::{r#mod, BuiltinOperationPtr},
    db::ModuleId,
    def,
    def::{AstId, ConstExprId, DefLoc, FunctionId, FunctionLoc, GlobalId, GlobalLoc, StructId, StructLoc},
    syntax::ast,
    ty,
    ty::Type,
    CompilerDb,
};
use ashley::def::IndexEntry;
use ashley_data_structures::Id;
use std::{collections::HashMap, sync::Arc};

////////////////////////////////////////////////////////////////////////////////////////////////////

///
#[derive(Clone, Eq, PartialEq)]
pub struct Scope {
    values: HashMap<String, ValueRes>,
    types: HashMap<String, TypeRes>,
}

pub(crate) struct OverloadSet {
    functions: Vec<FunctionLoc>,
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
    /// The name resolves to a local variable in the current scope.
    Local(Id<ty::body::LocalVar>),
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

    pub(crate) fn add_local_var(&mut self, name: &str, local_var: Id<ty::body::LocalVar>) {
        // NOTE: shadowing is OK
        self.values.insert(name.to_string(), ValueRes::Local(local_var));
    }

    pub(crate) fn resolve_type_name(&self, name: &str) -> Option<TypeRes> {
        self.types.get(name).cloned()
    }

    pub(crate) fn resolve_value_name(&self, name: &str) -> Option<ValueRes> {
        self.values.get(name).cloned()
    }

    /// Returns an iterator over all structs defined in the module.
    pub fn structs<'a>(&'a self) -> impl Iterator<Item = (&'a str, StructId)> + 'a {
        self.types.iter().filter_map(|(name, ty_res)| match ty_res {
            TypeRes::Struct(s) => Some((name.as_str(), *s)),
            TypeRes::Primitive(_) => None,
        })
    }

    /// Returns an iterator over all global variables defined in the module.
    pub fn globals<'a>(&'a self) -> impl Iterator<Item = (&'a str, GlobalId)> + 'a {
        self.values.iter().filter_map(|(name, value_res)| match value_res {
            ValueRes::Global(g) => Some((name.as_str(), *g)),
            _ => None,
        })
    }

    /// Returns an iterator over all functions defined in the module.
    pub fn functions<'a>(&'a self) -> impl Iterator<Item = (&'a str, FunctionId)> + 'a {
        self.values.iter().filter_map(|(name, value_res)| match value_res {
            ValueRes::Function(f) => Some((name.as_str(), *f)),
            _ => None,
        })
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
            // there is a operation called "r#mod" because it conflicts with the rust keyword (mod)
            self.values.insert(
                b.name.trim_start_matches("r#").to_string(),
                ValueRes::BuiltinFunction(*b),
            );
        }
    }
}

pub(crate) fn scope_for_body_query(compiler: &dyn CompilerDb, body: ConstExprId) -> Scope {
    todo!()
}

pub(crate) fn scope_for_definition_query(compiler: &dyn CompilerDb, def: DefLoc) -> Scope {
    module_scope_query(compiler, def.module())
}

pub(crate) fn module_scope_query(compiler: &dyn CompilerDb, module: ModuleId) -> Scope {
    // this query will re-run when the module items have changed
    let index = module.index(compiler);

    let mut scope = Scope::new();
    //scope.add_builtin_functions();

    for def in index.definitions.iter() {
        match *def {
            IndexEntry::Struct { ref name, ast_id } => {
                let id = compiler.struct_id(StructLoc { module, strukt: ast_id });
                scope.types.insert(name.clone(), TypeRes::Struct(id));
            }
            IndexEntry::Function { ref name, ast_id } => {
                let id = compiler.function_id(FunctionLoc {
                    module,
                    function: ast_id,
                });
                scope.values.insert(name.clone(), ValueRes::Function(id));
            }
            IndexEntry::Global { ref name, ast_id } => {
                let id = compiler.global_id(GlobalLoc { module, global: ast_id });
                scope.values.insert(name.clone(), ValueRes::Global(id));
            }
            IndexEntry::Import { .. } => {
                // TODO resolve imports
            }
        }
    }

    /*for (struct_id, struct_data) in index.structs.iter_full() {
        let id = compiler.struct_id(StructLoc {
            module,
            strukt: struct_id,
        });
        scope.types.insert(struct_data.name.clone(), TypeRes::Struct(id));
    }
    for (function_id, function_data) in index.functions.iter_full() {
        let id = compiler.function_id(FunctionLoc {
            module,
            function: function_id,
        });
        scope.values.insert(function_data.name.clone(), ValueRes::Function(id));
    }
    for (global_id, global_data) in index.globals.iter_full() {
        let id = compiler.global_id(GlobalLoc {
            module,
            global: global_id,
        });
        scope.values.insert(global_data.name.clone(), ValueRes::Global(id));
    }*/

    scope
}
