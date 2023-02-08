//! Typed AST representation
mod builtins;
mod consteval;
mod def;
mod expr;
mod item;
mod overload;
mod scope;
mod stmt;
mod swizzle;
pub mod ty;

pub use def::{Def, Qualifier, Visibility};
pub use expr::Expr;
pub use ty::{FunctionType, ScalarType, Type, TypeKind};

use crate::{
    builtins::BuiltinTypes,
    diagnostic::Diagnostics,
    syntax::ast,
    tast::{
        scope::{Scope},
        stmt::Stmt,
    },
    utils::{Id, TypedVec},
};
use std::{
    collections::HashSet,
    hash::Hash,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

// TODO: separate the items / types from the bodies of functions and initializers

pub type ExprId = Id<Expr>;
pub type StmtId = Id<Stmt>;
pub type BlockId = Id<Block>;
pub type PackageImportId = Id<Package>;
pub type LocalDefId = Id<Def>;
pub type LocalVarId = Id<LocalVar>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct DefId {
    pub package: Option<PackageImportId>,
    pub local_def: LocalDefId,
}

impl From<LocalDefId> for DefId {
    fn from(local_def: LocalDefId) -> Self {
        DefId {
            package: None,
            local_def,
        }
    }
}

pub struct LocalVar {
    pub name: String,
    pub ty: Type,
}

pub struct Block {
    pub stmts: Vec<StmtId>,
}

impl Block {
    pub fn new() -> Block {
        Block { stmts: Vec::new() }
    }
}

pub struct TypedBody {
    pub stmts: TypedVec<Stmt>,
    pub exprs: TypedVec<Expr>,
    pub local_vars: TypedVec<LocalVar>,
    pub blocks: TypedVec<Block>,
}

impl TypedBody {
    pub fn new() -> TypedBody {
        TypedBody {
            stmts: TypedVec::new(),
            exprs: TypedVec::new(),
            local_vars: TypedVec::new(),
            blocks: TypedVec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Package {
    pub defs: TypedVec<Def, LocalDefId>,
}

impl Package {
    /// Imports all the definitions of this package into the given scope.
    pub(crate) fn import(&self, scope: &mut Scope) {
        for (id, def) in self.defs.iter_full() {
            scope.add_def(DefId::from(id), def);
        }
    }
}

/// The API is simply name (+arguments) -> Package (set of definitions).
///
/// The resolver is responsible for looking up the package name in the filesystem, or other sources
/// (it could be a procedurally generated package).
pub trait PackageResolver {
    fn resolve(&self, name: &str) -> Option<Package>;
}

pub struct DummyPackageResolver;

impl PackageResolver for DummyPackageResolver {
    fn resolve(&self, _name: &str) -> Option<Package> {
        None
    }
}

trait IdentExt {
    // TODO rename this to to_unique_name
    fn to_string_opt(&self) -> String;
}

impl IdentExt for Option<ast::Ident> {
    fn to_string_opt(&self) -> String {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        self.as_ref()
            .map(|ident| ident.text().to_string())
            .unwrap_or_else(|| format!("__anon_{}", COUNTER.fetch_add(1, Ordering::Relaxed)))
    }
}

/// Represents a type-checked module.
///
/// Bodies of items (functions and initializers) are type-checked on-demand.
#[derive(Debug)]
pub struct Module {
    /// Imported packages.
    packages: TypedVec<Package, PackageImportId>,
    /// All definitions in this module.
    defs: TypedVec<Def>,
}

impl Module {
    /// Creates a new module.
    pub fn new() -> Module {
        Module {
            packages: Default::default(),
            defs: Default::default(),
        }
    }

    /// Returns the definition with the given ID.
    pub fn def(&self, id: DefId) -> &Def {
        match id.package {
            Some(package) => &self.packages[package].defs[id.local_def],
            None => &self.defs[id.local_def],
        }
    }
}

/// Types interner.
pub struct Types {
    types: HashSet<Arc<TypeKind>>,
}

impl Types {
    pub fn new() -> Self {
        Types { types: HashSet::new() }
    }

    /// Interns a type.
    pub fn intern(&mut self, kind: impl Into<TypeKind>) -> Type {
        // TODO replace with get_or_insert_owned once stabilized
        let kind = kind.into();
        if let Some(ty) = self.types.get(&kind) {
            Type(ty.clone())
        } else {
            let ty = Arc::new(kind);
            self.types.insert(ty.clone());
            Type(ty)
        }
    }
}

/// Context for type-checking a single source file.
pub(crate) struct TypeCheckCtxt<'a, 'diag> {
    module: &'a mut Module,
    diag: &'a mut Diagnostics<'diag>,
    tyctxt: &'a mut TypeCtxt,
    package_resolver: &'a mut dyn PackageResolver,
    scopes: Vec<Scope>,
}

pub struct TypeCtxt {
    /// Interned set of types.
    types: Types,
    builtins: BuiltinTypes,
    error: Type,
}

impl TypeCtxt {
    pub fn new() -> Self {
        let mut types = Types::new();
        let error_type = types.intern(TypeKind::Error);
        let builtin_types = BuiltinTypes::new(&mut types);
        TypeCtxt {
            types,
            builtins: builtin_types,
            error: error_type,
        }
    }

    pub fn ty(&mut self, kind: impl Into<TypeKind>) -> Type {
        self.types.intern(kind)
    }

    pub fn typecheck_items(
        &mut self,
        ast: ast::Root,
        package_resolver: &mut dyn PackageResolver,
        diag: &mut Diagnostics,
    ) -> Module {
        let mut module = Module::new();
        diag.set_current_source(ast.source_id);
        let mut ctxt = TypeCheckCtxt {
            module: &mut module,
            diag,
            tyctxt: self,
            package_resolver,
            scopes: Vec::new(),
        };
        ctxt.typecheck_module(&ast.module);
        module
    }

    pub fn typecheck_item_body(&mut self, module: &Module, def: LocalDefId, diag: &mut Diagnostics) -> TypedBody {
        let mut scope = Scope::new();
        // bring package contents in scope
        for package in module.packages.iter() {
            package.import(&mut scope);
        }
        // reconstruct the set of visible declarations before the item body (there are no forward references in GLSL)
        for def_index in 0..def.index() {
            let id = LocalDefId::from_index(def_index);
            let def = &module.defs[id];
            scope.add_def(DefId::from(id), def);
        }

        let mut typed_body = TypedBody::new();
        let error_type = self.ty(TypeKind::Error);

        let mut ctxt = TypeCheckBodyCtxt {
            module,
            diag,
            tyctxt: self,
            scopes: vec![scope],
            typed_body: &mut typed_body,
            error_type,
        };

        typed_body
    }
}

pub struct TypeCheckBodyCtxt<'a, 'diag> {
    module: &'a Module,
    tyctxt: &'a mut TypeCtxt,
    diag: &'a mut Diagnostics<'diag>,
    scopes: Vec<Scope>,
    typed_body: &'a mut TypedBody,
    error_type: Type,
}

#[cfg(test)]
mod tests {}
