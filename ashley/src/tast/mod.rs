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
mod lower;

pub use def::{Def, Qualifier, Visibility};
pub use expr::Expr;
pub use ty::{FunctionType, ScalarType, Type, TypeKind};

use crate::{
    builtins::PrimitiveTypes,
    diagnostic::Diagnostics,
    syntax::ast,
    tast::{scope::Scope, stmt::Stmt},
    utils::{Id, TypedVec},
};
use ashley::tast::def::DefKind;
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

#[derive(Debug)]
pub struct LocalVar {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<StmtId>,
}

impl Block {
    pub fn new() -> Block {
        Block { stmts: Vec::new() }
    }
}

#[derive(Debug)]
pub struct TypedBody {
    pub stmts: TypedVec<Stmt>,
    pub exprs: TypedVec<Expr>,
    pub local_vars: TypedVec<LocalVar>,
    pub blocks: TypedVec<Block>,
    pub params: Vec<LocalVarId>,
}

impl TypedBody {
    pub fn new() -> TypedBody {
        TypedBody {
            stmts: TypedVec::new(),
            exprs: TypedVec::new(),
            local_vars: TypedVec::new(),
            blocks: TypedVec::new(),
            params: vec![],
        }
    }

    pub fn entry_block(&self) -> BlockId {
        BlockId::from_index(0)
    }

    pub fn root_expr(&self) -> ExprId {
        ExprId::from_index(self.exprs.len() - 1)
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

    /// Iterates over all the local definitions in this module.
    pub fn definitions(&self) -> impl Iterator<Item = (LocalDefId, &Def)> {
        self.defs.iter_full()
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
pub(crate) struct TypeCheckItemCtxt<'a, 'diag> {
    module: &'a mut Module,
    diag: &'a mut Diagnostics<'diag>,
    tyctxt: &'a mut TypeCtxt,
    package_resolver: &'a mut dyn PackageResolver,
    scopes: Vec<Scope>,
}

/// Type-checking context.
pub struct TypeCtxt {
    types: Types,
    prim_tys: PrimitiveTypes,
    error: Type,
}

impl TypeCtxt {
    pub fn new() -> Self {
        let mut types = Types::new();
        let error_type = types.intern(TypeKind::Error);
        let builtin_types = PrimitiveTypes::new(&mut types);
        TypeCtxt {
            types,
            prim_tys: builtin_types,
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
        let mut ctxt = TypeCheckItemCtxt {
            module: &mut module,
            diag,
            tyctxt: self,
            package_resolver,
            scopes: Vec::new(),
        };
        ctxt.define_builtin_functions();
        ctxt.typecheck_module(&ast.module);
        module
    }

    fn build_scope_for_def_body(&mut self, module: &Module, def: LocalDefId) -> Scope {
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
        scope
    }

    pub fn typecheck_body(&mut self, module: &Module, def: LocalDefId, diag: &mut Diagnostics) -> TypedBody {
        let scope = self.build_scope_for_def_body(module, def);

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

        match ctxt.module.def(DefId::from(def)).kind {
            DefKind::Function(ref func) => {
                if let Some(ref ast) = func.ast {
                    if let Some(ref body) = ast.block() {
                        // create local vars for function parameters
                        for param in func.parameters.iter() {
                            let id = ctxt.typed_body.local_vars.push(LocalVar {
                                name: param.name.clone(),
                                ty: param.ty.clone(),
                            });
                            ctxt.typed_body.params.push(id);
                        }
                        ctxt.typecheck_block(body);
                    }
                }
            }
            DefKind::Global(ref global) => {
                if let Some(ref ast) = global.ast {
                    if let Some(ref initializer) = ast.initializer() {
                        if let Some(ref expr) = initializer.expr() {
                            let expr = ctxt.typecheck_expr(expr);
                            ctxt.add_expr(expr);
                        }
                    }
                }
            }
            DefKind::Struct(_) => {}
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

impl<'a, 'diag> TypeCheckBodyCtxt<'a, 'diag> {
    pub(crate) fn convert_type(
        &mut self,
        ty: ast::Type,
    ) -> Type {
        self.tyctxt.convert_type(ty, self.module, &self.scopes, self.diag)
    }
}

#[cfg(test)]
mod tests {}
