//! Typed AST representation
mod builtins;
mod consteval;
mod def;
mod expr;
mod item;
mod lower;
mod overload;
mod scope;
mod stmt;
mod swizzle;
pub mod ty;

pub use def::{Def, FunctionDef, Qualifier, Visibility};
pub use expr::Expr;
pub use lower::lower_to_hir;
pub(crate) use scope::Scope;
pub use ty::{FunctionType, ScalarType, Type, TypeKind};

use crate::{
    builtins::PrimitiveTypes,
    package::Package,
    syntax::ast,
    tast::{def::DefKind, stmt::Stmt, ty::convert_type},
    utils::{Id, TypedVec},
    Session,
};
use std::{
    collections::HashSet,
    hash::Hash,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

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
    pub errors: usize,
    pub entry_block: Option<BlockId>,
}

impl TypedBody {
    pub fn new() -> TypedBody {
        TypedBody {
            stmts: TypedVec::new(),
            exprs: TypedVec::new(),
            local_vars: TypedVec::new(),
            blocks: TypedVec::new(),
            params: vec![],
            entry_block: None,
            errors: 0,
        }
    }

    pub fn root_expr(&self) -> ExprId {
        ExprId::from_index(self.exprs.len() - 1)
    }

    pub fn has_errors(&self) -> bool {
        self.errors > 0
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

/// Type-checking context.
///
/// Holds the interner for types, and pre-interned versions of commonly-used types.
pub struct TypeCtxt {
    /// Types interner.
    types: Types,
    /// Pre-interned primitive types.
    prim_tys: PrimitiveTypes,
    /// Error type (used for error recovery).
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
}

/// Context for type-checking a single source file.
pub(crate) struct TypeCheckItemCtxt<'a, 'diag> {
    sess: &'a mut Session<'diag>,
    module: &'a mut Module,
    scopes: Vec<Scope>,
}

impl<'a, 'diag> TypeCheckItemCtxt<'a, 'diag> {
    pub(crate) fn convert_type(&mut self, ty: ast::Type) -> Type {
        convert_type(&mut self.sess.tyctxt, ty, self.module, &self.scopes, &mut self.sess.diag)
    }
}

fn build_scope_for_def_body(module: &Module, def: LocalDefId) -> Scope {
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

pub(crate) fn typecheck_items(sess: &mut Session, ast: ast::Root) -> Module {
    let mut module = Module::new();
    sess.diag.set_current_source(ast.source_id);
    let mut ctxt = TypeCheckItemCtxt {
        sess,
        module: &mut module,
        scopes: Vec::new(),
    };
    ctxt.define_builtin_functions();
    ctxt.typecheck_module(&ast.module);
    module
}

pub(crate) struct TypeCheckBodyCtxt<'a, 'diag> {
    sess: &'a mut Session<'diag>,
    module: &'a Module,
    scopes: Vec<Scope>,
    typed_body: &'a mut TypedBody,
    //error_type: Type,
}

impl<'a, 'diag> TypeCheckBodyCtxt<'a, 'diag> {
    pub(crate) fn convert_type(&mut self, ty: ast::Type) -> Type {
        convert_type(&mut self.sess.tyctxt, ty, self.module, &self.scopes, &mut self.sess.diag)
    }
}


// TODO: some defs do not have bodies, return None for them
pub(crate) fn typecheck_body(sess: &mut Session, module: &Module, def: LocalDefId) -> TypedBody {
    let scope = build_scope_for_def_body(module, def);

    // HACK: to count the number of errors during type-checking, count the difference in number of errors
    let initial_err_count = sess.diag.error_count();

    let mut typed_body = TypedBody::new();
    let mut ctxt = TypeCheckBodyCtxt {
        module,
        sess,
        scopes: vec![scope],
        typed_body: &mut typed_body,
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
                    let entry_block = ctxt.typecheck_block(body);
                    ctxt.typed_body.entry_block = Some(entry_block);
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

    let final_err_count = sess.diag.error_count();
    typed_body.errors = final_err_count - initial_err_count;
    typed_body
}


#[cfg(test)]
mod tests {}
