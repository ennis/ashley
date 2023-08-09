//! Typed AST representation
mod attributes;
mod builtins;
mod consteval;
mod def;
mod expr;
mod item;
mod layout;
mod lower;
mod overload;
mod scope;
mod stmt;
mod swizzle;
pub mod ty;

pub use def::{Def, DefKind, FunctionDef, Qualifier, Visibility};
pub use expr::Expr;
pub use lower::lower_to_hir;
pub(crate) use scope::Scope;
pub use ty::{FunctionType, ScalarType, Type, TypeKind};

use self::layout::std_array_stride;

use crate::{
    builtins::PrimitiveTypes,
    session::{PackageId, QueryError},
    syntax::ast,
    tast::{stmt::Stmt, ty::convert_type},
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
pub type LocalDefId = Id<Def>;
pub type LocalVarId = Id<LocalVar>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct DefId {
    pub package: PackageId,
    pub local_def: LocalDefId,
}

impl DefId {
    pub fn new(package: PackageId, local_def_id: LocalDefId) -> DefId {
        DefId {
            package,
            local_def: local_def_id,
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
    pub imported_packages: Vec<PackageId>,
    /// All definitions in this module.
    pub defs: TypedVec<Def>,
    // TODO errors?
}

impl Module {
    /// Creates a new module.
    pub fn new() -> Module {
        Module {
            imported_packages: vec![],
            defs: Default::default(),
        }
    }

    // TODO: move somewhere else
    /*/// Returns the definition with the given ID.
    pub fn def(&self, id: DefId) -> &Def {
        match id.package {
            Some(package) => &self.packages[package].defs[id.local_def],
            None => &self.defs[id.local_def],
        }
    }*/

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
    package: PackageId,
    module: &'a mut Module,
    scopes: Vec<Scope>,
}

impl<'a, 'diag> TypeCheckItemCtxt<'a, 'diag> {
    pub(crate) fn convert_type(&mut self, ty: ast::Type) -> Type {
        convert_type(ty, self.sess, &self.scopes)
    }
}

pub(crate) fn typecheck_items(sess: &mut Session, package: PackageId, ast: ast::Root) -> Module {
    let mut module = Module::new();
    sess.diag.set_current_source(ast.source_id);
    let mut ctxt = TypeCheckItemCtxt {
        sess,
        package,
        module: &mut module,
        scopes: Vec::new(),
    };
    ctxt.define_builtin_functions();
    ctxt.typecheck_module(&ast.module);
    module
}

pub(crate) struct TypeCheckBodyCtxt<'a, 'diag> {
    sess: &'a mut Session<'diag>,
    scopes: Vec<Scope>,
    typed_body: &'a mut TypedBody,
}

impl<'a, 'diag> TypeCheckBodyCtxt<'a, 'diag> {
    pub(crate) fn convert_type(&mut self, ty: ast::Type) -> Type {
        convert_type(ty, self.sess, &self.scopes)
    }
}

// TODO: some defs do not have bodies, return None for them

/// Builds a scope with all visible global-scope definitions visible to the body of the specified definition.
fn build_scope_for_def_body(sess: &mut Session, def_id: DefId) -> Result<Scope, QueryError> {
    let mut scope = Scope::new();

    // bring definitions from imported packages in the scope
    // TODO: import only externally visible definitions,
    // TODO: import only the items specified in the import clause
    // for now there's no syntax for that (we bring the whole package in scope), but this might change at some point
    let imported_packages = sess.imports(def_id.package)?;
    for imp_pkg in imported_packages {
        let imp_defs = sess.definitions(imp_pkg)?;
        for imp_local_def_id in imp_defs {
            // retrieve the name of the definition, and add a new entry to the scope under that name
            // NOTE: at some point we may gain the ability to import definitions under different names,
            // and this will need to be updated.
            let imp_def_id = DefId::new(imp_pkg, imp_local_def_id);
            let imp_def = sess.pkgs.def(imp_def_id);
            scope.add_def(imp_def_id, imp_def);
        }
    }

    // reconstruct the set of visible declarations before the item body
    // (there are no forward references in GLSL)
    for prev_def_index in 0..def_id.local_def.index() {
        let prev_def_id = DefId::new(def_id.package, LocalDefId::from_index(prev_def_index));
        let prev_def = sess.pkgs.def(prev_def_id);
        scope.add_def(prev_def_id, prev_def);
    }

    Ok(scope)
}

/// Typechecks the body of a definition.
pub(crate) fn typecheck_body(sess: &mut Session, def: DefId) -> Result<TypedBody, QueryError> {
    let scope = build_scope_for_def_body(sess, def)?;

    // HACK: to count the number of errors during type-checking, count the difference in number of errors
    let initial_err_count = sess.diag.error_count();

    let mut typed_body = TypedBody::new();
    let mut ctxt = TypeCheckBodyCtxt {
        sess,
        scopes: vec![scope],
        typed_body: &mut typed_body,
    };

    match ctxt.sess.pkgs.def(def).kind {
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
                    // typecheck main block
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
    Ok(typed_body)
}

#[cfg(test)]
mod tests {}
