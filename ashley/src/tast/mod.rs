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

pub use attributes::{AttrParseError, Attribute, AttributeMultiplicity, AttributeTarget};
pub(crate) use attributes::{AttributeChecker, AttributeCheckerImpl};
pub use def::{Def, DefKind, FunctionDef, Qualifier, Visibility};
pub use expr::Expr;
pub use lower::lower_to_hir;
pub(crate) use scope::Scope;
pub use ty::{FunctionType, ScalarType, Type, TypeKind};

use crate::{
    builtins::PrimitiveTypes,
    session::{CompilerDb, DefId, ModuleId, SourceFileId},
    syntax::ast,
    tast::{scope::Res, stmt::Stmt, ty::convert_type},
    utils::{Id, IndexVec},
};
use std::{
    collections::HashSet,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Utility trait to parse a value with access to the diagnostics output.
pub trait ParseFrom<T>: Sized {
    fn parse_from(value: T, compiler: &dyn CompilerDb) -> Result<Self, ()>;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

pub type ExprId = Id<Expr>;
pub type StmtId = Id<Stmt>;
pub type BlockId = Id<Block>;
pub type LocalDefId = Id<Def>;
pub type LocalVarId = Id<LocalVar>;

/*#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct DefId {
    pub package: PackageId,
    pub local_def: LocalDefId,
}*/

/*impl DefId {
    pub fn new(package: PackageId, local_def_id: LocalDefId) -> DefId {
        DefId {
            package,
            local_def: local_def_id,
        }
    }
}*/

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalVar {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<StmtId>,
}

impl Block {
    pub fn new() -> Block {
        Block { stmts: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBody {
    pub stmts: IndexVec<Stmt>,
    pub exprs: IndexVec<Expr>,
    pub local_vars: IndexVec<LocalVar>,
    pub blocks: IndexVec<Block>,
    pub params: Vec<LocalVarId>,
    pub errors: usize,
    pub entry_block: Option<BlockId>,
}

impl TypedBody {
    pub fn new() -> TypedBody {
        TypedBody {
            stmts: IndexVec::new(),
            exprs: IndexVec::new(),
            local_vars: IndexVec::new(),
            blocks: IndexVec::new(),
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
#[derive(Debug, Eq, PartialEq)]
pub struct Module {
    /// Imported packages.
    pub imported_packages: Vec<ModuleId>,
    /// All definitions in this module, in the order in which they appear in the source file
    /// (hence the IndexMap, to preserve this order).
    pub defs: indexmap::IndexMap<DefId, Def>,
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

    /// Iterates over all the local definitions in this module.
    pub fn definitions(&self) -> impl Iterator<Item = (&DefId, &Def)> {
        self.defs.iter()
    }
}

/// Types interner.
pub struct Types {
    types: Mutex<HashSet<Arc<TypeKind>>>,
}

impl Types {
    pub fn new() -> Self {
        Types {
            types: Mutex::new(HashSet::new()),
        }
    }

    /// Interns a type.
    pub fn intern(&self, kind: impl Into<TypeKind>) -> Type {
        // TODO replace with get_or_insert_owned once stabilized
        let kind = kind.into();
        let mut types = self.types.lock().unwrap();
        if let Some(ty) = types.get(&kind) {
            Type(ty.clone())
        } else {
            let ty = Arc::new(kind);
            types.insert(ty.clone());
            Type(ty)
        }
    }
}

/// Type-checking context.
///
/// Holds the interner for types, and pre-interned versions of commonly-used types.
///
/// TODO integrate with CompilerDB?
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

    pub fn ty(&self, kind: impl Into<TypeKind>) -> Type {
        self.types.intern(kind)
    }
}

/// Context for item-level type-checking.
pub(crate) struct TypeCheckItemCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    module: ModuleId,
    typed_module: &'a mut Module,
    scopes: Vec<Scope>,
}

impl<'a> TypeCheckItemCtxt<'a> {
    pub(crate) fn convert_type(&mut self, ty: ast::Type) -> Type {
        convert_type(ty, self.compiler, &self.scopes)
    }
}

pub(crate) fn typecheck_items(
    compiler: &dyn CompilerDb,
    module: ModuleId,
    source_file: SourceFileId,
    ast: ast::Module,
) -> Module {
    let mut typed_module = Module::new();

    let mut ctxt = TypeCheckItemCtxt {
        compiler,
        module,
        typed_module: &mut typed_module,
        scopes: Vec::new(),
    };
    ctxt.define_builtin_functions();
    compiler.push_diagnostic_source_file(source_file);
    ctxt.typecheck_module(&ast);
    compiler.pop_diagnostic_source_file();

    typed_module
}

pub(crate) struct TypeCheckBodyCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    scopes: Vec<Scope>,
    typed_body: &'a mut TypedBody,
}

impl<'a> TypeCheckBodyCtxt<'a> {
    pub(crate) fn convert_type(&mut self, ty: ast::Type) -> Type {
        convert_type(ty, self.compiler, &self.scopes)
    }
}

// TODO: some defs do not have bodies, return None for them

/// Builds a scope with all visible global-scope definitions visible to the body of the specified definition.
fn build_scope_for_def_body(compiler: &dyn CompilerDb, def_id: DefId) -> Scope {
    let mut scope = Scope::new();

    // bring definitions from imported packages in the scope
    // TODO: import only externally visible definitions,
    // TODO: import only the items specified in the import clause
    // for now there's no syntax for that (we bring the whole package in scope), but this might change at some point

    let module = compiler.parent_module(def_id);
    let imported_packages = compiler.module_imports(module);

    for &imp_pkg in imported_packages {
        let imp_defs = compiler.module_definitions(imp_pkg);
        for &imp_def_id in imp_defs {
            // retrieve the name of the definition, and add a new entry to the scope under that name
            // NOTE: at some point we may gain the ability to import definitions under different names,
            // and this will need to be updated.
            let imp_def = compiler.definition(imp_def_id);
            scope.add_def(imp_def_id, imp_def);
        }
    }

    // Reconstruct the set of visible declarations before the item body. These are the declarations
    // appearing before the item body. `module_definitions` returns them in declaration order,
    // so add all definitions until we encounter `def_id`.
    let mod_defs = compiler.module_definitions(module);
    let mut i = 0;
    while i < mod_defs.len() && mod_defs[i] != def_id {
        let prev_def = compiler.definition(mod_defs[i]);
        scope.add_def(mod_defs[i], prev_def);
        i += 1;
    }
    scope
}

/// Typechecks the body of a definition.
pub(crate) fn typecheck_body(compiler: &dyn CompilerDb, def: DefId) -> TypedBody {
    let scope = build_scope_for_def_body(compiler, def);

    // HACK: to count the number of errors during type-checking, count the difference in number of errors
    let initial_err_count = compiler.diag().error_count();

    let mut typed_body = TypedBody::new();
    let mut ctxt = TypeCheckBodyCtxt {
        compiler,
        scopes: vec![scope],
        typed_body: &mut typed_body,
    };

    match ctxt.compiler.definition(def).kind {
        DefKind::Function(ref func) => {
            if let Some(ref ast_ptr) = func.ast {
                let func_ast = ctxt.compiler.ast_node_for_def(def, ast_ptr);
                if let Some(ref body) = func_ast.block() {
                    // create local vars for function parameters
                    let mut param_scope = Scope::new();
                    for param in func.parameters.iter() {
                        let id = ctxt.typed_body.local_vars.push(LocalVar {
                            name: param.name.clone(),
                            ty: param.ty.clone(),
                        });
                        param_scope.add(param.name.clone(), Res::Local(id));
                        ctxt.typed_body.params.push(id);
                    }
                    ctxt.scopes.push(param_scope);
                    // typecheck main block
                    let entry_block = ctxt.typecheck_block(body);
                    ctxt.typed_body.entry_block = Some(entry_block);
                }
            }
        }
        DefKind::Global(ref global) => {
            if let Some(ref ast_ptr) = global.ast {
                let global_ast = ctxt.compiler.ast_node_for_def(def, ast_ptr);
                if let Some(ref initializer) = global_ast.initializer() {
                    if let Some(ref expr) = initializer.expr() {
                        let expr = ctxt.typecheck_expr(expr);
                        ctxt.add_expr(expr);
                    }
                }
            }
        }
        DefKind::Struct(_) => {}
    };

    let final_err_count = compiler.diag().error_count();
    typed_body.errors = final_err_count - initial_err_count;
    typed_body
}

#[cfg(test)]
mod tests {}
