//! Compilation cache
use crate::{
    diagnostic::Span,
    ir, item,
    item::{
        body,
        body::{Body, BodyMap},
        AstIdMap, BodyId, BodyLoc, DefId, ModuleItems, Scope, StructId,
    },
    layout,
    layout::LayoutInfo,
    resolver::{DummyPackageResolver, PackageResolver},
    source_file::LineCharacterRange,
    syntax,
    syntax::{ast, AstNode, Lang, SyntaxDiagnostic, SyntaxNode, SyntaxNodePtr},
    ty,
    ty::TypeCtxt,
    utils::Counter,
    ConstantValue, SourceFile, SourceFileId,
};

use crate::item::AstId;
use ashley_db::{
    define_database_tables, new_key_type, table::HasTables, Database, DatabaseExt, DbIndex, Revision, Runtime, TableOps,
};
use rowan::{ast::AstPtr, GreenNode};
use std::{
    collections::{HashMap, HashSet},
    fmt,
    fmt::Formatter,
    sync::Arc,
};

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, thiserror::Error)]
pub enum QueryError {
    #[error("package has no source code attached")]
    NoSource,
    #[error("requested definition was not in cache")]
    NotInCache,
}

#[derive(Debug, thiserror::Error)]
pub enum SessionError {
    #[error("an attribute with the same name already exists")]
    AttributeAlreadyExists,
}

////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////
// ATTRIBUTE REGISTRATION

/*/// Attribute registration information.
pub struct AttributeRegistration {
    /// Name of the attribute.
    pub(crate) name: String,
    /// Valid target items for the attribute.
    pub(crate) valid_targets: AttributeTarget,
    /// Whether this attribute can appear more than once on an item.
    pub(crate) multiplicity: AttributeMultiplicity,
    /// The list of attributes that are mutually exclusive with this one.
    pub(crate) mutually_exclusive: Vec<String>,
    /// User-provided closure to further verify the syntax of the attribute.
    pub(crate) checker: Box<dyn AttributeChecker + Send>,
}*/

//pub type CustomAttributes = HashMap<String, AttributeRegistration>;

////////////////////////////////////////////////////////////////////////////////////////////////////

new_key_type! {
    /// Uniquely identifies a module.
    pub struct ModuleId;
}

impl ModuleId {
    pub fn items<'a>(&self, compiler: &'a dyn CompilerDb) -> &'a ModuleItems {
        compiler.module_items(*self)
    }
}

/*
/// Namespace of a definition name.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum Namespace {
    /// Type names
    Type,
    /// Value names (constants, globals, locals, and functions, even though they aren't quite values).
    Value,
}

/// Location and name of a definition.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct DefName {
    pub module: ModuleId,
    pub namespace: Namespace,
    pub name: String,
}

/// `Debug` proxy for `DefId`s.
///
/// Prints the name of the defined item alongside the unique ID.
pub struct DefDebug<'a>(DefId, &'a Def);

impl<'a> fmt::Debug for DefDebug<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DefId({:04x} `{}`)", self.0 .0, self.1.name)
    }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Information about a source-file-based module.
#[derive(Clone, Default, Eq, PartialEq)]
pub struct ModuleData {
    /// Canonicalized URL of the module.
    ///
    /// For source-file modules, this is the URL of the source file.
    /// FIXME redundant with ModuleId
    pub url: String,
    /// The of the source file, for source-file modules. `None` otherwise.
    pub source: Option<SourceFileId>,
}

////////////////////////////////////////////////////////////////////////////////////////////////////

// issue with returning references: projections
// example: module_items_and_ast_map(Arc<ModuleItems>, Arc<AstIdMap>)
// -> module_items changes less than the AST map
// -> by depending only on `module_items` we can protect dependents on changes to the AstIdMap
// -> but if we return references (&ModuleItems, &AstIdMap), then we must clone the whole ModuleItems in the projection,
//    whereas, with Arc, this clone is cheap
//
//  What if, instead of projections, we use "specify"?
// Have two queries instead: module_items and module_ast_map.
// But the same query sets both: module_items and module_ast_map, via "specify"

define_database_tables! {
    pub trait CompilerDb [CompilerDbStorage] {

        // --- Singletons ---
        input source_file_counter[set_source_file_counter, update_source_file_counter](_dummy: ()) -> Arc<Counter<SourceFileId>>;
        input tyctxt_instance[set_tyctxt, update_tyctxt](_dummy: ()) -> Arc<TypeCtxt>;
        input resolver_instance[set_resolver, update_resolver](_dummy: ()) -> Arc<DummyPackageResolver>;
        //input custom_attributes_instance[set_custom_attributes](_dummy: ()) -> Arc<CustomAttributes>;

        // --- Interned IDs ---
        intern module_id[lookup_module_id](module_name: String)-> ModuleId;
        intern body_id[lookup_body_id](body_loc: BodyLoc) -> BodyId;

        //intern struct_id[lookup_struct_id](loc: StructLoc) -> StructId;
        //intern def_id[lookup_def_id](def_name: DefName) -> DefId;

        // --- Input data ---
        //input module_source[set_module_source](module: ModuleId) -> SourceFileId;
        input source_file[set_source_file, update_source_file](source_file: SourceFileId) -> SourceFile;
        input module_data[set_module_data, update_module_data](module: ModuleId) -> ModuleData;

        // --- Queries ---
        query (source_id: SourceFileId) -> {
            /// Syntax tree.
            syntax_tree: GreenNode [set_syntax_tree],
            /// Diagnostics resulting from parsing.
            syntax_diagnostics: Vec<SyntaxDiagnostic> [set_syntax_diagnostics]
        } => syntax::syntax_tree_with_diagnostics_query;

        // Definitions in a module (not including imports).
        query module_definitions(module: ModuleId) -> Vec<DefId> => module_definitions_query;

        /// Module imports (import statements).
        query module_imports(module: ModuleId) -> Vec<ModuleId> => module_imports_query;

        /// Module dependencies (transitive imports).
        query module_dependencies(module: ModuleId) -> Vec<ModuleId> => module_dependencies_query;

        // Results of definition type-checking.
        //query typechecked_module(module: ModuleId) -> Arc<tast::Module> => typechecked_module_query;

        // Results of body type-checking.
        //query typechecked_body(def: DefId) -> Arc<tast::TypedBody> => typechecked_body_query;

        // ----------
        query (module: ModuleId) -> {
            module_items: ModuleItems   [set_module_items],
            module_map: AstIdMap        [set_module_map]
        } => item::module_items_and_ast_map_query;

        //query module_items_and_ast_map(module: ModuleId) -> (Arc<ModuleItems>, Arc<AstIdMap>) => item::module_items_and_ast_map_query;
        //query module_items(module: ModuleId) -> Arc<ModuleItems>  => item::module_items_query;

        query scope_for_body(body: BodyId) -> Scope  => item::scope_for_body_query;

        //query scope_for_definition(definition: DefId) -> Scope  => item::scope_for_definition_query;
        //query def_ty(definition: DefWithTypeId) -> ty::Type => ty::def_ty_query;

        query (body: BodyId) -> {
            body: Body  [set_body],
            body_map: BodyMap  [set_body_map]
        } => item::body::body_and_map_query;

        query (body: BodyId) -> {
            ty_body: Body  [set_ty_body],
            ty_body_diagnostics: Vec<ty::body::TyBodyDiagnostic>  [set_ty_body_diagnostics]
        } => ty::body::ty_body_query;

        /*query (body: BodyId) -> {
            const_eval: Option<ConstantValue>  [set_const_eval],
            const_eval_diagnostics: Vec<ty::body::ConstEvalDiagnostic>  [set_const_eval_diagnostics]
        } => ty::body::const_eval_query;*/

        //query layout(strukt: StructId) -> LayoutInfo  => layout::layout_query;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

fn module_definitions_query(compiler: &dyn CompilerDb, module: ModuleId) -> Vec<DefId> {
    let _span = trace_span!("module_definitions_query").entered();
    //compiler.runtime().set_query_label("query_package_definitions");
    //compiler.typechecked_module(module).defs.keys().cloned().collect()
    todo!()
}

/// Module imports (import statements).
fn module_imports_query(compiler: &dyn CompilerDb, module: ModuleId) -> Vec<ModuleId> {
    let _span = trace_span!("module_imports_query").entered();
    //let tmod = compiler.typechecked_module(module);
    //tmod.imported_packages.clone()
    todo!()
}

/// Module dependencies (transitive imports).
fn module_dependencies_query(compiler: &dyn CompilerDb, module: ModuleId) -> Vec<ModuleId> {
    /*compiler.runtime().set_query_label("query_package_dependencies");
    // TODO resolve imports without typechecking
    let mut deps = HashSet::new();
    let mut visit = vec![module];
    while let Some(module) = visit.pop() {
        let module = compiler.typechecked_module(module);
        for import in module.imported_packages.iter() {
            if deps.insert(*import) {
                visit.push(*import);
            }
        }
    }
    deps.into_iter().collect()*/
    todo!()
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Compiler instance.
pub struct Compiler {
    db_storage: CompilerDbStorage,
    db_runtime: Runtime,
}

impl Compiler {
    /// Creates a new `Compiler` instance.
    pub fn new() -> Compiler {
        let mut compiler = Compiler {
            db_storage: CompilerDbStorage::new(0),
            db_runtime: Runtime::new(),
        };

        let tyctxt = TypeCtxt::new();
        let resolver = Arc::new(DummyPackageResolver);
        //let custom_attributes = Default::default();

        compiler.set_tyctxt((), Arc::new(tyctxt));
        compiler.set_resolver((), resolver);
        compiler.set_source_file_counter((), Arc::new(Counter::new()));
        //compiler.set_custom_attributes((), custom_attributes);
        compiler
    }

    /// Compiles the package to HIR (in-memory SPIR-V).
    pub fn compile_to_hir(&mut self, package: ModuleId) -> Result<ir::Module, QueryError> {
        todo!("compile_to_hir")
        //lower_to_hir(self, package)
    }

    /// Compiles the package and its dependencies to a standalone SPIR-V module.
    ///
    /// This includes all dependencies into the module.
    pub fn compile_to_spirv(&mut self, package: ModuleId) -> Result<Vec<u32>, QueryError> {
        todo!("compile_to_spirv")
        /*let hir = lower_to_hir(self, package)?;
        let spv_bytecode = ir::write_spirv(&hir);
        Ok(spv_bytecode)*/
    }
}

impl Database for Compiler {
    fn maybe_changed_after(&self, db_index: DbIndex, after: Revision) -> bool {
        self.db_storage
            .maybe_changed_after(self, db_index, after)
            .expect("invalid database index")
    }

    fn on_new_revision(&mut self, revision: Revision) {
        self.db_storage.on_new_revision(revision);
    }

    fn runtime(&self) -> &Runtime {
        &self.db_runtime
    }

    fn runtime_mut(&mut self) -> &mut Runtime {
        &mut self.db_runtime
    }
}

impl HasTables<CompilerDbStorage> for Compiler {
    fn tables(&self) -> &CompilerDbStorage {
        &self.db_storage
    }
    fn tables_mut(&mut self) -> &mut CompilerDbStorage {
        &mut self.db_storage
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl dyn CompilerDb {
    pub fn tyctxt(&self) -> Arc<TypeCtxt> {
        CompilerDb::tyctxt_instance(self, ()).clone()
    }

    /*pub fn custom_attributes(&self) -> Arc<CustomAttributes> {
        CompilerDb::custom_attributes_instance(self, ())
    }*/

    /*/// Creates a new diagnostic with `Error` severity.
    ///
    /// Emit the diagnostic with `.emit()`.
    ///
    /// # Example
    /// ```
    ///# fn main() {
    ///diag.error("Syntax error").emit();
    ///# }
    /// ```
    pub fn diag_error(&self, message: impl Into<String>) -> DiagnosticBuilder {
        DiagnosticBuilder {
            compiler: self,
            diag: Diagnostic::new(Severity::Error).with_message(message.into()),
        }
    }

    /// Creates a new diagnostic with `Warning` severity.
    ///
    /// # Example
    /// ```
    ///# fn main() {
    ///diag.warning(format!("Unused parameter: {}", param_name)).emit();
    ///# }
    /// ```
    pub fn diag_warn(&self, message: impl Into<String>) -> DiagnosticBuilder {
        DiagnosticBuilder {
            compiler: self,
            diag: Diagnostic::new(Severity::Warning).with_message(message.into()),
        }
    }

    /// Creates a new diagnostic with `Bug` severity.
    ///
    /// Emit the diagnostic with `.emit()`.
    ///
    /// # Example
    /// ```
    ///# fn main() {
    ///diag.bug("Internal compiler error").emit();
    ///# }
    /// ```
    pub fn diag_bug(&self, message: impl Into<String>) -> DiagnosticBuilder {
        DiagnosticBuilder {
            compiler: self,
            diag: Diagnostic::new(Severity::Bug).with_message(message.into()),
        }
    }*/

    /// Dereferences a pointer to an AST node (`AstPtr<N>`).
    pub fn ast_node<N: AstNode<Language = Lang>>(&self, module: ModuleId, ptr: &AstPtr<N>) -> N {
        let md = self.module_data(module);
        let source_file = md.source.expect("`module` should have an associated source file");
        let syntax_tree = self.syntax_tree(source_file);
        let syntax_node = SyntaxNode::new_root(syntax_tree.clone());
        ptr.to_node(&syntax_node)
    }

    pub fn create_module_from_source_file(&mut self, canonical_url: &str, contents: &str) -> (ModuleId, SourceFileId) {
        // create source file & module
        let source_file_id = self.source_file_counter(()).next();
        let module_id = self.module_id(canonical_url.to_string());
        self.set_source_file(source_file_id, SourceFile::new(canonical_url, contents));
        self.set_module_data(
            module_id,
            ModuleData {
                url: canonical_url.to_string(),
                source: Some(source_file_id),
            },
        );
        (module_id, source_file_id)
    }

    pub fn module_source(&self, module_id: ModuleId) -> SourceFileId {
        self.module_data(module_id).source.expect("no source for module")
    }

    pub fn update_source_file_contents(&mut self, source_file: SourceFileId, new_contents: &str) {
        self.update_source_file(source_file, &move |src| src.update(new_contents));
    }

    /*/// Dereferences a pointer to an AST node that is contained in the same module as `def_id`.
    pub fn ast_node_for_def<N: AstNode<Language = Lang>>(&self, def_id: DefId, ptr: &AstPtr<N>) -> N {
        let module = self.parent_module(def_id);
        self.ast_node(module, ptr)
    }*/

    /// Returns the syntax tree of the given module.
    ///
    /// The module be a "source-file module" (i.e. have been created with an attached source file).
    pub fn module_syntax_tree(&self, module: ModuleId) -> (SourceFileId, GreenNode) {
        let md = self.module_data(module);
        let source_file = md.source.expect("`module` should have an associated source file");
        let syntax_tree = self.syntax_tree(source_file);
        (source_file, syntax_tree.clone())
    }

    /*pub fn module_syntax(&self, module: ModuleId) -> (SourceFileId, SyntaxNode) {
        let (source_file, syntax) = self.module_syntax_tree(module);
        (source_file, syntax.to_root())
    }*/

    /// Returns the line and byte offset within the line for the specified `Span`.
    pub fn span_to_line_character_range(&self, span: Span) -> LineCharacterRange {
        let source_file_data = self.source_file(span.file);
        source_file_data.line_character_range(span.range)
    }

    /*pub fn ast_id_to_node<N: AstNode>(&self, module: ModuleId, id: AstId<N>) -> N {
        let module_map = self.module_map(module);
        module_map.get_node_ptr(id)
    }*/

    /*pub fn definition(&self, definition: DefId) -> Arc<Def> {
        let module = self.parent_module(definition);
        let ty_module = self.typechecked_module(module);
        let def = ty_module
            .defs
            .get(&definition)
            .expect("definition was not found in its expected package");
        def.clone()
    }*/

    /*pub fn parent_module(&self, definition: DefId) -> ModuleId {
        self.lookup_def_id(definition).module
    }*/
}

////////////////////////////////////////////////////////////////////////////////////////////////////

pub struct DebugWith<'a, T>(&'a dyn CompilerDb, &'a T);

impl<'a, T> fmt::Debug for DebugWith<'a, T>
where
    T: DebugWithDb,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.1.fmt(f, self.0)
    }
}

// Again, inspired by salsa
pub trait DebugWithDb: Sized {
    fn debug_with<'a>(&'a self, compiler: &'a dyn CompilerDb) -> DebugWith<'a, Self> {
        DebugWith(compiler, self)
    }

    fn fmt<'a>(&'a self, formatter: &mut fmt::Formatter<'_>, compiler: &'a dyn CompilerDb) -> fmt::Result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn package_names() {
        let name = String::from("foo");
        let name2 = String::from("foo2");
        let package_name = ModuleName::new(&name);
        let package_name_with_args = ModuleName::new(&name).arg(42);
        let same_package_name_with_args = ModuleName::new(&name).arg(42);

        eprintln!("{}", package_name);
        eprintln!("{}", package_name_with_args);
        eprintln!("{}", same_package_name_with_args);

        let mut compiler = Compiler::new();
        /*let (id, created) = compiler.create_source_package(package_name);
        assert!(created);
        let (id2, created) = compiler.get_or_create_package(package_name_with_args.clone());
        assert!(created);
        let (id3, created) = compiler.get_or_create_package(package_name_with_args.clone());
        assert_eq!(id2, id3);
        assert!(!created);*/
    }
}
