//! Compilation cache
use crate::{
    diagnostic::{Diagnostic, DiagnosticBuilder, DiagnosticSink, Diagnostics, TermDiagnosticSink},
    hir,
    resolver::{DummyPackageResolver, PackageResolver},
    syntax,
    syntax::{ast, AstNode, Lang, SyntaxNode, SyntaxNodePtr},
    tast,
    tast::{
        lower_to_hir, typecheck_body, typecheck_items, Attribute, AttributeChecker, AttributeCheckerImpl,
        AttributeMultiplicity, AttributeTarget, Def, Module, TypeCtxt, TypedBody,
    },
    SourceFile,
};
use ashley_db::{
    define_database_tables, new_key_type, table::HasTables, Database, DatabaseExt, DbIndex, Revision, Runtime, TableOps,
};
use codespan_reporting::{diagnostic::Severity, term};
use rowan::{ast::AstPtr, GreenNode};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    marker::PhantomData,
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

#[derive(Clone, PartialEq)]
pub struct SyntaxTree {
    pub green_node: GreenNode,
    pub root: SyntaxNodePtr,
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// ATTRIBUTE REGISTRATION

/// Attribute registration information.
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
}

pub type CustomAttributes = HashMap<String, AttributeRegistration>;

////////////////////////////////////////////////////////////////////////////////////////////////////

new_key_type! {
    /// Uniquely identifies a module.
    pub struct ModuleId;
}

new_key_type! {
    /// Identifies a source file.
    pub struct SourceFileId;
}

new_key_type! {
    /// Uniquely identifies a definition within a package.
    pub struct DefId;
}

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
}

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

define_database_tables! {
    struct CompilerQueryData [dyn CompilerDb] {

        // --- Interned IDs ---
        intern module_id(String)-> ModuleId;
        intern def_id(DefName) -> DefId;

        // --- Index generators ---
        index source_file_id(SourceFileId);

        // --- Input data ---
        input module_source(ModuleId) -> SourceFileId;
        input source_file(SourceFileId) -> SourceFile;
        input module_data(ModuleId) -> ModuleData;

        // --- Queries ---

        /// Syntax trees for each source file.
        query syntax_tree(compiler: &dyn CompilerDb, source_id: &SourceFileId) -> SyntaxTree {
            compiler.runtime().set_query_label("query_source_file_syntax_tree");
            let source_file = compiler.source_file(*source_id);
            let green_node = syntax::parse(compiler, &source_file.contents, *source_id);
            let root = SyntaxNode::new_root(green_node.clone());
            SyntaxTree {
                green_node,
                root: SyntaxNodePtr::new(&root),
            }
        }

        /// Definitions in a module (not including imports).
        query module_definitions(compiler: &dyn CompilerDb, module: &ModuleId) -> Vec<DefId> {
            compiler.runtime().set_query_label("query_package_definitions");
            compiler.typechecked_module(*module).defs.keys().cloned().collect()
        }

        /// Module imports (import statements).
        query module_imports(compiler: &dyn CompilerDb, module: &ModuleId) -> Vec<ModuleId> {
            compiler.runtime().set_query_label("query_package_imports");
            // TODO resolve imports without typechecking
            let tmod = compiler.typechecked_module(*module);
            tmod.imported_packages.clone()
        }

        /// Module dependencies (transitive imports).
        query module_dependencies(compiler: &dyn CompilerDb, module: &ModuleId) -> Vec<ModuleId> {
            compiler.runtime().set_query_label("query_package_dependencies");
            // TODO resolve imports without typechecking
            let mut deps = HashSet::new();
            let mut visit = vec![*module];
            while let Some(module) = visit.pop() {
                let module = compiler.typechecked_module(module);
                for import in module.imported_packages.iter() {
                    if deps.insert(*import) {
                        visit.push(*import);
                    }
                }
            }
            deps.into_iter().collect()
        }

        /// Results of definition type-checking.
        query typechecked_module(compiler: &dyn CompilerDb, module: &ModuleId) -> tast::Module {
            compiler.runtime().set_query_label("query_typecheck_module");
            let (source_file, syntax_tree) = compiler.module_syntax_tree(*module);
            let syntax_node = SyntaxNode::new_root(syntax_tree.green_node.clone());
            typecheck_items(compiler, *module, source_file, ast::Module::cast(syntax_node).expect("invalid AST"))
        }

        /// Results of body type-checking.
        query typechecked_body(compiler: &dyn CompilerDb, def: &DefId) -> tast::TypedBody {
            compiler.runtime().set_query_label("query_typecheck_body");
            typecheck_body(compiler, *def)
        }
    }
}

/// Compilation session.
pub struct CompilerDbData {
    /// Diagnostics output
    diag: Diagnostics,
    /// Type
    tyctxt: TypeCtxt,
    resolver: Arc<dyn PackageResolver + Sync + Send>,
    /// Registered custom attributes.
    custom_attributes: CustomAttributes,
    /// Stack that tracks which source file is being processed, for the purpose of diagnostics.
    ///
    /// See `push_diagnostic_source_file`, `pop_diagnostic_source_file`.
    source_file_stack: RefCell<Vec<SourceFileId>>,
    query: CompilerQueryData,
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// COMPILER DB TRAIT

pub trait CompilerDb: HasTables<CompilerDbData> {
    /// Returns the `TypeCtxt` (types interner).
    fn tyctxt(&self) -> &TypeCtxt;

    /// Returns the set of registered attributes.
    fn custom_attributes(&self) -> &CustomAttributes;

    /// Returns the diagnostics output object.
    fn diag(&self) -> &Diagnostics;

    // --- queries ---

    /// Returns the `DefId` corresponding to the specified name and namespace.
    fn def_id(&self, package: ModuleId, name: String, namespace: Namespace) -> DefId;

    /// Returns information about the given module.
    fn module_data(&self, module: ModuleId) -> &ModuleData;

    /// Returns the list of the definitions in a module, in the order in which they appear in the module source code.
    fn module_definitions(&self, module: ModuleId) -> &[DefId];

    /// Returns the module containing of the given definition.
    fn parent_module(&self, definition: DefId) -> ModuleId;

    /// Returns the transitive list of dependencies of the given module.
    fn module_dependencies(&self, module: ModuleId) -> &[ModuleId];

    /// Returns the modules directly imported by the given module.
    fn module_imports(&self, module: ModuleId) -> &[ModuleId];

    /// Returns information about the given definition.
    fn definition(&self, definition: DefId) -> &Def;

    /// Retrieves the syntax tree of a source file.
    fn syntax_tree(&self, source_file: SourceFileId) -> &SyntaxTree;

    /// Returns the source file with the given ID.
    fn source_file(&self, source_file: SourceFileId) -> &SourceFile;

    /// Returns the type-checked module for the given package.
    ///
    /// May trigger type-checking and resolution of definitions (but not bodies) if necessary.
    /// The module may have errors.
    fn typechecked_module(&self, module: ModuleId) -> &tast::Module;

    /// Returns the type-checked body of the specified definition.
    fn typechecked_def_body(&self, def: DefId) -> &tast::TypedBody;

    /// Specifies that the given source file should be used as default location for subsequent diagnostics.
    ///
    /// This stays until `pop_diagnostic_source_file` is called.
    fn push_diagnostic_source_file(&self, source: SourceFileId);

    /// Restores the previous source file for diagnostic locations.
    ///
    /// Counterpart of `push_diagnostic_source_file`
    fn pop_diagnostic_source_file(&self);

    /// Returns the source file to be used as the default location for diagnostics.
    ///
    /// This is the last source file passed to `push_diagnostic_source_file`.
    fn diagnostic_source_file(&self) -> Option<SourceFileId>;

    // --- update ---

    /// Adds a new source file module.
    ///
    /// # Arguments
    /// * canonical_url Canonical URL of the source file. It must be canonicalized before calling this function.
    /// * contents Contents of the source file.
    ///
    /// # Return value
    ///
    /// A tuple `(module_id, source_file_id)` containing the ID of the created module and the ID of the
    /// source file associated to the module.
    ///
    /// # Notes
    ///
    /// This function cannot create instances of
    ///
    fn create_module_from_source_file(&mut self, canonical_url: &str, contents: &str) -> (ModuleId, SourceFileId);

    /// Updates the contents of the specified source file.
    fn update_source_file(&mut self, source_file: SourceFileId, new_contents: String);
}

impl<DB> CompilerDb for DB
where
    DB: HasTables<CompilerDbData>,
{
    fn tyctxt(&self) -> &TypeCtxt {
        &self.tables().tyctxt
    }

    fn custom_attributes(&self) -> &CustomAttributes {
        &self.tables().custom_attributes
    }

    fn diag(&self) -> &Diagnostics {
        &self.tables().diag
    }

    fn def_id(&self, module: ModuleId, name: String, namespace: Namespace) -> DefId {
        self.tables().query.def_id.intern(DefName {
            module,
            namespace,
            name,
        })
    }

    fn module_data(&self, id: ModuleId) -> &ModuleData {
        self.tables().query.module_data.fetch(self, id)
    }

    fn module_definitions(&self, id: ModuleId) -> &[DefId] {
        self.tables().query.module_definitions.fetch(self, id)
    }

    fn parent_module(&self, definition: DefId) -> ModuleId {
        self.tables().query.def_id.fetch(definition).module
    }

    fn module_dependencies(&self, id: ModuleId) -> &[ModuleId] {
        self.tables().query.module_dependencies.fetch(self, id)
    }

    fn module_imports(&self, id: ModuleId) -> &[ModuleId] {
        self.tables().query.module_imports.fetch(self, id)
    }

    fn definition(&self, definition: DefId) -> &Def {
        let module = self.parent_module(definition);
        let ty_module = self.typechecked_module(module);
        let def = ty_module
            .defs
            .get(&definition)
            .expect("definition was not found in its expected package");
        def
    }

    fn syntax_tree(&self, source_file: SourceFileId) -> &SyntaxTree {
        self.tables().query.syntax_tree.fetch(self, source_file)
    }

    fn source_file(&self, source_file: SourceFileId) -> &SourceFile {
        self.tables().query.source_file.fetch(self, source_file)
    }

    fn typechecked_module(&self, module: ModuleId) -> &Module {
        self.tables().query.typechecked_module.fetch(self, module)
    }

    fn typechecked_def_body(&self, definition: DefId) -> &TypedBody {
        self.tables().query.typechecked_body.fetch(self, definition)
    }

    fn push_diagnostic_source_file(&self, source_file: SourceFileId) {
        self.tables().source_file_stack.borrow_mut().push(source_file);
    }

    fn pop_diagnostic_source_file(&self) {
        self.tables().source_file_stack.borrow_mut().pop();
    }

    fn diagnostic_source_file(&self) -> Option<SourceFileId> {
        self.tables().source_file_stack.borrow().last().cloned()
    }

    fn create_module_from_source_file(&mut self, canonical_url: &str, contents: &str) -> (ModuleId, SourceFileId) {
        self.with_new_revision(move |db, rev| {
            // create source file & module
            let source_file_id = db.tables().query.source_file_id.new_input();
            let module_id = db.tables().query.module_id.intern(canonical_url.to_string());
            db.tables_mut()
                .query
                .source_file
                .set(rev, source_file_id, SourceFile::new(canonical_url, contents));
            db.tables_mut().query.module_data.set(
                rev,
                module_id,
                ModuleData {
                    url: canonical_url.to_string(),
                    source: Some(source_file_id),
                },
            );
            (module_id, source_file_id)
        })
    }

    fn update_source_file(&mut self, source_file: SourceFileId, new_contents: String) {
        self.with_new_revision(move |db, rev| {
            db.tables_mut()
                .query
                .source_file
                .update(rev, source_file, move |source_file| source_file.update(new_contents));
        });
    }
}

impl CompilerDbData {
    pub fn new(base_table_index: usize) -> CompilerDbData {
        let diag = {
            let mut d = Diagnostics::new();
            d.add_sink(TermDiagnosticSink::new(term::Config::default()));
            d
        };
        let tyctxt = TypeCtxt::new();
        let resolver = Arc::new(DummyPackageResolver);
        let custom_attributes = Default::default();
        CompilerDbData {
            diag,
            tyctxt,
            resolver,
            custom_attributes,
            source_file_stack: RefCell::new(vec![]),
            query: CompilerQueryData::new(base_table_index),
        }
    }

    /// Sets a diagnostic sink.
    ///
    /// This overrides any previously set diagnostic sink.
    pub fn set_diagnostic_sink(&mut self, sink: impl DiagnosticSink + Send + 'static) {
        self.diag.clear_sinks();
        self.diag.add_sink(sink)
    }

    /// Registers a custom attribute.
    pub fn register_custom_attribute<T: Attribute + 'static>(
        &mut self,
        name: &str,
        targets: AttributeTarget,
        multiplicity: AttributeMultiplicity,
    ) -> Result<(), SessionError> {
        let checker: Box<dyn AttributeChecker + Send> = Box::new(AttributeCheckerImpl::<T>(PhantomData));
        if self.custom_attributes.contains_key(name) {
            return Err(SessionError::AttributeAlreadyExists);
        }
        // TODO mutually exclusive attrs
        self.custom_attributes.insert(
            name.to_string(),
            AttributeRegistration {
                name: name.to_string(),
                valid_targets: targets,
                multiplicity,
                mutually_exclusive: vec![],
                checker,
            },
        );
        Ok(())
    }
}

/// Compiler instance.
pub struct Compiler {
    db_data: CompilerDbData,
    db_runtime: Runtime,
}

impl Compiler {
    /// Creates a new `Compiler` instance.
    pub fn new() -> Compiler {
        Compiler {
            db_data: CompilerDbData::new(0),
            db_runtime: Runtime::new(),
        }
    }

    /// Compiles the package to HIR (in-memory SPIR-V).
    pub fn compile_to_hir(&mut self, package: ModuleId) -> Result<hir::Module, QueryError> {
        lower_to_hir(self, package)
    }

    /// Compiles the package and its dependencies to a standalone SPIR-V module.
    ///
    /// This includes all dependencies into the module.
    pub fn compile_to_spirv(&mut self, package: ModuleId) -> Result<Vec<u32>, QueryError> {
        let hir = lower_to_hir(self, package)?;
        let spv_bytecode = hir::write_spirv(&hir);
        Ok(spv_bytecode)
    }
}

impl Database for Compiler {
    fn maybe_changed_after(&self, db_index: DbIndex, after: Revision) -> bool {
        self.db_data
            .query
            .maybe_changed_after(self, db_index, after)
            .expect("invalid database index")
    }

    fn runtime(&self) -> &Runtime {
        &self.db_runtime
    }

    fn runtime_mut(&mut self) -> &mut Runtime {
        &mut self.db_runtime
    }
}

impl HasTables<CompilerDbData> for Compiler {
    fn tables(&self) -> &CompilerDbData {
        &self.db_data
    }
    fn tables_mut(&mut self) -> &mut CompilerDbData {
        &mut self.db_data
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl dyn CompilerDb {
    /// Creates a new diagnostic with `Error` severity.
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
    }

    /// Dereferences a pointer to an AST node (`AstPtr<N>`).
    pub fn ast_node<N: AstNode<Language = Lang>>(&self, module: ModuleId, ptr: &AstPtr<N>) -> N {
        let md = self.module_data(module);
        let source_file = md.source.expect("`module` should have an associated source file");
        let syntax_tree = self.syntax_tree(source_file);
        let syntax_node = SyntaxNode::new_root(syntax_tree.green_node.clone());
        ptr.to_node(&syntax_node)
    }

    /// Dereferences a pointer to an AST node that is contained in the same module as `def_id`.
    pub fn ast_node_for_def<N: AstNode<Language = Lang>>(&self, def_id: DefId, ptr: &AstPtr<N>) -> N {
        let module = self.parent_module(def_id);
        self.ast_node(module, ptr)
    }

    /// Returns the syntax tree of the given module.
    ///
    /// The module be a "source-file module" (i.e. have been created with an attached source file).
    pub fn module_syntax_tree(&self, module: ModuleId) -> (SourceFileId, &SyntaxTree) {
        let md = self.module_data(module);
        let source_file = md.source.expect("`module` should have an associated source file");
        let syntax_tree = self.syntax_tree(source_file);
        (source_file, syntax_tree)
    }
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
