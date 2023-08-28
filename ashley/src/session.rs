//! Compilation cache
use crate::{
    diagnostic::{DiagnosticSink, Diagnostics, SourceFileProvider, SourceId, TermDiagnosticSink},
    hir, syntax,
    syntax::{ast, AstNode, Lang, SyntaxNode, SyntaxNodePtr},
    tast,
    tast::{
        lower_to_hir, typecheck_body, typecheck_items, Attribute, AttributeChecker, AttributeCheckerImpl,
        AttributeMultiplicity, AttributeTarget, Def, DefId, LocalDefId, TypeCtxt,
    },
    termcolor,
    utils::{Id, IndexMap, IndexVecMap},
};
use codespan_reporting::term;
use rowan::{ast::AstPtr, GreenNode};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt, fs,
    marker::PhantomData,
    path::PathBuf,
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

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Arguments of a package name.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PackageArg<'a> {
    /// String argument.
    String(Cow<'a, str>),
    /// Integer argument.
    Int(i32),
}

impl fmt::Display for PackageArg<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PackageArg::String(s) => write!(f, "{s:?}"),
            PackageArg::Int(i) => write!(f, "{i}"),
        }
    }
}

impl<'a> From<&'a str> for PackageArg<'a> {
    fn from(s: &'a str) -> Self {
        PackageArg::String(Cow::Borrowed(s))
    }
}

impl From<String> for PackageArg<'_> {
    fn from(s: String) -> Self {
        PackageArg::String(Cow::Owned(s))
    }
}

impl From<i32> for PackageArg<'_> {
    fn from(i: i32) -> Self {
        PackageArg::Int(i)
    }
}

/// Package name. Composed of a base name and a list of arguments.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageName<'a> {
    pub base_name: Cow<'a, str>,

    //pub args: SmallVec<[PackageArg<'a>; 2]>,
    // can't use SmallVec because of a variance issue: https://github.com/servo/rust-smallvec/issues/146
    // at least it won't allocate by default
    pub args: Vec<PackageArg<'a>>,
}

impl fmt::Display for PackageName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.base_name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl<'a> PackageName<'a> {
    pub fn new(base_name: impl Into<Cow<'a, str>>) -> Self {
        // TODO: validate name
        Self {
            base_name: base_name.into(),
            args: vec![],
        }
    }

    pub fn arg(mut self, arg: impl Into<PackageArg<'a>>) -> Self {
        self.args.push(arg.into());
        self
    }

    pub fn into_static(self) -> PackageName<'static> {
        PackageName {
            base_name: Cow::Owned(self.base_name.into_owned()),
            args: self
                .args
                .into_iter()
                .map(|arg| match arg {
                    PackageArg::String(s) => PackageArg::String(Cow::Owned(s.into_owned())),
                    PackageArg::Int(i) => PackageArg::Int(i),
                })
                .collect(),
        }
    }
}

impl<'a> From<&'a str> for PackageName<'a> {
    fn from(s: &'a str) -> Self {
        Self::new(s)
    }
}

impl From<String> for PackageName<'_> {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}

#[derive(Debug, thiserror::Error)]
#[error("package resolution error")]
pub struct PackageResolutionError(#[from] anyhow::Error);

/// The API is simply name (+arguments) -> Package (set of definitions).
///
/// The resolver is responsible for looking up the package name in the filesystem, or other sources
/// (it could be a procedurally generated package).
pub trait PackageResolver {
    /// Resolves the package by name.
    fn resolve(&self, cache: &mut Session, name: &PackageName) -> Result<PackageId, PackageResolutionError>;
}

/// A package resolver that does nothing.
pub struct DummyPackageResolver;

impl PackageResolver for DummyPackageResolver {
    fn resolve(&self, _session: &mut Session, _name: &PackageName) -> Result<PackageId, PackageResolutionError> {
        Err(PackageResolutionError(anyhow::anyhow!(
            "dummy resolver could not resolve package"
        )))
    }
}

pub struct FileSystemPackageResolver {
    paths: Vec<PathBuf>,
}

impl FileSystemPackageResolver {
    pub fn new(paths: Vec<PathBuf>) -> Self {
        Self { paths }
    }
}

impl PackageResolver for FileSystemPackageResolver {
    fn resolve(&self, session: &mut Session, name: &PackageName) -> Result<PackageId, PackageResolutionError> {
        let mut path = PathBuf::new();
        path.push(name.base_name.as_ref());
        path.push(".glsl");
        for p in self.paths.iter() {
            let mut full_path = p.clone();
            full_path.push(&path);
            if full_path.exists() {
                let source_path = full_path.to_string_lossy().into_owned();
                let source_text =
                    fs::read_to_string(&full_path).map_err(|e| PackageResolutionError(anyhow::Error::new(e)))?;
                let package = session.create_source_package(name.clone(), &source_path, source_text.as_str());
                return Ok(package);
            }
        }
        Err(PackageResolutionError(anyhow::anyhow!(
            "could not find package `{}` in paths",
            name
        )))
    }
}

pub enum PackageStatus {
    /// AST not parsed yet.
    Unparsed,
    /// Currently parsing the AST.
    Parsing,
    /// AST parsed, but not type-checked yet.
    Parsed,
    /// Currently type-checking the AST.
    Checking,
    /// AST type-checked.
    Checked,
}

pub struct PackageSyntax {
    source_id: SourceId,
    green_node: GreenNode,
    root: SyntaxNodePtr,
}

pub struct PackageCacheEntry {
    /// AST of the package (green tree + root node).
    pub(crate) syntax: Option<PackageSyntax>,
    pub(crate) status: PackageStatus,
    /// Type-checked package module (not including bodies).
    pub(crate) module: Option<tast::Module>,
    /// Type-checked definition bodies.
    pub(crate) bodies: IndexVecMap<LocalDefId, tast::TypedBody>,
}

pub type PackageId = Id<PackageCacheEntry>;

pub struct DefDebug<'a>(DefId, &'a Def);

impl<'a> fmt::Debug for DefDebug<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "DefId({:04x}:{:04x} `{}`)",
            self.0.package.0, self.0.local_def.0, self.1.name
        )
    }
}

pub struct Pkgs {
    packages: IndexMap<PackageName<'static>, PackageCacheEntry, PackageId>,
}

impl Pkgs {
    /// Resolves a definition by ID.
    pub fn def(&self, id: DefId) -> &Def {
        if let Some(ref tast) = self.packages[id.package].module {
            tast.defs.get(id.local_def).expect("invalid DefId")
        } else {
            todo!("typecheck module")
        }
    }

    /// Returns a debug proxy for the definition.
    pub fn debug_def(&self, id: DefId) -> DefDebug {
        DefDebug(id, self.def(id))
    }

    /// Returns the typed ast for the specified package.
    pub fn module(&self, id: PackageId) -> Option<&tast::Module> {
        self.packages[id].module.as_ref()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SessionError {
    #[error("an attribute with the same name already exists")]
    AttributeAlreadyExists,
}

/// Registered attribute.
pub(crate) struct AttributeRegistration {
    pub(crate) name: String,
    pub(crate) valid_targets: AttributeTarget,
    pub(crate) multiplicity: AttributeMultiplicity,
    pub(crate) mutually_exclusive: Vec<String>,
    pub(crate) checker: Box<dyn AttributeChecker + Send>,
}

/// Compilation session.
pub struct Session {
    /// Diagnostics output
    pub diag: Diagnostics,
    /// Types
    pub tyctxt: TypeCtxt,
    /// Packages & associated modules and definitions.
    pub pkgs: Pkgs,
    pub(crate) source_files: SourceFileProvider,
    resolver: Arc<dyn PackageResolver + Sync + Send>,

    /// Registered custom attributes.
    pub(crate) custom_attributes: HashMap<String, AttributeRegistration>,
}

impl Session {
    /// Creates a new session with a dummy package resolver, and print diagnostics to stderr.
    pub fn new() -> Session {
        let source_files = SourceFileProvider::new();
        let mut diag = Diagnostics::new(source_files.clone());
        diag.add_sink(TermDiagnosticSink::new(term::Config::default()));
        Self {
            diag,
            tyctxt: TypeCtxt::new(),
            source_files,
            resolver: Arc::new(DummyPackageResolver),
            pkgs: Pkgs {
                packages: IndexMap::new(),
            },
            custom_attributes: Default::default(),
        }
    }

    /// Sets the package resolver.
    pub fn with_package_resolver(mut self, resolver: impl PackageResolver + Sync + Send + 'static) -> Session {
        self.resolver = Arc::new(resolver);
        self
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

    /*/// Sets the diagnostics output.
    pub fn with_diagnostic_output(mut self, diag: impl WriteColor + 'a) -> Session<'a> {
        self.diag = diag;
        self
    }*/

    /// Gets the package with the given name, or creates it if it doesn't exist.
    pub(crate) fn get_or_create_package<'b>(&mut self, name: impl Into<PackageName<'b>>) -> (PackageId, bool) {
        let name = name.into();
        if let Some((id, _, _)) = self.pkgs.packages.get_full(&name) {
            return (id, false);
        }
        let name = name.into_static();
        let (id, _) = self.pkgs.packages.insert_full(
            name,
            PackageCacheEntry {
                syntax: None,
                status: PackageStatus::Unparsed,
                module: None,
                bodies: IndexVecMap::new(),
                //hir: None,
            },
        );
        (id, true)
    }

    //----------------------------------------------------------------------------------------------
    // QUERIES

    /// Resolves the package with the given name.
    pub fn resolve_package(&mut self, name: &PackageName) -> Result<PackageId, PackageResolutionError> {
        let resolver = self.resolver.clone();
        resolver.resolve(self, name)
    }

    /// Returns all definitions in the specified package.
    ///
    /// May trigger type-checking and resolution of definitions (but not bodies) if necessary.
    pub fn definitions(&mut self, id: PackageId) -> Result<Vec<LocalDefId>, QueryError> {
        Ok(self.get_module(id)?.definitions().map(|(id, _)| id).collect())
    }

    /// Returns the packages directly imported by the specified package.
    pub fn imports(&mut self, id: PackageId) -> Result<Vec<PackageId>, QueryError> {
        Ok(self.get_module(id)?.imported_packages.clone())
    }

    /// Returns the transitive list of dependencies of the specified package.
    pub fn dependencies(&mut self, id: PackageId) -> Result<Vec<PackageId>, QueryError> {
        let mut deps = HashSet::new();
        let mut visit = vec![id];
        while let Some(package) = visit.pop() {
            let module = self.get_module(package)?;
            for import in module.imported_packages.iter() {
                if deps.insert(*import) {
                    visit.push(*import);
                }
            }
        }
        Ok(deps.into_iter().collect())
    }

    /// Returns the AST of the specified package.
    ///
    /// Panics if the id doesn't refer to a source package.
    pub fn get_ast(&self, id: PackageId) -> Result<ast::Root, QueryError> {
        let syntax = self.pkgs.packages[id].syntax.as_ref().ok_or(QueryError::NoSource)?;
        let syntax_node = SyntaxNode::new_root(syntax.green_node.clone());
        Ok(ast::Root {
            source_id: syntax.source_id,
            module: ast::Module::cast(syntax_node).expect("invalid AST"),
        })
    }

    /// Returns the typed AST module for the given package.
    ///
    /// May trigger type-checking and resolution of definitions (but not bodies) if necessary.
    /// The module may have errors.
    pub fn get_module(&mut self, package: PackageId) -> Result<&tast::Module, QueryError> {
        if self.pkgs.packages[package].module.is_none() {
            let ast = self.get_ast(package)?;
            let module = typecheck_items(self, package, ast);
            let p = &mut self.pkgs.packages[package];
            p.module = Some(module);
        }

        Ok(self.pkgs.packages[package].module.as_ref().unwrap())
    }

    /// Returns an already computed type-checked module.
    pub fn get_module_cached(&self, package: PackageId) -> Result<&tast::Module, QueryError> {
        self.pkgs.packages[package]
            .module
            .as_ref()
            .ok_or(QueryError::NotInCache)
    }

    fn typecheck_bodies_inner(&mut self, package: PackageId) -> Result<(), QueryError> {
        let defs = self.definitions(package)?;
        for def in defs {
            let tbody = typecheck_body(self, DefId::new(package, def))?;
            self.pkgs.packages[package].bodies.insert(def, tbody);
        }
        Ok(())
    }

    /// Type-checks all bodies in the given package and their dependencies.
    pub fn typecheck_bodies(&mut self, package: PackageId) -> Result<(), QueryError> {
        let dependencies = self.dependencies(package)?;
        for dep in dependencies {
            self.typecheck_bodies_inner(dep)?;
        }
        self.typecheck_bodies_inner(package)?;
        Ok(())
    }

    /// Type-checks the body of the specified definition.
    pub fn get_typed_body(&mut self, def: DefId) -> Result<&tast::TypedBody, QueryError> {
        // ensure that the typed AST is built
        let _ = self.get_module(def.package);
        if self.pkgs.packages[def.package].bodies.get(def.local_def).is_none() {
            // typecheck body
            let typechecked = typecheck_body(self, def)?;
            self.pkgs.packages[def.package]
                .bodies
                .insert(def.local_def, typechecked);
        }

        // body must be there otherwise we'd have returned early due to an error condition
        Ok(self.pkgs.packages[def.package].bodies.get(def.local_def).unwrap())
    }

    pub fn get_typed_body_cached(&self, def: DefId) -> Result<&tast::TypedBody, QueryError> {
        self.pkgs.packages[def.package]
            .bodies
            .get(def.local_def)
            .ok_or(QueryError::NotInCache)
    }

    pub(crate) fn get_ast_node<N: AstNode<Language = Lang>>(
        &self,
        package_id: PackageId,
        ptr: &AstPtr<N>,
    ) -> Result<N, QueryError> {
        let package_syntax = self.get_ast(package_id)?;
        Ok(ptr.to_node(package_syntax.module.syntax()))
    }

    /// Compiles the package to HIR (in-memory SPIR-V).
    pub fn compile_to_hir(&mut self, package: PackageId) -> Result<hir::Module, QueryError> {
        lower_to_hir(self, package)
    }

    /// Compiles the package and its dependencies to a standalone SPIR-V module.
    ///
    /// This includes all dependencies into the module.
    pub fn compile_to_spirv(&mut self, package: PackageId) -> Result<Vec<u32>, QueryError> {
        let hir = lower_to_hir(self, package)?;
        let spv_bytecode = hir::transform::write_spirv(&hir);
        Ok(spv_bytecode)
    }

    //----------------------------------------------------------------------------------------------
    // Provider entry points

    /// Provides a package as a source file.
    ///
    /// # Arguments
    /// * package package name
    /// * file_name identifier for the associated source file, usually a path
    /// * source source text
    pub fn create_source_package<'b>(
        &mut self,
        package_name: impl Into<PackageName<'b>>,
        file_name: &str,
        source: &str,
    ) -> PackageId {
        // FIXME: if called again, should invalidate stuff that changed
        let (package_id, created) = self.get_or_create_package(package_name);
        assert!(created);
        assert!(matches!(self.pkgs.packages[package_id].status, PackageStatus::Unparsed));
        self.pkgs.packages[package_id].status = PackageStatus::Parsing;
        let source_id = self.source_files.register_source(file_name, source);
        let green_node = syntax::parse(self, source, source_id);
        let root = SyntaxNode::new_root(green_node.clone());
        self.pkgs.packages[package_id].syntax = Some(PackageSyntax {
            source_id,
            green_node,
            root: SyntaxNodePtr::new(&root),
        });
        self.pkgs.packages[package_id].status = PackageStatus::Parsed;
        package_id
    }

    /// Provides a package as a set of definitions.
    pub fn create_def_package(&mut self, _package: PackageName) -> PackageId {
        todo!()
    }

    /*pub fn definitions(&mut self, package_id: PackageId) -> &tast::Module {
        let entry = &mut self.entries[package_id];
        assert!(matches!(entry.status, PackageStatus::Parsed));
        entry.status = PackageStatus::Checking;
        let package_name = self.entries.get_key(package_id).unwrap();
        let package_id = self.resolver.resolve(self, package_name)?;
        entry.status = PackageStatus::Checked;
        Ok(())
    }*/
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// Query system

////////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn package_names() {
        let name = String::from("foo");
        let name2 = String::from("foo2");
        let package_name = PackageName::new(&name);
        let package_name_with_args = PackageName::new(&name).arg(42);
        let same_package_name_with_args = PackageName::new(&name).arg(42);

        eprintln!("{}", package_name);
        eprintln!("{}", package_name_with_args);
        eprintln!("{}", same_package_name_with_args);

        let mut cache = Session::new();
        let (id, created) = cache.get_or_create_package(package_name);
        assert!(created);
        let (id2, created) = cache.get_or_create_package(package_name_with_args.clone());
        assert!(created);
        let (id3, created) = cache.get_or_create_package(package_name_with_args.clone());
        assert_eq!(id2, id3);
        assert!(!created);
    }
}
