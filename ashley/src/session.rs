//! Compilation cache
use crate::{
    diagnostic::{Diagnostics, SourceFileProvider, SourceId},
    hir, syntax,
    syntax::ast,
    tast,
    tast::TypeCtxt,
    termcolor,
    utils::{Id, TypedIndexMap},
};

use crate::termcolor::WriteColor;
use codespan_reporting::{
    term,
    term::termcolor::{ColorChoice, StandardStream},
};
use indexmap::IndexMap;
use smallvec::SmallVec;
use std::{borrow::Cow, collections::HashMap, fmt, fs, path::PathBuf, sync::Arc};
use thiserror::Error;

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

#[derive(Debug, Error)]
#[error("package resolution error")]
pub struct PackageResolutionError(#[from] anyhow::Error);

/// The API is simply name (+arguments) -> Package (set of definitions).
///
/// The resolver is responsible for looking up the package name in the filesystem, or other sources
/// (it could be a procedurally generated package).
pub trait PackageResolver {
    /// Resolves the package by name and imports its definitions into the module.
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
                let (package, _) = session.get_or_create_package(name.clone());
                session.parse(package, &source_path, source_text.as_str());
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

pub struct PackageCacheEntry {
    /// AST of the package.
    pub(crate) ast: Option<ast::Root>,
    pub(crate) status: PackageStatus,
    pub(crate) module: Option<tast::Module>,
    pub(crate) bodies: Option<Vec<tast::TypedBody>>,
    pub(crate) hir: Option<hir::Module>,
}

pub type PackageId = Id<PackageCacheEntry>;

/// Compilation session.
pub struct Session<'a> {
    pub diag: Diagnostics<'a>,
    pub tyctxt: TypeCtxt,
    pub(crate) source_files: SourceFileProvider,
    resolver: Arc<dyn PackageResolver>,
    entries: TypedIndexMap<PackageName<'static>, PackageCacheEntry>,
}

impl<'a> Session<'a> {
    /// Creates a new compilation cache with a dummy package resolver, and print diagnostics to stdout.
    pub fn new() -> Session<'a> {
        let source_files = SourceFileProvider::new();
        let diag = Diagnostics::new(
            source_files.clone(),
            termcolor::StandardStream::stderr(termcolor::ColorChoice::Always),
            term::Config::default(),
        );
        Self {
            diag,
            tyctxt: TypeCtxt::new(),
            source_files,
            resolver: Arc::new(DummyPackageResolver),
            entries: TypedIndexMap::new(),
        }
    }

    /// Sets the package resolver.
    pub fn with_package_resolver(mut self, resolver: impl PackageResolver + 'static) -> Session<'a> {
        self.resolver = Arc::new(resolver);
        self
    }

    /// Sets the diagnostics output.
    pub fn with_diagnostic_output(mut self, writer: impl WriteColor + 'a) -> Session<'a> {
        self.diag = Diagnostics::new(self.source_files.clone(), writer, term::Config::default());
        self
    }

    /// Gets the package with the given name, or creates it if it doesn't exist.
    pub fn get_or_create_package<'b>(&mut self, name: impl Into<PackageName<'b>>) -> (PackageId, bool) {
        let name = name.into();
        if let Some((id, _, _)) = self.entries.get_full(&name) {
            return (id, false);
        }
        let name = name.into_static();
        let (id, _) = self.entries.insert_full(
            name,
            PackageCacheEntry {
                ast: None,
                status: PackageStatus::Unparsed,
                module: None,
                bodies: None,
                hir: None,
            },
        );
        (id, true)
    }

    /// Resolves the package with the given name.
    pub fn resolve_package(&mut self, name: &PackageName) -> Result<PackageId, PackageResolutionError> {
        let resolver = self.resolver.clone();
        resolver.resolve(self, name)
    }

    pub fn package(&mut self, id: PackageId) -> &PackageCacheEntry {
        &self.entries[id]
    }

    pub fn package_mut(&mut self, id: PackageId) -> &mut PackageCacheEntry {
        &mut self.entries[id]
    }

    pub fn parse(
        &mut self,
        package_id: PackageId,
        file_name: &str,
        source: &str,
    ) -> ast::Root {
        if let Some(ref ast) = self.entries[package_id].ast {
            return ast.clone();
        }

        assert!(matches!(self.entries[package_id].status, PackageStatus::Unparsed));
        self.entries[package_id].status = PackageStatus::Parsing;

        let source_id = self.source_files.register_source(file_name, source);
        let root = syntax::parse(self, source, source_id);

        self.entries[package_id].ast = Some(root.clone());
        self.entries[package_id].status = PackageStatus::Parsed;
        root
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
