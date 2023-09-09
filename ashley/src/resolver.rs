//! Package resolvers.
use crate::{session::CompilerDb, ModuleName};
use std::path::PathBuf;

/*/// Error trying to resolve a package name.
#[derive(Debug, thiserror::Error)]
#[error("package resolution error")]
pub struct PackageResolutionError(#[from] anyhow::Error);*/

/// The result of package name resolution.
pub enum PackageResolution {
    /// The package name resolved to a source file.
    Source { path: PathBuf, text: String },
}

/// The API is simply name (+arguments) -> Package (set of definitions).
///
/// The resolver is responsible for looking up the package name in the filesystem, or other sources
/// (it could be a procedurally generated package).
pub trait PackageResolver {
    /// Resolves the package by name.
    ///
    /// If resolution fails, the resolver should return `None` and emit a diagnostic.
    fn resolve(&self, compiler: &dyn CompilerDb, name: &ModuleName) -> Option<PackageResolution>;
}

/// A package resolver that does nothing.
pub struct DummyPackageResolver;

impl PackageResolver for DummyPackageResolver {
    fn resolve(&self, compiler: &dyn CompilerDb, name: &ModuleName) -> Option<PackageResolution> {
        compiler.diag_error(format!("unknown package `{name}`")).emit();
        None
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
    fn resolve(&self, _compiler: &dyn CompilerDb, _name: &ModuleName) -> Option<PackageResolution> {
        todo!("FileSystemPackageResolver")
        /*let mut path = PathBuf::new();
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
        )))*/
    }
}
