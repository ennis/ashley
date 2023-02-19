use std::path::PathBuf;

/// A package resolver that looks up packages in the filesystem.
pub struct FilesystemPackageResolver {
    root: PathBuf,
}