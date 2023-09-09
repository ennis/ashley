extern crate self as ashley; // so that we can use ashley-derive proc-macros in this crate
#[macro_use]
extern crate tracing; // to get the log macros without the need for `use`

pub mod builtins;
pub mod diagnostic;
pub mod hir;
mod package_name;
pub mod resolver;
mod session;
mod source_file;
pub mod syntax;
pub mod tast;
pub mod utils;

pub use package_name::{ModuleName, PackageArg};
pub use resolver::FileSystemPackageResolver;
pub use session::{Compiler, CompilerDb, DefDebug, QueryError};
pub use source_file::SourceFile;

// re-export codespan_reporting and termcolor so that it's easier to set up a session
pub use codespan_reporting::{self, term::termcolor};
