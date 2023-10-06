extern crate self as ashley; // so that we can use ashley-derive proc-macros in this crate
#[macro_use]
extern crate tracing; // to get the log macros without the need for `use`

pub mod builtins;
mod constant_value;
mod db;
pub mod def;
pub mod diagnostic;
pub mod ir;
mod layout;
mod package_name;
pub mod resolver;
mod source_file;
pub mod syntax;
//pub mod tast;
pub mod attribute;
pub mod ty;
pub mod utils;

pub use constant_value::ConstantValue;
pub use db::{Compiler, CompilerDb, ModuleId, QueryError};
pub use package_name::{ModuleName, PackageArg};
pub use resolver::FileSystemPackageResolver;
pub use source_file::{LineCharacterPosition, LineCharacterRange, SourceFile, SourceFileId};

// re-export codespan_reporting and termcolor so that it's easier to set up a session
pub use codespan_reporting::{self, term::termcolor};
