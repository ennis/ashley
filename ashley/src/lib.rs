extern crate self as ashley;
#[macro_use]
extern crate tracing;

pub mod builtins;
pub mod diagnostic;
pub mod hir;
pub mod query;
mod session;
pub mod syntax;
pub mod tast;
pub mod utils;

pub use session::{DefDebug, FileSystemPackageResolver, QueryError, Session};

// re-export codespan_reporting and termcolor so that it's easier to set up a session
pub use codespan_reporting::{self, term::termcolor};

// The main entry point for the compiler.
//pub fn compile()
