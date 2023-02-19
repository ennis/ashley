extern crate self as ashley;

pub mod builtins;
pub mod diagnostic;
pub mod hir;
pub mod package;
pub mod syntax;
pub mod tast;
mod utils;
mod session;

pub use session::Session;

// re-export codespan_reporting and termcolor so that it's easier to set up a session
pub use codespan_reporting;
pub use codespan_reporting::term::termcolor;


// The main entry point for the compiler.
//pub fn compile()