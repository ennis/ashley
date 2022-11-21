extern crate self as ashley;

pub mod diagnostic;
pub mod dialect;
pub mod syntax;
pub mod hir;
mod id_vec;
mod lower;
mod utils;

// High-level workflow:
// - register source code into the session object
// - possibly load serialized modules into the session
// - query the ast of a file
//
//      sess.ast(source_id, diag) -> Option<Module>
//
// - query the HIR of a file
//
//      let hir_session = HirSession::new(...)
//      let hir_mod = hir.module(source_id);
//      let hir_lowered = hir.lowered_shader_module(source_id);
//
// - pass: shader module lowering (module -> module with only functions)
//         result: ShaderNode
//
// - create a new HIR module:
//
//      let builder = HirBuilder::new();
//      builder.build_function(...);
//
//
// Three sessions:
//      - syntax session, for parsing to ASTs, syntax highlighting
//      - HIR session, for getting the HIR and applying transformations
//      - CG session, for codegen
//
// CG depends on HIR, HIR depends on syntax
//
// What to codegen?
// - the codegen granularity is the whole session
