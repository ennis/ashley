use ashley::{
    builtins::BuiltinTypes,
    diagnostic::{Diagnostics, SourceFileProvider},
    syntax,
    syntax::{ast, ast::AstNode},
    tast::{DummyPackageResolver, Module},
};
use codespan_reporting::{
    term,
    term::termcolor::{ColorChoice, StandardStream},
};
use ashley::tast::TypeCtxt;

#[test]
fn type_ctxt() {
    use std::{fs, sync::Arc};

    insta::glob!("inputs/*.glsl", |path| {
        let source: Arc<str> = fs::read_to_string(path).unwrap().into();
        let sources = ashley::diagnostic::SourceFileProvider::new();
        let src_id = sources.register_source(path.to_str().unwrap(), source.clone());
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        let mut diag = Diagnostics::new(sources, src_id, &mut writer, term::Config::default());

        let ast_root = syntax::parse(&source, src_id, &mut diag);

        let mut type_ctxt = TypeCtxt::new();

        let mut package_resolver = DummyPackageResolver;
        let module = type_ctxt.typecheck_items(ast_root, &mut package_resolver, &mut diag);

        insta::assert_debug_snapshot!(module);
    });
}
