use ashley::{
    syntax::{Lang, SyntaxNode},
    Session,
};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::{fs, sync::Arc};

#[test]
fn test_syntax() {
    insta::glob!("inputs/*.glsl", |path| {
        let source: Arc<str> = fs::read_to_string(path).unwrap().into();
        let mut sess = Session::new();
        let file_stem = path.file_stem().unwrap().to_str().unwrap();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let pkg = sess.create_source_package(file_stem, file_name, &source);
        let ast = sess.get_ast(pkg).unwrap();
        insta::assert_debug_snapshot!(ast)
    });
}
