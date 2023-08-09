use ashley::{
    syntax::{ast::AstNode, Lang, SyntaxNode},
    Session,
};
use std::{fs, path::Path, sync::Arc};

#[test]
fn test_syntax() {
    fn test_syntax_one(path: &Path) {
        let source: Arc<str> = fs::read_to_string(path).unwrap().into();
        let mut sess = Session::new();
        let file_stem = path.file_stem().unwrap().to_str().unwrap();
        let file_name = path.file_name().unwrap().to_str().unwrap();
        let pkg = sess.create_source_package(file_stem, file_name, &source);
        let module_syntax = sess.get_ast(pkg).unwrap().module.syntax().clone();
        insta::assert_debug_snapshot!(module_syntax)
    }
    insta::glob!("inputs/syntax/*.glsl", test_syntax_one);
    insta::glob!("inputs/*.glsl", test_syntax_one);
}
