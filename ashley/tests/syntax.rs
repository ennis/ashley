
use std::fs;
use ashley::syntax::{Lang, Session, SyntaxNode};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

#[test]
fn test_syntax() {
    insta::glob!("inputs/*.as", |path| {
        let source = fs::read_to_string(path).unwrap();
        let mut sess = ashley::syntax::Session::new();
        let src_id = sess.register_source(path.to_str().unwrap(), source);
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        insta::assert_debug_snapshot!(sess.parse(src_id, &mut writer))
    });
}