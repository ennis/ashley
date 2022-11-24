use ashley::syntax::{Lang, SyntaxNode};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::{fs, sync::Arc};

#[test]
fn test_syntax() {
    insta::glob!("inputs/*.as", |path| {
        let source: Arc<str> = fs::read_to_string(path).unwrap().into();
        let mut sources = ashley::diagnostic::SourceFileProvider::new();
        let src_id = sources.register_source(path.to_str().unwrap(), source.clone());
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        insta::assert_debug_snapshot!(ashley::syntax::parse(&source, src_id, sources, &mut writer))
    });
}
