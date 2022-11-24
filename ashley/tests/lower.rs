use ashley::syntax::{ast, Lang, SyntaxNode};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::{fs, sync::Arc};
use std::path::{Path, PathBuf};
use codespan_reporting::diagnostic::Diagnostic;
use ashley::hir::{HirArena, HirCtxt, write_hir_html_file};
use ashley::syntax::ast::{AstNode, Module};

#[test]
fn test_lower() {
    insta::glob!("inputs/*.as", |path| {
        let source: Arc<str> = fs::read_to_string(path).unwrap().into();
        let mut sources = ashley::diagnostic::SourceFileProvider::new();
        let src_id = sources.register_source(path.to_str().unwrap(), source.clone());
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        let module = Module::cast(ashley::syntax::parse(&source, src_id, sources.clone(), &mut writer)).unwrap();

        let hir_arena = HirArena::new();
        let mut hir_ctxt = HirCtxt::new(&hir_arena);
        let region = ashley::lower::lower_module(&mut hir_ctxt, module, src_id, sources.clone(), &mut writer);

        let root = env!("CARGO_MANIFEST_DIR");
        let file_name = Path::new(path).file_name().unwrap().to_str().unwrap();
        let out_path = PathBuf::from(root).join("tests/out").join(format!("{file_name}.out.html"));
        write_hir_html_file(&hir_ctxt, out_path.to_str().unwrap(), region, sources.clone());
    });
}
