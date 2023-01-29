use ashley::{
    hir,
    syntax::{
        ast,
        ast::{AstNode, Module},
        Lang, SyntaxNode,
    },
};
use codespan_reporting::{
    diagnostic::Diagnostic,
    term::termcolor::{ColorChoice, StandardStream},
};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};
use rspirv::binary::Disassemble;

#[test]
fn test_lower() {
    insta::glob!("inputs/*.as", |path| {
        let source: Arc<str> = fs::read_to_string(path).unwrap().into();
        let mut sources = ashley::diagnostic::SourceFileProvider::new();
        let src_id = sources.register_source(path.to_str().unwrap(), source.clone());
        let ast_module;

        {
            let writer = StandardStream::stderr(ColorChoice::Always);
            ast_module = Module::cast(ashley::syntax::parse(&source, src_id, sources.clone(), writer)).unwrap();
        }

        let mut hir = hir::Module::new();

        {
            let writer = StandardStream::stderr(ColorChoice::Always);
            ashley::lower::lower(&mut hir, ast_module, src_id, sources.clone(), writer);
        }

        let spirv = ashley::hir::transform::write_spirv(&hir);

        // load and format spir-v
        let spirv = rspirv::dr::load_words(&spirv).unwrap();
        let dis = spirv.disassemble();
        insta::assert_snapshot!(dis)
    });
}
