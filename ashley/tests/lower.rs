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
use rspirv::binary::Disassemble;
use spirv_tools::{
    val::{Validator, ValidatorOptions},
    Error, TargetEnv,
};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};
use spirv_tools::assembler::DisassembleOptions;
use spirv_tools::assembler::Assembler;

fn validate_spirv(code: &[u32]) {
    let validator = spirv_tools::val::create(None);
    let result = validator.validate(
        code,
        Some(ValidatorOptions {
            relax_struct_store: false,
            relax_logical_pointer: false,
            before_legalization: false,
            relax_block_layout: None,
            uniform_buffer_standard_layout: false,
            scalar_block_layout: false,
            skip_block_layout: false,
            max_limits: vec![],
        }),
    );
    match result {
        Ok(_) => {}
        Err(err) => {
            eprintln!("SPIR-V validation failed: {}", err)
        }
    }
}

#[test]
fn test_lower() {
    insta::glob!("inputs/*.glsl", |path| {

        let mut sources = ashley::diagnostic::SourceFileProvider::new();
        let source: Arc<str> = fs::read_to_string(path).unwrap().into();
        let src_id = sources.register_source(path.to_str().unwrap(), source.clone());
        let diag = ashley::diagnostic::Diagnostics::new(source_provider, file, diag_writer, term::Config::default());

        let ast_module;

        // 1. parse AST
        {
            let writer = StandardStream::stderr(ColorChoice::Always);
            ast_module = Module::cast(ashley::syntax::parse(&source, src_id, sources.clone(), writer)).unwrap();
        }

        // 2. lower AST to HIR
        let mut hir = hir::Module::new();
        {
            let writer = StandardStream::stderr(ColorChoice::Always);
            ashley::lower::lower(&mut hir, ast_module, src_id, sources.clone(), writer);
        }

        // 3. emit SPIR-V bytecode
        let spv_bytecode = ashley::hir::transform::write_spirv(&hir);
        validate_spirv(&spv_bytecode);

        // 4. load bytecode with rspirv and disassemble
        let assembler = spirv_tools::assembler::create(None);
        let dis = assembler.disassemble(&spv_bytecode, DisassembleOptions::default()).unwrap().unwrap();
        insta::assert_snapshot!(dis)
    });
}
