use ashley::{
    builtins::PrimitiveTypes,
    diagnostic::{Diagnostics, SourceFileProvider},
    hir, syntax,
    syntax::{ast, ast::AstNode},
    tast::{lower_to_hir, DummyPackageResolver, Module, TypeCtxt},
};
use codespan_reporting::{
    term,
    term::termcolor::{ColorChoice, StandardStream},
};
use spirv_tools::{
    assembler::{Assembler, DisassembleOptions},
    val::{Validator, ValidatorOptions},
};
use std::{fs, path::Path, sync::Arc};

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

fn test_lower_one(path: &Path) {
    let source: Arc<str> = fs::read_to_string(path).unwrap().into();
    let sources = SourceFileProvider::new();
    let src_id = sources.register_source(path.to_str().unwrap(), source.clone());
    let mut writer = StandardStream::stderr(ColorChoice::Always);
    let mut diag = Diagnostics::new(sources, src_id, &mut writer, term::Config::default());

    // 0. parse
    let ast_root = syntax::parse(&source, src_id, &mut diag);

    // 1. typecheck
    let mut type_ctxt = TypeCtxt::new();
    let mut package_resolver = DummyPackageResolver;
    let module = type_ctxt.typecheck_items(ast_root, &mut package_resolver, &mut diag);
    let mut bodies = Vec::new();
    for (def_id, def) in module.definitions() {
        if def.builtin {
            continue;
        }
        let body = type_ctxt.typecheck_body(&module, def_id, &mut diag);
        bodies.push(body);
    }

    // 2. lower to HIR
    let hir = lower_to_hir(&mut type_ctxt, module, &mut diag);

    // 3. emit SPIR-V bytecode
    let spv_bytecode = ashley::hir::transform::write_spirv(&hir);
    validate_spirv(&spv_bytecode);

    // 4. load bytecode with rspirv and disassemble
    let assembler = spirv_tools::assembler::create(None);
    let dis = assembler
        .disassemble(&spv_bytecode, DisassembleOptions::default())
        .unwrap()
        .unwrap();
    insta::assert_snapshot!(dis)
}

#[test]
fn test_lower() {
    insta::glob!("inputs/*.glsl", |path| { test_lower_one(path) });
}
