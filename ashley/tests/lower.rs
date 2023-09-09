use ashley::{
    hir,
    hir::TypeData,
    tast::{lower_to_hir, Module},
    Compiler, CompilerDb, ModuleName,
};
use spirv::StorageClass;
use spirv_tools::{
    assembler::{Assembler, DisassembleOptions},
    val::{Validator, ValidatorOptions},
};
use std::{fs, path::Path, sync::Arc};

fn validate_spirv(module_name: &str, code: &[u32]) {
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
            eprintln!("[{}] SPIR-V validation failed: {}", module_name, err)
        }
    }
}

fn print_interface(hir: &hir::Module) {
    let mut inputs = vec![];
    let mut outputs = vec![];
    let mut images = vec![];
    let mut uniforms = vec![];
    let mut samplers = vec![];
    let mut storage_buffers = vec![];

    for (g, gdata) in hir.globals.iter() {
        match gdata.storage_class {
            StorageClass::Input => {
                inputs.push(g);
            }
            StorageClass::Output => {
                outputs.push(g);
            }
            StorageClass::Uniform => match hir.types[gdata.ty] {
                TypeData::Pointer {
                    storage_class,
                    pointee_type,
                } => match storage_class {
                    StorageClass::Uniform => {
                        uniforms.push(g);
                    }
                    StorageClass::StorageBuffer => {
                        storage_buffers.push(g);
                    }
                    _ => {}
                },
                TypeData::Image(img) => {
                    images.push(g);
                }
                TypeData::Sampler => {
                    samplers.push(g);
                }
                TypeData::SamplerShadow => {
                    samplers.push(g);
                }
                _ => {}
            },
            _ => {}
        }
    }

    if !inputs.is_empty() {
        eprintln!("Inputs: \n");
        for input in inputs.iter() {
            let gd = &hir.globals[*input];
            eprintln!("\t {} : {:?}", gd.name, hir.debug_type(gd.ty));
        }
    }
    if !outputs.is_empty() {
        eprintln!("\nOutputs: \n");
        for output in outputs.iter() {
            let gd = &hir.globals[*output];
            eprintln!("\t {} : {:?}", gd.name, hir.debug_type(gd.ty));
        }
    }
    if !uniforms.is_empty() {
        eprintln!("\nUniforms: \n");
        for uniform in uniforms.iter() {
            let gd = &hir.globals[*uniform];
            eprintln!("\t {} : {:?}", gd.name, hir.debug_type(gd.ty));
        }
    }
    if !images.is_empty() {
        eprintln!("\nTextures: \n");
        for tex in images.iter() {
            let gd = &hir.globals[*tex];
            eprintln!("\t {} : {:?}", gd.name, hir.debug_type(gd.ty));
        }
    }
}

fn test_lower_one(path: &Path) {
    let source: Arc<str> = fs::read_to_string(path).unwrap().into();
    let mut compiler = Compiler::new();

    let file_stem = path.file_stem().unwrap().to_str().unwrap();
    let file_name = path.file_name().unwrap().to_str().unwrap();

    // 0. parse
    let (pkg, _) = compiler.create_source_module(ModuleName::from(file_stem), file_name, &source);

    // 2. lower to HIR
    let hir = lower_to_hir(&compiler, pkg).expect("failed to create hir");
    print_interface(&hir);

    // 3. emit SPIR-V bytecode
    let spv_bytecode = hir::write_spirv(&hir);
    validate_spirv(file_stem, &spv_bytecode);

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
