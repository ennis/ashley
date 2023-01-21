use serde::Deserialize;
use serde_json as json;
use std::{
    fs,
    fs::File,
    io::{BufReader, Write},
    path::Path,
    process,
};

mod hir;
mod utils;

//--------------------------------------------------------------------------------------------------
const CORE_GRAMMAR: &str = "codegen/spirv.core.grammar.json";
const EXT_INST_GRAMMARS: &[(&str, &str, &str)] =
    &[("GLSL.std.450", "glsl", "codegen/extinst.glsl.std.450.grammar.json")];
const GLSL_BUILTIN_GRAMMAR: &str = "codegen/glsl.grammar.json";

//--------------------------------------------------------------------------------------------------
fn load_json<T: for<'a> Deserialize<'a>>(path: &str) -> T {
    let file = File::open(path).expect(&format!("failed to open JSON file: `{}`", path));
    let reader = BufReader::new(file);
    json::from_reader(reader).expect("failed to parse JSON file")
}

/*fn write(path: &Path, contents: impl ToString) {
    let mut f = File::create(path).unwrap_or_else(|_| panic!("cannot open file: {:?}", path));
    write_autogen_comment(&mut f);
    write!(f, "{}", contents.to_string()).unwrap()
}

fn write_formatted(path: &Path, contents: impl ToString) {
    write(path, contents);
    match process::Command::new("rustfmt").arg(path).status() {
        Ok(status) if !status.success() => {
            println!("cargo:warning=failed to rustfmt {:?}", path);
        }
        Ok(_) => {}
        Err(_) => {
            println!("cargo:warning=failed to execute rustfmt");
        }
    };
}*/

//--------------------------------------------------------------------------------------------------

fn main() {
    // --- HIR instruction builders ---
    {
        let mut output = File::create("ashley/src/hir/autogen/builder.rs").expect("failed to open output file");
        {
            let core_grammar = load_json(CORE_GRAMMAR);
            let impl_block = hir::generate_impl_block(&core_grammar);
            write!(output, "// --- Core instructions --- \n{impl_block}\n").unwrap();
        }

        for (ext, ext_prefix, file) in EXT_INST_GRAMMARS {
            let ext_grammar = load_json(*file);
            let impl_block = hir::generate_ext_impl_block(&ext_grammar, ext, ext_prefix);
            write!(output, "\n// --- {} --- \n{impl_block}\n", ext).unwrap();
        }
    }

    // --- GLSL builtin instruction signatures ---
}
