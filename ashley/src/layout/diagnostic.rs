use crate::{def::FieldLoc, diagnostic::Diagnostic, ty::Type, CompilerDb};

// Layout-check diagnostics
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BlockLayoutDiagnostic {
    MemberOverlap {
        member: FieldLoc,
        explicit_offset: u32,
        computed_next_offset: u32,
    },
    OpaqueType {
        ref_chain: Vec<FieldLoc>,
        ty: Type,
    },
    InvalidStd140ArrayStride {
        ref_chain: Vec<FieldLoc>,
        ty: Type,
        stride: u32,
    },
    // struct {{ref_chain[0].struct_name}} layout is not compatible with std140 rules
    // note: in member {{ref_chain[0]}}
    // note: in member {{ref_chain[...]}}
    // note: matrix type {{ty}} has a layout incompatible with std140 rules, consider using {{std140_matrix_alternative}}
    InvalidStd140MatrixLayout {
        ref_chain: Vec<FieldLoc>,
        ty: Type,
    },
    MisalignedOffset {
        ref_chain: Vec<FieldLoc>,
        explicit_offset: u32,
        required_alignment: u32,
    },
    LayoutMismatch {
        ref_chain: Vec<FieldLoc>,
        computed_offset: u32,
        expected_offset: u32,
    },
}

impl BlockLayoutDiagnostic {
    pub fn render(&self, compiler: &dyn CompilerDb) -> Diagnostic {
        match self {
            BlockLayoutDiagnostic::MemberOverlap { .. } => {
                todo!()
            }
            BlockLayoutDiagnostic::OpaqueType { .. } => {
                todo!()
            }
            BlockLayoutDiagnostic::InvalidStd140ArrayStride { ref_chain, ty, stride } => {
                /*let (items, ast_map) = compiler.module_items_and_ast_map(ref_chain[0].strukt.module);
                let (source_file, root) = compiler.module_syntax(ref_chain[0].strukt.module);

                let struct_name = &items[ref_chain[0].strukt.strukt].name;
                let mut diag = Diagnostic::error(format!(
                    "struct `{struct_name}` is not compatible with std140 layout rules"
                ))
                .span(&InFile::new(
                    source_file,
                    ast_map.get_node(items[ref_chain[0].strukt.strukt].ast).to_node(&root),
                ));

                for field in ref_chain.iter() {
                    let (items, _ast_map) = compiler.module_items_and_ast_map(field.strukt.module);
                    let struct_name = &items[field.strukt.strukt].name;
                    let field_name = &items[field.strukt.strukt].fields[field.field].name;
                    diag = diag.note(format!("in member `{struct_name}::{field_name}`"));
                }

                diag = diag.note(format!(
                    "array type `{ty}` must have a stride that is a multiple of 16, its stride is {stride}"
                ));

                diag*/
                todo!()
            }
            BlockLayoutDiagnostic::InvalidStd140MatrixLayout { .. } => {
                todo!()
            }
            BlockLayoutDiagnostic::MisalignedOffset { .. } => {
                todo!()
            }
            BlockLayoutDiagnostic::LayoutMismatch { .. } => {
                todo!()
            }
        }
    }
}
