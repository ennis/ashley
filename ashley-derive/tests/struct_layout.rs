use ashley::{
    ir,
    ir::{
        types::{Field, StructType},
        Layout, Module, StructLayout, Type, TypeData,
    },
    utils::MemoryLayout,
};

#[repr(C)]
#[derive(MemoryLayout, Copy, Clone)]
#[layout(std430)]
struct TestLayout {
    a: [i32; 3],
    b: f32,
    c: [f32; 3],
}

#[test]
fn test_vertex_layout() {
    let mut hir = ir::Module::new();

    let ty = TestLayout::hir_type(&mut hir);

    let i32_ty = hir.ty_int();
    let i32a3_ty = hir.define_type(TypeData::Array {
        element_type: i32_ty,
        size: 3,
        stride: Some(4),
    });
    let f32_ty = hir.ty_float();
    let f32a3_ty = hir.define_type(TypeData::Array {
        element_type: f32_ty,
        size: 3,
        stride: Some(4),
    });
    let tydata = &hir.types[ty];

    assert_eq!(
        tydata,
        &TypeData::Struct(StructType {
            name: Some("TestLayout".into()),
            fields: vec![
                Field {
                    ty: i32a3_ty,
                    name: Some("a".into()),
                    interpolation: None,
                },
                Field {
                    ty: f32_ty,
                    name: Some("b".into()),
                    interpolation: None,
                },
                Field {
                    ty: f32a3_ty,
                    name: Some("c".into()),
                    interpolation: None,
                },
            ]
            .into(),
            layout: Some(StructLayout {
                offsets: vec![0, 12, 16],
                layouts: vec![]
            }),
            block: false,
        })
    )
}
