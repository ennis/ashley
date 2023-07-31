use ashley::{
    hir,
    hir::{
        types::{Field, StructType},
        Module, Type, TypeData,
    },
    utils::HirType,
};

#[repr(C)]
#[derive(HirType, Copy, Clone)]
//#[glsl_layout(std430)]
struct TestLayout {
    a: [i32; 3],
    b: f32,
    c: [f32; 3],
}

#[test]
fn test_vertex_layout() {
    let mut hir = hir::Module::new();

    let ty = TestLayout::hir_repr(&mut hir);

    let i32_ty = hir.ty_int();
    let i32a3_ty = hir.define_type(TypeData::Array(i32_ty, 3));
    let f32_ty = hir.ty_float();
    let f32a3_ty = hir.define_type(TypeData::Array(f32_ty, 3));
    let tydata = hir.type_data(ty);

    assert_eq!(
        tydata,
        &TypeData::Struct(StructType {
            name: Some("TestLayout".into()),
            fields: vec![
                Field {
                    ty: i32a3_ty,
                    name: Some("a".into()),
                    offset: Some(0),
                },
                Field {
                    ty: f32_ty,
                    name: Some("b".into()),
                    offset: Some(12),
                },
                Field {
                    ty: f32a3_ty,
                    name: Some("c".into()),
                    offset: Some(16),
                },
            ]
            .into()
        })
    )
}
