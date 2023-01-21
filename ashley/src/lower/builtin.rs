use crate::{
    hir,
    hir::{types::ScalarType, TypeData},
    lower::Scope,
};
use rspirv::spirv;
use std::sync::Arc;

macro_rules! image_type {
    ($sampling:ident $sampled_ty:ident $dim:ident Arrayed $arrayed:literal Multisampled $ms:literal) => {
        TypeData::Image(Arc::new(ImageType {
            sampled_type: ScalarType::$sampled_ty,
            dim: spirv::Dim::$dim,
            arrayed: $arrayed,
            depth: None,
            ms: $ms,
            sampled: ImageSampling::$sampling,
            image_format: spirv::ImageFormat::Unknown,
            access: None,
        }))
    };
}

macro_rules! define_builtin_types {
    ($struct_name:ident, $register_fn:ident; $($name:ident $alias:literal $tydata:expr;)*) => {

        pub(crate) struct $struct_name {
            $(pub(crate) $name: hir::Type,)*
        }

        fn $register_fn(module: &mut hir::Module, scope: &mut Scope) -> $struct_name {
            $(
            let $name = module.define_type($tydata);
            scope.define_type($alias, None, $name);
            )*

            $struct_name {
                $($name,)*
            }
        }
    };
}


define_builtin_types! {
    BuiltinTypes, register_builtin_types;

    void                "void"             TypeData::Unit;
    texture_1d          "texture1D"        image_type!(Sampled Float Dim1D    Arrayed false Multisampled false);
    texture_2d          "texture2D"        image_type!(Sampled Float Dim2D    Arrayed false Multisampled false);
    texture_3d          "texture3D"        image_type!(Sampled Float Dim3D    Arrayed false Multisampled false);
    texture_cube        "textureCube"      image_type!(Sampled Float DimCube  Arrayed false Multisampled false);
    texture_1d_array    "texture1DArray"   image_type!(Sampled Float Dim1D    Arrayed true  Multisampled false);
    texture_2d_array    "texture2DArray"   image_type!(Sampled Float Dim3D    Arrayed true  Multisampled false);
    texture_1d_ms       "texture1DMS"      image_type!(Sampled Float Dim1D    Arrayed false Multisampled true);
    texture_2d_ms       "texture2DMS"      image_type!(Sampled Float Dim2D    Arrayed false Multisampled true);
    i_texture_1d        "itexture1D"       image_type!(Sampled Int   Dim1D    Arrayed false Multisampled false);
    i_texture_2d        "itexture2D"       image_type!(Sampled Int   Dim2D    Arrayed false Multisampled false);
    i_texture_3d        "itexture3D"       image_type!(Sampled Int   Dim3D    Arrayed false Multisampled false);
    i_texture_cube      "itextureCube"     image_type!(Sampled Int   DimCube  Arrayed false Multisampled false);
    i_texture_1d_array  "itexture1DArray"  image_type!(Sampled Int   Dim1D    Arrayed true  Multisampled false);
    i_texture_2d_array  "itexture2DArray"  image_type!(Sampled Int   Dim3D    Arrayed true  Multisampled false);
    u_texture_1d        "utexture1D"       image_type!(Sampled UnsignedInt   Dim1D    Arrayed false Multisampled false);
    u_texture_2d        "utexture2D"       image_type!(Sampled UnsignedInt   Dim2D    Arrayed false Multisampled false);
    u_texture_3d        "utexture3D"       image_type!(Sampled UnsignedInt   Dim3D    Arrayed false Multisampled false);
    u_texture_cube      "utextureCube"     image_type!(Sampled UnsignedInt   DimCube  Arrayed false Multisampled false);
    u_texture_1d_array  "utexture1DArray"  image_type!(Sampled UnsignedInt   Dim1D    Arrayed true  Multisampled false);
    u_texture_2d_array  "utexture2DArray"  image_type!(Sampled UnsignedInt   Dim3D    Arrayed true  Multisampled false);
    image_1d            "image1D"          image_type!(ReadWrite Float Dim1D    Arrayed false  Multisampled false);
    image_2d            "image2D"          image_type!(ReadWrite Float Dim2D    Arrayed false  Multisampled false);
    image_3d            "image3D"          image_type!(ReadWrite Float Dim3D    Arrayed false  Multisampled false);
    image_cube          "imageCube"        image_type!(ReadWrite Float DimCube  Arrayed false  Multisampled false);
    image_1d_array      "image1DArray"     image_type!(ReadWrite Float Dim1D    Arrayed true   Multisampled false);
    image_2d_array      "image2DArray"     image_type!(ReadWrite Float Dim3D    Arrayed true   Multisampled false);
    image_1d_ms         "image1DMS"        image_type!(ReadWrite Float Dim1D    Arrayed false  Multisampled true);
    image_2d_ms         "image2DMS"        image_type!(ReadWrite Float Dim2D    Arrayed false  Multisampled true);
    i_image_1d          "iimage1D"         image_type!(ReadWrite Int   Dim1D    Arrayed false Multisampled false);
    i_image_2d          "iimage2D"         image_type!(ReadWrite Int   Dim2D    Arrayed false Multisampled false);
    i_image_3d          "iimage3D"         image_type!(ReadWrite Int   Dim3D    Arrayed false Multisampled false);
    i_image_cube        "iimageCube"       image_type!(ReadWrite Int   DimCube  Arrayed false Multisampled false);
    i_image_1d_array    "iimage1DArray"    image_type!(ReadWrite Int   Dim1D    Arrayed true  Multisampled false);
    i_image_2d_array    "iimage2DArray"    image_type!(ReadWrite Int   Dim3D    Arrayed true  Multisampled false);
    u_image_1d          "uimage1D"         image_type!(ReadWrite UnsignedInt   Dim1D    Arrayed false Multisampled false);
    u_image_2d          "uimage2D"         image_type!(ReadWrite UnsignedInt   Dim2D    Arrayed false Multisampled false);
    u_image_3d          "uimage3D"         image_type!(ReadWrite UnsignedInt   Dim3D    Arrayed false Multisampled false);
    u_image_cube        "uimageCube"       image_type!(ReadWrite UnsignedInt   DimCube  Arrayed false Multisampled false);
    u_image_1d_array    "uimage1DArray"    image_type!(ReadWrite UnsignedInt   Dim1D    Arrayed true  Multisampled false);
    u_image_2d_array    "uimage2DArray"    image_type!(ReadWrite UnsignedInt   Dim3D    Arrayed true  Multisampled false);
    float               "float"            TypeData::Scalar(ScalarType::Float);
    double              "double"           TypeData::Scalar(ScalarType::Double);
    uint                "uint"             TypeData::Scalar(ScalarType::UnsignedInt);
    int                 "int"              TypeData::Scalar(ScalarType::Int);
    vec2                "vec2"             TypeData::Vector(ScalarType::Float, 2);
    vec3                "vec3"             TypeData::Vector(ScalarType::Float, 3);
    vec4                "vec4"             TypeData::Vector(ScalarType::Float, 4);
    ivec2               "ivec2"            TypeData::Vector(ScalarType::Int, 2);
    ivec3               "ivec3"            TypeData::Vector(ScalarType::Int, 3);
    ivec4               "ivec4"            TypeData::Vector(ScalarType::Int, 4);
    uvec2               "uvec2"            TypeData::Vector(ScalarType::UnsignedInt, 2);
    uvec3               "uvec3"            TypeData::Vector(ScalarType::UnsignedInt, 3);
    uvec4               "uvec4"            TypeData::Vector(ScalarType::UnsignedInt, 4);
    bool                "bool"             TypeData::Scalar(ScalarType::Bool);
    bvec2               "bvec2"            TypeData::Vector(ScalarType::Bool, 2);
    bvec3               "bvec3"            TypeData::Vector(ScalarType::Bool, 3);
    bvec4               "bvec4"            TypeData::Vector(ScalarType::Bool, 4);
}
