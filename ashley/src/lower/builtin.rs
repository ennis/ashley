use crate::{
    hir,
    hir::{
        types::{ImageSampling, ScalarType, ImageType},
        TypeData,
    },
    lower::Scope,
};
use rspirv::spirv;
use std::sync::Arc;

macro_rules! image_type {
    ($sampling:ident $sampled_ty:ident $dim:ident Arrayed $arrayed:literal Multisampled $ms:literal) => {
        TypeData::Image(ImageType {
            sampled_type: ScalarType::$sampled_ty,
            dim: spirv::Dim::$dim,
            arrayed: $arrayed,
            depth: None,
            ms: $ms,
            sampled: ImageSampling::$sampling,
            image_format: spirv::ImageFormat::Unknown,
            access: None,
        })
    };
}

macro_rules! define_builtin_types {
    ($struct_name:ident, $register_fn:ident; $($name:ident $tydata:expr;)*) => {

        #[allow(non_snake_case)]
        pub(crate) struct $struct_name {
            $(pub(crate) $name: hir::Type,)*
        }

        pub(crate) fn $register_fn(module: &mut hir::Module, scope: &mut Scope) -> $struct_name {
            $(
            let $name = module.define_type($tydata);
            scope.define_type(std::stringify!($name).to_string(), None, $name);
            )*

            $struct_name {
                $($name,)*
            }
        }
    };
}

define_builtin_types! {
    BuiltinTypes, register_builtin_types;

    void            TypeData::Unit;
    float           TypeData::Scalar(ScalarType::Float);
    double          TypeData::Scalar(ScalarType::Double);
    uint            TypeData::Scalar(ScalarType::UnsignedInt);
    int             TypeData::Scalar(ScalarType::Int);
    bool            TypeData::Scalar(ScalarType::Bool);
    vec2            TypeData::Vector(ScalarType::Float, 2);
    vec3            TypeData::Vector(ScalarType::Float, 3);
    vec4            TypeData::Vector(ScalarType::Float, 4);
    ivec2           TypeData::Vector(ScalarType::Int, 2);
    ivec3           TypeData::Vector(ScalarType::Int, 3);
    ivec4           TypeData::Vector(ScalarType::Int, 4);
    uvec2           TypeData::Vector(ScalarType::UnsignedInt, 2);
    uvec3           TypeData::Vector(ScalarType::UnsignedInt, 3);
    uvec4           TypeData::Vector(ScalarType::UnsignedInt, 4);
    bvec2           TypeData::Vector(ScalarType::Bool, 2);
    bvec3           TypeData::Vector(ScalarType::Bool, 3);
    bvec4           TypeData::Vector(ScalarType::Bool, 4);
    dvec2           TypeData::Vector(ScalarType::Double, 2);
    dvec3           TypeData::Vector(ScalarType::Double, 3);
    dvec4           TypeData::Vector(ScalarType::Double, 4);

    sampler         TypeData::Sampler;
    samplerShadow   TypeData::SamplerShadow;

    texture1D        image_type!(Sampled Float Dim1D     Arrayed false Multisampled false);
    texture1DArray   image_type!(Sampled Float Dim1D     Arrayed true  Multisampled false);
    texture2D        image_type!(Sampled Float Dim2D     Arrayed false Multisampled false);
    texture2DArray   image_type!(Sampled Float Dim2D     Arrayed true  Multisampled false);
    texture2DMS      image_type!(Sampled Float Dim2D     Arrayed false Multisampled true);
    texture2DMSArray image_type!(Sampled Float Dim2D     Arrayed true  Multisampled true);
    texture2DRect    image_type!(Sampled Float DimRect   Arrayed false Multisampled false);
    texture3D        image_type!(Sampled Float Dim3D     Arrayed false Multisampled false);
    textureCube      image_type!(Sampled Float DimCube   Arrayed false Multisampled false);
    textureCubeArray image_type!(Sampled Float DimCube   Arrayed true  Multisampled false);
    textureBuffer    image_type!(Sampled Float DimBuffer Arrayed false Multisampled false);

    itexture1D        image_type!(Sampled Int Dim1D     Arrayed false Multisampled false);
    itexture1DArray   image_type!(Sampled Int Dim1D     Arrayed true  Multisampled false);
    itexture2D        image_type!(Sampled Int Dim2D     Arrayed false Multisampled false);
    itexture2DArray   image_type!(Sampled Int Dim2D     Arrayed true  Multisampled false);
    itexture2DMS      image_type!(Sampled Int Dim2D     Arrayed false Multisampled true);
    itexture2DMSArray image_type!(Sampled Int Dim2D     Arrayed true  Multisampled true);
    itexture2DRect    image_type!(Sampled Int DimRect   Arrayed false Multisampled false);
    itexture3D        image_type!(Sampled Int Dim3D     Arrayed false Multisampled false);
    itextureCube      image_type!(Sampled Int DimCube   Arrayed false Multisampled false);
    itextureCubeArray image_type!(Sampled Int DimCube   Arrayed true  Multisampled false);
    itextureBuffer    image_type!(Sampled Int DimBuffer Arrayed false Multisampled false);

    utexture1D        image_type!(Sampled UnsignedInt Dim1D     Arrayed false Multisampled false);
    utexture1DArray   image_type!(Sampled UnsignedInt Dim1D     Arrayed true  Multisampled false);
    utexture2D        image_type!(Sampled UnsignedInt Dim2D     Arrayed false Multisampled false);
    utexture2DArray   image_type!(Sampled UnsignedInt Dim2D     Arrayed true  Multisampled false);
    utexture2DMS      image_type!(Sampled UnsignedInt Dim2D     Arrayed false Multisampled true);
    utexture2DMSArray image_type!(Sampled UnsignedInt Dim2D     Arrayed true  Multisampled true);
    utexture2DRect    image_type!(Sampled UnsignedInt DimRect   Arrayed false Multisampled false);
    utexture3D        image_type!(Sampled UnsignedInt Dim3D     Arrayed false Multisampled false);
    utextureCube      image_type!(Sampled UnsignedInt DimCube   Arrayed false Multisampled false);
    utextureCubeArray image_type!(Sampled UnsignedInt DimCube   Arrayed true  Multisampled false);
    utextureBuffer    image_type!(Sampled UnsignedInt DimBuffer Arrayed false Multisampled false);

    image1D        image_type!(ReadWrite Float Dim1D     Arrayed false Multisampled false);
    image1DArray   image_type!(ReadWrite Float Dim1D     Arrayed true  Multisampled false);
    image2D        image_type!(ReadWrite Float Dim2D     Arrayed false Multisampled false);
    image2DArray   image_type!(ReadWrite Float Dim2D     Arrayed true  Multisampled false);
    image2DMS      image_type!(ReadWrite Float Dim2D     Arrayed false Multisampled true);
    image2DMSArray image_type!(ReadWrite Float Dim2D     Arrayed true  Multisampled true);
    image2DRect    image_type!(ReadWrite Float DimRect   Arrayed false Multisampled false);
    image3D        image_type!(ReadWrite Float Dim3D     Arrayed false Multisampled false);
    imageCube      image_type!(ReadWrite Float DimCube   Arrayed false Multisampled false);
    imageCubeArray image_type!(ReadWrite Float DimCube   Arrayed true  Multisampled false);
    imageBuffer    image_type!(ReadWrite Float DimBuffer Arrayed false Multisampled false);

    iimage1D        image_type!(ReadWrite Int Dim1D     Arrayed false Multisampled false);
    iimage1DArray   image_type!(ReadWrite Int Dim1D     Arrayed true  Multisampled false);
    iimage2D        image_type!(ReadWrite Int Dim2D     Arrayed false Multisampled false);
    iimage2DArray   image_type!(ReadWrite Int Dim2D     Arrayed true  Multisampled false);
    iimage2DMS      image_type!(ReadWrite Int Dim2D     Arrayed false Multisampled true);
    iimage2DMSArray image_type!(ReadWrite Int Dim2D     Arrayed true  Multisampled true);
    iimage2DRect    image_type!(ReadWrite Int DimRect   Arrayed false Multisampled false);
    iimage3D        image_type!(ReadWrite Int Dim3D     Arrayed false Multisampled false);
    iimageCube      image_type!(ReadWrite Int DimCube   Arrayed false Multisampled false);
    iimageCubeArray image_type!(ReadWrite Int DimCube   Arrayed true  Multisampled false);
    iimageBuffer    image_type!(ReadWrite Int DimBuffer Arrayed false Multisampled false);

    uimage1D        image_type!(ReadWrite UnsignedInt Dim1D     Arrayed false Multisampled false);
    uimage1DArray   image_type!(ReadWrite UnsignedInt Dim1D     Arrayed true  Multisampled false);
    uimage2D        image_type!(ReadWrite UnsignedInt Dim2D     Arrayed false Multisampled false);
    uimage2DArray   image_type!(ReadWrite UnsignedInt Dim2D     Arrayed true  Multisampled false);
    uimage2DMS      image_type!(ReadWrite UnsignedInt Dim2D     Arrayed false Multisampled true);
    uimage2DMSArray image_type!(ReadWrite UnsignedInt Dim2D     Arrayed true  Multisampled true);
    uimage2DRect    image_type!(ReadWrite UnsignedInt DimRect   Arrayed false Multisampled false);
    uimage3D        image_type!(ReadWrite UnsignedInt Dim3D     Arrayed false Multisampled false);
    uimageCube      image_type!(ReadWrite UnsignedInt DimCube   Arrayed false Multisampled false);
    uimageCubeArray image_type!(ReadWrite UnsignedInt DimCube   Arrayed true  Multisampled false);
    uimageBuffer    image_type!(ReadWrite UnsignedInt DimBuffer Arrayed false Multisampled false);
}
