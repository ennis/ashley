use crate::{
    hir,
    tast::{
        ty::{ImageSampling, ImageType},
        Module, ScalarType, Type, TypeKind, Types,
    },
};
use smallvec::SmallVec;

//--------------------------------------------------------------------------------------------------

macro_rules! image_type {
    ($sampling:ident $sampled_ty:ident $dim:ident Arrayed $arrayed:literal Multisampled $ms:literal) => {
        TypeKind::Image(ImageType {
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
    ($struct_name:ident; $($name:ident $tykind:expr;)*) => {

        #[allow(non_snake_case)]
        pub struct $struct_name {
            $(pub $name: Type,)*
        }

        impl $struct_name {
            #[allow(non_snake_case)]
            pub fn new(types: &mut Types) -> $struct_name {
                $(let $name = types.intern($tykind);)*

                $struct_name {
                    $($name,)*
                }
            }

            pub fn resolve(&self, name: &str) -> Option<Type> {
                match name {
                    $(
                        std::stringify!($name) => Some(self.$name.clone()),
                    )*
                    _ => None,
                }
            }
        }
    };
}

define_builtin_types! {
    BuiltinTypes;

    void            TypeKind::Unit;
    float           TypeKind::Scalar(ScalarType::Float);
    double          TypeKind::Scalar(ScalarType::Double);
    uint            TypeKind::Scalar(ScalarType::UnsignedInt);
    int             TypeKind::Scalar(ScalarType::Int);
    bool            TypeKind::Scalar(ScalarType::Bool);
    vec2            TypeKind::Vector(ScalarType::Float, 2);
    vec3            TypeKind::Vector(ScalarType::Float, 3);
    vec4            TypeKind::Vector(ScalarType::Float, 4);
    ivec2           TypeKind::Vector(ScalarType::Int, 2);
    ivec3           TypeKind::Vector(ScalarType::Int, 3);
    ivec4           TypeKind::Vector(ScalarType::Int, 4);
    uvec2           TypeKind::Vector(ScalarType::UnsignedInt, 2);
    uvec3           TypeKind::Vector(ScalarType::UnsignedInt, 3);
    uvec4           TypeKind::Vector(ScalarType::UnsignedInt, 4);
    bvec2           TypeKind::Vector(ScalarType::Bool, 2);
    bvec3           TypeKind::Vector(ScalarType::Bool, 3);
    bvec4           TypeKind::Vector(ScalarType::Bool, 4);
    dvec2           TypeKind::Vector(ScalarType::Double, 2);
    dvec3           TypeKind::Vector(ScalarType::Double, 3);
    dvec4           TypeKind::Vector(ScalarType::Double, 4);

    mat2            TypeKind::Matrix { component_type: ScalarType::Float, columns: 2, rows: 2 };
    mat3            TypeKind::Matrix { component_type: ScalarType::Float, columns: 3, rows: 3 };
    mat4            TypeKind::Matrix { component_type: ScalarType::Float, columns: 4, rows: 4 };
    dmat2           TypeKind::Matrix { component_type: ScalarType::Double, columns: 2, rows: 2 };
    dmat3           TypeKind::Matrix { component_type: ScalarType::Double, columns: 3, rows: 3 };
    dmat4           TypeKind::Matrix { component_type: ScalarType::Double, columns: 4, rows: 4 };
    mat2x2          TypeKind::Matrix { component_type: ScalarType::Float, columns: 2, rows: 2 };
    mat2x3          TypeKind::Matrix { component_type: ScalarType::Float, columns: 2, rows: 3 };
    mat2x4          TypeKind::Matrix { component_type: ScalarType::Float, columns: 2, rows: 4 };
    mat3x2          TypeKind::Matrix { component_type: ScalarType::Float, columns: 3, rows: 2 };
    mat3x3          TypeKind::Matrix { component_type: ScalarType::Float, columns: 3, rows: 3 };
    mat3x4          TypeKind::Matrix { component_type: ScalarType::Float, columns: 3, rows: 4 };
    mat4x2          TypeKind::Matrix { component_type: ScalarType::Float, columns: 4, rows: 2 };
    mat4x3          TypeKind::Matrix { component_type: ScalarType::Float, columns: 4, rows: 3 };
    mat4x4          TypeKind::Matrix { component_type: ScalarType::Float, columns: 4, rows: 4 };
    dmat2x2         TypeKind::Matrix { component_type: ScalarType::Double, columns: 2, rows: 2 };
    dmat2x3         TypeKind::Matrix { component_type: ScalarType::Double, columns: 2, rows: 3 };
    dmat2x4         TypeKind::Matrix { component_type: ScalarType::Double, columns: 2, rows: 4 };
    dmat3x2         TypeKind::Matrix { component_type: ScalarType::Double, columns: 3, rows: 2 };
    dmat3x3         TypeKind::Matrix { component_type: ScalarType::Double, columns: 3, rows: 3 };
    dmat3x4         TypeKind::Matrix { component_type: ScalarType::Double, columns: 3, rows: 4 };
    dmat4x2         TypeKind::Matrix { component_type: ScalarType::Double, columns: 4, rows: 2 };
    dmat4x3         TypeKind::Matrix { component_type: ScalarType::Double, columns: 4, rows: 3 };
    dmat4x4         TypeKind::Matrix { component_type: ScalarType::Double, columns: 4, rows: 4 };

    sampler         TypeKind::Sampler;
    samplerShadow   TypeKind::SamplerShadow;

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

//--------------------------------------------------------------------------------------------------

/// Pseudo-type used in signatures of built-in functions.
///
/// This is mainly used so that we can specify vector overloads in a more compact way (`vecN` instead
/// of duplicating signatures four times).
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum PseudoType {
    void,
    float,
    double,
    int,
    uint,
    bool,
    vec2,
    vec3,
    vec4,
    ivec2,
    ivec3,
    ivec4,
    uvec2,
    uvec3,
    uvec4,
    dvec2,
    dvec3,
    dvec4,
    bvec2,
    bvec3,
    bvec4,
    vecN,
    dvecN,
    ivecN,
    uvecN,
    bvecN,

    mat2,
    mat3,
    mat4,
    mat2x2,
    mat2x3,
    mat2x4,
    mat3x2,
    mat3x3,
    mat3x4,
    mat4x2,
    mat4x3,
    mat4x4,
    dmat2,
    dmat3,
    dmat4,
    dmat2x2,
    dmat2x3,
    dmat2x4,
    dmat3x2,
    dmat3x3,
    dmat3x4,
    dmat4x2,
    dmat4x3,
    dmat4x4,

    highp_vecN,
    highp_ivecN,
    modf_result_vecN,
    modf_result_dvecN,
    frexp_result_highp_vecN,

    image1D,
    image1DArray,
    image2D,
    image2DArray,
    image2DMS,
    image2DMSArray,
    image2DRect,
    image3D,
    imageCube,
    imageCubeArray,
    imageBuffer,

    iimage1D,
    iimage1DArray,
    iimage2D,
    iimage2DArray,
    iimage2DMS,
    iimage2DMSArray,
    iimage2DRect,
    iimage3D,
    iimageCube,
    iimageCubeArray,
    iimageBuffer,

    uimage1D,
    uimage1DArray,
    uimage2D,
    uimage2DArray,
    uimage2DMS,
    uimage2DMSArray,
    uimage2DRect,
    uimage3D,
    uimageCube,
    uimageCubeArray,
    uimageBuffer,

    texture1D,
    texture1DArray,
    texture2D,
    texture2DArray,
    texture2DMS,
    texture2DMSArray,
    texture2DRect,
    texture3D,
    textureCube,
    textureCubeArray,
    textureBuffer,

    itexture1D,
    itexture1DArray,
    itexture2D,
    itexture2DArray,
    itexture2DMS,
    itexture2DMSArray,
    itexture2DRect,
    itexture3D,
    itextureCube,
    itextureCubeArray,
    itextureBuffer,

    utexture1D,
    utexture1DArray,
    utexture2D,
    utexture2DArray,
    utexture2DMS,
    utexture2DMSArray,
    utexture2DRect,
    utexture3D,
    utextureCube,
    utextureCubeArray,
    utextureBuffer,

    gimage1D,
    gimage1DArray,
    gimage2D,
    gimage2DArray,
    gimage2DMS,
    gimage2DMSArray,
    gimage2DRect,
    gimage3D,
    gimageCube,
    gimageCubeArray,
    gimageBuffer,

    gtexture1D,
    gtexture1DArray,
    gtexture2D,
    gtexture2DArray,
    gtexture2DMS,
    gtexture2DMSArray,
    gtexture2DRect,
    gtexture3D,
    gtextureCube,
    gtextureCubeArray,
    gtextureBuffer,

    gvec4,

    subpassInput,
    subpassInputMS,
    sampler,
    samplerShadow,
}

/// Signature of a built-in operation (function or operator).
#[derive(Copy, Clone)]
pub struct BuiltinSignature {
    pub parameter_types: &'static [PseudoType],
    pub result_type: PseudoType,
    pub lower:
        fn(&mut (), &mut hir::FunctionBuilder, args: &[hir::IdRef], types: &[hir::Type], ret: hir::Type) -> hir::Value,
}

#[derive(Copy, Clone)]
pub(crate) enum ImageClass {
    F,
    SI,
    UI,
}

fn pseudo_type_to_concrete_type(
    pseudo_type: PseudoType,
    builtins: &BuiltinTypes,
    vec_len: u8,
    image_class: ImageClass,
) -> Type {
    use ImageClass as IC;
    use PseudoType as PT;
    use TypeKind as TK;
    match pseudo_type {
        PT::void => builtins.void.clone(),
        PT::float => builtins.float.clone(),
        PT::double => builtins.double.clone(),
        PT::int => builtins.int.clone(),
        PT::uint => builtins.uint.clone(),
        PT::bool => builtins.bool.clone(),
        PT::vecN => match vec_len {
            1 => builtins.float.clone(),
            2 => builtins.vec2.clone(),
            3 => builtins.vec3.clone(),
            4 => builtins.vec4.clone(),
            _ => panic!("invalid vector length"),
        },
        PT::dvecN => match vec_len {
            1 => builtins.double.clone(),
            2 => builtins.dvec2.clone(),
            3 => builtins.dvec3.clone(),
            4 => builtins.dvec4.clone(),
            _ => panic!("invalid vector length"),
        },
        PT::ivecN => match vec_len {
            1 => builtins.int.clone(),
            2 => builtins.ivec2.clone(),
            3 => builtins.ivec3.clone(),
            4 => builtins.ivec4.clone(),
            _ => panic!("invalid vector length"),
        },
        PT::uvecN => match vec_len {
            1 => builtins.uint.clone(),
            2 => builtins.uvec2.clone(),
            3 => builtins.uvec3.clone(),
            4 => builtins.uvec4.clone(),
            _ => panic!("invalid vector length"),
        },
        PT::bvecN => match vec_len {
            1 => builtins.bool.clone(),
            2 => builtins.bvec2.clone(),
            3 => builtins.bvec3.clone(),
            4 => builtins.bvec4.clone(),
            _ => panic!("invalid vector length"),
        },
        PT::vec2 => builtins.vec2.clone(),
        PT::vec3 => builtins.vec3.clone(),
        PT::vec4 => builtins.vec4.clone(),
        PT::ivec2 => builtins.ivec2.clone(),
        PT::ivec3 => builtins.ivec3.clone(),
        PT::ivec4 => builtins.ivec4.clone(),
        PT::uvec2 => builtins.uvec2.clone(),
        PT::uvec3 => builtins.uvec3.clone(),
        PT::uvec4 => builtins.uvec4.clone(),
        PT::dvec2 => builtins.dvec2.clone(),
        PT::dvec3 => builtins.dvec3.clone(),
        PT::dvec4 => builtins.dvec4.clone(),
        PT::bvec2 => builtins.bvec2.clone(),
        PT::bvec3 => builtins.bvec3.clone(),
        PT::bvec4 => builtins.bvec4.clone(),
        PT::highp_vecN => {
            todo!()
        }
        PT::highp_ivecN => {
            todo!()
        }
        PT::modf_result_vecN => {
            todo!()
        }
        PT::modf_result_dvecN => {
            todo!()
        }
        PT::frexp_result_highp_vecN => {
            todo!()
        }

        PT::mat2 => builtins.mat2.clone(),
        PT::mat3 => builtins.mat3.clone(),
        PT::mat4 => builtins.mat4.clone(),
        PT::mat2x2 => builtins.mat2x2.clone(),
        PT::mat2x3 => builtins.mat2x3.clone(),
        PT::mat2x4 => builtins.mat2x4.clone(),
        PT::mat3x2 => builtins.mat3x2.clone(),
        PT::mat3x3 => builtins.mat3x3.clone(),
        PT::mat3x4 => builtins.mat3x4.clone(),
        PT::mat4x2 => builtins.mat4x2.clone(),
        PT::mat4x3 => builtins.mat4x3.clone(),
        PT::mat4x4 => builtins.mat4x4.clone(),
        PT::dmat2 => builtins.dmat2.clone(),
        PT::dmat3 => builtins.dmat3.clone(),
        PT::dmat4 => builtins.dmat4.clone(),
        PT::dmat2x2 => builtins.dmat2x2.clone(),
        PT::dmat2x3 => builtins.dmat2x3.clone(),
        PT::dmat2x4 => builtins.dmat2x4.clone(),
        PT::dmat3x2 => builtins.dmat3x2.clone(),
        PT::dmat3x3 => builtins.dmat3x3.clone(),
        PT::dmat3x4 => builtins.dmat3x4.clone(),
        PT::dmat4x2 => builtins.dmat4x2.clone(),
        PT::dmat4x3 => builtins.dmat4x3.clone(),
        PT::dmat4x4 => builtins.dmat4x4.clone(),

        PT::image1D => builtins.image1D.clone(),
        PT::image1DArray => builtins.image1DArray.clone(),
        PT::image2D => builtins.image2D.clone(),
        PT::image2DArray => builtins.image2DArray.clone(),
        PT::image2DMS => builtins.image2DMS.clone(),
        PT::image2DMSArray => builtins.image2DMSArray.clone(),
        PT::image2DRect => builtins.image2DRect.clone(),
        PT::image3D => builtins.image3D.clone(),
        PT::imageCube => builtins.imageCube.clone(),
        PT::imageCubeArray => builtins.imageCubeArray.clone(),
        PT::imageBuffer => builtins.imageBuffer.clone(),
        PT::uimage1D => builtins.uimage1D.clone(),
        PT::uimage1DArray => builtins.uimage1DArray.clone(),
        PT::uimage2D => builtins.uimage2D.clone(),
        PT::uimage2DArray => builtins.uimage2DArray.clone(),
        PT::uimage2DMS => builtins.uimage2DMS.clone(),
        PT::uimage2DMSArray => builtins.uimage2DMSArray.clone(),
        PT::uimage2DRect => builtins.uimage2DRect.clone(),
        PT::uimage3D => builtins.uimage3D.clone(),
        PT::uimageCube => builtins.uimageCube.clone(),
        PT::uimageCubeArray => builtins.uimageCubeArray.clone(),
        PT::uimageBuffer => builtins.uimageBuffer.clone(),
        PT::iimage1D => builtins.iimage1D.clone(),
        PT::iimage1DArray => builtins.iimage1DArray.clone(),
        PT::iimage2D => builtins.iimage2D.clone(),
        PT::iimage2DArray => builtins.iimage2DArray.clone(),
        PT::iimage2DMS => builtins.iimage2DMS.clone(),
        PT::iimage2DMSArray => builtins.iimage2DMSArray.clone(),
        PT::iimage2DRect => builtins.iimage2DRect.clone(),
        PT::iimage3D => builtins.iimage3D.clone(),
        PT::iimageCube => builtins.iimageCube.clone(),
        PT::iimageCubeArray => builtins.iimageCubeArray.clone(),
        PT::iimageBuffer => builtins.iimageBuffer.clone(),

        PT::texture1D => builtins.texture1D.clone(),
        PT::texture1DArray => builtins.texture1DArray.clone(),
        PT::texture2D => builtins.texture2D.clone(),
        PT::texture2DArray => builtins.texture2DArray.clone(),
        PT::texture2DMS => builtins.texture2DMS.clone(),
        PT::texture2DMSArray => builtins.texture2DMSArray.clone(),
        PT::texture2DRect => builtins.texture2DRect.clone(),
        PT::texture3D => builtins.texture3D.clone(),
        PT::textureCube => builtins.textureCube.clone(),
        PT::textureCubeArray => builtins.textureCubeArray.clone(),
        PT::textureBuffer => builtins.textureBuffer.clone(),

        PT::itexture1D => builtins.itexture1D.clone(),
        PT::itexture1DArray => builtins.itexture1DArray.clone(),
        PT::itexture2D => builtins.itexture2D.clone(),
        PT::itexture2DArray => builtins.itexture2DArray.clone(),
        PT::itexture2DMS => builtins.itexture2DMS.clone(),
        PT::itexture2DMSArray => builtins.itexture2DMSArray.clone(),
        PT::itexture2DRect => builtins.itexture2DRect.clone(),
        PT::itexture3D => builtins.itexture3D.clone(),
        PT::itextureCube => builtins.itextureCube.clone(),
        PT::itextureCubeArray => builtins.itextureCubeArray.clone(),
        PT::itextureBuffer => builtins.itextureBuffer.clone(),

        PT::utexture1D => builtins.utexture1D.clone(),
        PT::utexture1DArray => builtins.utexture1DArray.clone(),
        PT::utexture2D => builtins.utexture2D.clone(),
        PT::utexture2DArray => builtins.utexture2DArray.clone(),
        PT::utexture2DMS => builtins.utexture2DMS.clone(),
        PT::utexture2DMSArray => builtins.utexture2DMSArray.clone(),
        PT::utexture2DRect => builtins.utexture2DRect.clone(),
        PT::utexture3D => builtins.utexture3D.clone(),
        PT::utextureCube => builtins.utextureCube.clone(),
        PT::utextureCubeArray => builtins.utextureCubeArray.clone(),
        PT::utextureBuffer => builtins.utextureBuffer.clone(),

        PT::gimage1D => match image_class {
            IC::F => builtins.image1D.clone(),
            IC::SI => builtins.iimage1D.clone(),
            IC::UI => builtins.uimage1D.clone(),
        },
        PT::gimage1DArray => match image_class {
            IC::F => builtins.image1DArray.clone(),
            IC::SI => builtins.iimage1DArray.clone(),
            IC::UI => builtins.uimage1DArray.clone(),
        },
        PT::gimage2D => match image_class {
            IC::F => builtins.image2D.clone(),
            IC::SI => builtins.iimage2D.clone(),
            IC::UI => builtins.uimage2D.clone(),
        },
        PT::gimage2DArray => match image_class {
            IC::F => builtins.image2DArray.clone(),
            IC::SI => builtins.iimage2DArray.clone(),
            IC::UI => builtins.uimage2DArray.clone(),
        },
        PT::gimage2DMS => match image_class {
            IC::F => builtins.image2DMS.clone(),
            IC::SI => builtins.iimage2DMS.clone(),
            IC::UI => builtins.uimage2DMS.clone(),
        },
        PT::gimage2DMSArray => match image_class {
            IC::F => builtins.image2DMSArray.clone(),
            IC::SI => builtins.iimage2DMSArray.clone(),
            IC::UI => builtins.uimage2DMSArray.clone(),
        },
        PT::gimage2DRect => match image_class {
            IC::F => builtins.image2DRect.clone(),
            IC::SI => builtins.iimage2DRect.clone(),
            IC::UI => builtins.uimage2DRect.clone(),
        },
        PT::gimage3D => match image_class {
            IC::F => builtins.image3D.clone(),
            IC::SI => builtins.iimage3D.clone(),
            IC::UI => builtins.uimage3D.clone(),
        },
        PT::gimageCube => match image_class {
            IC::F => builtins.imageCube.clone(),
            IC::SI => builtins.iimageCube.clone(),
            IC::UI => builtins.uimageCube.clone(),
        },
        PT::gimageCubeArray => match image_class {
            IC::F => builtins.imageCubeArray.clone(),
            IC::SI => builtins.iimageCubeArray.clone(),
            IC::UI => builtins.uimageCubeArray.clone(),
        },
        PT::gimageBuffer => match image_class {
            IC::F => builtins.imageBuffer.clone(),
            IC::SI => builtins.iimageBuffer.clone(),
            IC::UI => builtins.uimageBuffer.clone(),
        },

        PT::gtexture1D => match image_class {
            IC::F => builtins.texture1D.clone(),
            IC::SI => builtins.itexture1D.clone(),
            IC::UI => builtins.utexture1D.clone(),
        },
        PT::gtexture1DArray => match image_class {
            IC::F => builtins.texture1DArray.clone(),
            IC::SI => builtins.itexture1DArray.clone(),
            IC::UI => builtins.utexture1DArray.clone(),
        },
        PT::gtexture2D => match image_class {
            IC::F => builtins.texture2D.clone(),
            IC::SI => builtins.itexture2D.clone(),
            IC::UI => builtins.utexture2D.clone(),
        },
        PT::gtexture2DArray => match image_class {
            IC::F => builtins.texture2DArray.clone(),
            IC::SI => builtins.itexture2DArray.clone(),
            IC::UI => builtins.utexture2DArray.clone(),
        },
        PT::gtexture2DMS => match image_class {
            IC::F => builtins.texture2DMS.clone(),
            IC::SI => builtins.itexture2DMS.clone(),
            IC::UI => builtins.utexture2DMS.clone(),
        },
        PT::gtexture2DMSArray => match image_class {
            IC::F => builtins.texture2DMSArray.clone(),
            IC::SI => builtins.itexture2DMSArray.clone(),
            IC::UI => builtins.utexture2DMSArray.clone(),
        },
        PT::gtexture2DRect => match image_class {
            IC::F => builtins.texture2DRect.clone(),
            IC::SI => builtins.itexture2DRect.clone(),
            IC::UI => builtins.utexture2DRect.clone(),
        },
        PT::gtexture3D => match image_class {
            IC::F => builtins.texture3D.clone(),
            IC::SI => builtins.itexture3D.clone(),
            IC::UI => builtins.utexture3D.clone(),
        },
        PT::gtextureCube => match image_class {
            IC::F => builtins.textureCube.clone(),
            IC::SI => builtins.itextureCube.clone(),
            IC::UI => builtins.utextureCube.clone(),
        },
        PT::gtextureCubeArray => match image_class {
            IC::F => builtins.textureCubeArray.clone(),
            IC::SI => builtins.itextureCubeArray.clone(),
            IC::UI => builtins.utextureCubeArray.clone(),
        },
        PT::gtextureBuffer => match image_class {
            IC::F => builtins.textureBuffer.clone(),
            IC::SI => builtins.itextureBuffer.clone(),
            IC::UI => builtins.utextureBuffer.clone(),
        },

        PT::gvec4 => match image_class {
            IC::F => builtins.vec4.clone(),
            IC::SI => builtins.ivec4.clone(),
            IC::UI => builtins.uvec4.clone(),
        },

        PT::subpassInput => {
            todo!()
        }
        PT::subpassInputMS => {
            todo!()
        }
        PT::sampler => builtins.sampler.clone(),
        PT::samplerShadow => builtins.samplerShadow.clone(),
    }
}

/// Describes a built-in operation (function or operator).
#[derive(Clone, Debug)]
pub struct BuiltinOperation {
    pub name: &'static str,
    pub signatures: &'static [BuiltinSignature],
}

macro_rules! builtin_operations {
    (
        $mod_name:ident, $array_name:ident;
        $(
            $op_name:ident {
                $( $ret_ty:ident ($($arg:ident),*) => $builder_fn:expr; )*
            }
        )*
    ) =>
    {
        #[allow(non_upper_case_globals)]
        pub mod $mod_name {
            use super::{BuiltinOperation, BuiltinSignature, PseudoType};
            $(pub static $op_name: BuiltinOperation = BuiltinOperation {
                name: std::stringify!($op_name),
                signatures: &[
                    $(
                        BuiltinSignature {
                            parameter_types: &[$(PseudoType::$arg),*],
                            result_type: PseudoType::$ret_ty,
                            lower: $builder_fn
                        }
                    ),*
                ]
            };)*
        }

        pub(super) static $array_name: &[&BuiltinOperation] = &[$(&$mod_name::$op_name),*];
    };
}

builtin_operations! {
    operations, OPERATION_SIGNATURES;

    //////////////////////////////////////////////////////
    // Operators
    //////////////////////////////////////////////////////
    And {
       bool(bool,bool)      => |_ctxt, fb, args, _types, ret| fb.emit_logical_and(ret, args[0], args[1]);
    }
    Or {
       bool(bool,bool)      => |_ctxt, fb, args, _types, ret| fb.emit_logical_or(ret, args[0], args[1]);
    }
    Eq {
       bvecN(vecN,vecN)     => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_equal(ret, args[0], args[1]);
       bvecN(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_equal(ret, args[0], args[1]);
       bvecN(ivecN,ivecN)   => |_ctxt, fb, args, _types, ret| fb.emit_i_equal(ret, args[0], args[1]);
       bvecN(uvecN,uvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_i_equal(ret, args[0], args[1]);
       bvecN(bvecN,bvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_logical_equal(ret, args[0], args[1]);
    }
    Ne {
       bvecN(vecN,vecN)     => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_not_equal(ret, args[0], args[1]);
       bvecN(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_not_equal(ret, args[0], args[1]);
       bvecN(ivecN,ivecN)   => |_ctxt, fb, args, _types, ret| fb.emit_i_not_equal(ret, args[0], args[1]);
       bvecN(uvecN,uvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_i_not_equal(ret, args[0], args[1]);
       bvecN(bvecN,bvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_logical_not_equal(ret, args[0], args[1]);
    }
    Gt {
        bvecN(vecN,vecN)      => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_greater_than(ret, args[0], args[1]);
        bvecN(dvecN,dvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_greater_than(ret, args[0], args[1]);
        bvecN(ivecN,ivecN)    => |_ctxt, fb, args, _types, ret| fb.emit_s_greater_than(ret, args[0], args[1]);
        bvecN(uvecN,uvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_u_greater_than(ret, args[0], args[1]);
    }
    Ge {
        bvecN(vecN,vecN)     => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_greater_than_equal(ret, args[0], args[1]);
        bvecN(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_greater_than_equal(ret, args[0], args[1]);
        bvecN(ivecN,ivecN)   => |_ctxt, fb, args, _types, ret| fb.emit_s_greater_than_equal(ret, args[0], args[1]);
        bvecN(uvecN,uvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_u_greater_than_equal(ret, args[0], args[1]);
    }
    Lt {
        bvecN(vecN,vecN)      => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_less_than(ret, args[0], args[1]);
        bvecN(dvecN,dvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_less_than(ret, args[0], args[1]);
        bvecN(ivecN,ivecN)    => |_ctxt, fb, args, _types, ret| fb.emit_s_less_than(ret, args[0], args[1]);
        bvecN(uvecN,uvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_u_less_than(ret, args[0], args[1]);
    }
    Le {
        bvecN(vecN,vecN)      => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_less_than_equal(ret, args[0], args[1]);
        bvecN(dvecN,dvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_f_ord_less_than_equal(ret, args[0], args[1]);
        bvecN(ivecN,ivecN)    => |_ctxt, fb, args, _types, ret| fb.emit_s_less_than_equal(ret, args[0], args[1]);
        bvecN(uvecN,uvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_u_less_than_equal(ret, args[0], args[1]);
    }
    Add {
        vecN(vecN,vecN)       => |_ctxt, fb, args, _types, ret| fb.emit_f_add(ret, args[0], args[1]);
        dvecN(dvecN,dvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_f_add(ret, args[0], args[1]);
        ivecN(ivecN,ivecN)    => |_ctxt, fb, args, _types, ret| fb.emit_i_add(ret, args[0], args[1]);
        uvecN(uvecN,uvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_i_add(ret, args[0], args[1]);
    }
    Mul {
        vec2(vec2,float)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[0], args[1]);
        vec3(vec3,float)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[0], args[1]);
        vec4(vec4,float)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[0], args[1]);
        vec2(float,vec2)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[1], args[0]);
        vec3(float,vec3)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[1], args[0]);
        vec4(float,vec4)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[1], args[0]);
        float(float,float)    => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);
        vec2(vec2,vec2)       => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);
        vec3(vec3,vec3)       => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);
        vec4(vec4,vec4)       => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);

        dvec2(dvec2,double)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[0], args[1]);
        dvec3(dvec3,double)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[0], args[1]);
        dvec4(dvec4,double)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[0], args[1]);
        dvec2(double,dvec2)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[1], args[0]);
        dvec3(double,dvec3)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[1], args[0]);
        dvec4(double,dvec4)      => |_ctxt, fb, args, _types, ret| fb.emit_vector_times_scalar(ret, args[1], args[0]);
        double(double,double)    => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);
        dvec2(dvec2,dvec2)       => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);
        dvec3(dvec3,dvec3)       => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);
        dvec4(dvec4,dvec4)       => |_ctxt, fb, args, _types, ret| fb.emit_f_mul(ret, args[0], args[1]);

        // TODO fix this
        ivecN(int,ivecN)    => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN,int)    => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN,ivecN)    => |_ctxt, fb, args, _types, ret| fb.emit_i_mul(ret, args[0], args[1]);

        uvecN(uint,uvecN)    => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN,uint)    => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN,uvecN)    => |_ctxt, fb, args, _types, ret| fb.emit_i_mul(ret, args[0], args[1]);
    }
    Sub {
        vecN(vecN,vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN,ivecN)   => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN,uvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }
    Div {
        vecN(vecN,vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN,ivecN)   => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN,uvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }
    Rem {  }
    Shl {  }
    Shr {  }
    BitXor {  }
    BitOr {  }
    BitAnd {  }
    Not {
    }
    Compl {  }
    UnaryMinus {
        ivecN(ivecN)   => |_ctxt, fb, args, _types, ret| fb.emit_s_negate(ret, args[0]);
        vecN(vecN)     => |_ctxt, fb, args, _types, ret| fb.emit_f_negate(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_f_negate(ret, args[0]);
    }

    //////////////////////////////////////////////////////
    // 8.1. Angle and Trigonometry Functions
    //////////////////////////////////////////////////////
    acos {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_acos(ret, args[0]);
    }
    acosh {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_acosh(ret, args[0]);
    }
    asin {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_asin(ret, args[0]);
    }
    asinh {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_asinh(ret, args[0]);
    }
    atan {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_atan(ret, args[0]);
        vecN(vecN,vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_atan2(ret, args[0], args[1]);
    }
    atanh {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_atanh(ret, args[0]);
    }
    cos {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_cos(ret, args[0]);
    }
    cosh {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_cosh(ret, args[0]);
    }
    sin {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_sin(ret, args[0]);
    }
    sinh {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_sinh(ret, args[0]);
    }
    tan {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_tan(ret, args[0]);
    }
    tanh {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_tanh(ret, args[0]);
    }
    radians {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_radians(ret, args[0]);
    }
    degrees {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_degrees(ret, args[0]);
    }

    //////////////////////////////////////////////////////
    // 8.2. Exponential Functions
    //////////////////////////////////////////////////////
    pow {
        vecN(vecN, vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_pow(ret, args[0], args[1]);
    }
    exp {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_exp(ret, args[0]);
    }
    log {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_log(ret, args[0]);
    }
    exp2 {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_exp2(ret, args[0]);
    }
    log2 {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_log2(ret, args[0]);
    }
    sqrt {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_sqrt(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_sqrt(ret, args[0]);
    }
    inversesqrt {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_inverse_sqrt(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_inverse_sqrt(ret, args[0]);
    }

    //////////////////////////////////////////////////////
    // 8.3. Common Functions
    //////////////////////////////////////////////////////

    abs {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_abs(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_abs(ret, args[0]);
        ivecN(ivecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_s_abs(ret, args[0]);
    }
    sign {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_sign(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_sign(ret, args[0]);
        ivecN(ivecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_s_sign(ret, args[0]);
    }
    floor {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_floor(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_floor(ret, args[0]);
    }
    trunc {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_trunc(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_trunc(ret, args[0]);
    }
    round {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_round(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_round(ret, args[0]);
    }
    roundEven {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_round_even(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_round_even(ret, args[0]);
    }
    ceil {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_ceil(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_ceil(ret, args[0]);
    }
    fract {
        vecN(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_fract(ret, args[0]);
        dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_fract(ret, args[0]);
    }

    r#mod {
        vecN(vecN,vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        vecN(vecN,float)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN,double)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    modf {
        modf_result_vecN(vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        modf_result_dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    min {
        vecN(vecN,vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        vecN(vecN,float)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN,double)   => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN,ivecN)   => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN,int)   => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN,uvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN,uint)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    max {
        vecN(vecN,vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        vecN(vecN,float)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN,double)   => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN,ivecN)   => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN,int)   => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN,uvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN,uint)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    clamp {
        vecN(vecN, vecN, vecN)         => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_clamp(ret, args[0], args[1], args[2]);
        vecN(vecN, float, float)       => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN, dvecN, dvecN)     => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_clamp(ret, args[0], args[1], args[2]);
        dvecN(dvecN, double, double)   => |_ctxt, fb, args, _types, ret| todo!();
        ivecN(ivecN, ivecN, ivecN)     => |_ctxt, fb, args, _types, ret| fb.emit_glsl_s_clamp(ret, args[0], args[1], args[2]);
        ivecN(ivecN, int, int)         => |_ctxt, fb, args, _types, ret| todo!();
        uvecN(uvecN, uvecN, uvecN)     => |_ctxt, fb, args, _types, ret| fb.emit_glsl_u_clamp(ret, args[0], args[1], args[2]);
        uvecN(uvecN, uint, uint)       => |_ctxt, fb, args, _types, ret| todo!();
    }

    mix {
        vecN(vecN, vecN, vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_mix(ret, args[0], args[1], args[2]);
        vecN(vecN, vecN, float)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN, dvecN, dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_f_mix(ret, args[0], args[1], args[2]);
        dvecN(dvecN, dvecN, double)   => |_ctxt, fb, args, _types, ret| todo!();
        vecN(vecN, vecN, bvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_select(ret, args[0], args[1], args[2]);
        dvecN(dvecN, dvecN, bvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_select(ret, args[0], args[1], args[2]);
        ivecN(ivecN, ivecN, bvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_select(ret, args[0], args[1], args[2]);
        uvecN(uvecN, uvecN, bvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_select(ret, args[0], args[1], args[2]);
        bvecN(bvecN, bvecN, bvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_select(ret, args[0], args[1], args[2]);
    }

    step {
        vecN(vecN, vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        vecN(float, vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN, dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(double, dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    smoothstep {
        vecN(vecN, vecN, vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        vecN(float, float, vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN, dvecN, dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(double, double, dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    isnan {
        bvecN(vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        bvecN(dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    isinf {
        bvecN(vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        bvecN(dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    floatBitsToInt {
        ivecN(highp_vecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    floatBitsToUint {
        uvecN(highp_vecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    intBitsToFloat {
        vecN(highp_ivecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    uintBitsToFloat {
        vecN(uvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    fma {
        vecN(vecN, vecN, vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        vecN(dvecN, dvecN, dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    frexp {
        frexp_result_highp_vecN(highp_vecN)   => |_ctxt, fb, args, _types, ret| todo!();
        //frexp_result_dvecN(dvecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    ldexp {
        vecN(highp_vecN, highp_ivecN)   => |_ctxt, fb, args, _types, ret| todo!();
        dvecN(dvecN, ivecN)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    //////////////////////////////////////////////////////
    // 8.5. Geometric Functions
    //////////////////////////////////////////////////////
    length {
        float(vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_length(ret, args[0]);
        double(dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_length(ret, args[0]);
    }

    distance {
        float(vecN,vecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_distance(ret, args[0], args[1]);
        double(dvecN,dvecN)   => |_ctxt, fb, args, _types, ret| fb.emit_glsl_distance(ret, args[0], args[1]);
    }

    // TODO

    //////////////////////////////////////////////////////
    // 8.6. Matrix functions
    //////////////////////////////////////////////////////
    matrixCompMult {
        mat2x2(mat2x2,mat2x2)   => |_ctxt, fb, args, _types, ret| todo!();
        mat2x3(mat2x3,mat2x3)   => |_ctxt, fb, args, _types, ret| todo!();
        mat2x4(mat2x4,mat2x4)   => |_ctxt, fb, args, _types, ret| todo!();
        mat3x2(mat3x2,mat3x2)   => |_ctxt, fb, args, _types, ret| todo!();
        mat3x3(mat3x3,mat3x3)   => |_ctxt, fb, args, _types, ret| todo!();
        mat3x4(mat3x4,mat3x4)   => |_ctxt, fb, args, _types, ret| todo!();
        mat4x2(mat4x2,mat4x2)   => |_ctxt, fb, args, _types, ret| todo!();
        mat4x3(mat4x3,mat4x3)   => |_ctxt, fb, args, _types, ret| todo!();
        mat4x4(mat4x4,mat4x4)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat2x2(dmat2x2,dmat2x2)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat2x3(dmat2x3,dmat2x3)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat2x4(dmat2x4,dmat2x4)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat3x2(dmat3x2,dmat3x2)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat3x3(dmat3x3,dmat3x3)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat3x4(dmat3x4,dmat3x4)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat4x2(dmat4x2,dmat4x2)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat4x3(dmat4x3,dmat4x3)   => |_ctxt, fb, args, _types, ret| todo!();
        dmat4x4(dmat4x4,dmat4x4)   => |_ctxt, fb, args, _types, ret| todo!();
    }


    //////////////////////////////////////////////////////
    // 8.9. Texture Functions
    // (this is not strictly GLSL, since textures & samplers are separated)
    //////////////////////////////////////////////////////
    textureSample {
        gvec4(gtexture1D, sampler, float)   => |_ctxt, fb, args, _types, ret| todo!();
        gvec4(gtexture2D, sampler, vec2)   => |_ctxt, fb, args, _types, ret| todo!();         // coords
        gvec4(gtexture2D, sampler, vec2, ivec2)   => |_ctxt, fb, args, _types, ret| todo!();   // coords,offset
        gvec4(gtexture2DArray, sampler, vec2, int)   => |_ctxt, fb, args, _types, ret| todo!();   // coords,array_index
        gvec4(gtexture2DArray, sampler, vec2, uint)   => |_ctxt, fb, args, _types, ret| todo!();   // coords,array_index
        gvec4(gtexture2DArray, sampler, vec2, int, ivec2)   => |_ctxt, fb, args, _types, ret| todo!();   // coords,array_index,offset
        gvec4(gtexture2DArray, sampler, vec2, uint, ivec2)   => |_ctxt, fb, args, _types, ret| todo!();   // coords,array_index,offset

        gvec4(gtexture3D, sampler, vec3)   => |_ctxt, fb, args, _types, ret| todo!();
        gvec4(gtextureCube, sampler, vec3)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    //////////////////////////////////////////////////////
    // 8.12. Image Functions
    //////////////////////////////////////////////////////
    imageSize {
         int(gimage1D)             => |_ctxt, fb, args, _types, ret| todo!();
         ivec2(gimage2D)           => |_ctxt, fb, args, _types, ret| todo!();
         ivec3(gimage3D)           => |_ctxt, fb, args, _types, ret| todo!();
         ivec2(gimageCube)         => |_ctxt, fb, args, _types, ret| todo!();
         ivec3(gimageCubeArray)    => |_ctxt, fb, args, _types, ret| todo!();
         ivec3(gimage2DArray)      => |_ctxt, fb, args, _types, ret| todo!();
         ivec2(gimage1DArray)      => |_ctxt, fb, args, _types, ret| todo!();
         ivec2(gimage2DMS)         => |_ctxt, fb, args, _types, ret| todo!();
         ivec3(gimage2DMSArray)    => |_ctxt, fb, args, _types, ret| todo!();
         int(gimageBuffer)         => |_ctxt, fb, args, _types, ret| todo!();
    }

    //////////////////////////////////////////////////////
    // 5.4. Constructors
    //////////////////////////////////////////////////////

    int {
        int(uint)   => |_ctxt, fb, args, _types, ret| todo!();
        int(bool)   => |_ctxt, fb, args, _types, ret| todo!();
        int(float)   => |_ctxt, fb, args, _types, ret| todo!();
        int(double)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    uint {
        // /* implicit */ uint(int)   => |_ctxt, fb, args, _types, ret| fb.emit_bitcast(ret, args[0]);
        uint(bool)   => |_ctxt, fb, args, _types, ret| todo!();
        uint(float)   => |_ctxt, fb, args, _types, ret| todo!();
        uint(double)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    bool {
        bool(int)   => |_ctxt, fb, args, _types, ret| todo!();
        bool(uint)   => |_ctxt, fb, args, _types, ret| todo!();
        bool(float)   => |_ctxt, fb, args, _types, ret| todo!();
        bool(double)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    float {
        // /* implicit */ float(int)   => |_ctxt, fb, args, _types, ret| fb.emit_convert_s_to_f(ret, args[0]);
        // /* implicit */ float(uint)   => |_ctxt, fb, args, _types, ret| fb.emit_convert_u_to_f(ret, args[0]);
        float(bool)   => |_ctxt, fb, args, _types, ret| todo!();
        float(double)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    double {
        // /* implicit */ double(int)   => |_ctxt, fb, args, _types, ret| fb.emit_convert_s_to_f(ret, args[0]);
        // /* implicit */ double(uint)   => |_ctxt, fb, args, _types, ret| fb.emit_convert_u_to_f(ret, args[0]);
        // /* implicit */ double(float)   => |_ctxt, fb, args, _types, ret| fb.emit_f_convert(ret, args[0]);
        double(bool)   => |_ctxt, fb, args, _types, ret| todo!();
    }

    vec2 {
        vec2(float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, &[args[0], args[0]]);
        vec2(float,float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        vec2(vec2)   => |_ctxt, fb, args, _types, ret| fb.emit_copy_object(ret, args[0]);
    }

    vec3 {
        vec3(float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, &[args[0], args[0], args[0]]);
        vec3(float,float,float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        vec3(vec2, float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        vec3(float, vec2)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        vec3(vec3)   => |_ctxt, fb, args, _types, ret| fb.emit_copy_object(ret, args[0]);
        vec3(vec4)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_extract(ret, args[0], &[0,1,2]);
    }

    vec4 {
        vec4(float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, &[args[0], args[0], args[0], args[0]]);
        vec4(float,float,float,float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);

        vec4(vec2, float, float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        vec4(float, vec2, float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        vec4(float, float, vec2)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);

        vec4(vec3, float)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);
        vec4(float, vec3)   => |_ctxt, fb, args, _types, ret| fb.emit_composite_construct(ret, args);

        vec4(vec4)   => |_ctxt, fb, args, _types, ret| fb.emit_copy_object(ret, args[0]);
    }
}
