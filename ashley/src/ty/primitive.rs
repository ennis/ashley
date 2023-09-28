use crate::ty::{ImageSampling, ImageType, Interner, ScalarType, StructField, Type, TypeKind};

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

macro_rules! builtin_struct_type {
    ($($name:ident : $t:expr),*) => {
        TypeKind::BuiltinStruct {
            name: String::new(),
            fields: vec![
                $(StructField::new(std::stringify!($name).to_string(), $t.clone(), None),)*
            ]
        }
    };
}

macro_rules! define_primitive_types {
    ($struct_name:ident; $($name:ident $tykind:expr;)* [private] $($priv_name:ident $priv_tykind:expr;)*) => {

        #[allow(non_snake_case)]
        pub struct $struct_name {
            $(pub $name: Type,)*
            $(pub $priv_name: Type,)*
        }

        impl $struct_name {
            #[allow(non_snake_case)]
            pub fn new(interner: &mut Interner) -> $struct_name {
                $(let $name = interner.intern($tykind);)*
                $(let $priv_name = interner.intern($priv_tykind);)*

                $struct_name {
                    $($name,)*
                    $($priv_name,)*
                }
            }

            pub fn resolve(&self, name: &str) -> Option<Type> {
                match name {
                    $(
                        std::stringify!($name) => { Some(self.$name.clone()) },
                    )*
                    _ => None,
                }
            }
        }
    };
}

// Primitive types and their constructor forms
define_primitive_types! {
    PrimitiveTypes;

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

    // TODO check strides
    mat2            TypeKind::Matrix { stride: 8, component_type: ScalarType::Float, columns: 2, rows: 2 };
    mat3            TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 3, rows: 3 };
    mat4            TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 4, rows: 4 };
    dmat2           TypeKind::Matrix { stride: 16, component_type: ScalarType::Double, columns: 2, rows: 2 };
    dmat3           TypeKind::Matrix { stride: 32, component_type: ScalarType::Double, columns: 3, rows: 3 };
    dmat4           TypeKind::Matrix { stride: 32, component_type: ScalarType::Double, columns: 4, rows: 4 };
    mat2x2          TypeKind::Matrix { stride: 8, component_type: ScalarType::Float, columns: 2, rows: 2 };
    mat2x3          TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 2, rows: 3 };
    mat2x4          TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 2, rows: 4 };
    mat3x2          TypeKind::Matrix { stride: 8, component_type: ScalarType::Float, columns: 3, rows: 2 };
    mat3x3          TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 3, rows: 3 };
    mat3x4          TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 3, rows: 4 };
    mat4x2          TypeKind::Matrix { stride: 8, component_type: ScalarType::Float, columns: 4, rows: 2 };
    mat4x3          TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 4, rows: 3 };
    mat4x4          TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 4, rows: 4 };
    dmat2x2         TypeKind::Matrix { stride: 16, component_type: ScalarType::Double, columns: 2, rows: 2 };
    dmat2x3         TypeKind::Matrix { stride: 32, component_type: ScalarType::Double, columns: 2, rows: 3 };
    dmat2x4         TypeKind::Matrix { stride: 32, component_type: ScalarType::Double, columns: 2, rows: 4 };
    dmat3x2         TypeKind::Matrix { stride: 16, component_type: ScalarType::Double, columns: 3, rows: 2 };
    dmat3x3         TypeKind::Matrix { stride: 32, component_type: ScalarType::Double, columns: 3, rows: 3 };
    dmat3x4         TypeKind::Matrix { stride: 32, component_type: ScalarType::Double, columns: 3, rows: 4 };
    dmat4x2         TypeKind::Matrix { stride: 16, component_type: ScalarType::Double, columns: 4, rows: 2 };
    dmat4x3         TypeKind::Matrix { stride: 32, component_type: ScalarType::Double, columns: 4, rows: 3 };
    dmat4x4         TypeKind::Matrix { stride: 32, component_type: ScalarType::Double, columns: 4, rows: 4 };

    // TODO matrix types with std140 strides
    std140_mat2     TypeKind::Matrix { stride: 16, component_type: ScalarType::Float, columns: 2, rows: 2 };

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

    // unnameable types
    [private]

    modf_result_float builtin_struct_type!(fract: float, whole: float);  //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), float.clone(), None), StructField::new("whole".to_string(), float.clone(), None) ] };
    modf_result_vec2  builtin_struct_type!(fract: vec2, whole: vec2); //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), vec2.clone(), None) ,  StructField::new("whole".to_string(), vec2.clone(), None) ] };
    modf_result_vec3  builtin_struct_type!(fract: vec3, whole: vec3); //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), vec3.clone(), None) ,  StructField::new("whole".to_string(), vec3.clone(), None) ] };
    modf_result_vec4  builtin_struct_type!(fract: vec4, whole: vec4); //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), vec4.clone(), None) ,  StructField::new("whole".to_string(), vec4.clone(), None) ] };

    modf_result_double builtin_struct_type!(fract: double, whole: double);// TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), double.clone(), None), StructField::new("whole".to_string(),double.clone(), None) ] };
    modf_result_dvec2  builtin_struct_type!(fract: dvec2, whole: dvec2);   //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), dvec2.clone(), None),  StructField::new("whole".to_string(),dvec2.clone(), None) ] };
    modf_result_dvec3  builtin_struct_type!(fract: dvec3, whole: dvec3);  // TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), dvec3.clone(), None),  StructField::new("whole".to_string(),dvec3.clone(), None) ] };
    modf_result_dvec4  builtin_struct_type!(fract: dvec4, whole: dvec4);  // TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), dvec4.clone(), None),  StructField::new("whole".to_string(),dvec4.clone(), None) ] };

    frexp_result_float builtin_struct_type!(fract: float, exp: float); //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), float.clone(), None) , StructField::new("exp".to_string(), float.clone(), None)  ] };
    frexp_result_vec2  builtin_struct_type!(fract: vec2, exp: vec2);   //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), vec2.clone(), None) ,  StructField::new("exp".to_string(), vec2.clone(), None)  ] };
    frexp_result_vec3  builtin_struct_type!(fract: vec3, exp: vec3);   //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), vec3.clone(), None) ,  StructField::new("exp".to_string(), vec3.clone(), None)  ] };
    frexp_result_vec4  builtin_struct_type!(fract: vec4, exp: vec4);   //TypeKind::Struct { name: String::new(), def: None, offsets: None, fields: vec![StructField::new("fract".to_string(), vec4.clone(), None) ,  StructField::new("exp".to_string(), vec4.clone(), None)  ] };
}
