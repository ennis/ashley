//! Shader (SPIR-V like) HIR dialect.


/// Primitive value types.
///
/// Scalar values of integral and floating-point types.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ScalarType {
    Int,
    UnsignedInt,
    Float,
    Double,
    Bool,
}

impl ScalarType {
    pub fn display(&self) -> &'static str {
        match *self {
            ScalarType::Int => "int",
            ScalarType::UnsignedInt => "uint",
            ScalarType::Float => "float",
            ScalarType::Double => "double",
            ScalarType::Bool => "bool",
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ImageDimension {
    Dim1D,
    Dim2D,
    Dim3D,
    DimCube,
    Dim1DArray,
    Dim2DArray,
}

/// Vector type (ty,size).
#[derive(Copy,Clone,Debug,Eq,PartialEq,Hash)]
//#[uuid("a02bb7a5-5c0a-4c52-855a-47c6bb30ba7c")]
pub struct VectorType(ScalarType, u8);

/// Matrix type. Element type, columns, rows, in this order.
#[derive(Copy,Clone,Debug,Eq,PartialEq,Hash)]
//#[uuid("be5259b7-b91d-42d7-8fa2-fc563cb00f5f")]
pub struct MatrixType(ScalarType, u8, u8);

/*macro_rules! impl_type {
    ($v:vis struct $n:name { $($attr_name:ident : $attr_ty:ty ;)* } ) => {
        $v struct $n<'a>(&'a Type);

        impl<'a> $n<'a> {
            $(
                $v fn $attr_name(&self) -> &$attr_ty {
                    self.0.attribute_by_name(std::stringify!($attr_name))
                }
            )*
        }
    };
}

impl_type!{
    pub struct VectorType();
}*/