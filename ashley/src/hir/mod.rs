mod builder;
mod constant;
mod list;
//pub mod print;
pub mod transform;
pub mod types;
mod visit;

use crate::{
    diagnostic::{DiagnosticBuilder, Diagnostics, SourceLocation},
    hir::{
        list::{List, ListNode},
        types::{Field, ScalarType, StructType},
    },
};
use ashley::utils::interner::UniqueArena;
use bumpalo::Bump;
use id_arena::Arena;
use ordered_float::OrderedFloat;
use rspirv::{
    spirv,
    spirv::{LinkageType, MemorySemantics},
};
use smallvec::SmallVec;
use spirv::Op;
use std::{
    fmt,
    hash::Hash,
    num::NonZeroU32,
    ops::{Bound, Deref, DerefMut, RangeBounds},
    sync::Arc,
};

pub use self::{builder::FunctionBuilder, constant::ConstantData, types::TypeData};

//--------------------------------------------------------------------------------------------------

macro_rules! id_types {
    ($($(#[$m:meta])* $v:vis struct $n:ident;)*) => {
        $(
        $(#[$m])*
        #[derive(Copy,Clone,Debug,Eq,PartialEq,Ord,PartialOrd,Hash)]
        #[repr(transparent)]
        $v struct $n(NonZeroU32);

        impl $n {
            $v fn index(&self) -> usize {
                (self.0.get() - 1) as usize
            }

            $v fn from_index(index: usize) -> $n {
                $n(unsafe { NonZeroU32::new_unchecked((index+1) as u32) })
            }
        }

        impl id_arena::ArenaBehavior for $n {
            type Id = Self;

            fn new_id(_arena_id: u32, index: usize) -> Self::Id {
                $n(unsafe { NonZeroU32::new_unchecked((index+1) as u32) })
            }

            fn index(id: Self::Id) -> usize {
                (id.0.get() - 1) as usize
            }

            fn arena_id(_: Self::Id) -> u32 {
                0
            }

            fn new_arena_id() -> u32 {
                0
            }
        }
        )*
    };
}

// Define the handle types for elements in the context arrays
id_types! {

    /// Handle to a HIR value.
    pub struct Value;

    /// Handle to a HIR function.
    pub struct Function;

    /// Interned handle to a HIR type.
    pub struct Type;

    /// Interned handle to a HIR constant.
    pub struct Constant;

    /// Interned handle to a HIR constant.
    pub struct GlobalVariable;

    /// Handle to a function basic block.
    pub struct Block;

    /// ID representing an imported extended instruction set.
    pub struct ExtendedInstructionSetId;
}

impl Type {
    /// Returns the pointee type (the type of the value produced by dereferencing the pointer).
    pub fn pointee_type(&self, m: &Module) -> Option<(Type, spirv::StorageClass)> {
        match m.types[*self] {
            TypeData::Pointer {
                pointee_type,
                storage_class,
            } => Some((pointee_type, storage_class)),
            _ => None,
        }
    }
}

/// Reference to a value or a constant.
#[derive(Copy, Clone, Debug)]
pub enum ValueOrConstant {
    Value(Value),
    Constant(Constant),
}

impl From<Value> for ValueOrConstant {
    fn from(value: Value) -> Self {
        ValueOrConstant::Value(value)
    }
}

impl From<Constant> for ValueOrConstant {
    fn from(constant: Constant) -> Self {
        ValueOrConstant::Constant(constant)
    }
}

//--------------------------------------------------------------------------------------------------

/// A reference to an object.
#[derive(Copy, Clone, Debug)]
pub enum IdRef {
    /// SSA value within the current function
    Value(Value),
    /// Function
    Function(Function),
    /// Constant
    Constant(Constant),
    /// Global variable
    Global(GlobalVariable),
}

impl IdRef {
    pub fn to_value_or_constant(&self) -> Option<ValueOrConstant> {
        match *self {
            IdRef::Value(v) => Some(ValueOrConstant::Value(v)),
            IdRef::Constant(v) => Some(ValueOrConstant::Constant(v)),
            IdRef::Function(_) => None,
            IdRef::Global(_) => None,
        }
    }

    pub fn to_function(&self) -> Option<Function> {
        match *self {
            IdRef::Function(f) => Some(f),
            _ => None,
        }
    }
}

impl From<Value> for IdRef {
    fn from(value: Value) -> Self {
        IdRef::Value(value)
    }
}

impl From<Constant> for IdRef {
    fn from(constant: Constant) -> Self {
        IdRef::Constant(constant)
    }
}

impl From<GlobalVariable> for IdRef {
    fn from(global: GlobalVariable) -> Self {
        IdRef::Global(global)
    }
}

impl From<Function> for IdRef {
    fn from(f: Function) -> Self {
        IdRef::Function(f)
    }
}

//--------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u32)]
pub enum PackedVectorFormat {
    PackedVectorFormat4x8Bit,
}

// TODO we may not need so much variants: we only need to distinguish between IDs in different arenas,
// the rest can go into a generic u32
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    ConstantRef(Constant),
    GlobalRef(GlobalVariable),
    FunctionRef(Function),
    ValueRef(Value),
    BlockRef(Block),
    TypeRef(Type),

    FPFastMathMode(spirv::FPFastMathMode),
    SelectionControl(spirv::SelectionControl),
    LoopControl(spirv::LoopControl),
    FunctionControl(spirv::FunctionControl),
    MemorySemantics(spirv::MemorySemantics),
    MemoryAccess(spirv::MemoryAccess),
    KernelProfilingInfo(spirv::KernelProfilingInfo),
    RayFlags(spirv::RayFlags),
    FragmentShadingRate(spirv::FragmentShadingRate),
    SourceLanguage(spirv::SourceLanguage),
    ExecutionModel(spirv::ExecutionModel),
    AddressingModel(spirv::AddressingModel),
    MemoryModel(spirv::MemoryModel),
    ExecutionMode(spirv::ExecutionMode),
    StorageClass(spirv::StorageClass),
    Dim(spirv::Dim),
    SamplerAddressingMode(spirv::SamplerAddressingMode),
    SamplerFilterMode(spirv::SamplerFilterMode),
    ImageOperands(spirv::ImageOperands),
    ImageFormat(spirv::ImageFormat),
    ImageChannelOrder(spirv::ImageChannelOrder),
    ImageChannelDataType(spirv::ImageChannelDataType),
    FPRoundingMode(spirv::FPRoundingMode),
    LinkageType(spirv::LinkageType),
    AccessQualifier(spirv::AccessQualifier),
    FunctionParameterAttribute(spirv::FunctionParameterAttribute),
    Decoration(spirv::Decoration),
    BuiltIn(spirv::BuiltIn),
    Scope(spirv::Scope),
    GroupOperation(spirv::GroupOperation),
    KernelEnqueueFlags(spirv::KernelEnqueueFlags),
    Capability(spirv::Capability),
    RayQueryIntersection(spirv::RayQueryIntersection),
    RayQueryCommittedIntersectionType(spirv::RayQueryCommittedIntersectionType),
    RayQueryCandidateIntersectionType(spirv::RayQueryCandidateIntersectionType),
    PackedVectorFormat(PackedVectorFormat),
    LiteralInt32(u32),
    LiteralInt64(u64),
    LiteralFloat32(OrderedFloat<f32>),
    LiteralFloat64(OrderedFloat<f64>),
    LiteralExtInstInteger(u32),
    LiteralSpecConstantOpInteger(spirv::Op),
    LiteralString(String),
}

impl From<i32> for Operand {
    fn from(value: i32) -> Self {
        Operand::LiteralInt32(value as u32)
    }
}

impl From<u32> for Operand {
    fn from(value: u32) -> Self {
        Operand::LiteralInt32(value as u32)
    }
}

impl From<f32> for Operand {
    fn from(value: f32) -> Self {
        Operand::LiteralFloat32(OrderedFloat::from(value))
    }
}

impl From<Value> for Operand {
    fn from(value: Value) -> Self {
        Operand::ValueRef(value)
    }
}

impl From<Function> for Operand {
    fn from(f: Function) -> Self {
        Operand::FunctionRef(f)
    }
}

impl From<Block> for Operand {
    fn from(b: Block) -> Self {
        Operand::BlockRef(b)
    }
}

impl From<Constant> for Operand {
    fn from(value: Constant) -> Self {
        Operand::ConstantRef(value)
    }
}

impl From<GlobalVariable> for Operand {
    fn from(var: GlobalVariable) -> Self {
        Operand::GlobalRef(var)
    }
}

impl From<Type> for Operand {
    fn from(t: Type) -> Self {
        Operand::TypeRef(t)
    }
}

impl From<ValueOrConstant> for Operand {
    fn from(value: ValueOrConstant) -> Self {
        match value {
            ValueOrConstant::Value(v) => Operand::ValueRef(v),
            ValueOrConstant::Constant(v) => Operand::ConstantRef(v),
        }
    }
}

impl From<IdRef> for Operand {
    fn from(value: IdRef) -> Self {
        match value {
            IdRef::Value(v) => Operand::ValueRef(v),
            IdRef::Constant(v) => Operand::ConstantRef(v),
            IdRef::Global(v) => Operand::GlobalRef(v),
            IdRef::Function(v) => Operand::FunctionRef(v),
        }
    }
}
macro_rules! impl_operand_from {
    ($($v:ident ($t:ty); )*) => {
        $(impl From<$t> for Operand {
            fn from(value: $t) -> Self {
                Operand::$v(value)
            }
        })*
    };
}

impl_operand_from! {
    FPFastMathMode(spirv::FPFastMathMode);
    SelectionControl(spirv::SelectionControl);
    LoopControl(spirv::LoopControl);
    FunctionControl(spirv::FunctionControl);
    MemorySemantics(spirv::MemorySemantics);
    MemoryAccess(spirv::MemoryAccess);
    KernelProfilingInfo(spirv::KernelProfilingInfo);
    RayFlags(spirv::RayFlags);
    FragmentShadingRate(spirv::FragmentShadingRate);
    SourceLanguage(spirv::SourceLanguage);
    ExecutionModel(spirv::ExecutionModel);
    AddressingModel(spirv::AddressingModel);
    MemoryModel(spirv::MemoryModel);
    ExecutionMode(spirv::ExecutionMode);
    StorageClass(spirv::StorageClass);
    Dim(spirv::Dim);
    SamplerAddressingMode(spirv::SamplerAddressingMode);
    SamplerFilterMode(spirv::SamplerFilterMode);
    ImageOperands(spirv::ImageOperands);
    ImageFormat(spirv::ImageFormat);
    ImageChannelOrder(spirv::ImageChannelOrder);
    ImageChannelDataType(spirv::ImageChannelDataType);
    FPRoundingMode(spirv::FPRoundingMode);
    LinkageType(spirv::LinkageType);
    AccessQualifier(spirv::AccessQualifier);
    FunctionParameterAttribute(spirv::FunctionParameterAttribute);
    Decoration(spirv::Decoration);
    BuiltIn(spirv::BuiltIn);
    Scope(spirv::Scope);
    GroupOperation(spirv::GroupOperation);
    KernelEnqueueFlags(spirv::KernelEnqueueFlags);
    Capability(spirv::Capability);
    RayQueryIntersection(spirv::RayQueryIntersection);
    RayQueryCommittedIntersectionType(spirv::RayQueryCommittedIntersectionType);
    RayQueryCandidateIntersectionType(spirv::RayQueryCandidateIntersectionType);
    PackedVectorFormat(PackedVectorFormat);
}

/// Data associated to an operation.
#[derive(Clone, Debug)]
pub struct InstructionData {
    pub opcode: spirv::Op,
    pub result: Option<Value>,
    pub operands: SmallVec<[Operand; 3]>,
}

/// SSA value data.
#[derive(Clone, Debug)]
pub struct ValueData {
    /// The type of the value.
    ty: Type,
}

impl ValueData {
    /// Creates a new SSA value with the specified type.
    pub fn new(ty: Type) -> ValueData {
        ValueData { ty }
    }
}

/*/// Operation cursor.
#[derive(Copy, Clone)]
pub enum Cursor {
    /// Insert at the end.
    End(RegionId),
    /// Insert before the specified operation.
    Before(RegionId, OperationId),
}*/

/// Function argument
#[derive(Clone, Debug)]
pub struct FunctionParameter {
    /// Name of the parameter, for debugging purposes only
    pub name: String,
    /// Type of the parameter
    pub ty: Type,
}

/// HIR functions or function declarations.
#[derive(Clone, Debug)]
pub struct FunctionData {
    pub parameters: Vec<FunctionParameter>,
    pub arguments: Vec<Value>,
    pub function_type: Type,
    pub name: String,
    pub linkage: Option<spirv::LinkageType>,
    pub values: Arena<ValueData, Value>,
    pub blocks: Arena<BlockData, Block>,
    pub entry_block: Option<Block>,
    pub function_control: spirv::FunctionControl,
}

impl FunctionData {
    /// Creates a new function with a block.
    pub fn new(
        name: String,
        function_type: Type,
        parameters: Vec<FunctionParameter>,
        linkage: Option<spirv::LinkageType>,
        function_control: spirv::FunctionControl,
    ) -> (FunctionData, Block) {
        let mut blocks = Arena::new();
        let mut values = Arena::new();

        let arguments: Vec<_> = parameters
            .iter()
            .map(|param| values.alloc(ValueData::new(param.ty)))
            .collect();

        let entry_block = blocks.alloc(BlockData::new());

        (
            FunctionData {
                parameters,
                arguments,
                function_type,
                name,
                linkage,
                values,
                blocks,
                entry_block: Some(entry_block),
                function_control,
            },
            entry_block,
        )
    }

    /// Creates a new function declaration.
    pub fn new_declaration(
        name: String,
        function_type: Type,
        parameters: Vec<FunctionParameter>,
        linkage: Option<spirv::LinkageType>,
        function_control: spirv::FunctionControl,
    ) -> FunctionData {
        FunctionData {
            parameters,
            arguments: vec![],
            function_type,
            name,
            linkage,
            values: Default::default(),
            blocks: Default::default(),
            entry_block: None,
            function_control,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum IntegerLiteral {
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
}

/// Block terminator instruction.
#[derive(Clone, Debug)]
pub enum TerminatingInstruction {
    Branch(Block),
    BranchConditional {
        condition: ValueOrConstant,
        true_block: Block,
        false_block: Block,
    },
    Switch {
        selector: ValueOrConstant,
        default: Block,
        target: Vec<(IntegerLiteral, Block)>,
    },
    Return,
    ReturnValue(IdRef),
    Unreachable,
    TerminateInvocation,
}

#[derive(Clone, Debug)]
pub struct BlockData {
    pub instructions: Vec<InstructionData>,
    pub terminator: Option<TerminatingInstruction>,
}

impl BlockData {
    pub fn new() -> BlockData {
        BlockData {
            instructions: vec![],
            terminator: None,
        }
    }
}

/// Describes the domain of a shader value.
#[derive(Copy, Clone, Debug)]
pub enum Domain {
    /// Uniforms
    Uniform,
    /// Vertex-domain value (vertex attribute or values in vertex shaders)
    VertexInvocation,
    /// Fragment-domain value (values in fragment shaders).
    ///
    /// Fragment-domain values can be used in fragment derivative operations (dFdx & dFdy).
    FragmentInvocation,
    /// Generic value inside a shader invocation (used for all other stages).
    Invocation,
}

/// Describes a global variable.
pub struct GlobalVariableData {
    pub name: String,
    /// Type of the global variable (not necessarily a pointer).
    /// TODO replace with a pointer?
    pub ty: Type,
    pub storage_class: spirv::StorageClass,
    pub linkage: Option<LinkageType>,
}

pub enum ProgramKind {
    Generic,
    Vertex,
    Fragment,
    Compute,
    Rasterize,
}

macro_rules! const_vec_builder {
    ($method2:ident,$method3:ident,$method4:ident, $component:ty, $scalar_ty:ident, $component_method:ident) => {
        pub fn $method2(&mut self, x: $component, y: $component) -> Constant {
            let x = self.$component_method(x);
            let y = self.$component_method(y);
            let ty = self.define_type(TypeData::Vector(ScalarType::$scalar_ty, 2));
            self.define_constant(ConstantData::Composite {
                ty,
                constituents: vec![x, y],
            })
        }
        pub fn $method3(&mut self, x: $component, y: $component, z: $component) -> Constant {
            let x = self.$component_method(x);
            let y = self.$component_method(y);
            let z = self.$component_method(z);
            let ty = self.define_type(TypeData::Vector(ScalarType::$scalar_ty, 3));
            self.define_constant(ConstantData::Composite {
                ty,
                constituents: vec![x, y, z],
            })
        }
        pub fn $method4(&mut self, x: $component, y: $component, z: $component, w: $component) -> Constant {
            let x = self.$component_method(x);
            let y = self.$component_method(y);
            let z = self.$component_method(z);
            let w = self.$component_method(w);
            let ty = self.define_type(TypeData::Vector(ScalarType::$scalar_ty, 4));
            self.define_constant(ConstantData::Composite {
                ty,
                constituents: vec![x, y, z, w],
            })
        }
    };
}

/// Container for HIR entities.
///
/// It holds regions, values, operations, types and attributes.
pub struct Module {
    pub(crate) types: UniqueArena<TypeData<'static>, Type>,
    pub(crate) constants: UniqueArena<ConstantData, Constant>,
    pub functions: id_arena::Arena<FunctionData, Function>,
    pub globals: id_arena::Arena<GlobalVariableData, GlobalVariable>,
    ty_unknown: Type,
    ty_unit: Type,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Module").finish_non_exhaustive()
    }
}

impl Module {
    pub fn new() -> Module {
        let mut types = UniqueArena::new();
        let constants = UniqueArena::new();
        let functions = Default::default();
        let globals = Default::default();
        let (ty_unknown, _) = types.insert(TypeData::Unknown);
        let (ty_unit, _) = types.insert(TypeData::Unit);

        Module {
            types,
            constants,
            functions,
            globals,
            ty_unknown,
            ty_unit,
        }
    }

    /// Interns a type.
    pub fn define_type(&mut self, ty: TypeData<'static>) -> Type {
        // TypeData<'1>, '1 is invariant
        // get_index_of() U == TypeData<'1>
        // TypeData<'static>: Borrow<TypeData<'1>>
        if let Some(id) = self.types.get_index_of(&ty) {
            id
        } else {
            self.types.insert(ty.into_static()).0
        }
    }

    /*/// Defines a structure type.
    pub fn define_struct_type(&mut self, name: Option<&str>, fields: impl IntoIterator<Item = Field>) -> Type {
        let fields = fields.collect::<Vec<_>>();
        let name = name.map(ToOwned::to_owned);
        let struct_type = StructType { name, fields };
        // TODO: intern struct types separately so that we don't allocate an arc every time
        self.define_type(TypeData::Struct(Arc::new(struct_type)))
    }*/

    /// Returns the unknown type.
    pub fn ty_unknown(&self) -> Type {
        self.ty_unknown
    }

    /// Alias for ty_unknown.
    pub fn error_type(&self) -> Type {
        self.ty_unknown
    }

    /// Returns the unknown type.
    pub fn ty_unit(&self) -> Type {
        self.ty_unit
    }

    /// Interns a constant.
    pub fn define_constant(&mut self, c: ConstantData) -> Constant {
        if let Some(id) = self.constants.get_index_of(&c) {
            id
        } else {
            self.constants.insert(c.clone()).0
        }
    }

    pub fn define_global_variable(&mut self, v: GlobalVariableData) -> GlobalVariable {
        self.globals.alloc(v)
    }

    /// Returns a pointer type.
    pub fn pointer_type(&mut self, pointee_type: Type, storage_class: spirv::StorageClass) -> Type {
        self.define_type(TypeData::Pointer {
            pointee_type,
            storage_class,
        })
    }

    /// Adds a function to the module.
    pub fn add_function(&mut self, function: FunctionData) -> Function {
        self.functions.alloc(function)
    }

    /// Emits a floating-point constant.
    pub fn const_f32(&mut self, value: f32) -> Constant {
        self.define_constant(ConstantData::F32(OrderedFloat::from(value)))
    }

    /// Emits an integer constant.
    pub fn const_i32(&mut self, value: i32) -> Constant {
        self.define_constant(ConstantData::I32(value))
    }
    /// Emits an integer constant.
    pub fn const_u32(&mut self, value: u32) -> Constant {
        self.define_constant(ConstantData::U32(value))
    }

    /// Emits a boolean constant.
    pub fn const_bool(&mut self, value: bool) -> Constant {
        self.define_constant(ConstantData::Bool(value))
    }

    const_vec_builder!(const_f32x2, const_f32x3, const_f32x4, f32, Float, const_f32);
    const_vec_builder!(const_i32x2, const_i32x3, const_i32x4, i32, Int, const_i32);
    const_vec_builder!(const_u32x2, const_u32x3, const_u32x4, u32, UnsignedInt, const_u32);
}

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    //use crate::hir::{print_hir_region_html, HirArena, HirCtxt};
    use std::{fs::File, io::Write, path::PathBuf};

    fn html_output_file_path(title: &str) -> PathBuf {
        const OUT_DIR: &str = "tests/out";
        let root = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(root).join(OUT_DIR).join(format!("{title}.html"))
    }
}
