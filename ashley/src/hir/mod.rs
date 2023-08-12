mod builder;
mod constant;
//mod list;
//pub mod print;
mod decoration;
mod layout;
pub mod transform;
pub mod types;
mod visit;

use crate::{
    diagnostic::SourceLocation,
    hir,
    hir::types::ScalarType,
    utils::{id_types, interner::UniqueArena},
};
use id_arena::Arena;
use ordered_float::OrderedFloat;
use rspirv::{spirv, spirv::LinkageType};
use smallvec::SmallVec;
use spirv::{Op, StorageClass};
use std::{collections::HashSet, fmt, hash::Hash, num::NonZeroU32};

pub use self::{
    builder::FunctionBuilder,
    constant::ConstantData,
    decoration::Decoration,
    layout::{ArrayLayout, InnerLayout, Layout, StructLayout},
    types::{Interpolation, InterpolationKind, InterpolationSampling, TypeData, TypeDataDebug},
};

//--------------------------------------------------------------------------------------------------

// Define the handle types for elements in the context arrays
id_types! {

    /// Handle to a HIR value.
    pub struct Value;

    /// Handle to a HIR function.
    pub struct Function;

    /// Handle to a function local.
    pub struct Local;

    /// Interned handle to a HIR type.
    pub struct Type;

    /// Interned handle to a HIR constant.
    pub struct Constant;

    /// Interned handle to a HIR constant.
    pub struct GlobalVariable;

    /// Handle to a function basic block.
    pub struct Block;

    /// Handle to an entry point definition.
    pub struct EntryPoint;

    /// ID representing an imported extended instruction set.
    pub struct ExtInstSet;
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
    /*pub fn to_value_or_constant(&self) -> Option<ValueOrConstant> {
        match *self {
            IdRef::Value(v) => Some(ValueOrConstant::Value(v)),
            IdRef::Constant(v) => Some(ValueOrConstant::Constant(v)),
            IdRef::Function(_) => None,
            IdRef::Global(_) => None,
        }
    }*/

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

/*impl From<Local> for IdRef {
    fn from(local: Local) -> Self {
        IdRef::Local(local)
    }
}*/

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
    //LocalRef(Local),
    TypeRef(Type),
    ExtInstSet(ExtInstSet),

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
            IdRef::Constant(c) => Operand::ConstantRef(c),
            IdRef::Global(g) => Operand::GlobalRef(g),
            IdRef::Function(f) => Operand::FunctionRef(f),
            //IdRef::Local(l) => Operand::LocalRef(l),
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

#[derive(Clone, Debug)]
pub struct LocalData {
    pub name: String,
    pub value: hir::Value,
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
    // TODO should they be block-local? There are instructions to specify their lifetime within a function (OpLifetimeStart/Stop)
    // but not sure if they can be used in shaders
    pub locals: Arena<LocalData, Local>,
    pub blocks: Arena<BlockData, Block>,
    /// TODO remove this, the entry block should be the first one in the arena
    pub entry_block: Option<Block>,
    pub function_control: spirv::FunctionControl,
}

#[derive(Clone, Debug)]
pub struct EntryPointData {
    pub function: Function,
    pub name: String,
    pub execution_model: spirv::ExecutionModel,
    pub shader_interface: Vec<GlobalVariable>,
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
        let locals = Arena::new();

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
                locals,
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
            locals: Default::default(),
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
        condition: IdRef,
        true_block: Block,
        false_block: Block,
    },
    Switch {
        selector: IdRef,
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

/*/// Describes the domain of a shader value.
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
}*/

/// Describes a global variable.
pub struct GlobalVariableData {
    /// Name of the global variable. Should be unique across all global variables and functions in a module.
    pub name: String,
    /// Type of the global variable (without the mandatory pointer indirection).
    pub ty: Type,
    /// Storage class.
    pub storage_class: spirv::StorageClass,
    /// Optional source location.
    pub source_location: Option<SourceLocation>,
    /// SPIR-V variable decorations.
    pub decorations: Vec<Decoration>,
    /// Linkage information.
    pub linkage: Option<LinkageType>,
    /// Whether the global variable has been explicitly removed with `remove_global_var`.
    ///
    /// Analyses should treat the variable as non-existent if this flag is true. Well-formed modules
    /// should not contain any reference to a deleted global variable.
    pub removed: bool,
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

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("an entry point with the same name and execution model already exists")]
    EntryPointAlreadyExists,
}

/// Container for HIR entities.
///
/// It holds regions, values, operations, types and attributes.
pub struct Module {
    pub types: UniqueArena<TypeData<'static>, Type>,
    pub constants: UniqueArena<ConstantData, Constant>,
    pub ext_inst_sets: UniqueArena<String, ExtInstSet>,
    pub functions: id_arena::Arena<FunctionData, Function>,
    pub globals: id_arena::Arena<GlobalVariableData, GlobalVariable>,
    pub entry_points: id_arena::Arena<EntryPointData, EntryPoint>,
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
        let extended_instruction_sets = UniqueArena::new();
        let functions = Default::default();
        let globals = Default::default();
        let entry_points = Default::default();
        let (ty_unknown, _) = types.insert(TypeData::Unknown);
        let (ty_unit, _) = types.insert(TypeData::Unit);

        Module {
            types,
            constants,
            ext_inst_sets: extended_instruction_sets,
            functions,
            globals,
            entry_points,
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

    pub fn import_extended_instruction_set(&mut self, set: &str) -> ExtInstSet {
        if let Some(id) = self.ext_inst_sets.get_index_of(set) {
            id
        } else {
            self.ext_inst_sets.insert(set.to_string()).0
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

    /// Defines an entry point.
    ///
    /// The shader interface is determined from the static call tree of the function.
    pub fn define_entry_point(
        &mut self,
        function: Function,
        name: &str,
        execution_model: spirv::ExecutionModel,
    ) -> Result<EntryPoint, Error> {
        let shader_interface = self.shader_interface(function);

        // can't have two entry points with the same name and execution model
        // FIXME: remove this check and push that to the caller, or kick it downstream towards validation
        if self
            .entry_points
            .iter()
            .any(|(_, ep)| &ep.name == name && ep.execution_model == execution_model)
        {
            return Err(Error::EntryPointAlreadyExists);
        }

        Ok(self.entry_points.alloc(EntryPointData {
            function,
            name: name.to_string(),
            execution_model,
            shader_interface,
        }))
    }

    /// Returns a proxy for debug-printing a type.
    pub fn debug_type(&self, ty: Type) -> TypeDataDebug {
        TypeDataDebug(self, &self.types[ty])
    }

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

    /// Defines and returns the float type.
    pub fn ty_float(&mut self) -> Type {
        self.define_type(TypeData::Scalar(ScalarType::Float))
    }

    /// Defines and returns the int type.
    pub fn ty_int(&mut self) -> Type {
        self.define_type(TypeData::Scalar(ScalarType::Int))
    }

    /// Defines and returns the int type.
    pub fn ty_uint(&mut self) -> Type {
        self.define_type(TypeData::Scalar(ScalarType::UnsignedInt))
    }

    /// Defines and returns the vec2 type.
    pub fn ty_vec2(&mut self) -> Type {
        self.define_type(TypeData::Vector(ScalarType::Float, 2))
    }
    /// Defines and returns the vec3 type.
    pub fn ty_vec3(&mut self) -> Type {
        self.define_type(TypeData::Vector(ScalarType::Float, 3))
    }
    /// Defines and returns the vec4 type.
    pub fn ty_vec4(&mut self) -> Type {
        self.define_type(TypeData::Vector(ScalarType::Float, 4))
    }

    /// Defines and returns the ivec2 type.
    pub fn ty_ivec2(&mut self) -> Type {
        self.define_type(TypeData::Vector(ScalarType::Int, 2))
    }
    /// Defines and returns the ivec3 type.
    pub fn ty_ivec3(&mut self) -> Type {
        self.define_type(TypeData::Vector(ScalarType::Int, 3))
    }
    /// Defines and returns the ivec4 type.
    pub fn ty_ivec4(&mut self) -> Type {
        self.define_type(TypeData::Vector(ScalarType::Int, 4))
    }

    /// Interns a constant.
    pub fn define_constant(&mut self, c: ConstantData) -> Constant {
        if let Some(id) = self.constants.get_index_of(&c) {
            id
        } else {
            self.constants.insert(c.clone()).0
        }
    }

    /// Defines a global variable.
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

    /// Returns an iterator over all interface variables (global vars with input, output or uniform storage classes).
    ///
    /// Global variables marked as removed are not considered.
    pub fn interface_variables(&self) -> impl Iterator<Item = (GlobalVariable, &'_ GlobalVariableData)> + '_ {
        self.globals.iter().filter(|(_, gdata)| {
            !gdata.removed
                && match gdata.storage_class {
                    StorageClass::Input
                    | StorageClass::Output
                    | StorageClass::Uniform
                    | StorageClass::UniformConstant => true,
                    _ => false,
                }
        })
    }

    /// Returns the shader interface of the specified function.
    ///
    /// This returns the set of all statically accessed interface variables in the static call tree of the function.
    pub fn shader_interface(&self, func: Function) -> Vec<GlobalVariable> {
        let interface: HashSet<_> = self.interface_variables().map(|(g, _)| g).collect();
        let mut accessed = HashSet::new();
        let mut call_stack = vec![func];
        self.shader_interface_rec(func, &interface, &mut accessed, &mut call_stack);
        accessed.into_iter().collect()
    }

    /// Recursive part of `shader_interface`.
    fn shader_interface_rec(
        &self,
        func: Function,
        interface: &HashSet<GlobalVariable>,
        accessed: &mut HashSet<GlobalVariable>,
        call_stack: &mut Vec<Function>,
    ) {
        for (b, block_data) in self.functions[func].blocks.iter() {
            for inst in block_data.instructions.iter() {
                if inst.opcode == Op::FunctionCall {
                    let Operand::FunctionRef(callee) = inst.operands[0] else {
                        panic!("malformed HIR");
                    };
                    // visit function if not recursing
                    if !call_stack.contains(&callee) {
                        call_stack.push(callee);
                        self.shader_interface_rec(callee, interface, accessed, call_stack);
                        call_stack.pop();
                    }
                } else {
                    for op in inst.operands.iter() {
                        if let Operand::GlobalRef(g) = op {
                            accessed.insert(*g);
                        }
                    }
                }
            }
        }
    }

    /// Finds a global variable by name.
    ///
    /// Global variables marked as removed are not considered.
    pub fn find_variable(&self, name: &str) -> Option<(GlobalVariable, &GlobalVariableData)> {
        self.globals
            .iter()
            .find(|(g, gdata)| !gdata.removed && &gdata.name == name)
    }

    /// Finds an interface variable by name.
    ///
    /// Global variables marked as removed are not considered.
    pub fn find_interface_variable(&self, name: &str) -> Option<(GlobalVariable, &GlobalVariableData)> {
        self.interface_variables()
            .find(|(g, gdata)| !gdata.removed && &gdata.name == name)
    }

    /// Flags the specified global variable as removed.
    ///
    /// Note that this doesn't actually remove the global variables from `self.globals` as the
    /// underlying data structure doesn't support it. Instead the `removed` flag of the `GlobalVariableData`
    /// is set.
    ///
    /// This doesn't check that the variable is not referenced. You must check that yourself before
    /// calling the function, otherwise the resulting module will be malformed.
    pub fn remove_global(&mut self, var: GlobalVariable) {
        self.globals[var].removed = true;
    }

    /// Returns the vector length of a vector type, or 1 for scalar types.
    ///
    /// # Panics
    ///
    /// If the type isn't a scalar or vector type.
    pub fn vector_length(&self, ty: Type) -> usize {
        match self.types[ty] {
            TypeData::Scalar(_) => 1,
            TypeData::Vector(_, length) => length as usize,
            _ => panic!("not a vector type"),
        }
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
