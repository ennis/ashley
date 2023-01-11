mod builder;
mod constant;
mod list;
pub mod print;
mod transform;
pub mod types;
mod visit;

use crate::{
    diagnostic::{DiagnosticBuilder, Diagnostics, SourceLocation},
    hir::list::{List, ListNode},
    utils::interner::Interner,
};
use bumpalo::Bump;
use ordered_float::OrderedFloat;
use rspirv::spirv;
use smallvec::SmallVec;
use std::{
    fmt,
    hash::Hash,
    num::NonZeroU32,
    ops::{Bound, Deref, DerefMut, RangeBounds},
};

pub use self::{builder::FunctionBuilder, constant::ConstantData, types::TypeData, visit::Visit};

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
    /// Handle to a HIR operation.
    pub struct OperationId;

    /// Handle to a HIR value.
    pub struct Value;

    /// Handle to a HIR function.
    pub struct Function;

    /// Interned handle to a HIR type.
    pub struct Type;

    /// Interned handle to a HIR constant.
    pub struct Constant;

    /// Interned handle to a HIR constant.
    pub struct InterfaceVariable;

    /// Handle to a function basic block.
    pub struct Block;
}

impl Value {
    /// Returns the type of the value.
    pub fn ty(&self, module: &Module) -> Type {
        module.values[self.0].ty
    }
}

//--------------------------------------------------------------------------------------------------

/// Operation linked list nodes.
pub type Operation = ListNode<OperationId, InstructionData>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Operand {
    Constant(Constant),
    ExtInst(u32),
    Function(Function),
    Value(Value),
    Block(Block),
    Type(Type),
}

/// Data associated to an operation.
#[derive(Copy, Clone, Debug)]
pub struct InstructionData {
    pub opcode: u32,
    pub result: Option<Value>,
    pub operands: SmallVec<[Operand; 3]>,
}

/// The kind of value represented in a `Value`.
#[derive(Clone, Debug)]
pub enum ValueKind {
    /// Operation result (operation ID, result index).
    OpResult(OperationId, u32),
    /// Region argument (region ID, argument index).
    Argument(Function, u32),
}

/// SSA value data.
#[derive(Debug)]
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
#[derive(Copy, Clone, Debug)]
pub struct FunctionParameter {
    /// Name of the parameter, for debugging purposes only
    pub name: String,
    /// Type of the parameter
    pub ty: Type,
}

/// HIR functions.
#[derive(Debug)]
pub struct FunctionData {
    pub parameters: Vec<FunctionParameter>,
    pub arguments: Vec<Value>,
    pub return_ty: Type,
    pub name: String,
    pub linkage: Option<spirv::LinkageType>,
    pub values: id_arena::Arena<ValueData, Value>,
    pub blocks: id_arena::Arena<BlockData, Block>,
    pub entry_block: Block,
}

#[derive(Copy, Clone, Debug)]
pub enum IntegerLiteral {
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
}

/// Block terminator instruction.
#[derive(Copy, Clone, Debug)]
pub enum TerminatingInstruction {
    Branch(Block),
    BranchConditional {
        condition: Value,
        true_block: Block,
        false_block: Block,
    },
    Switch {
        selector: Value,
        default: Block,
        target: Vec<(IntegerLiteral, Block)>,
    },
    Return,
    ReturnValue(Value),
    Unreachable,
    TerminateInvocation,
}

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

/// Describes a program input value.
pub struct InterfaceVariableData {
    pub name: String,
    pub ty: Type,
}

pub enum ProgramKind {
    Generic,
    Vertex,
    Fragment,
    Compute,
    Rasterize,
}

/// Container for HIR entities.
///
/// It holds regions, values, operations, types and attributes.
pub struct Module {
    pub(crate) types_interner: Interner<TypeData, Type>,
    pub(crate) constants_interner: Interner<ConstantData, Constant>,
    pub types: id_arena::Arena<TypeData, Type>,
    pub constants: id_arena::Arena<ConstantData, Constant>,
    pub functions: id_arena::Arena<FunctionData, Function>,
    pub interface: id_arena::Arena<InterfaceVariableData, InterfaceVariable>,
    pub ops: id_arena::Arena<Operation, OperationId>,
    ty_unknown: Type,
    ty_unit: Type,
}

impl fmt::Debug for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("HirCtxt")
            .field("values", &self.values)
            .field("regions", &self.regions)
            .field("ops", &self.ops)
            .finish_non_exhaustive()
    }
}

impl Module {
    pub fn new() -> Module {
        let mut types_interner = Interner::new();
        let constants_interner = Interner::new();
        let mut types = Default::default();
        let constants = Default::default();
        let functions = Default::default();
        let interface = Default::default();
        let ops = Default::default();
        let ty_unknown = types_interner
            .intern(TypeData::Unknown, || types.alloc(TypeData::Unknown))
            .0;
        let ty_unit = types_interner.intern(TypeData::Unit, || types.alloc(TypeData::Unit)).0;

        Module {
            types_interner,
            constants_interner,
            types,
            constants,
            functions,
            interface,
            ops,
            ty_unknown,
            ty_unit,
        }
    }

    /// Interns a type.
    pub fn define_type<T>(&mut self, ty: TypeData) -> Type {
        self.types_interner
            .intern(ty.clone(), || self.types.alloc(ty.clone()))
            .0
    }

    /// Returns the unknown type.
    pub fn ty_unknown(&self) -> Type {
        self.ty_unknown
    }

    /// Returns the unknown type.
    pub fn ty_unit(&self) -> Type {
        self.ty_unit
    }

    /// Interns a constant.
    pub fn define_constant<T>(&mut self, v: ConstantData) -> Constant {
        self.constants_interner
            .intern(v.clone(), || self.constants.alloc(v.clone()))
            .0
    }

    /// Adds a function to this context.
    pub fn add_function(&mut self, function: FunctionData) -> Function {
        self.functions.alloc(function)
    }
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
