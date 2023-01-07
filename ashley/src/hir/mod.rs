mod builder;
mod constant;
mod list;
pub mod print;
pub mod types;
mod visit;

use crate::{
    diagnostic::{DiagnosticBuilder, Diagnostics, SourceLocation},
    hir::{
        constant::ConstantImpl,
        list::{List, ListNode},
    },
    utils::interner::Interner,
};
use bumpalo::Bump;
use ordered_float::OrderedFloat;
use smallvec::SmallVec;
use std::{
    fmt,
    hash::Hash,
    num::NonZeroU32,
    ops::{Bound, Deref, DerefMut, RangeBounds},
};

pub use self::{
    builder::FunctionBuilder,
    types::TypeImpl,
    visit::{IRVisitable, Visit},
};

//--------------------------------------------------------------------------------------------------

/// Byte-span source location attribute.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Location {
    /// An actual location in source code.
    Source(SourceLocation),
    /// Represents an unknown location.
    Unknown,
}

impl Location {
    pub fn to_source_location(&self) -> Option<SourceLocation> {
        match self {
            Location::Source(loc) => Some(*loc),
            Location::Unknown => None,
        }
    }
}

impl Default for Location {
    fn default() -> Self {
        Location::Unknown
    }
}

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
    pub struct ValueId;

    /// Handle to a HIR function.
    pub struct FunctionId;

    /// Interned handle to a HIR type.
    pub struct Type;

    /// Interned handle to a HIR constant.
    pub struct Constant;
}

impl ValueId {
    /// Returns the type of the value.
    pub fn ty<'ir>(&self, ctxt: &HirCtxt<'ir>) -> Type {
        ctxt.values[*self].ty
    }
}

impl OperationId {
    /// Returns a pointer to the underlying op.
    pub fn op(&self, ctxt: &HirCtxt) -> &Op {
        &ctxt.ops[*self].data.op
    }

    /// Returns a mutable pointer to the underlying op.
    pub fn op_mut<'a, 'ir>(&self, ctxt: &'a mut HirCtxt<'ir>) -> &'a mut Op<'ir> {
        &mut ctxt.ops[*self].data.op
    }
}

impl RegionId {
    /// Returns this regions's arguments.
    pub fn arguments<'ir>(&self, ctxt: &HirCtxt<'ir>) -> &'ir [ValueId] {
        &ctxt.regions[*self].arguments
    }
}

//--------------------------------------------------------------------------------------------------

/// Operation linked list nodes.
pub type Operation<'ir> = ListNode<OperationId, InstructionData<'ir>>;

pub enum Operand {
    Constant(Constant),
    ExtInst(u32),
    Function(FunctionId),
    Value(ValueId),
    Type(Type),
}

#[derive(Copy, Clone, Debug)]
pub struct InstructionResult {
    pub value: ValueId,
    pub ty: Type,
}

/// Data associated to an operation.
#[derive(Copy, Clone, Debug)]
pub struct InstructionData<'ir> {
    pub opcode: u32,
    pub result: Option<InstructionResult>,
    pub operands: SmallVec<[Operand; 3]>,
    pub function: FunctionId,
}

/// The kind of value represented in a `Value`.
#[derive(Clone, Debug)]
pub enum ValueKind {
    /// Operation result (operation ID, result index).
    OpResult(OperationId, u32),
    /// Region argument (region ID, argument index).
    Argument(FunctionId, u32),
}

/// HIR value.
///
/// Represents an immutable value in the HIR.
/// For now, there are two kinds of represented values: operation results (the most common), and region arguments.
#[derive(Debug)]
pub struct Value {
    /// The type of the value.
    ty: Type,
    /// Value kind.
    kind: ValueKind,
}

impl Value {
    /// Creates a new value representing the result of an operation.
    ///
    /// # Arguments
    /// * op the operation producing the result
    /// * index the index of the result
    /// * ty value type
    pub fn result(op: OperationId, index: u32, ty: Type) -> Value {
        Value {
            ty,
            kind: ValueKind::OpResult(op, index),
        }
    }

    /// Creates a new value representing a region argument.
    ///
    /// # Arguments
    /// * region the region owning the argument
    /// * index the index of the region argument
    /// * ty value type
    pub fn argument(f: FunctionId, index: u32, ty: Type) -> Value {
        Value {
            ty,
            kind: ValueKind::Argument(f, index),
        }
    }
}

/// Arena allocator used by `HirCtxt`s.
///
/// The same allocator can be shared by multiple `HirCtxt`s, but usually one is created for each `HirCtxt`.
/// It is not owned by `HirCtxt`s themselves because it would lead to self-referential lifetimes.
pub struct HirArena(pub(crate) Bump);

impl HirArena {
    /// Creates a new `HirArena`.
    pub fn new() -> HirArena {
        HirArena(Bump::new())
    }
}

/// Operation cursor.
#[derive(Copy, Clone)]
pub enum Cursor {
    /// Insert at the end.
    End(RegionId),
    /// Insert before the specified operation.
    Before(RegionId, OperationId),
}

/// Function argument
#[derive(Copy, Clone, Debug)]
pub struct FunctionParameter<'a> {
    /// Name of the parameter, for debugging purposes only
    pub name: &'a str,
    /// Type of the parameter
    pub ty: Type,
}

/// HIR functions.
#[derive(Debug)]
pub struct Function<'a> {
    pub parameters: Vec<FunctionParameter<'a>>,
    pub arguments: Vec<ValueId>,
    pub return_ty: Type,
    pub name: &'a str,
    /// Ordered list of operations in the region.
    pub ops: List<OperationId, InstructionData<'a>>,
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
pub struct InterfaceVariableData<'a> {
    pub name: &'a str,
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
pub struct HirCtxt<'hir> {
    pub(crate) arena: &'hir HirArena,
    pub(crate) types_interner: Interner<TypeImpl, Type>,
    pub(crate) constants_interner: Interner<ConstantImpl, Constant>,
    pub types: id_arena::Arena<TypeImpl, Type>,
    pub constants: id_arena::Arena<ConstantImpl, Constant>,
    pub functions: id_arena::Arena<Function<'hir>, FunctionId>,
    pub interface: id_arena::Arena<InterfaceVariableData<'hir>, InterfaceVariable>,
    pub values: id_arena::Arena<Value, ValueId>,
    pub ops: id_arena::Arena<Operation<'hir>, OperationId>,

    ty_unknown: Type,
    ty_unit: Type,
}

impl<'hir> fmt::Debug for HirCtxt<'hir> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("HirCtxt")
            .field("values", &self.values)
            .field("regions", &self.regions)
            .field("ops", &self.ops)
            .finish_non_exhaustive()
    }
}

impl<'hir> HirCtxt<'hir> {
    /// Creates a new HirCtxt, that uses the specified arena allocator.
    pub fn new(arena: &'hir HirArena) -> HirCtxt<'hir> {
        let mut types_interner = Interner::new();
        let constants_interner = Interner::new();
        let mut types = Default::default();
        let constants = Default::default();
        let functions = Default::default();
        let programs = Default::default();
        let pipelines = Default::default();
        let values = Default::default();
        let regions = Default::default();
        let ops = Default::default();
        let ty_unknown = types_interner
            .intern(TypeImpl::Unknown, || types.alloc(TypeImpl::Unknown))
            .0;
        let ty_unit = types_interner.intern(TypeImpl::Unit, || types.alloc(TypeImpl::Unit)).0;

        HirCtxt {
            arena,
            types_interner,
            constants_interner,
            types,
            constants,
            functions,
            interface: (),
            values,
            ops,
            ty_unknown,
            ty_unit,
        }
    }

    /// Allocates data into the HIR arena.
    pub fn alloc<T>(&mut self, src: T) -> &'hir T {
        self.arena.0.alloc(src)
    }

    /// Allocates a string into the HIR arena.
    pub fn alloc_str(&mut self, str: &str) -> &'hir str {
        self.arena.0.alloc_str(str)
    }

    /// Allocates data into the HIR arena.
    pub fn alloc_slice_copy<T: Copy>(&mut self, src: &[T]) -> &'hir [T] {
        self.arena.0.alloc_slice_copy(src)
    }

    /// Interns a type.
    pub fn intern_type<T>(&mut self, ty: TypeImpl) -> Type {
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
    pub fn intern_constant<T>(&mut self, v: ConstantImpl) -> Constant {
        self.constants_interner
            .intern(v.clone(), || self.constants.alloc(v.clone()))
            .0
    }

    /// Adds a program to this context.
    pub fn add_program(&mut self, program: Program) -> ProgramId {
        self.programs.alloc(program)
    }

    /// Adds a function to this context.
    pub fn add_function(&mut self, function: Function) -> FunctionId {
        self.functions.alloc(function)
    }

    /// Creates a new empty region with no parent and no arguments.
    pub fn create_region(&mut self) -> RegionId {
        self.regions.alloc(RegionData {
            arguments: Default::default(),
            ops: Default::default(),
        })
    }
}

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::hir::{print_hir_region_html, HirArena, HirCtxt};
    use std::{fs::File, io::Write, path::PathBuf};

    fn html_output_file_path(title: &str) -> PathBuf {
        const OUT_DIR: &str = "tests/out";
        let root = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(root).join(OUT_DIR).join(format!("{title}.html"))
    }
}
