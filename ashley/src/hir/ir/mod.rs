mod arena;
mod ty;

use crate::{
    hir::ir::ty::TypeCtxt,
    id_vec::{Id, IdVec},
    syntax,
    syntax::{SourceId, Span},
};
use bumpalo::Bump;
use std::{any::Any, collections::HashSet, fmt, marker::PhantomData};

pub use arena::{Arena, ArenaAlloc, ArenaAny, Interner};
pub use ty::Type;

/*pub(crate) struct PredefinedTypes<'hir> {
    pub(crate) ty_void: Type<'hir>,
    pub(crate) ty_bool: Type<'hir>,
    pub(crate) ty_int: Type<'hir>,
    pub(crate) ty_uint: Type<'hir>,
    pub(crate) ty_float: Type<'hir>,
    pub(crate) ty_double: Type<'hir>,
    pub(crate) ty_sampler: Type<'hir>,
    pub(crate) ty_sampler_shadow: Type<'hir>,
    pub(crate) ty_vec2: Type<'hir>,
    pub(crate) ty_vec3: Type<'hir>,
    pub(crate) ty_vec4: Type<'hir>,
    pub(crate) ty_dvec2: Type<'hir>,
    pub(crate) ty_dvec3: Type<'hir>,
    pub(crate) ty_dvec4: Type<'hir>,
    pub(crate) ty_ivec2: Type<'hir>,
    pub(crate) ty_ivec3: Type<'hir>,
    pub(crate) ty_ivec4: Type<'hir>,
    pub(crate) ty_uvec2: Type<'hir>,
    pub(crate) ty_uvec3: Type<'hir>,
    pub(crate) ty_uvec4: Type<'hir>,
    pub(crate) ty_bvec2: Type<'hir>,
    pub(crate) ty_bvec3: Type<'hir>,
    pub(crate) ty_bvec4: Type<'hir>,
    pub(crate) ty_mat2: Type<'hir>,
    pub(crate) ty_mat3: Type<'hir>,
    pub(crate) ty_mat4: Type<'hir>,
    pub(crate) ty_mat2x3: Type<'hir>,
    pub(crate) ty_mat2x4: Type<'hir>,
    pub(crate) ty_mat3x2: Type<'hir>,
    pub(crate) ty_mat3x4: Type<'hir>,
    pub(crate) ty_mat4x2: Type<'hir>,
    pub(crate) ty_mat4x3: Type<'hir>,
    pub(crate) ty_string: Type<'hir>,
}*/

/*impl<'hir> PredefinedTypes<'hir> {
    #[rustfmt::skip]
    pub(crate) fn new(arena: &'hir Arena<'hir>, type_set: &mut HashSet<&'hir TypeKind<'hir>>) -> PredefinedTypes<'hir> {
        /*let ty_void = Type(arena.type_kinds.alloc(TypeKind::Void));
        let ty_bool = Type(arena.type_kinds.alloc(TypeKind::Scalar(ScalarType::Bool)));
        let ty_int = Type(arena.type_kinds.alloc(TypeKind::Scalar(ScalarType::Int)));
        let ty_uint = Type(arena.type_kinds.alloc(TypeKind::Scalar(ScalarType::UnsignedInt)));
        let ty_float = Type(arena.type_kinds.alloc(TypeKind::Scalar(ScalarType::Float)));
        let ty_double = Type(arena.type_kinds.alloc(TypeKind::Scalar(ScalarType::Double)));
        let ty_sampler = Type(arena.type_kinds.alloc(TypeKind::Sampler));
        let ty_sampler_shadow = Type(arena.type_kinds.alloc(TypeKind::ShadowSampler));
        let ty_vec2 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Float, 2)));
        let ty_vec3 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Float, 3)));
        let ty_vec4 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Float, 4)));
        let ty_dvec2 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Double, 2)));
        let ty_dvec3 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Double, 3)));
        let ty_dvec4 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Double, 4)));
        let ty_ivec2 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Int, 2)));
        let ty_ivec3 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Int, 3)));
        let ty_ivec4 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Int, 4)));
        let ty_uvec2 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::UnsignedInt, 2)));
        let ty_uvec3 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::UnsignedInt, 3)));
        let ty_uvec4 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::UnsignedInt, 4)));
        let ty_bvec2 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Bool, 2)));
        let ty_bvec3 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Bool, 3)));
        let ty_bvec4 = Type(arena.type_kinds.alloc(TypeKind::Vector(ScalarType::Bool, 4)));
        let ty_mat2 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 2, 2)));
        let ty_mat3 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 3, 3)));
        let ty_mat4 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 4, 4)));
        let ty_mat2x3 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 2, 3)));
        let ty_mat2x4 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 2, 4)));
        let ty_mat3x2 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 3, 2)));
        let ty_mat3x4 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 3, 4)));
        let ty_mat4x2 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 4, 2)));
        let ty_mat4x3 = Type(arena.type_kinds.alloc(TypeKind::Matrix(ScalarType::Float, 4, 3)));
        let ty_string = Type(arena.type_kinds.alloc(TypeKind::String));

        type_set.insert(ty_void.0);
        type_set.insert(ty_bool.0);
        type_set.insert(ty_int.0);
        type_set.insert(ty_uint.0);
        type_set.insert(ty_float.0);
        type_set.insert(ty_double.0);
        type_set.insert(ty_sampler.0);
        type_set.insert(ty_sampler_shadow.0);
        type_set.insert(ty_vec2.0);
        type_set.insert(ty_vec3.0);
        type_set.insert(ty_vec4.0);
        type_set.insert(ty_dvec2.0);
        type_set.insert(ty_dvec3.0);
        type_set.insert(ty_dvec4.0);
        type_set.insert(ty_ivec2.0);
        type_set.insert(ty_ivec3.0);
        type_set.insert(ty_ivec4.0);
        type_set.insert(ty_uvec2.0);
        type_set.insert(ty_uvec3.0);
        type_set.insert(ty_uvec4.0);
        type_set.insert(ty_bvec2.0);
        type_set.insert(ty_bvec3.0);
        type_set.insert(ty_bvec4.0);
        type_set.insert(ty_mat2.0);
        type_set.insert(ty_mat3.0);
        type_set.insert(ty_mat4.0);
        type_set.insert(ty_mat2x3.0);
        type_set.insert(ty_mat2x4.0);
        type_set.insert(ty_mat3x2.0);
        type_set.insert(ty_mat3x4.0);
        type_set.insert(ty_mat4x2.0);
        type_set.insert(ty_mat4x3.0);
        type_set.insert(ty_string.0);

        PredefinedTypes {
            ty_void,
            ty_bool,
            ty_int,
            ty_uint,
            ty_float,
            ty_double,
            ty_sampler,
            ty_sampler_shadow,
            ty_vec2,
            ty_vec3,
            ty_vec4,
            ty_dvec2,
            ty_dvec3,
            ty_dvec4,
            ty_ivec2,
            ty_ivec3,
            ty_ivec4,
            ty_uvec2,
            ty_uvec3,
            ty_uvec4,
            ty_bvec2,
            ty_bvec3,
            ty_bvec4,
            ty_mat2,
            ty_mat3,
            ty_mat4,
            ty_mat2x3,
            ty_mat2x4,
            ty_mat3x2,
            ty_mat3x4,
            ty_mat4x2,
            ty_mat4x3,
            ty_string
        }*/
    }
}*/

pub struct HirCtxt<'hir> {
    arena: &'hir Arena,
    ty_ctxt: TypeCtxt<'hir>,
    values: IdVec<Value>,
    blocks: IdVec<Block<'hir>>,
    /// Top-level instructions.
    instrs: Vec<Instr<'hir>>,
}

pub type ValueId = Id<Value>;
pub type BlockId = Id<Block<'static>>;

impl<'hir> fmt::Debug for HirCtxt<'hir> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("HirCtxt")
            //.field("types", &self.types)
            .field("values", &self.values)
            .field("blocks", &self.blocks)
            .finish_non_exhaustive()
    }
}

impl<'hir> HirCtxt<'hir> {
    pub fn new(arena: &'hir Arena) -> HirCtxt<'hir> {
        let ty_ctxt = TypeCtxt::new();
        HirCtxt {
            arena,
            ty_ctxt,
            values: IdVec::new(),
            blocks: IdVec::new(),
            instrs: vec![],
        }
    }
}

//--------------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Block<'hir> {
    instrs: Vec<Instr<'hir>>,
}

#[derive(Clone, Debug)]
pub struct Value;

type DialectId = u16;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Opcode(pub DialectId, pub u16);

impl Opcode {
    pub fn dialect(&self) -> DialectId {
        self.0
    }

    pub fn opcode(&self) -> u16 {
        self.1
    }
}

#[derive(Clone, Debug)]
pub struct Instr<'hir> {
    pub opcode: Opcode,
    pub operands: &'hir [ValueId],
    pub results: &'hir [ValueId],
    //pub attributes: &'hir [&'hir dyn Attribute],
    pub blocks: Vec<Block<'hir>>,
    pub location: Span,
}

/*instruction!(
    IAdd <0, "add"> {lhs, rhs} -> {0} attr {} region {}
    Func<0, "func">
);

// turns into

pub struct IAdd<'hir> {
    instr: &'hir Instr<'hir>,
}

impl<'hir> IAdd<'hir> {
    pub fn lhs(&self) -> ValueId {
        self.instr.operands[0]
    }

    pub fn rhs(&self) -> ValueId {
        self.instr.operands[1]
    }
}

#[derive(Default)]
pub struct IAddBuilder {
    location: Span,
    lhs: ValueId,
    rhs: ValueId,
}

impl IAddBuilder {
    pub fn lhs(mut self, value: ValueId) -> Self {

    }

    pub fn rhs(mut self, value: ValueId) -> Self {

        let block_id = ...;
        let instr = ...;

        let lhs = ctx.const_val(0);
        let rhs = ctx.const_val(1);

        IAdd::build(&ctx).lhs(...).rhs(...).location().append(block_id);

    }

    pub fn append(mut self, block: BlockId) -> ValueId {

    }
}

pub trait InstrDesc<'hir> {
    const OPCODE: Opcode;
    fn instr(&self) -> &'hir Instr<'hir>;
}

impl<'hir> InstrDesc<'hir> for IAdd<'hir> {
    const OPCODE: Opcode = Opcode(0,0);

    fn instr(&self) -> &'hir Instr<'hir> {
        self.instr
    }
}

*/

// Goals:
// * easy to modify / patch / transform functions
// * examples of transformations:
//      * lowering closures / monomorphization (patch function definitions? clone functions, patch call sites)
//           * can also do the transformation in the backend
//      * transform snippets into pure functions
//      * constant folding / uniform folding (replace expressions, remove statements?)
//      * instrumentation (insert probes to dump the value of a variable in a shader) (insert statements, insert shader interfaces)
//           -> HIR expressions have debugging info? markers? metadata?
//
// Take note of the execution contexts:
// * compute shaders with access to shared memory in the workgroup
// * subgroup "voting" operations
//
// Modules and individual functions can have execution contexts, e.g. run in compute, requesting a specific workgroup size.
// In this case, it's up to the host "composer" to translate that to draws/dispatches.
//
// Eventually, individual *values* within a function may have different execution contexts/variabilities
//  -> within a function, have both vertex & fragment values
//
// Can go even crazier and represent whole graphs (rendering pipelines) with this language.
//  -> advantages?

// Types:
// - basic data types, usable inside and outside of shaders
// - image types (float/int image 1D/2D/3D/Cube)
//      - can be sampled inside shaders, or written to in some instances

// Operations:
// - "stages" shader stages w/ inputs and outputs as global variables
//      - defines a scope
//      - contain "function" ops
//      - contain global variables only visible in scope
//      - can access functions in the parent scope
// - functions
// - one function is the entry point
//
// - Fixed-function ops:
//      - rasterizer config
//      - gpu_rasterize (rasterizer cfg, vertex attrib cfg

// Lowering passes:
// -

// Optimization passes:
// Q:

#[cfg(test)]
mod tests {
    use crate::hir::ir::{HirCtxt, Instr, Value};

    mod ins {
        use crate::hir::ir::Opcode;
        pub const OP_UNDEF: Opcode = Opcode(0, 0);
        pub const OP_VARIABLE: Opcode = Opcode(0, 1);
    }
    use crate::syntax::Span;
    use ins::*;

    #[test]
    fn basic_hir() {
        /*let arena = Arena::new();
        let mut ctxt = HirCtxt::new(&arena);

        let r_undef = ctxt.values.push(Value);
        ctxt.instrs.push(Instr {
            opcode: OP_UNDEF,
            operands: &[],
            results: &[r_undef],
            //attributes: &[],
            blocks: vec![],
            location: Default::default()
        });

        //let undef_ty =

        let r_var = ctxt.values.push(Value);
        ctxt.instrs.push(Instr {
            opcode: OP_VARIABLE,
            operands: &[],
            results: &[],
            //attributes: &[],
            blocks: vec![],
            location: Default::default()
        });*/
    }
}
