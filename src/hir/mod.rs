pub(crate) mod ty;

use crate::hir::ty::{ScalarType, TypeKind};
use crate::id_vec::{Id, IdVec};
use crate::syntax;
use bumpalo::Bump;
use std::collections::HashSet;
use std::fmt;
use ty::Type;
use crate::syntax::SourceId;

pub struct Arena<'hir> {
    pub(crate) dropless: Bump,
    pub(crate) type_kinds: typed_arena::Arena<TypeKind<'hir>>,
}

pub(crate) struct PredefinedTypes<'hir> {
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
}

impl<'hir> PredefinedTypes<'hir> {
    #[rustfmt::skip]
    pub(crate) fn new(arena: &'hir Arena<'hir>, type_set: &mut HashSet<&'hir TypeKind<'hir>>) -> PredefinedTypes<'hir> {
        let ty_void = Type(arena.type_kinds.alloc(TypeKind::Void));
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
        }
    }
}

pub struct HirCtxtInner<'hir> {
    syntax: &'hir syntax::Session,
    arena: &'hir Arena<'hir>,
    items: IdVec<Item<'hir>>,
    types: HashSet<&'hir TypeKind<'hir>>,

    predefined_types: PredefinedTypes<'hir>,
    modules: IdVec<Module>,

}

impl<'hir> fmt::Debug for HirCtxtInner<'hir> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("HirCtxt")
            .field("syntax", &self.syntax)
            .field("items", &self.items)
            .field("types", &self.types)
            .field("modules", &self.modules)
            .finish_non_exhaustive()
    }
}

impl<'hir> HirCtxtInner<'hir> {
    pub fn new(arena: &'hir Arena<'hir>, syntax: &'hir syntax::Session) -> HirCtxtInner<'hir> {
        let mut types = HashSet::default();
        let predefined_types = PredefinedTypes::new(arena, &mut types);
        HirCtxtInner {
            syntax,
            arena,
            items: IdVec::new(),
            types,
            predefined_types,
            modules: IdVec::new(),
        }
    }

    pub fn ty_uint(&self) -> Type<'hir> {
        self.predefined_types.ty_uint
    }

    pub fn ty_int(&self) -> Type<'hir> {
        self.predefined_types.ty_int
    }

    pub fn ty_float(&self) -> Type<'hir> {
        self.predefined_types.ty_float
    }

    pub fn ty_bool(&self) -> Type<'hir> {
        self.predefined_types.ty_bool
    }

    pub fn make_vector(&mut self, elem_ty: ScalarType, count: u8) -> Type<'hir> {
        self.make_type(TypeKind::Vector(elem_ty, count))
    }

    pub fn make_matrix(&mut self, elem_ty: ScalarType, rows: u8, cols: u8) -> Type<'hir> {
        self.make_type(TypeKind::Matrix(elem_ty, rows, cols))
    }

    pub fn make_pointer(&mut self, pointee_ty: Type<'hir>) -> Type<'hir> {
        self.make_type(TypeKind::Pointer(pointee_ty))
    }

    pub fn make_type(&mut self, kind: TypeKind<'hir>) -> Type<'hir> {
        if let Some(kind) = self.types.get(&kind).cloned() {
            Type(kind)
        } else {
            let kind = self.arena.type_kinds.alloc(kind);
            self.types.insert(kind);
            Type(kind)
        }
    }

    /*/// Returns the HIR module for the specified source ID.
    pub fn module(&mut self, source_id: SourceId) -> &Module {

    }*/
}

//--------------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub struct FnDecl<'hir> {
    name: &'hir str,
    args: &'hir [Type<'hir>],
    ret_ty: Type<'hir>,
}


pub type StmtId = Id<Stmt>;
pub type LocalId = Id<Stmt>;

// statements:
//
// assign (place, rvalue)

// Issue with defines: possible to re-use the same temporary ID, which is bad

// rvalue:
// - function call
// - bin expr
// - ...


// places:
// - local
// - local w/ projection
// - temporary:

// - locals are mutable


pub struct Local {

}

#[derive(Clone,Debug)]
pub enum PlaceLocation {
    Local(LocalId),
    //Temporary(TemporaryId),
}

pub struct PlaceAccess {
    location: PlaceLocation,

}

// and store it / read it once
#[derive(Clone,Debug)]
pub struct Place {
    location: PlaceLocation,
}

pub enum Value {

}

#[derive(Clone, Debug)]
pub enum Stmt {
    Assign(Place)

}

#[derive(Clone, Debug)]
pub enum Item<'hir> {
    FnDecl(FnDecl<'hir>),
}

pub type ItemId = Id<Item<'static>>;

#[derive(Clone, Debug)]
pub struct Module {
    source_id: syntax::SourceId,
    items: Vec<ItemId>,
}

// Module interface:
// * inputs (and uniforms)
// * outputs
//  -> more generally, "global variables" with metadata (variability)

//
// "Magic operations" that represent rasterization


#[derive(Copy, Clone, Debug)]
pub struct HirCtxt<'hir>(&'hir HirCtxtInner<'hir>);


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

