mod attr;
mod builder;
pub mod constraint;
mod list;
mod print;
mod visit;

use crate::{
    diagnostic::{DiagnosticBuilder, Diagnostics},
    hir::{
        attr::AttrInterner,
        list::{List, ListNode},
    },
};
use bumpalo::Bump;
use ordered_float::OrderedFloat;
use std::{
    fmt,
    hash::Hash,
    num::NonZeroU32,
    ops::{Bound, Deref, DerefMut, RangeBounds},
};

pub use self::{
    attr::{Attr, AttributeBase, BooleanAttr, FloatAttr, IntegerAttr, Location, StringAttr},
    builder::{build_operation, operation_format, operation_formats, Builder},
    constraint::{operation_constraint, AttrConstraint, OperationConstraint, RegionConstraint, ValueConstraint, MatchCtxt},
    print::{print_hir_region_html, write_hir_html_file, write_ir, IRPrintable, IRPrinter, IRSyntaxElem, ToIRSyntax},
    visit::{IRVisitable, Visit},
};

//--------------------------------------------------------------------------------------------------

/*/// Definition of an operation.
///
/// Describes the format of an operation, its opcode, and printing
/// Those are statically registered with the TODO macro.
pub struct OperationDef {
    pub mnemonic: &'static str,
    pub opcode: Opcode,
}

impl OperationDef {
    pub const fn new(opcode: Opcode, mnemonic: &'static str) -> OperationDef {
        OperationDef { mnemonic, opcode }
    }
}

inventory::collect!(OperationDef);*/

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

id_types! {
    /// Region index.
    pub struct RegionId;

    /// Operation index;
    pub struct OperationId;

    /// Value index.
    pub struct ValueId;
}

impl ValueId {
    /// Tries to match a pattern on this value.
    pub fn pmatch<'ir, T>(&self, ctxt: &HirCtxt<'ir>) -> Option<T>
    where
        T: ValueConstraint<'ir>,
    {
        let mcx = MatchCtxt::new(ctxt);
        T::try_match(&mcx, *self)
    }

    /// Returns the type of the value.
    pub fn ty<'ir>(&self, ctxt: &HirCtxt<'ir>) -> Attr<'ir> {
        ctxt.values[*self].ty
    }
}

impl OperationId {
    /// Tries to match a pattern on this operation.
    pub fn pmatch<'ir, T>(&self, ctxt: &HirCtxt<'ir>) -> Option<T>
    where
        T: OperationConstraint<'ir>,
    {
        let mcx = MatchCtxt::new(ctxt);
        T::try_match(&mcx, *self)
    }

    /// Returns the operation mnemonic string.
    pub fn mnemonic(&self, ctxt: &HirCtxt) -> &'static str {
        ctxt.ops[*self].data.opcode
    }

    /// Returns the first operand.
    pub fn lhs(&self, ctxt: &HirCtxt) -> Option<ValueId> {
        self.operand(ctxt, 0)
    }

    /// Returns the second operand.
    pub fn rhs(&self, ctxt: &HirCtxt) -> Option<ValueId> {
        self.operand(ctxt, 1)
    }

    /// Returns an operand by index.
    pub fn operand(&self, ctxt: &HirCtxt, index: usize) -> Option<ValueId> {
        ctxt.ops[*self].data.operands.get(index).cloned()
    }

    /// Returns this operation's operands.
    pub fn operands<'ir>(&self, ctxt: &HirCtxt<'ir>) -> &'ir [ValueId] {
        ctxt.ops[*self].data.operands
    }

    /// Returns a result by index.
    pub fn result(&self, ctxt: &HirCtxt, index: usize) -> Option<ValueId> {
        ctxt.ops[*self].data.results.get(index).cloned()
    }

    /// Returns this operation's results.
    pub fn results<'ir>(&self, ctxt: &HirCtxt<'ir>) -> &'ir [ValueId] {
        ctxt.ops[*self].data.results
    }

    /// Returns this operation's first region
    pub fn regions<'ir>(&self, ctxt: &HirCtxt<'ir>) -> &'ir [RegionId] {
        ctxt.ops[*self].data.regions
    }

    /// Returns a range of operands.
    pub fn operand_range<'ir>(&self, ctxt: &HirCtxt<'ir>, range: impl RangeBounds<usize>) -> &'ir [ValueId] {
        let operands = ctxt.ops[*self].data.operands;
        let start = range.start_bound().cloned();
        let end = range.end_bound().cloned();
        &operands[(start, end)]
    }

    /// Returns a range of results.
    pub fn result_range<'ir>(&self, ctxt: &HirCtxt<'ir>, range: impl RangeBounds<usize>) -> &'ir [ValueId] {
        let results = ctxt.ops[*self].data.results;
        let start = range.start_bound().cloned();
        let end = range.end_bound().cloned();
        &results[(start, end)]
    }
}

impl RegionId {
    /// Tries to match a pattern on this region.
    pub fn pmatch<'ir, T>(&self, ctxt: &HirCtxt<'ir>) -> Option<T>
    where
        T: RegionConstraint<'ir>,
    {
        let mcx = MatchCtxt::new(ctxt);
        T::try_match(&mcx, *self)
    }

    /// Returns this regions's arguments.
    pub fn arguments<'ir>(&self, ctxt: &HirCtxt<'ir>) -> &'ir [ValueId] {
        ctxt.regions[*self].arguments
    }
}

//--------------------------------------------------------------------------------------------------

/// Region definition.
#[derive(Clone, Debug)]
pub struct RegionData<'hir> {
    /// Values representing the region arguments.
    pub arguments: &'hir [ValueId],
    /// Ordered list of operations in the region.
    pub ops: List<OperationId, OperationData<'hir>>,
}

/// The kind of value represented in a `Value`.
#[derive(Clone, Debug)]
pub enum ValueKind {
    /// Operation result (operation ID, result index).
    OpResult(OperationId, u32),
    /// Region argument (region ID, argument index).
    RegionArg(RegionId, u32),
}

/// HIR value.
///
/// Represents an immutable value in the HIR.
/// For now, there are two kinds of represented values: operation results (the most common), and region arguments.
#[derive(Debug)]
pub struct Value<'hir> {
    /// The type of the value.
    ty: Attr<'hir>,
    /// Value kind.
    kind: ValueKind,
}

impl<'hir> Value<'hir> {
    /// Creates a new value representing the result of an operation.
    ///
    /// # Arguments
    /// * op the operation producing the result
    /// * index the index of the result
    /// * ty value type
    pub fn result(op: OperationId, index: u32, ty: Attr<'hir>) -> Value<'hir> {
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
    pub fn region_argument(reg: RegionId, index: u32, ty: Attr<'hir>) -> Value<'hir> {
        Value {
            ty,
            kind: ValueKind::RegionArg(reg, index),
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

struct RegionCreateInfo<'hir> {
    arguments: Vec<Attr<'hir>>,
}

/// Builder object used to initialize a new operation.
pub struct OperationCreateInfo<'hir> {
    opcode: &'static str,
    attributes: Vec<Attr<'hir>>,
    operands: Vec<ValueId>,
    result_types: Vec<Attr<'hir>>,
    regions: Vec<RegionCreateInfo<'hir>>,
    location: Location,
}

impl<'hir> OperationCreateInfo<'hir> {
    /// Creates a new `OperationCreateInfo` object.
    ///
    /// The object initially has zero attributes, operands, results and regions.
    pub fn new(opcode: &'static str, location: Location) -> OperationCreateInfo<'hir> {
        OperationCreateInfo {
            opcode,
            attributes: vec![],
            operands: vec![],
            result_types: vec![],
            regions: vec![],
            location,
        }
    }

    /// Adds attributes to the created operation.
    pub fn add_attributes(&mut self, attrs: impl AsRef<[Attr<'hir>]>) {
        self.attributes.extend_from_slice(attrs.as_ref())
    }

    /// Adds operands to the created operation.
    pub fn add_operands(&mut self, operands: impl AsRef<[ValueId]>) {
        self.operands.extend_from_slice(operands.as_ref())
    }

    /// Adds operation result types.
    ///
    /// This will cause additional result values to be created for the operation.
    pub fn add_result_types(&mut self, types: impl AsRef<[Attr<'hir>]>) {
        self.result_types.extend_from_slice(types.as_ref());
    }

    /// Adds a new region to the operation.
    pub fn add_region(&mut self, arg_types: Vec<Attr<'hir>>) {
        self.regions.push(RegionCreateInfo { arguments: arg_types })
    }
}

/// Container for HIR entities.
///
/// It holds regions, values, operations, types and attributes.
pub struct HirCtxt<'hir> {
    pub(crate) arena: &'hir HirArena,
    pub(crate) interner: AttrInterner<'hir>,
    pub(crate) attributes: Vec<Attr<'hir>>,
    pub values: id_arena::Arena<Value<'hir>, ValueId>,
    pub regions: id_arena::Arena<RegionData<'hir>, RegionId>,
    pub ops: id_arena::Arena<Operation<'hir>, OperationId>,
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
        /*let mut op_def_map = HashMap::new();
        for op_def in inventory::iter::<OperationDef> {
            op_def_map.insert(op_def.opcode, op_def);
        }*/

        HirCtxt {
            arena,
            interner: AttrInterner::new(),
            attributes: vec![],
            values: id_arena::Arena::new(),
            regions: id_arena::Arena::new(),
            ops: id_arena::Arena::new(),
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

    /// Interns an attribute.
    fn intern_attr_inner<T>(&mut self, attr: T) -> &'hir T
    where
        T: AttributeBase<'hir> + Eq + Hash,
    {
        let (attr, inserted) = self.interner.intern(self.arena, attr);
        if inserted {
            self.attributes.push(Attr(attr));
        }
        attr
    }

    /// Interns an attribute.
    pub fn intern_attr<T>(&mut self, value: T) -> Attr<'hir, T>
    where
        T: AttributeBase<'hir> + Eq + Hash,
    {
        Attr(self.intern_attr_inner(value))
    }

    /// Creates a new empty region with no parent and no arguments.
    pub fn create_region(&mut self) -> RegionId {
        let region = self.regions.alloc(RegionData {
            arguments: &[],
            ops: Default::default(),
        });
        region
    }

    /*/// Creates and appends a new subregion under the specified operation.
    pub fn create_subregion(
        &mut self,
        parent_op: OperationId,
        arg_types: &[Attr<'hir>],
    ) -> (RegionId, &'hir [ValueId]) {
        // Allocate values for the subregion
        let region_id = self.regions.next_id();
        let mut arguments = vec![];
        for (i, ty) in arg_types.iter().enumerate() {
            arguments.push(self.values.alloc(Value::region_argument(region_id, i as u32, *ty)));
        }
        let arguments = self.alloc_slice_copy(&arguments);
        let region = self.regions.alloc(Region::new(RegionData {
            arguments,
            ops: Default::default(),
        }));
        self.ops[parent_op].data.regions.append(region, &mut self.regions);
        (region, arguments)
    }*/

    /// A short-hand method to build an operation.
    pub fn create_operation(
        &mut self,
        at: Cursor,
        create_info: OperationCreateInfo<'hir>,
    ) -> (OperationId, &'hir [ValueId]) {
        let id = self.ops.next_id();
        let attributes = self.arena.0.alloc_slice_fill_iter(create_info.attributes);
        let operands = self.arena.0.alloc_slice_fill_iter(create_info.operands);
        let results = self
            .arena
            .0
            .alloc_slice_fill_copy(create_info.result_types.len(), ValueId::from_index(0));
        // create result values
        for i in 0..create_info.result_types.len() {
            results[i] = self
                .values
                .alloc(Value::result(id, i as u32, create_info.result_types[i]));
        }

        let regions = self
            .arena
            .0
            .alloc_slice_fill_copy(create_info.regions.len(), RegionId::from_index(0));
        for (i, region) in create_info.regions.iter().enumerate() {
            let region_id = self.regions.next_id();
            let n_args = region.arguments.len();
            let arguments = self.arena.0.alloc_slice_fill_copy(n_args, ValueId::from_index(0));
            for (i_arg, arg) in region.arguments.iter().enumerate() {
                arguments[i] = self
                    .values
                    .alloc(Value::region_argument(region_id, i_arg as u32, arg.clone()));
            }
            regions[i] = self.regions.alloc(RegionData {
                arguments,
                ops: List::default(),
            });
        }

        let data = OperationData {
            opcode: create_info.opcode,
            attributes,
            operands,
            results,
            regions,
            location: create_info.location,
        };
        let op = self.ops.alloc(Operation::new(data));
        assert_eq!(op, id);
        match at {
            Cursor::End(region) => {
                self.regions[region].ops.append(op, &mut self.ops);
            }
            Cursor::Before(region, next) => {
                self.regions[region].ops.insert_before(op, next, &mut self.ops);
            }
        }

        (op, results)
    }
}

pub type Operation<'hir> = ListNode<OperationId, OperationData<'hir>>;

/// Definition of an operation.
#[derive(Copy, Clone, Debug)]
pub struct OperationData<'hir> {
    // 8 + 16 + 16 + 16 + 16 + 24 = 96 bytes per instruction + 8 bytes for links
    pub opcode: &'static str,
    /// Attributes.
    pub attributes: &'hir [Attr<'hir>],
    /// Operands.
    pub operands: &'hir [ValueId],
    /// Results of the operation.
    pub results: &'hir [ValueId],
    /// Child regions.
    pub regions: &'hir [RegionId],
    pub location: Location,
}

impl<'hir> Default for OperationData<'hir> {
    fn default() -> Self {
        OperationData {
            opcode: "undef",
            attributes: &[],
            operands: &[],
            results: &[],
            regions: &[],
            location: Location::default(),
        }
    }
}

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::{
        dialect::base::{FunctionType, ScalarType, ScalarTypeKind},
        hir::{print_hir_region_html, HirArena, HirCtxt},
    };
    use std::{fs::File, io::Write, path::PathBuf};

    fn html_output_file_path(title: &str) -> PathBuf {
        const OUT_DIR: &str = "tests/out";
        let root = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(root).join(OUT_DIR).join(format!("{title}.html"))
    }

    fn attr_lifetime_test() {}
}

// entry point:
// gets ctxt: HitCtxt<'hir>, operation ID
//
// ctxt[op].data...
// ctxt[r].data...
// ctxt[v]
// ctxt.remove(...)
//
//

// e.g.
//
// scf.select  <condition>, <region arguments ...> { true_region } else { false_region }
// scf.loop <condition>, <region arguments ...>
//
// scf.loop %v4 (%r1 <- %v0, %r2 <- %v1) {
//
// }
//
// scf.if %v74 (
//      then { scf.yield v75 }
//      else { (v77, v78) -> ... }
// )
//
// Region inputs
// Issue: given an SSA value id, go back to the instruction that produced it? (def-use)

// Block == Control region
// Inside blocks: operations that define a sub-control region:
// * select (two subregions)
// * loop (one subregion)
//
// RVSDG:
// * regions have inputs (visible variables)
//    * and outputs (made visible to parent region)
// * theta regions (loops): have a set of loop variables -> region values
//

/*inst!{

    %0 = 1
    %1 = vec2(...)

    (%18, %19) = loop (%2 = %0, %3 = %1)  {

        break (%2, %3)

        // specify new values for %2 and %3
        continue (...)
    } while

}*/

// checks when inserting an instruction
// - check that SSA value ID is valid and accessible
// -

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
//      * inlining: insert blocks in the middle of functions
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
