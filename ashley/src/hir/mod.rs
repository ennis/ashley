mod builder;
pub mod constraint;
mod list;
mod print;
mod visit;
mod types;
mod op;

use crate::{
    diagnostic::{DiagnosticBuilder, Diagnostics},
    hir::{
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
use crate::diagnostic::SourceLocation;
use crate::hir::op::Op;
use crate::hir::types::TypeImpl;
pub use crate::utils::interner::{Attr, AttributeBase, BooleanAttr, FloatAttr, IntegerAttr, Location, StringAttr};
use crate::utils::interner::{AttrInterner, Interner};

pub use self::{
    builder::{build_operation, Builder, operation_format, operation_formats},
    constraint::{AttrConstraint, MatchCtxt, operation_constraint, OperationConstraint, RegionConstraint, ValueConstraint},
    print::{IRPrintable, IRPrinter, IRSyntaxElem, print_hir_region_html, ToIRSyntax, write_hir_html_file, write_ir},
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

impl<'a> IRPrintable<'a> for Location {
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>) {
        match self {
            Location::Source(src_loc) => {
                write_ir!(printer, "loc(", *src_loc, ")");
            }
            Location::Unknown => {
                write_ir!(printer, "loc(?)");
            }
        }
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
    /// Handle to a HIR region.
    pub struct RegionId;

    /// Handle to a HIR operation.
    pub struct OperationId;

    /// Handle to a HIR value.
    pub struct ValueId;

    /// Interned handle to a HIR type.
    pub struct Type;
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

    /// Returns a pointer to the underlying op.
    pub fn op(&self, ctxt: &HirCtxt) -> &Op {
        &ctxt.ops[*self].data.op
    }

    /// Returns a mutable pointer to the underlying op.
    pub fn op_mut<'a, 'ir>(&self, ctxt: &'a mut HirCtxt<'ir>) -> &'a mut Op<'ir> {
        &mut ctxt.ops[*self].data.op
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

/// Operation linked list nodes.
pub type Operation<'ir> = ListNode<OperationId, OperationData<'ir>>;

/// Data associated to an operation.
#[derive(Copy, Clone, Debug)]
pub struct OperationData<'ir> {
    /// Operation kind
    op: Op<'ir>,
    /// Operands
    operands: &'ir mut [ValueId],
    /// Results:

    /// Location
    location: Location,
    /// Parent region
    region: RegionId,
}

/// Region definition.
#[derive(Clone, Debug)]
pub struct RegionData {
    /// Values representing the region arguments.
    pub arguments: Vec<ValueId>,
    /// Ordered list of operations in the region.
    pub ops: List<OperationId, OperationData>,
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
    pub fn region_argument(reg: RegionId, index: u32, ty: Type) -> Value {
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

    pub(crate) types_interner: Interner<TypeImpl, Type>,
    pub types: id_arena::Arena<TypeImpl, Type>,
    pub values: id_arena::Arena<Value, ValueId>,
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

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::{
        dialect::base::{FunctionType, ScalarType, ScalarTypeKind},
        hir::{HirArena, HirCtxt, print_hir_region_html},
    };
    use std::{fs::File, io::Write, path::PathBuf};

    fn html_output_file_path(title: &str) -> PathBuf {
        const OUT_DIR: &str = "tests/out";
        let root = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(root).join(OUT_DIR).join(format!("{title}.html"))
    }

    fn attr_lifetime_test() {}
}
