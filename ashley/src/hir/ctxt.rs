use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use bumpalo::Bump;
use crate::hir::{Attribute, RegionData, StringAttr};
use crate::hir::intern::Interner;
use crate::hir::{AttributeBase, Opcode, Operation, OperationDef, OperationId, Region, RegionId, Value, ValueId};

/// Arena allocator used by `HirCtxt`s.
///
/// The same allocator can be shared by multiple `HirCtxt`s, but usually one is created for each `HirCtxt`.
/// It is not owned by `HirCtxt`s themselves because it would lead to self-referential lifetimes.
pub struct HirArena(Bump);

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

/// Container for HIR entities.
///
/// It holds regions, values, operations, types and attributes.
pub struct HirCtxt<'hir> {
    arena: &'hir HirArena,
    /// Map opcode -> OperationDef.
    op_def_map: HashMap<Opcode, &'static OperationDef>,
    interner: Interner<'hir>,
    attributes: Vec<Attribute<'hir>>,
    pub values: id_arena::Arena<Value, ValueId>,
    pub regions: id_arena::Arena<Region<'hir>, RegionId>,
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
        let mut op_def_map = HashMap::new();
        for op_def in inventory::iter::<OperationDef> {
            op_def_map.insert(op_def.opcode, op_def);
        }

        HirCtxt {
            arena,
            op_def_map,
            interner: Interner::new(),
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

    /*/// Creates a source location attribute.
    pub fn source_location(&mut self, source_location: SourceLocation) -> Attribute<'hir> {
        self.intern_attr(SourceLocationAttr(source_location))
    }*/

    /*/// Returns the "unknown" location attribute.
    pub fn unknown_location(&mut self) -> Attribute<'hir> {
        self.intern_attr(UnknownLocationAttr)
    }*/

    /*/// Returns a constant operand of undefined type.
    pub fn undef(&self, ) -> Operand {
        //let ty = self.intern_type()
    }*/

    /// Interns an attribute.
    fn intern_attr_inner<T>(&mut self, attr: T) -> &'hir T
        where
            T: AttributeBase<'hir> + Eq + Hash,
    {
        let (attr, inserted) = self.interner.intern(self.arena, attr);
        if inserted {
            self.types_and_attributes.push(TypeOrAttr::Attribute(Attribute(attr)));
        }
        attr
    }

    /// Interns an attribute.
    pub fn intern_attr<T>(&mut self, value: T) -> Attribute<'hir, T> where T: AttributeBase<'hir> + Eq + Hash {
        Attribute(self.intern_attr_inner(value))
    }

    /*/// Creates a string attribute.
    pub fn string_attr(&mut self, str: &str) -> &'hir StringAttr<'hir> {
        let str = self.alloc_str(str);
        self.intern_attr_inner(StringAttr(str))
    }*/

    /// Creates a new region.
    pub fn create_region(&mut self) -> RegionId {
        let region = self.regions.alloc(Region::new(RegionData {
            arguments: &[],
            ops: Default::default(),
        }));
        region
    }

    /// Emits the "undef" instruction at the cursor position and returns the value.
    pub fn undef(&mut self, at: Cursor) -> ValueId {
        let (_, r) = self.create_operation(at, Opcode(0, 0), 1, [], [], Default::default());
        r[0]
    }

    /// Emits a floating-point constant.
    pub fn fp_const(&mut self, value: f64) -> Attribute<'hir> {
        let attr = self.intern_attr(FloatAttr(value));
        attr.upcast()
    }

    /// Emits an integer constant.
    pub fn int_const(&mut self, value: i128) -> Attribute<'hir> {
        let attr = self.intern_attr(IntegerAttr(value));
        attr.upcast()
    }

    /// Emits a boolean constant.
    pub fn bool_const(&mut self, value: bool) -> Attribute<'hir> {
        let attr = self.intern_attr(IntegerAttr(value));
        attr.upcast()
    }

    /// A short-hand method to build an operation.
    pub fn create_operation<Operands, Attributes>(
        &mut self,
        at: Cursor,
        opcode: Opcode,
        result_count: usize,
        attrs: Attributes,
        operands: Operands,
        location: Location,
    ) -> (OperationId, &'hir [ValueId])
        where
            Operands: IntoIterator<Item = ValueId>,
            Operands::IntoIter: ExactSizeIterator,
            Attributes: IntoIterator<Item = Attribute<'hir>>,
            Attributes::IntoIter: ExactSizeIterator,
    {
        let id = self.ops.next_id();
        let attributes = self.arena.0.alloc_slice_fill_iter(attrs);
        let operands = self.arena.0.alloc_slice_fill_iter(operands);
        let results = self.arena.0.alloc_slice_fill_copy(result_count, ValueId::from_index(0));
        for i in 0..result_count {
            results[i] = self.values.alloc(Value::OpResult(id, i as u32));
        }
        let data = OperationData {
            opcode,
            attributes,
            operands,
            results,
            regions: Default::default(),
            location
        };
        let op = self.ops.alloc(Operation::new(data));
        match at {
            Cursor::End(region) => {
                self.regions[region].data.ops.append(op, &mut self.ops);
            }
            Cursor::Before(region, next) => {
                self.regions[region].data.ops.insert_before(op, next, &mut self.ops);
            }
        }
        (op, results)
    }

    /// Creates and appends a new subregion under the specified operation.
    pub fn create_subregion(&mut self, parent_op: OperationId, arg_count: usize) -> (RegionId, &'hir [ValueId]) {
        // Allocate values for the subregion
        let region_id = self.regions.next_id();
        let mut arguments = vec![];
        for i in 0..arg_count {
            arguments.push(self.values.alloc(Value::RegionArg(region_id, i as u32)));
        }
        let arguments = self.alloc_slice_copy(&arguments);
        let region = self.regions.alloc(Region::new(RegionData {
            arguments,
            ops: Default::default(),
        }));
        self.ops[parent_op].data.regions.append(region, &mut self.regions);
        (region, arguments)
    }
}
