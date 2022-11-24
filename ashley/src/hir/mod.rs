mod attr;
mod intern;
mod list;
mod print;
mod ty;
mod visit;
mod ctxt;
mod matchers;

use self::{
    intern::Interner,
    list::{List, ListNode},
};
use bumpalo::Bump;
use std::{collections::HashMap, fmt, hash::Hash, num::NonZeroU32};
use ashley::diagnostic::SourceLocation;

pub use self::{
    ctxt::{HirCtxt, HirArena, Cursor},
    attr::{Attribute, Location, AttributeBase, IntegerAttr, FloatAttr, StringAttr, TypeAttr},
    print::{print_hir_region_html, write_hir_html_file, write_ir, IRPrintable, IRPrinter, IRSyntaxElem},
    ty::{Type, TypeBase},
    visit::{IRVisitable, Visit},
};

//--------------------------------------------------------------------------------------------------

/// Definition of an operation.
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

inventory::collect!(OperationDef);

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

//--------------------------------------------------------------------------------------------------

/// Operation operand.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Operand {
    /// References a value.
    Value(ValueId),
}

/// Region definition.
#[derive(Clone, Debug)]
pub struct RegionData<'hir> {
    /// Values representing the region arguments.
    pub arguments: &'hir [ValueId],
    /// Ordered list of operations in the region.
    pub ops: List<OperationId, OperationData<'hir>>,
}

pub type Region<'hir> = ListNode<RegionId, RegionData<'hir>>;

#[derive(Clone, Debug)]
pub enum Value {
    /// The ith result of an operation.
    OpResult(OperationId, u32),
    /// The ith region argument.
    RegionArg(RegionId, u32),
}

type DialectId = u16;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Opcode(pub DialectId, pub u16);

impl Opcode {
    pub fn dialect(&self) -> DialectId {
        self.0
    }
    pub fn opcode(&self) -> u16 {
        self.1
    }
}

pub type Operation<'hir> = ListNode<OperationId, OperationData<'hir>>;

/// Definition of an operation.
#[derive(Copy, Clone, Debug)]
pub struct OperationData<'hir> {
    pub opcode: Opcode,
    /// Attributes.
    pub attributes: &'hir [Attribute<'hir>],
    /// Operands.
    pub operands: &'hir [ValueId],
    /// Results of the operation.
    pub results: &'hir [ValueId],
    /// Child regions.
    pub regions: List<RegionId, RegionData<'hir>>,
    pub location: Location,
}

impl<'hir> Default for OperationData<'hir> {
    fn default() -> Self {
        OperationData {
            opcode: Opcode(0, 0),
            attributes: &[],
            operands: &[],
            results: &[],
            regions: List::default(),
            location: Location::default(),
        }
    }
}

/*/// Builder for `Operation`s
pub struct OperationBuilder<'a, 'hir> {
    ctxt: &'a mut HirCtxt<'hir>,
    id: OperationId,
    data: OperationData<'hir>,
}

impl<'a, 'hir> OperationBuilder<'a, 'hir> {
    /// Sets the operands of this operation.
    pub fn operands<I>(mut self, operands: I) -> Self
    where
        I: IntoIterator<Item = Operand>,
        I::IntoIter: ExactSizeIterator,
    {
        self.data.operands = self.ctxt.arena.0.alloc_slice_fill_iter(operands);
        self
    }

    /// Allocates the result values for the operation.
    ///
    /// # Arguments
    /// * count the number of result values of the operation
    pub fn results(mut self, count: usize) -> Self {
        // TODO smallvec
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(self.ctxt.values.alloc(Value::OpResult efop: self.id }));
        }
        let result_values = self.ctxt.arena.0.alloc_slice_copy(&values);
        self.data.results = result_values;
        self
    }

    /*/// Allocates child regions for the operation.
    ///
    /// The regions are initially empty.
    ///
    /// # Arguments
    /// * count the number of regions of the operation
    pub fn regions(mut self, count: usize) -> Self {
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(self.ctxt.values.alloc(Value { defop: self.id }));
        }
        let result_values = self.ctxt.arena.alloc_slice_copy(&values);
    }*/

    /// Sets the attributes.
    pub fn attributes<I>(mut self, attributes: I) -> Self
    where
        I: IntoIterator<Item = Attribute<'hir>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.data.attributes = self.ctxt.arena.0.alloc_slice_fill_iter(attributes);
        self
    }

    /*/// Sets the child regions of the operation.
    pub fn regions<I>(mut self, regions: I) -> Self
    where
        I: IntoIterator<Item = RegionId>,
        I::IntoIter: ExactSizeIterator,
    {
        self.data.regions = self.ctxt.arena.0.alloc_slice_fill_iter(regions);
        self
    }*/

    /// Finishes building the operation.
    pub fn build(mut self) -> (OperationId, &'hir [ValueId]) {
        let result_values = self.data.results;
        let id = self.ctxt.ops.alloc(ListNode::new(self.data));
        (id, result_values)
        /*let id = if let Some(parent_region) = parent_region {
            let region: &mut RegionData = &mut self.ctxt.regions[parent_region].data;
            if let Some(insert_after) = insert_after {
                region.ops.insert_after(self.data, insert_after, &mut self.ctxt.ops)
            } else {
                region.ops.append(self.data, &mut self.ctxt.ops)
            }
        } else {
        };
        assert_eq!(id, self.id);*/
    }
}*/


//--------------------------------------------------------------------------------------------------

pub trait OperationFormat {
    const DEFINITION: OperationDef;
}

//--------------------------------------------------------------------------------------------------

/// Builtin operation

inventory::submit! {
    OperationDef::new(Opcode(0,0), "undef")
}

#[macro_export]
macro_rules! dialect {
    (
        $(#[$dialect_meta:meta])*
        $v:vis dialect($dialect:ident, $opcodes_enum:ident, $dialect_name:literal, $dialect_id:literal);
        $(
            $(#[$m:meta])* operation($n:ident, $opcode:ident, $mnemonic:literal);
        )*
    ) => {
        #[repr(u16)]
        $v enum $opcodes_enum {
            $($n,)*
        }

        $(
            $v const $opcode: $crate::hir::Opcode = $crate::hir::Opcode($dialect_id, $opcodes_enum::$n as u16);
            inventory::submit! {
                $crate::hir::OperationDef::new($crate::hir::Opcode($dialect_id, $opcodes_enum::$n as u16), $mnemonic)
            }

            /*$(#[$m])*
            $v struct $n<'a, 'hir>($v &'a $crate::hir::Operation<'hir>);
            impl<'a, 'hir> $crate::hir::OperationFormat for $n<'a,'hir> {
                const DEFINITION: $crate::hir::OperationDef = $crate::hir::OperationDef {
                    mnemonic: $mnemonic,
                    opcode: $crate::hir::Opcode($dialect_id, $opcodes_enum::$n as u16),
                };
            }*/

            /*impl<'a, 'hir> $n<'a, 'hir> {
                $(
                    $v fn $attr_name(&self) -> &'hir $attr_ty {
                        self.0.data.attributes[$attr_index].cast::<$attr_ty>().unwrap()
                    }
                )*

                $(
                    $v fn $operand_name(&self) -> $crate::hir::Operand {
                        self.0.data.operands[$operand_index]
                    }
                )*
            }*/
        )*

        /*// extension trait
        $(#[$dialect_meta])*
        $v trait $dialect: $crate::hir::OperationInserter {
            $(
                fn $builder_fn(&mut self, ctxt: &mut HirCtxt, $($attr_name : Attribute,),* $($operand_name : ValueId,)* ) -> ($crate::hir::OperationId, &'hir [$crate::hir::ValueId; $n_results]) {
                    let (op, r) = ctxt
                        .build_operation($n::DEFINITION.opcode)
                        .attributes([$($attr_name),*])
                        .results($n_results)
                        .build();
                    self.insert_operation(ctxt, op);
                    (op, r.try_into().unwrap())
                }
            )*
        }*/
    };
}

pub use dialect;

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::{
        dialect::base::{FunctionType, ScalarType, ScalarTypeKind},
        hir::{print_hir_region_html, HirArena, HirCtxt, Operand, TypeAttr},
    };
    use std::{fs::File, io::Write, path::PathBuf};

    mod ins {
        use crate::hir::{Opcode, OperationDef};
        pub const OP_UNDEF: Opcode = Opcode(0, 0);
        pub const OP_VARIABLE: Opcode = Opcode(0, 1);
        pub const OP_FUNCTION: Opcode = Opcode(0, 2);
        pub const OP_ADD: Opcode = Opcode(0, 3);
        pub const OP_SUB: Opcode = Opcode(0, 4);
        pub const OP_RETURN: Opcode = Opcode(0, 5);

        inventory::submit! {
            OperationDef::new(OP_UNDEF, "test.undef")
        }

        inventory::submit! {
            OperationDef::new(OP_FUNCTION, "test.fn")
        }

        inventory::submit! {
            OperationDef::new(OP_VARIABLE, "test.var")
        }

        inventory::submit! {
            OperationDef::new(OP_ADD, "test.add")
        }

        inventory::submit! {
            OperationDef::new(OP_SUB, "test.sub")
        }

        inventory::submit! {
            OperationDef::new(OP_RETURN, "test.return")
        }
    }
    use crate::{hir::RegionBuilder, syntax::Span};
    use ins::*;

    fn html_output_file_path(title: &str) -> PathBuf {
        const OUT_DIR: &str = "tests/out";
        let root = env!("CARGO_MANIFEST_DIR");
        PathBuf::from(root).join(OUT_DIR).join(format!("{title}.html"))
    }

    #[test]
    fn basic_hir() {
        use crate::dialect::base::BaseDialectBuilder;

        let arena = HirArena::new();
        let mut ctxt = HirCtxt::new(&arena);

        let ctxt = &mut ctxt;
        let root_region = ctxt.create_region();

        {
            let mut builder = RegionBuilder::new(ctxt, root_region);

            // Function op
            let f32_type = builder.ctxt().intern_type(ScalarType(ScalarTypeKind::Float));
            let arg_types = builder.ctxt().alloc_slice_copy(&[f32_type]);
            let fn_def = builder.base_func(
                FunctionType {
                    return_ty: f32_type,
                    arg_types,
                },
                Span::default(),
            );

            // fn body
            {
                let mut body_builder = builder.subregion(fn_def.body);
                let loop_def = body_builder.base_loop(&[fn_def.arguments[0]]);
                {
                    let mut loop_builder = body_builder.subregion(loop_def.body);
                    // a = a + 1
                    let cst_undef = loop_builder.undef();
                    let value = loop_builder.base_add(loop_def.iter_vars[0], cst_undef);
                    loop_builder.base_yield(&[value]);
                }
                body_builder.base_yield(&[loop_def.results[0]]);
            }
        }

        let mut html = String::new();
        print_hir_region_html(ctxt, "basic_hir", root_region, &mut html).unwrap();

        let mut file = File::create(html_output_file_path("basic_hir")).unwrap();
        file.write(html.as_bytes()).unwrap();
    }
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
