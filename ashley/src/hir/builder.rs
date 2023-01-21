use crate::{
    diagnostic::Diagnostics,
    hir::{
        constant::ConstantData, Block, BlockData, Constant, Function, FunctionData, FunctionParameter, InstructionData,
        Module, Operand, Operation, OperationId, Type, TypeData, Value, ValueData, ValueOrConstant,
    },
};
use ashley::hir::{types::ScalarType, TerminatingInstruction};
use bumpalo::Bump;
use id_arena::Arena;
use ordered_float::OrderedFloat;
use rspirv::spirv;
use smallvec::SmallVec;
use std::{collections::HashMap, fmt, hash::Hash, ops::Deref};
use crate::hir::GlobalVariable;

struct InstBuilder {
    opcode: spirv::Op,
    result_type: Option<Type>,
    operands: SmallVec<[Operand; 3]>,
}

impl InstBuilder {
    fn new(opcode: spirv::Op) -> InstBuilder {
        InstBuilder {
            opcode,
            result_type: None,
            operands: Default::default(),
        }
    }

    fn set_result(&mut self, result_type: Type) {
        self.result = Some(result_type);
    }
}

trait IntoOperand {
    fn write_operand(&self, builder: &mut InstBuilder);
}

impl IntoOperand for Value {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::ValueRef(*self))
    }
}

impl IntoOperand for (Value, Value) {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::ValueRef(self.0));
        builder.operands.push(Operand::ValueRef(self.1));
    }
}

impl<'a> IntoOperand for &'a [Value] {
    fn write_operand(&self, builder: &mut InstBuilder) {
        for v in self {
            builder.operands.push(Operand::ValueRef(v));
        }
    }
}

impl IntoOperand for Function {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::FunctionRef(*self))
    }
}

impl IntoOperand for Constant {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::ConstantRef(*self))
    }
}

impl IntoOperand for Type {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::TypeRef(*self))
    }
}

pub struct ImageOperands {
    //pub bits: spirv::ImageOperands,
    pub bias: Option<Value>,
    pub lod: Option<Value>,
    pub grad: Option<(Value, Value)>, // dx, dy
    pub const_offset: Option<Constant>,
    pub offset: Option<Value>,
    pub const_offsets: Option<Constant>,
    pub sample: Option<Value>,
    pub min_lod: Option<Value>,
    pub sign_extend: bool,
    pub zero_extend: bool,
}

impl Default for ImageOperands {
    fn default() -> Self {
        ImageOperands {
            bias: None,
            lod: None,
            grad: None,
            const_offset: None,
            offset: None,
            const_offsets: None,
            sample: None,
            min_lod: None,
            sign_extend: false,
            zero_extend: false,
        }
    }
}

impl IntoOperand for ImageOperands {
    fn write_operand(&self, builder: &mut InstBuilder) {
        let mut bits = spirv::ImageOperands::empty();
        // NOTE: the order is important: operands indicated by smaller-numbered bits should appear first
        if let Some(bias) = self.bias {
            bits.set(spirv::ImageOperands::BIAS, true);
            bias.write_operand(builder);
        }
        if let Some(lod) = self.lod {
            bits.set(spirv::ImageOperands::LOD, true);
            lod.write_operand(builder);
        }
        if let Some(grad) = self.grad {
            bits.set(spirv::ImageOperands::GRAD, true);
            grad.0.write_operand(builder);
            grad.1.write_operand(builder);
        }
        if let Some(const_offset) = self.const_offset {
            bits.set(spirv::ImageOperands::CONST_OFFSET, true);
            const_offset.write_operand(builder);
        }
        if let Some(offset) = self.offset {
            bits.set(spirv::ImageOperands::OFFSET, true);
            offset.write_operand(builder);
        }
        if let Some(const_offsets) = self.const_offsets {
            bits.set(spirv::ImageOperands::CONST_OFFSETS, true);
            const_offsets.write_operand(builder);
        }
        if let Some(sample) = self.sample {
            bits.set(spirv::ImageOperands::SAMPLE, true);
            sample.write_operand(builder);
        }
        if let Some(min_lod) = self.min_lod {
            bits.set(spirv::ImageOperands::MIN_LOD, true);
            min_lod.write_operand(builder);
        }
        if let Some(sign_extend) = self.sign_extend {
            bits.set(spirv::ImageOperands::SIGN_EXTEND, true);
            sign_extend.write_operand(builder);
        }
        if let Some(zero_extend) = self.zero_extend {
            bits.set(spirv::ImageOperands::ZERO_EXTEND, true);
            zero_extend.write_operand(builder);
        }
    }
}

/// Used to build a HIR function.

// FIXME: this is a bit crap: since we have no reference to the parent module, we can't infer the type
// of operations that reference globals or constants
// TODO: borrow module instead
pub struct FunctionBuilder<'a> {
    function: &'a mut FunctionData,
    block: Block,
}

impl<'a> FunctionBuilder<'a> {
    ///
    pub fn new(function: &'a mut FunctionData, block: Block) -> FunctionBuilder {
        FunctionBuilder { function, block }
    }

    /// Inserts an operation at the current cursor position.
    fn append_inst(&mut self, inst: InstBuilder) -> Option<Value> {
        // allocate result value
        let result = if let Some(result_type) = inst.result_type {
            Some(self.function.values.alloc(ValueData::new(result_type)))
        } else {
            None
        };

        self.function.blocks[self.block].instructions.push(InstructionData {
            opcode: inst.opcode,
            result,
            operands: inst.operands,
        });

        result
    }

    /// Enters a new block.
    pub fn begin_block(&mut self) -> Block {
        let block = self.function.blocks.alloc(BlockData {
            instructions: vec![],
            terminator: None,
        });
        self.block = block;
        block
    }

    /// References the specified constant.
    pub fn use_const(&mut self, result_type: Type, constant: Constant) -> Value {
        let mut i = InstBuilder::new(spirv::Op::CopyObject);
        i.result_type = Some(result_type);
        i.operands.push(Operand::ConstantRef(constant));
        self.append_inst(i).unwrap()
    }

    // TODO: infer result_type
    pub fn access_global(&mut self, result_type: Type, global_var: GlobalVariable) -> Value {
        let mut i = InstBuilder::new(spirv::Op::CopyObject);
        i.result_type = Some(result_type);
        i.operands.push(Operand::GlobalRef(global_var));
        self.append_inst(i).unwrap()
    }

    pub fn access_chain(&mut self, result_type: Type, base: Value, indices: &[ValueOrConstant]) -> Value {
        let mut i = InstBuilder::new(spirv::Op::AccessChain);
        i.result_type = Some(result_type);
        i.operands.push(base.into());
        for idx in indices {
            i.operands.push(idx.into());
        }
        self.append_inst(i).unwrap()
    }

    /// Returns the type of the specified value.
    pub fn ty(&mut self, v: Value) -> Type {
        self.function.values[v].ty
    }

    /// Switch to the specified block in the current function.
    pub fn select_block(&mut self, block: Block) {
        assert!(
            self.function.blocks[block].terminator.is_none(),
            "cannot switch to a terminated block"
        );
        self.block = block;
    }

    fn terminate_block(&mut self, inst: TerminatingInstruction) {
        let block = &mut self.function.blocks[self.block];
        assert!(block.terminator.is_none(), "block already terminated");
        block.terminator = Some(inst);
    }

    pub fn emit_selection_merge(&mut self, merge_block: Block) {
        let mut i = InstBuilder::new(spirv::Op::SelectionMerge);
        i.operands.push(Operand::BlockRef(merge_block));
        self.append_inst(i);
    }

    pub fn emit_loop_merge(&mut self, merge_block: Block, continue_target: Block) {
        let mut i = InstBuilder::new(spirv::Op::LoopMerge);
        i.operands.push(Operand::BlockRef(merge_block));
        i.operands.push(Operand::BlockRef(continue_target));
        self.append_inst(i);
    }

    pub fn branch(&mut self, target: Block) {
        self.terminate_block(TerminatingInstruction::Branch(target));
    }

    pub fn branch_conditional(&mut self, condition: Value, true_block: Block, false_block: Block) {
        self.terminate_block(TerminatingInstruction::BranchConditional {
            condition,
            true_block,
            false_block,
        });
    }

    pub fn ret(&mut self) {
        self.terminate_block(TerminatingInstruction::Return);
    }

    pub fn ret_value(&mut self, value: impl Into<ValueOrConstant>) {
        self.terminate_block(TerminatingInstruction::ReturnValue(value))
    }

    pub fn finish(self) -> FunctionData {
        self.function
    }
}

include!("autogen/builder.rs");
