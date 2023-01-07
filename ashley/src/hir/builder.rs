use crate::{
    diagnostic::Diagnostics,
    hir::{
        constant::ConstantImpl, Constant, Cursor, Function, FunctionId, FunctionParameter, HirCtxt, InstructionData,
        InstructionResult, Location, Operand, Operation, OperationId, Type, TypeImpl, Value, ValueId, ValueKind,
    },
};
use bumpalo::Bump;
use ordered_float::OrderedFloat;
use smallvec::SmallVec;
use std::{collections::HashMap, fmt, hash::Hash, ops::Deref};

struct InstBuilder {
    opcode: u32,
    result_type: Option<Type>,
    operands: SmallVec<[Operand; 3]>,
}

impl InstBuilder {
    fn set_result(&mut self, result_type: Type) {
        self.result = Some(result_type);
    }
}

trait IntoOperand {
    fn write_operand(&self, builder: &mut InstBuilder);
}

impl IntoOperand for ValueId {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::Value(*self))
    }
}

impl IntoOperand for (ValueId, ValueId) {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::Value(self.0));
        builder.operands.push(Operand::Value(self.1));
    }
}

impl<'a> IntoOperand for &'a [ValueId] {
    fn write_operand(&self, builder: &mut InstBuilder) {
        for v in self {
            builder.operands.push(Operand::Value(v));
        }
    }
}

impl IntoOperand for FunctionId {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::Function(*self))
    }
}

impl IntoOperand for Constant {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::Constant(*self))
    }
}

impl IntoOperand for Type {
    fn write_operand(&self, builder: &mut InstBuilder) {
        builder.operands.push(Operand::Type(*self))
    }
}

/// Used to build a HIR function.
pub struct FunctionBuilder<'a, 'hir> {
    pub ctxt: &'a mut HirCtxt<'hir>,
    diag: &'a Diagnostics,
    function: FunctionId,
}

impl<'a, 'hir> Deref for FunctionBuilder<'a, 'hir> {
    type Target = HirCtxt<'hir>;

    fn deref(&self) -> &Self::Target {
        &*self.ctxt
    }
}

impl<'a, 'hir> FunctionBuilder<'a, 'hir> {
    ///
    pub fn new(
        ctxt: &'a mut HirCtxt<'hir>,
        diag: &'a Diagnostics,
        return_ty: Type,
        parameters: Vec<FunctionParameter>,
    ) -> FunctionBuilder<'a, 'hir> {
        let function_id = ctxt.functions.next_id();
        let arguments: Vec<_> = parameters
            .iter()
            .enumerate()
            .map(|(i, param)| ctxt.alloc_value(Value::argument(function_id, i as u32, param.ty)))
            .collect();
        let function = ctxt.functions.alloc(Function {
            parameters,
            arguments,
            return_ty,
            name: "", // TODO
            ops: Default::default(),
        });

        FunctionBuilder { ctxt, diag, function }
    }

    /// Returns the underlying HirCtxt.
    pub fn ctxt_mut(&mut self) -> &mut HirCtxt<'hir> {
        self.ctxt
    }

    /// Returns the diagnostics instance associated with this builder.
    pub fn diagnostics(&self) -> &'a Diagnostics {
        self.diag
    }

    /// Emits a floating-point constant.
    pub fn fp_const(&mut self, value: f64) -> Constant {
        self.ctxt.intern_constant(ConstantImpl::F64(OrderedFloat::from(value)))
    }

    /// Emits an integer constant.
    pub fn int_const(&mut self, value: i64) -> Constant {
        self.ctxt.intern_constant(ConstantImpl::I64(value))
    }

    /// Emits a boolean constant.
    pub fn bool_const(&mut self, value: bool) -> Constant {
        self.ctxt.intern_constant(ConstantImpl::Boolean(value))
    }

    /// Inserts an operation at the current cursor position.
    fn append_inst(&mut self, inst: InstBuilder) -> Option<ValueId> {
        // allocate result value
        let next_id = self.ctxt.ops.next_id();
        let result = if let Some(result_type) = inst.result_type {
            Some(InstructionResult {
                value: self.ctxt.values.alloc(Value::result(next_id, 0, result_type)),
                ty: result_type,
            })
        } else {
            None
        };

        let id = self.ctxt.ops.alloc(Operation::new(InstructionData {
            opcode: inst.opcode,
            result,
            operands: inst.operands,
            function: self.function,
        }));

        self.ctxt.functions[self.function].ops.append(id, &mut self.ctxt.ops);
        result.map(|r| r.value)
    }
}

include!("autogen/builder.rs");