//! Base dialect.
mod types;

use crate::{
    hir::{
        dialect, Attribute, HirCtxt, Operand, Operation, OperationData, OperationFormat, OperationId,
        RegionBuilder, RegionId, Type, TypeAttr, ValueId,
    },
    syntax::Span,
};
pub use types::{
    ArrayType, Field, FunctionType, ImageDimension, ImageType, SampledImageType, ScalarType, ScalarTypeKind,
    StructType, UnknownType, VectorType, TupleType
};

// Base dialect instructions

dialect! {
    pub dialect(BaseDialect, BaseDialectOpcodes, "base", 0x0002);

    /// Function definition
    operation(Func, OP_BASE_FUNC, "func");

    /// Constant
    operation(Constant, OP_BASE_CONST, "const");

    /// Addition
    operation(Add, OP_BASE_ADD, "add");

    /// Subtraction
    operation(Sub, OP_BASE_SUB, "sub");

    /// Conditionals
    operation(Select, OP_BASE_SELECT, "select");

    /// Loop.
    operation(Loop, OP_BASE_LOOP, "loop");

    /// Control-flow yield.
    operation(Yield, OP_BASE_YIELD, "yield");
}

#[derive(Copy, Clone, Debug)]
pub struct LoopOperation<'hir> {
    pub op: OperationId,
    pub results: &'hir [ValueId],
    pub body: RegionId,
    pub iter_vars: &'hir [ValueId],
}

#[derive(Copy, Clone, Debug)]
pub struct FunctionDefinition<'hir> {
    pub op: OperationId,
    pub result: ValueId,
    pub body: RegionId,
    pub arguments: &'hir [ValueId],
}


pub trait BaseDialectBuilder<'hir> {
    /// Inserts an operation that produces a constant value.
    ///
    /// # Arguments
    ///
    /// * constant an attribute whose value represents the constant
    fn base_constant(&mut self, constant: Attribute<'hir>) -> ValueId;

    /// Inserts a function declaration.
    ///
    /// # Arguments
    ///
    /// * ty a TypeAttr specifying the function type
    ///
    /// # Return value
    ///
    /// returns the value ID corresponding to the function, and the body region
    fn base_func(&mut self, ty: FunctionType<'hir>, location: Span) -> FunctionDefinition<'hir>;

    /// Inserts an addition operation.
    fn base_add(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId;

    /// Inserts a subtraction operation.
    fn base_sub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId;

    /// Inserts a loop operation.
    ///
    /// # Arguments
    /// * iter_args
    fn base_loop(&mut self, iter_args: &[ValueId]) -> LoopOperation<'hir>;


    /// Yields values from a control-flow region.
    fn base_yield(&mut self, iter_args: &[ValueId]);
}

impl<'a, 'hir> BaseDialectBuilder<'hir> for RegionBuilder<'a, 'hir> {
    fn base_constant(&mut self, constant: Attribute<'hir>) -> ValueId {
        let (_, r) = self.insert_operation(OP_BASE_CONST, 1, [constant], []);
        r[0]
    }

    fn base_func(&mut self, ty: FunctionType<'hir>, location: Span) -> FunctionDefinition<'hir> {
        // TODO do something with the location
        let num_args = ty.arg_types.len();
        let ty = self.ctxt().intern_type(ty);
        let attr = self.ctxt().intern_attr(TypeAttr(ty));
        let (op, result) = self.insert_operation(OP_BASE_FUNC, 1, [attr], []);
        let (body, arguments) = self.create_subregion(op, num_args);
        FunctionDefinition {
            op,
            result: result[0],
            body,
            arguments
        }
    }

    fn base_add(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let (_, r) = self.insert_operation(OP_BASE_ADD, 2, [], [lhs, rhs]);
        r[0]
    }

    fn base_sub(&mut self, lhs: ValueId, rhs: ValueId) -> ValueId {
        let (_, r) = self.insert_operation(OP_BASE_SUB, 2, [], [lhs, rhs]);
        r[0]
    }

    fn base_loop(&mut self, iter_args: &[ValueId]) -> LoopOperation<'hir>
    {
        let num_iter_args = iter_args.len();
        let (op, results) = self.insert_operation(OP_BASE_LOOP, num_iter_args, [], iter_args.iter().cloned());
        let (body, iter_vars) = self.create_subregion(op, num_iter_args);
        LoopOperation {
            op,
            results,
            body,
            iter_vars,
        }
    }

    fn base_yield(&mut self, values: &[ValueId]) {
        let (_op, _results) = self.insert_operation(OP_BASE_YIELD, 0, [], values.iter().cloned());
    }
}
