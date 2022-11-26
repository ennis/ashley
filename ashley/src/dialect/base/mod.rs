//! Base dialect.
mod types;

use ashley::hir::matchers::OpResultCountM;
use ashley::hir::Value;
use crate::{
    diagnostic::SourceLocation,
    hir::{
        dialect, Attribute, Cursor, HirCtxt, Location, Operand, Operation, OperationData, OperationId,
        RegionId, TypeAttr, ValueId,
    },
};
pub use types::{
    ArrayType, Field, FunctionType, ImageDimension, ImageType, SampledImageType, ScalarType, ScalarTypeKind,
    StructType, TupleType, UnitType, UnknownType, VectorType,
};
use crate::hir::matchers::{OpcodeM, OperandIndex};



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

    /// Multiplication
    operation(Mul, OP_BASE_MUL, "mul");

    /// Division
    operation(Div, OP_BASE_DIV, "div");

    /// Conditionals
    operation(Select, OP_BASE_SELECT, "select");

    /// Loop.
    operation(Loop, OP_BASE_LOOP, "loop");

    /// Control-flow yield.
    operation(Yield, OP_BASE_YIELD, "yield");

    /// Call function.
    operation(Call, OP_BASE_CALL, "call");
}

//#[derive(OperationFormat, OperationMatcher)]
//#[operation(opcode="func")]
pub struct OpBaseFunc<'hir> {
    pub _opcode: OpcodeM<0x0001,0x0001>,
    pub ty: FunctionType<'hir>,
    pub body: RegionId,
    pub _result: OpResultCountM<1>,
}

//#[derive(OperationFormat, OperationMatcher)]
//#[operation(opcode="loop")]
pub struct OpBaseLoop<const N: usize> {
    pub _opcode: OpcodeM<0x0001,0x0001>,
    pub iter_args: [ValueId; N],
    pub body: RegionId,
    pub _result: OpResultCountM<N>,
}

//#[derive(OperationFormat, OperationMatcher)]
//#[operation(opcode="call")]
pub struct OpBaseCall<const N: usize> {
    pub _opcode: OpcodeM<0x0001,0x0001>,
    pub args: [ValueId; N],
    pub _result: OpResultCountM<N>,
}

//#[derive(OperationFormat, OperationMatcher)]
//#[operation(opcode="mod")]
pub struct OpBaseModulo<T=ValueId, U=ValueId> {
    pub _opcode: OpcodeM<0x0001,0x0001>,
    pub lhs: OperandIndex<T, 0>,
    pub rhs: OperandIndex<U, 1>,
    pub _result: OpResultCountM<1>,
}

// OpBaseModulo::new(lhs, rhs).build(ctxt), or ctxt.insert_op(at, OpBaseModulo::new(lhs, rhs)

/*pub trait BaseDialectBuilder<'hir> {
    /// Inserts an operation that produces a constant value.
    ///
    /// # Arguments
    ///
    /// * constant an attribute whose value represents the constant
    fn base_constant(&mut self, cur: Cursor, constant: Attribute<'hir>, loc: Location) -> ValueId;

    /// Inserts a function declaration.
    ///
    /// # Arguments
    ///
    /// * ty a TypeAttr specifying the function type
    ///
    /// # Return value
    ///
    /// returns the value ID corresponding to the function, and the body region
    fn base_func(&mut self, cur: Cursor, ty: FunctionType<'hir>, loc: Location) -> FunctionDefinition<'hir>;

    /// Inserts an addition operation.
    fn base_add(&mut self, cur: Cursor, lhs: ValueId, rhs: ValueId, loc: Location) -> ValueId;

    /// Inserts a multiplication operation.
    fn base_mul(&mut self, cur: Cursor, lhs: ValueId, rhs: ValueId, loc: Location) -> ValueId;

    /// Inserts a division operation.
    fn base_div(&mut self, cur: Cursor, lhs: ValueId, rhs: ValueId, loc: Location) -> ValueId;

    /// Inserts a subtraction operation.
    fn base_sub(&mut self, cur: Cursor, lhs: ValueId, rhs: ValueId, loc: Location) -> ValueId;

    /// Inserts a loop operation.
    ///
    /// # Arguments
    /// * iter_args
    fn base_loop(&mut self, cur: Cursor, iter_args: &[ValueId], loc: Location) -> LoopOperation<'hir>;

    /// Yields values from a control-flow region.
    fn base_yield(&mut self, cur: Cursor, iter_args: &[ValueId], loc: Location);

    /// Calls a function.
    fn base_call(&mut self, cur: Cursor, function: ValueId, args: &[ValueId], loc: Location);
}

impl<'hir> BaseDialectBuilder<'hir> for HirCtxt<'hir> {
    fn base_constant(&mut self, cur: Cursor, constant: Attribute<'hir>, loc: Location) -> ValueId {
        let (_, r) = self.create_operation(cur, OP_BASE_CONST, 1, [constant], [], loc);
        r[0]
    }

    fn base_func(&mut self, cur: Cursor, ty: FunctionType<'hir>, loc: Location) -> FunctionDefinition<'hir> {
        // TODO do something with the location
        let num_args = ty.arg_types.len();
        let ty = self.intern_type(ty);
        let attr = self.intern_attr(TypeAttr(ty));
        let (op, result) = self.create_operation(cur, OP_BASE_FUNC, 1, [attr.upcast()], [], loc);
        let (body, arguments) = self.create_subregion(op, num_args);
        FunctionDefinition {
            op,
            result: result[0],
            body,
            arguments,
        }
    }

    fn base_add(&mut self, cur: Cursor, lhs: ValueId, rhs: ValueId, loc: Location) -> ValueId {
        let (_, r) = self.create_operation(cur, OP_BASE_ADD, 2, [], [lhs, rhs], loc);
        r[0]
    }

    fn base_mul(&mut self, cur: Cursor, lhs: ValueId, rhs: ValueId, loc: Location) -> ValueId {
        let (_, r) = self.create_operation(cur, OP_BASE_MUL, 2, [], [lhs, rhs], loc);
        r[0]
    }

    fn base_div(&mut self, cur: Cursor, lhs: ValueId, rhs: ValueId, loc: Location) -> ValueId {
        let (_, r) = self.create_operation(cur, OP_BASE_DIV, 2, [], [lhs, rhs], loc);
        r[0]
    }

    fn base_sub(&mut self, cur: Cursor, lhs: ValueId, rhs: ValueId, loc: Location) -> ValueId {
        let (_, r) = self.create_operation(cur, OP_BASE_SUB, 2, [], [lhs, rhs], loc);
        r[0]
    }

    fn base_loop(&mut self, cur: Cursor, iter_args: &[ValueId], loc: Location) -> LoopOperation<'hir> {
        let num_iter_args = iter_args.len();
        let (op, results) = self.create_operation(cur, OP_BASE_LOOP, num_iter_args, [], iter_args.iter().cloned(), loc);
        let (body, iter_vars) = self.create_subregion(op, num_iter_args);
        LoopOperation {
            op,
            results,
            body,
            iter_vars,
        }
    }

    fn base_yield(&mut self, cur: Cursor, values: &[ValueId], loc: Location) {
        let (_op, _results) = self.create_operation(cur, OP_BASE_YIELD, 0, [], values.iter().cloned(), loc);
    }

    fn base_call(&mut self, cur: Cursor, function: ValueId, args: &[ValueId], loc: Location) {
        todo!()
        //let (_op, _results) = self.create_operation(cur, OP_BASE_CALL, 0, [], args.iter().cloned(), loc);
    }
}*/
