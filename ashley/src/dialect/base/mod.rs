//! Base dialect.
mod types;

use crate::{
    diagnostic::{DiagnosticBuilder, SourceLocation},
    hir::{
        build_operation,
        constraint::{
            FirstResult, OpResult, OpResults, Operands, Region,
            ValueType,
        },
        operation_format, operation_formats, Attr, AttrConstraint, Builder, Cursor, HirCtxt, Location, Operation,
        OperationConstraint, OperationCreateInfo, OperationData, OperationId, RegionId, ValueConstraint, ValueId,
    },
    operation_constraint, operation_constraints,
};
use ashley::hir::constraint::MatchCtxt;
use inventory::iter;
pub use types::{
    ArrayType, BaseDialectAttrExt, Field, FunctionType, ImageDimension, ImageType, MatrixType, SampledImageType,
    ScalarType, ScalarTypeKind, StructType, TupleType, UnitType, UnknownType, VectorType,
};

/// Returns whether the specified type can be used as an operand of arithmetic operations.
fn is_arithmetic_type(ty: Attr) -> bool {
    if let Some(ScalarType(s)) = ty.cast() {
        *s != ScalarTypeKind::Bool
    } else if let Some(VectorType(s, len)) = ty.cast() {
        *s != ScalarTypeKind::Bool
    } else {
        false
    }
}

/// Matches a value of boolean type.
#[derive(Copy, Clone, Debug)]
pub struct BooleanValue;

impl<'ir> ValueConstraint<'ir> for BooleanValue {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, value: ValueId) -> Option<Self> {
        value.ty(b).is_bool().then_some(BooleanValue)
    }
}

/// Constrains an attribute to be an arithmetic type.
#[derive(Copy, Clone, Debug)]
pub struct ArithmeticType;

impl<'ir> AttrConstraint<'ir> for ArithmeticType {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, attr: Attr<'ir>) -> Option<Self> {
        is_arithmetic_type(attr).then_some(ArithmeticType)
    }
}

/*/// Checks the validity of a function call operation.
pub struct IsValidFunctionCall;
impl<'a> OperationConstraint<'a> for IsValidFunctionCall {
    fn try_match(b: &Builder<'_, 'a>, op: OperationId) -> Result<Self, DiagnosticBuilder<'a>> {
        let &FunctionType { arg_types, return_ty } = op.operand(b, 0)?.ty(b).cast()?;
        let call_site_result_ty = op.result(b, 0)?.ty(b);
        let call_site_argument_types = op.operand_range(b, 1..).iter().map(|v| v.ty(b)).collect::<Vec<_>>();
        (arg_types == call_site_argument_types && return_ty == call_site_result_ty)
            .then(|| IsValidFunctionCall)
            .ok_or(b.diagnostics().error("invalid function call"))
    }
}*/

fn check_arguments<'ir>(cx: &MatchCtxt<'_, 'ir>, args: &[ValueId], types: &[Attr<'ir>]) -> bool {
    args.iter().map(|v| v.ty(cx)).eq(types.iter().cloned())
}

/// Predicate that checks that two value sequences have the same length and, for each value pair,
/// the same type.
fn same_types(cx: &MatchCtxt, a: &[ValueId], b: &[ValueId]) -> bool {
    if a.len() != b.len() {
        return false
    } else {
        a.iter().map(|v| v.ty(cx)).eq(b.iter().map(|v| v.ty(cx)))
    }
}


/*operation_formats! {
    pub Constant: FirstResult(result) => { result: ValueId };

    /// Functions.
    pub Function<'ir>: op, FirstResult(result), FirstRegion(body), FirstRegion(Region(arguments)) => {
        op: OperationId,
        result: ValueId,
        body: RegionId,
        arguments: &'ir [ValueId]
    };

    /// Function call
    pub Call<'ir>: FirstResult(result), LHS(function), OperandRange::<1>(arguments), IsValidFunctionCall => { result: ValueId, function: ValueId, arguments: &'ir [ValueId] };

    //----------------------------------------------------------------------------------------------
    // ARITHMETIC

    /// Arithmetic add operation.
    ///
    /// The two operands should have the same arithmetic type.
    pub Add: FirstResult(result), LHS(lhs), RHS(rhs), ArithmeticOperands => { result: ValueId, lhs: ValueId, rhs: ValueId };

    /// Subtraction operation.
    ///
    /// The two operands should have the same arithmetic type.
    pub Sub: OpResult(result), LHS(lhs), RHS(rhs), ArithmeticOperands => { result: ValueId, lhs: ValueId, rhs: ValueId };

    /// Multiplication operation.
    ///
    /// The two operands should have the same arithmetic type.
    pub Mul: OpResult(result), LHS(lhs), RHS(rhs), ArithmeticOperands => { result: ValueId, lhs: ValueId, rhs: ValueId };

    /// Division operation.
    ///
    /// The two operands should have the same arithmetic type.
    pub Div: OpResult(result), LHS(lhs), RHS(rhs), ArithmeticOperands => { result: ValueId, lhs: ValueId, rhs: ValueId };

    /// Modulo operation.
    ///
    /// The two operands should have the same arithmetic type.
    pub Mod: OpResult(result), LHS(lhs), RHS(rhs), ArithmeticOperands => { result: ValueId, lhs: ValueId, rhs: ValueId };

    /// Control-flow yield.
    pub Yield<'ir>: Operands(values) => { values: &'ir [ValueId] };


    //----------------------------------------------------------------------------------------------
    // LOGICAL

    // Logical and operation.
    //pub And: Result(BooleanValue(result)), LHS(BooleanValue(lhs)), RHS(BooleanValue(rhs)) if |b| { lhs.ty(b).is_bool() &&  } =>


    //----------------------------------------------------------------------------------------------
    // CONTROL FLOW

    /// Loops.
    ///
    /// Loops have one subregion corresponding to the loop body. The arguments of this region are the loop variables
    /// that may change each iteration.
    ///
    /// The loop body should be terminated by either a `base.yield` operation or a `base.continue` operation.
    pub Loop<'a>: op, OpResults(results), FirstRegion(body), FirstRegion(Region(iter_vars)) => {
        op: OperationId,
        results: &'a [ValueId],
        body: RegionId,
        iter_vars:&'a [ValueId]
    };

    /// Conditionals
    pub Select<'a>: OpResults(results), LHS(BooleanValue(condition)), FirstRegion(then_branch), SecondRegion(else_branch) => {
        condition: ValueId,
        then_branch: RegionId,
        else_branch: RegionId,
        results: &'a [ValueId]
    };

}*/

operation_constraints! {
    /// Matches an operation with two operands.
    pub BinaryOperation { lhs: ValueId, rhs: ValueId } = {
        ""(lhs, rhs)
    }

    /// Loads a constant into a value.
    pub Constant { result: ValueId } = {
        "base.const"() {} -> (result)
    }

    /// Function definition
    pub Function<'ir> {
        op: OperationId,
        arguments: &'ir [ValueId],
        body: RegionId,
        result: ValueId
    } = {
        op: "base.func" { body @ Region(arguments) } -> (result @ !FunctionType { .. })
    }

    /// Function call
    pub Call<'ir> { result: ValueId, function: ValueId, arguments: &'ir [ValueId] } = {
        "base.call"(function, ..arguments) -> (result:!ret_ty) where
            function: !_:FunctionType { return_ty = ret_ty, arg_types = arg_types },
            |cx| check_arguments(cx, arguments, arg_types),
    }

    //----------------------------------------------------------------------------------------------
    // ARITHMETIC

    /// Addition
    pub Add { result: ValueId, lhs: ValueId, rhs: ValueId } = { "base.add"(lhs:T, rhs:T) -> (result:T) where T: ArithmeticType }

    /// Subtraction
    pub Sub { result: ValueId, lhs: ValueId, rhs: ValueId } = { "base.sub"(lhs:T, rhs:T) -> (result:T) where T: ArithmeticType }

    /// Multiplication
    pub Mul { result: ValueId, lhs: ValueId, rhs: ValueId } = { "base.mul"(lhs:T, rhs:T) -> (result:T) where T: ArithmeticType }

    /// Division
    pub Div { result: ValueId, lhs: ValueId, rhs: ValueId } = { "base.div"(lhs:T, rhs:T) -> (result:T) where T: ArithmeticType }

    /// Modulo
    pub Mod { result: ValueId, lhs: ValueId, rhs: ValueId } = { "base.mod"(lhs:T, rhs:T) -> (result:T) where T: ArithmeticType }

    //----------------------------------------------------------------------------------------------
    // CONTROL FLOW

    /// Loops.
    ///
    /// Loops have one subregion corresponding to the loop body. The arguments of this region are the loop variables
    /// that may change each iteration.
    ///
    /// The loop body should be terminated by either a `base.yield` operation or a `base.continue` operation.
    pub Loop<'a> {
        op: OperationId,
        results: &'a [ValueId],
        body: RegionId,
        iter_vars:&'a [ValueId]
    } = {
        op: "base.loop"(..iter_vars_init) { body:Region(iter_vars) } -> (..results) where |cx| {
            same_types(cx, iter_vars, results) && same_types(cx, iter_vars_init, results)
        }
    }

    /// Conditionals
    pub Select<'a> {
        condition: ValueId,
        then_branch: RegionId,
        else_branch: RegionId,
        results: &'a [ValueId]
    } = {
        "base.select"(condition:BooleanValue) { then_branch, else_branch } -> (..results)
    }
}

impl<'hir> Loop<'hir> {
    /// Constructs a loop operation.
    ///
    /// # Arguments
    /// * iter_vars input iteration variables
    fn build(b: &mut Builder<'_, 'hir>, iter_vars: &'hir [ValueId], loc: Location) -> Loop<'hir> {
        let mut iter_var_types = vec![];
        for &v in iter_vars {
            iter_var_types.push(b.value_type(v));
        }
        build_operation!(b; "base.loop"(~iter_vars) { body(~&iter_var_types) } -> (~&iter_var_types) at loc)
    }
}

//--------------------------------------------------------------------------------------------------

macro_rules! impl_binop_builders {
    ( $($(#[$m:meta])* $v:vis $name:ident [$mnemonic:literal]; )* )
    =>
    {
        $(impl $name {
            $(#[$m])*
            $v fn build(b: &mut Builder, lhs: ValueId, rhs: ValueId, loc: Location) -> $name {
                let lhs_ty = b.value_type(lhs);
                build_operation!(b; $mnemonic(lhs, rhs) -> (lhs_ty) at loc)
            }
        })*
    };
}

impl_binop_builders!(
    /// Constructs an `add` operation.
    pub Add["base.add"];
    /// Constructs a `sub` operation.
    pub Sub["base.sub"];
    /// Constructs a `mul` operation.
    pub Mul["base.mul"];
    /// Constructs a `div` operation.
    pub Div["base.div"];
    /// Constructs a `mod` operation.
    pub Mod["base.mod"];
);


impl<'hir> Function<'hir> {
    /// Constructs a `func` operation
    ///
    /// # Arguments
    ///
    /// * function_type the type of the function, must be an instance of `FunctionType`
    /// * loc source location
    pub fn build(b: &mut Builder<'_, 'hir>, function_type: Attr<'hir>, loc: Location) -> Function<'hir> {
        let &FunctionType { arg_types, .. } = function_type.cast().expect("invalid function type");
        build_operation!(b; "base.func" { body(~arg_types) } -> (function_type) at loc)
    }
}

impl Constant {
    /// Constructs a constant.
    pub fn build<'hir>(b: &mut Builder<'_, 'hir>, constant: Attr<'hir>, loc: Location) -> Constant {
        let ty = b.unknown_type().upcast();
        build_operation!(b; "base.const" [constant] -> (ty) at loc)
    }
}

impl<'ir> Call<'ir> {
    /// Constructs a function call.
    ///
    /// # Arguments
    /// * function the function to call, must be of function type
    /// * args the arguments to the function, must match the types of the function.
    pub fn build(b: &mut Builder<'_, 'ir>, function: ValueId, args: &[ValueId], loc: Location) -> Call<'ir> {
        let &FunctionType { return_ty, arg_types } = function.ty(b).cast().expect("invalid function value");
        build_operation!(b; "base.call"(function, ~args) -> (return_ty) at loc)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        diagnostic::{Diagnostics, SourceFileProvider},
        dialect::{base, base::ArithmeticType},
        hir,
        hir::{
            constraint,
            constraint::{Region, ValueType},
            HirArena, HirCtxt, ValueId,
        },
    };
    use ashley::hir::Location;
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

    fn hir_test(f: impl FnOnce(&mut hir::Builder)) {
        let arena = HirArena::new();
        let mut ctxt = HirCtxt::new(&arena);
        let region = ctxt.create_region();
        let files = SourceFileProvider::new();
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        let diag = Diagnostics::new(files, writer, Default::default());
        let mut builder = hir::Builder::new(&mut ctxt, &diag, region);
        f(&mut builder)
    }

    #[test]
    fn test_match() {
        hir_test(|b| {
            let loc = Location::Unknown;
            let ty = b.f32_ty();
            let u = b.undef_typed(ty.upcast());
            let r = base::Add::build(b, u, u, loc).result;

            if let Some(m) = r.pmatch(b) {
                let m: ValueId = m;
                // OK
                eprintln!("matched any value")
            }

            if let Some(ValueType(IsArithmeticType)) = r.pmatch(b) {
                eprintln!("value of arithmetic type");
            } else {
                panic!("expected arithmetic type");
            };

            let f = base::Function::build(b, ty.upcast(), loc);

            /*if let Some(FirstRegion(Region(args))) = f.op.pmatch(b) {
                let args: &[ValueId] = args;
            }*/
        })
    }
}
