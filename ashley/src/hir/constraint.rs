use std::cell::Cell;
use std::ops::Deref;
use crate::{
    hir::{Attr, AttributeBase, Builder, OperationId, RegionId, ValueId},
};
use crate::hir::HirCtxt;


pub struct MatchCtxt<'a, 'ir> {
    ctxt: &'a HirCtxt<'ir>,
    error: Cell<&'static str>,
}

impl<'a, 'ir> MatchCtxt<'a, 'ir> {

    pub(crate) fn new(ctxt: &'a HirCtxt<'ir>) -> MatchCtxt<'a, 'ir> {
        MatchCtxt {
            ctxt,
            error: Cell::new(""),
        }
    }

    /// Report a failure matching a pattern.
    pub fn failure<T>(&self, info: &'static str) -> Option<T> {
        self.error.set(info);
        None
    }
}

impl<'a, 'ir> Deref for MatchCtxt<'a, 'ir> {
    type Target = HirCtxt<'ir>;

    fn deref(&self) -> &Self::Target {
        self.ctxt
    }
}

//--------------------------------------------------------------------------------------------------
//
// Attribute constraints
//

/// Represents a constraint applied to an attribute.
///
/// `AttrConstraint`s are also `ValueConstraint`s: when applied to a value, the attribute constraint
/// is matched on the _type_ of the value.
pub trait AttrConstraint<'ir>: Sized {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, attr: Attr<'ir>) -> Option<Self>;
}

impl<'ir> AttrConstraint<'ir> for Attr<'ir> {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, attr: Attr<'ir>) -> Option<Self> {
        Some(attr)
    }
}

impl<'ir, T> AttrConstraint<'ir> for T
where
    T: AttributeBase<'ir> + Copy,
{
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, attr: Attr<'ir>) -> Option<Self> {
        Some(*attr.cast()?)
    }
}

pub trait AttrSequenceConstraint<'ir>: Sized {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, attrs: &[Attr<'ir>]) -> Option<Self>;
}

impl<'ir> AttrSequenceConstraint<'ir> for Vec<Attr<'ir>> {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, attrs: &[Attr<'ir>]) -> Option<Self> {
        Some(attrs.into())
    }
}

// Attribute constraints are also value constraints that match all values with types that match the
// attribute constraint.
impl<'ir, A> ValueConstraint for A where A: AttrConstraint<'ir> {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, value: ValueId) -> Option<Self> {
        A::try_match(b, value.ty(b))
    }
}

//--------------------------------------------------------------------------------------------------
//
// Value constraints
//
pub trait ValueConstraint<'ir>: Sized {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, value: ValueId) -> Option<Self>;
}

impl<'ir> ValueConstraint<'ir> for ValueId {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, value: ValueId) -> Option<Self> {
        Some(value)
    }
}

/// Matches the type of a value.
#[derive(Copy, Clone, Debug)]
pub struct ValueType<'ir>(pub Attr<'ir>);

impl<'ir> ValueConstraint<'ir> for ValueType<'ir>
{
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, value: ValueId) -> Option<Self> {
        Some(ValueType(value.ty(b)))
    }
}

pub trait ValueSequenceConstraint<'ir>: Sized {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, values: &[ValueId]) -> Option<Self>;
}

impl<'ir> ValueSequenceConstraint<'ir> for Vec<ValueId> {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, values: &[ValueId]) -> Option<Self> {
        Some(values.into())
    }
}

/*/// Matches the types of the values.
#[derive(Copy, Clone, Debug)]
pub struct ValueTypes<'a, T>(pub T);

impl<'a, T> ValueSequenceConstraint<'a> for ValueTypes<'a, T> where T: AttrSequenceConstraint<'a> {
    fn try_match(b: &Builder<'_, 'a>, values: &[ValueId]) -> Option<Self> {
        Some(ValueTypes(values.iter().map(|&v| b.value_type(v)).collect()))
    }
}*/

//--------------------------------------------------------------------------------------------------
//
// Region constraints
//

/// Patterns on regions.
pub trait RegionConstraint<'ir>: Sized {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, region: RegionId) -> Option<Self>;
}

impl<'ir> RegionConstraint<'ir> for RegionId {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, region: RegionId) -> Option<Self> {
        Some(region)
    }
}

/// Region pattern that matches the arguments of the region.
#[derive(Copy, Clone, Debug)]
pub struct Region<'ir>(pub &'ir [ValueId]);

impl<'ir> RegionConstraint<'ir> for Region<'ir> {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, region: RegionId) -> Option<Self> {
        Some(Region(b.ctxt.regions[region].arguments))
    }
}

//--------------------------------------------------------------------------------------------------
//
// Operation constraints
//

/// Patterns on operations.
pub trait OperationConstraint<'ir>: Sized {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self>;
}

impl<'ir> OperationConstraint<'ir> for OperationId {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        Some(op)
    }
}

/// Operation pattern that matches operations with a single result.
#[derive(Copy, Clone, Debug)]
pub struct OpResult<T>(pub T);
impl<'ir, T> OperationConstraint<'ir> for OpResult<T>
where
    T: ValueConstraint<'ir>,
{
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        let op_results = op.results(b);
        if op_results.len() != 1 {
            None
        } else {
            T::try_match(b, *op_results.get(0).unwrap()).map(OpResult)
        }
    }
}

/// Operation pattern that matches the first result of an operation.
#[derive(Copy, Clone, Debug)]
pub struct FirstResult<T>(pub T);
impl<'ir, T> OperationConstraint<'ir> for FirstResult<T>
where
    T: ValueConstraint<'ir>,
{
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        T::try_match(b, *op.results(b).get(0)?).map(FirstResult)
    }
}

/// Matches the results of an operation.
#[derive(Copy, Clone, Debug)]
pub struct OpResults<'ir>(pub &'ir [ValueId]);
impl<'ir> OperationConstraint<'ir> for OpResults<'ir> {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        Some(OpResults(op.results(b)))
    }
}

/// Matches the operands of an operation.
#[derive(Copy, Clone, Debug)]
pub struct Operands<'ir>(pub &'ir [ValueId]);
impl<'ir> OperationConstraint<'ir> for Operands<'ir> {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        Some(Operands(op.results(b)))
    }
}

/*/// Matches the first subregion of an operation.
#[derive(Copy, Clone, Debug)]
pub struct FirstRegion<T>(pub T);

impl<'ir, T> OperationConstraint<'ir> for FirstRegion<T>
where
    T: RegionConstraint<'ir>,
{
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        T::try_match(b, op.first_region(b)?).map(FirstRegion)
    }
}

/// Matches the second subregion of an operation.
#[derive(Copy, Clone, Debug)]
pub struct SecondRegion<T>(pub T);

impl<'ir, T> OperationConstraint<'ir> for SecondRegion<T>
where
    T: RegionConstraint<'ir>,
{
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        T::try_match(b, b.second_region(op)?).map(SecondRegion)
    }
}*/

/*/// Matches the range of operands starting from `START`.
///
/// Fails to match if START is strictly greater than the number of operands.
pub struct OperandRange<'ir, const START: usize>(pub &'ir [ValueId]);
impl<'ir, const START: usize> OperationConstraint<'ir> for OperandRange<'ir, START> {
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        let operands = op.operands(b);
        if START > operands.len() {
            None
        } else {
            Some(OperandRange(&operands[START..]))
        }
    }
}*/

/*/// Operation pattern that matches the first operand (left-hand-side) of a two-operand operation.
#[derive(Copy, Clone, Debug)]
pub struct LHS<T>(pub T);
impl<'ir, T> OperationConstraint<'ir> for LHS<T>
where
    T: ValueConstraint<'ir>,
{
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        T::try_match(b, *b.operands(op).get(0)?).map(LHS)
    }
}

pub type FirstOperand<T> = LHS<T>;

/// Operation pattern that matches the second operand (right-hand-side) of a two-operand operation.
#[derive(Copy, Clone, Debug)]
pub struct RHS<T>(pub T);
impl<'ir, T> OperationConstraint<'ir> for RHS<T>
where
    T: ValueConstraint<'ir>,
{
    fn try_match<'a>(b: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
        T::try_match(b, *b.operands(op).get(1)?).map(RHS)
    }
}*/

macro_rules! impl_opmatch_tuple {
    ($($v:ident:$ts:ident),*) => {
        impl<'ir, $($ts,)*> OperationConstraint<'ir> for ($($ts,)*) where $($ts: OperationConstraint<'ir>,)* {
            fn try_match<'a>(m: &MatchCtxt<'a, 'ir>, op: OperationId) -> Option<Self> {
                Some( ($($ts::try_match(m, op)?,)* ) )
            }
        }
    };
}

impl_opmatch_tuple!(a: A);
impl_opmatch_tuple!(a: A, b: B);
impl_opmatch_tuple!(a: A, b: B, c: C);
impl_opmatch_tuple!(a: A, b: B, c: C, d: D);
impl_opmatch_tuple!(a: A, b: B, c: C, d: D, e: E);
impl_opmatch_tuple!(a: A, b: B, c: C, d: D, e: E, f: F);
impl_opmatch_tuple!(a: A, b: B, c: C, d: D, e: E, f: F, g: G);
impl_opmatch_tuple!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H);
impl_opmatch_tuple!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I);
impl_opmatch_tuple!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J);

//--------------------------------------------------------------------------------------------------

pub(crate) use ashley_derive::operation_constraint_match_body;

/// Defines an operation constraint with the constrain DSL.
///
/// TODO DSL reference, examples
#[macro_export]
macro_rules! operation_constraint {
    ($(#[$m:meta])* $v:vis $name:ident <$lt:lifetime> { $($pattern_var:ident : $pattern_var_ty:ty ),* } = { $($body:tt)* }) => {
        $(#[$m])*
        $v struct $name <$lt> {
            $($v $pattern_var : $pattern_var_ty),*
        }

        impl<$lt> $crate::hir::OperationConstraint<$lt> for $name<$lt> {
            fn try_match<'a__>(b__: &$crate::hir::MatchCtxt<'a__, $lt>, op__: $crate::hir::OperationId) -> Option<Self> {
                $crate::hir::constraint::operation_constraint_match_body!(
                    $name { $($pattern_var),* } $($body)*
                )
            }
        }
    };

    ($(#[$m:meta])* $v:vis $name:ident { $($pattern_var:ident : $pattern_var_ty:ty ),* } = { $($body:tt)* } ) => {
         $(#[$m])*
         $v struct $name {
             $($v $pattern_var : $pattern_var_ty),*
         }

         impl<'ir> $crate::hir::OperationConstraint<'ir> for $name {
             fn try_match<'a__>(b__: &$crate::hir::MatchCtxt<'a__, 'ir>, op__: $crate::hir::OperationId) -> Option<Self> {
                 $crate::hir::constraint::operation_constraint_match_body!(
                     $name { $($pattern_var),* } $($body)*
                 )
             }
         }
    };
}

pub use operation_constraint;
/// Helper macro to define multiple operation constraints.
///
/// See `operation_constraint`.
#[macro_export]
macro_rules! operation_constraints {
    ( $($(#[$m:meta])* $v:vis $name:ident $(<$lt:lifetime>)? { $($pattern_var:ident : $pattern_var_ty:ty ),* } = { $($body:tt)* })* ) => {
        $( $crate::hir::constraint::operation_constraint!($(#[$m])* $v $name $(<$lt>)? { $($pattern_var : $pattern_var_ty ),* } = { $($body)* }); )*
    };
}
