//! Operation formats & matchers
use std::hash::Hash;
use crate::hir::{Attribute, AttributeBase, HirCtxt, Location, Opcode, OperationId, RegionId, Value, ValueId};
use crate::hir::ctxt::OperationBuilder;

pub trait OperationFormat<'hir> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>);
}

pub trait OperationArgument<'hir> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>);
}

impl<'hir, T> OperationArgument<'hir> for T where T: OperationFormat<'hir> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        OperationFormat::build(self, ctxt, builder)
    }
}

pub trait ValueFormat<'hir> {
    fn to_value(&self, ctxt: &mut HirCtxt<'hir>) -> ValueId;
}

impl<'hir> ValueFormat<'hir> for ValueId {
    fn to_value(&self, _ctxt: &mut HirCtxt<'hir>) -> ValueId {
        *self
    }
}

pub trait AttributeFormat<'hir> {
    fn into_attr(self, ctxt: &mut HirCtxt<'hir>) -> Attribute<'hir>;
}

impl<'hir> AttributeFormat<'hir> for Attribute<'hir> {
    fn into_attr(self, _ctxt: &mut HirCtxt<'hir>) -> Attribute<'hir> {
        *self
    }
}

impl<'hir, T> AttributeFormat<'hir> for T where T: AttributeBase<'hir> + Eq + Hash {
    fn into_attr(self, ctxt: &mut HirCtxt<'hir>) -> Attribute<'hir> {
        ctxt.intern_attr(self)
    }
}

/// Values that can be matched against a ValueId.
pub trait ValuePattern<'hir>: 'hir {
    fn match_value(ctxt: &HirCtxt<'hir>, value: ValueId) -> Option<Self>;
}

pub trait OperationPattern<'hir>: 'hir {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self>;
}

pub trait AttributePattern<'hir>: 'hir {
    fn match_attr(ctxt: &HirCtxt<'hir>, attr: Attribute<'hir>) -> Option<Self>;
}

// ValueId matches any value id
impl<'hir> ValuePattern<'hir> for ValueId {
    fn match_value(ctxt: &HirCtxt<'hir>, value: ValueId) -> Option<Self> {
        Some(value)
    }
}

// OperationId matches any operation
impl<'hir> OperationPattern<'hir> for OperationId {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
        Some(op)
    }
}

// Attribute matches any attribute
impl<'hir> AttributePattern<'hir> for Attribute<'hir> {
    fn match_attr(ctxt: &HirCtxt<'hir>, attr: Attribute<'hir>) -> Option<Self> {
        Some(attr)
    }
}

impl<'hir, T, const I: usize> OperationArgument<'hir> for OperandIndex<T,I> where T: ValueFormat<'hir> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        builder.positional_operand(I, self.0.to_value(ctxt))
    }
}

/// Matches an operand of an operation.
pub struct OperandIndex<T, const I: usize>(T);

impl<'hir, T, const I: usize> OperationPattern<'hir> for OperandIndex<T, I> where T: ValuePattern<'hir> {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
        let op = &ctxt.ops[op];
        let operand = *op.data.operands.get(I as usize)?;
        T::match_value(ctxt, operand).map(OperandIndex)
    }
}

/*impl<'hir, T> OperationArgument<'hir> for T where T: ValueFormat<'hir> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        builder.push_operand(self.0.to_value(ctxt))
    }
}*/

/// Matches an operation with a specific opcode.
#[derive(Copy, Clone)]
pub struct OpcodeM<const DIALECT: u16, const OPCODE: u16>;

impl<'hir, const DIALECT: u16, const OPCODE: u16> OperationPattern<'hir> for OpcodeM<DIALECT, OPCODE> {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
        if ctxt.ops[op].data.opcode == Opcode(DIALECT, OPCODE) {
            Some(OpcodeM)
        } else {
            None
        }
    }
}

impl<'hir, const DIALECT: u16, const OPCODE: u16> OperationArgument<'hir> for OpcodeM<DIALECT, OPCODE> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        builder.opcode(Opcode(DIALECT, OPCODE));
    }
}

/// Matches an operation with a specific number of results.
pub struct OpResultCountM<const COUNT: usize>;

impl<'hir, const COUNT: usize> OperationPattern<'hir> for OpResultCountM<COUNT> {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
        if ctxt.ops[op].data.results.len() == COUNT {
            Some(OpResultCountM)
        } else {
            None
        }
    }
}

impl<'hir, const COUNT: usize> OperationArgument<'hir> for OpResultCountM<COUNT> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        builder.result_count(COUNT);
    }
}


/// Matches a positional attribute.
pub struct PositionalAttr<T, const I: usize>(T);

impl<'hir, T, const I: usize> OperationPattern<'hir> for PositionalAttr<T, I> where T: AttributePattern<'hir> {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
        todo!()
    }
}


impl<'hir, T, const I: usize> OperationArgument<'hir> for PositionalAttr<T, I> where T: AttributeFormat<'hir> {
    fn build(&self, ctxt: &HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        let attr = self.0.into_attr(ctxt);
        builder.positional_attribute(I, attr);
    }
}


/// Matches a value that is a result of the specified operation matcher.
#[derive(Copy,Clone)]
pub struct OpResult<T>(T);

impl<'hir, T> ValuePattern<'hir> for OpResult<T> where OpResult<T>: 'hir, T: OperationPattern<'hir> {
    fn match_value(ctxt: &HirCtxt<'hir>, value: ValueId) -> Option<Self> {
        match ctxt.values[value] {
            Value::OpResult(op, _i) => {
                if let Some(op) = T::match_op(ctxt, op) {
                    Some(OpResult(op))
                } else {
                    None
                }
            }
            Value::RegionArg(_, _) => {
                None
            }
        }
    }
}

/// Matches a region argument.
#[derive(Copy,Clone)]
pub struct RegionArg<T>(T);

impl<'hir, T> ValuePattern<'hir> for RegionArg<T> where RegionArg<T>: 'hir, T: ValuePattern<'hir> {
    fn match_value(ctxt: &HirCtxt<'hir>, value: ValueId) -> Option<Self> {
        match ctxt.values[value] {
            Value::OpResult(_, _i) => {
                None
            }
            Value::RegionArg(_, _) => {
                Some(RegionArg(value))
            }
        }
    }
}


/// Matches the location of an operation.
#[derive(Copy,Clone)]
pub struct OpLocationM(Location);

impl<'hir> OperationPattern<'hir> for OpLocationM {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
        Some(OpLocationM(ctxt.ops[op].data.location))
    }
}

impl<'hir> OperationArgument<'hir> for OpLocationM {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        builder.location(self.0);
    }
}


//--------------------------------------------------------------------------------------------------

macro_rules! impl_opmatch_tuple {
    ($($v:ident:$ts:ident),*) => {
        impl<'hir, $($ts,)*> OperationPattern<'hir> for ($($ts,)*) where $($ts: OperationPattern<'hir>,)* {
            fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
                match ($($ts::match_op(ctxt, op),)* ) {
                    v @ ($(Some($v),)*) => Some(($($v,)*)),
                    _ => None
                }
            }
        }
    };
}

impl_opmatch_tuple!(a:A);
impl_opmatch_tuple!(a:A,b:B);
impl_opmatch_tuple!(a:A,b:B,c:C);
impl_opmatch_tuple!(a:A,b:B,c:C,d:D);
impl_opmatch_tuple!(a:A,b:B,c:C,d:D,e:E);
impl_opmatch_tuple!(a:A,b:B,c:C,d:D,e:E,f:F);
impl_opmatch_tuple!(a:A,b:B,c:C,d:D,e:E,f:F,g:G);
impl_opmatch_tuple!(a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H);
impl_opmatch_tuple!(a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I);
impl_opmatch_tuple!(a:A,b:B,c:C,d:D,e:E,f:F,g:G,h:H,i:I,j:J);
