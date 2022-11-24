//! Operation formats & matchers
use crate::hir::{Attribute, HirCtxt, OperationId, RegionId, Value, ValueId};

pub struct OperationBuilder<'hir> {
    attributes: Vec<Attribute<'hir>>,
    operands: Vec<ValueId>,
    regions: Vec<RegionId>,
    result_count: usize,
}

impl<'hir> OperationBuilder<'hir> {
    pub fn positional_attribute(&mut self, index: usize, attr: Attribute<'hir>) {
        todo!()
    }

    pub fn push_attribute(&mut self, attr: Attribute<'hir>) {
        self.attributes.push(attr)
    }

    pub fn positional_operand(&mut self, index: usize, value: ValueId) {
        todo!()
    }

    pub fn finish(self, ctxt: &mut HirCtxt<'hir>) -> OperationId {
        todo!()
    }
}

pub trait OperationArgument<'hir> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>);
}

pub trait ValueFormat<'hir> {
    fn to_value(&self, ctxt: &mut HirCtxt<'hir>) -> ValueId;
}

impl<'hir> ValueFormat<'hir> for ValueId {
    fn to_value(&self, _ctxt: &mut HirCtxt<'hir>) -> ValueId {
        *self
    }
}

/// Values that can be matched against a ValueId.
pub trait ValueMatcher<'hir>: 'hir {
    fn match_value(ctxt: &HirCtxt<'hir>, value: ValueId) -> Option<Self>;
}

pub trait OperationMatcher<'hir>: 'hir {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self>;
}

pub trait AttributeMatcher<'hir>: 'hir {
    fn match_attr(ctxt: &HirCtxt<'hir>, attr: Attribute<'hir>) -> Option<Self>;
}

// ValueId matches any value id
impl<'hir> ValueMatcher<'hir> for ValueId {
    fn match_value(ctxt: &HirCtxt<'hir>, value: ValueId) -> Option<Self> {
        Some(value)
    }
}

// OperationId matches any operation
impl<'hir> OperationMatcher<'hir> for OperationId {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
        Some(op)
    }
}

// Attribute matches any attribute
impl<'hir> AttributeMatcher<'hir> for Attribute<'hir> {
    fn match_attr(ctxt: &HirCtxt<'hir>, attr: Attribute<'hir>) -> Option<Self> {
        Some(attr)
    }
}

impl<'hir, T, const I: usize> OperationArgument<'hir> for Attribute<'hir> where T: ValueFormat<'hir> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        builder.positional_operand(I, self.0.to_value(ctxt))
    }
}

/// Matches an operand of an operation.
pub struct OperandIndex<T, const I: usize>(T);

impl<'hir, T, const I: usize> OperationMatcher<'hir> for OperandIndex<T, I> where T: ValueMatcher<'hir> {
    fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
        let op = &ctxt.ops[op];
        let operand = *op.data.operands.get(I as usize)?;
        T::match_value(ctxt, operand).map(OperandIndex)
    }
}

impl<'hir, T, const I: usize> OperationArgument<'hir> for OperandIndex<T, I> where T: ValueFormat<'hir> {
    fn build(&self, ctxt: &mut HirCtxt<'hir>, builder: &mut OperationBuilder<'hir>) {
        builder.positional_operand(I, self.0.to_value(ctxt))
    }
}


/// Matches a positional attribute.
pub struct PositionalAttr<T, const I: usize>(T);

impl<'hir, T, const I: usize> OperationMatcher<'hir> for PositionalAttr<T, I> where T: AttributeMatcher<'hir> {
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

impl<'hir, T> ValueMatcher<'hir> for OpResult<T> where OpResult<T>: 'hir, T: OperationMatcher<'hir> {
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

impl<'hir, T> ValueMatcher<'hir> for RegionArg<T> where RegionArg<T>: 'hir, T: ValueMatcher<'hir> {
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
