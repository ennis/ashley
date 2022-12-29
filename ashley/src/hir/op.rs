use crate::hir::{RegionId, ValueId};

/// Addition
#[derive(Copy, Clone, Debug)]
pub struct AddOp {
    pub result: ValueId,
    pub lhs: ValueId,
    pub rhs: ValueId,
}

/// Subtraction
#[derive(Copy, Clone, Debug)]
pub struct SubOp {
    pub result: ValueId,
    pub lhs: ValueId,
    pub rhs: ValueId,
}

/// Multiplication
#[derive(Copy, Clone, Debug)]
pub struct MulOp {
    pub result: ValueId,
    pub lhs: ValueId,
    pub rhs: ValueId,
}

/// Division
#[derive(Copy, Clone, Debug)]
pub struct DivOp {
    pub result: ValueId,
    pub lhs: ValueId,
    pub rhs: ValueId,
}

/// Modulo
#[derive(Copy, Clone, Debug)]
pub struct ModOp {
    pub result: ValueId,
    pub lhs: ValueId,
    pub rhs: ValueId,
}

/// Select (conditionals).
#[derive(Copy, Clone, Debug)]
pub struct SelectOp {
    pub condition: ValueId,
    pub then_region: RegionId,
    pub else_region: RegionId
}

/// Loops
#[derive(Debug)]
pub struct LoopOp<'a> {
    pub body: RegionId,
    pub iter_args: &'a mut [ValueId],
}

pub struct CallOp<'a> {
    pub function: FunctionId,
    pub arguments: &'a mut [ValueId],
    pub result: ValueId,
}

/// IR operations.
#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Op<'a> {
    BinaryOp {
        op: BinaryOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    Select(SelectOp),
    Loop(LoopOp<'a>),
    Call(CallOp<'a>)
}


