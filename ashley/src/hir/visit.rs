/*//!
use crate::{
    AtomicFunction, Barrier, Block, Expression, Function,
    Handle, Range, Statement, SwitchCase,
};

////////////////////////////////////////////////////////////////////////////////////////////////////
pub fn visit_block<V>(v: &mut V, block: &Block)
    where
        V: Visit + ?Sized,
{
    for stmt in block.iter() {
        v.visit_statement(stmt);
    }
}

pub fn visit_switch_case<V>(v: &mut V, case: &SwitchCase)
    where
        V: Visit + ?Sized,
{
    v.visit_block(&case.body);
}

pub fn visit_statement<V>(v: &mut V, statement: &Statement)
    where
        V: Visit + ?Sized,
{
    match *statement {
        Statement::Emit(ref exprs) => v.visit_emit(exprs),
        Statement::Block(ref block) => v.visit_block(block),
        Statement::If {
            ref accept,
            ref reject,
            ..
        } => {
            v.visit_block(accept);
            v.visit_block(reject);
        }
        Statement::Switch {
            ref cases,
            ..
        } => {
            for case in cases {
                v.visit_switch_case(case);
            }
        }
        Statement::Loop {
            ref body,
            ref continuing,
            ..
        } => {
            v.visit_block(body);
            v.visit_block(continuing);
        }
        Statement::Break => {
            v.visit_break();
        }
        Statement::Continue => {
            v.visit_continue();
        }
        Statement::Return { .. } => {
            v.visit_return();
        }
        Statement::Kill => {
            v.visit_kill();
        }
        Statement::Barrier(barrier) => {
            v.visit_barrier(barrier);
        }
        Statement::Store { value, pointer } => {
            v.visit_store(value, pointer);
        }
        Statement::ImageStore {
            image,
            value,
            array_index,
            coordinate,
        } => {
            v.visit_image_store(image, coordinate, array_index, value);
        }
        Statement::Atomic {
            pointer,
            fun,
            value,
            result,
        } => v.visit_atomic(pointer, fun, value, result),
        Statement::Call {
            function,
            ref arguments,
            result,
        } => v.visit_call(function, arguments, result),
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
pub trait Visit {
    fn visit_block(&mut self, block: &Block) {
        visit_block(self, block);
    }

    fn visit_emit(&mut self, _exprs: &Range<Expression>) {}

    fn visit_switch_case(&mut self, case: &SwitchCase) {
        visit_switch_case(self, case)
    }

    fn visit_statement(&mut self, statement: &Statement) {
        visit_statement(self, statement)
    }

    fn visit_break(&mut self) {}

    fn visit_continue(&mut self) {}

    fn visit_return(&mut self) {}

    fn visit_kill(&mut self) {}

    fn visit_barrier(&mut self, _barrier: Barrier) {}

    fn visit_store(&mut self, _pointer: Handle<Expression>, _value: Handle<Expression>) {}

    fn visit_image_store(
        &mut self,
        _image: Handle<Expression>,
        _coordinate: Handle<Expression>,
        _array_index: Option<Handle<Expression>>,
        _value: Handle<Expression>,
    ) {
    }

    fn visit_atomic(
        &mut self,
        _pointer: Handle<Expression>,
        _fun: AtomicFunction,
        _value: Handle<Expression>,
        _result: Handle<Expression>,
    ) {
    }

    fn visit_call(
        &mut self,
        _function: Handle<Function>,
        _arguments: &Vec<Handle<Expression>>,
        _result: Option<Handle<Expression>>,
    ) {
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
pub fn visit_block_mut<V>(v: &mut V, block: &mut Block)
    where
        V: VisitMut + ?Sized,
{
    for stmt in block.iter_mut() {
        v.visit_statement_mut(stmt);
    }
}

pub fn visit_switch_case_mut<V>(v: &mut V, case: &mut SwitchCase)
    where
        V: VisitMut + ?Sized,
{
    v.visit_block_mut(&mut case.body);
}

pub fn visit_statement_mut<V>(v: &mut V, statement: &mut Statement)
    where
        V: VisitMut + ?Sized,
{
    match *statement {
        Statement::Emit(ref mut exprs) => v.visit_emit_mut(exprs),
        Statement::Block(ref mut block) => v.visit_block_mut(block),
        Statement::If {
            ref mut accept,
            ref mut reject,
            ..
        } => {
            v.visit_block_mut(accept);
            v.visit_block_mut(reject);
        }
        Statement::Switch {
            ref mut cases,
            ..
        } => {
            for case in cases {
                v.visit_switch_case_mut(case);
            }
        }
        Statement::Loop {
            ref mut body,
            ref mut continuing,
            ..
        } => {
            v.visit_block_mut(body);
            v.visit_block_mut(continuing);
        }
        Statement::Break => {
            v.visit_break_mut();
        }
        Statement::Continue => {
            v.visit_continue_mut();
        }
        Statement::Return { .. } => {
            v.visit_return_mut();
        }
        Statement::Kill => {
            v.visit_kill_mut();
        }
        Statement::Barrier(ref mut barrier) => {
            v.visit_barrier_mut(barrier);
        }
        Statement::Store {
            ref mut value,
            ref mut pointer,
        } => {
            v.visit_store_mut(value, pointer);
        }
        Statement::ImageStore {
            ref mut image,
            ref mut value,
            ref mut array_index,
            ref mut coordinate,
        } => {
            v.visit_image_store_mut(image, coordinate, array_index, value);
        }
        Statement::Atomic {
            ref mut pointer,
            ref mut fun,
            ref mut value,
            ref mut result,
        } => v.visit_atomic_mut(pointer, fun, value, result),
        Statement::Call {
            ref mut function,
            ref mut arguments,
            ref mut result,
        } => v.visit_call_mut(function, arguments, result),
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
pub trait VisitMut {
    fn visit_block_mut(&mut self, block: &mut Block) {
        visit_block_mut(self, block);
    }

    fn visit_emit_mut(&mut self, _exprs: &mut Range<Expression>) {
    }

    fn visit_switch_case_mut(&mut self, case: &mut SwitchCase) {
        visit_switch_case_mut(self, case)
    }

    fn visit_statement_mut(&mut self, statement: &mut Statement) {
        visit_statement_mut(self, statement)
    }

    fn visit_break_mut(&mut self) {}

    fn visit_continue_mut(&mut self) {}

    fn visit_return_mut(&mut self) {}

    fn visit_kill_mut(&mut self) {}

    fn visit_barrier_mut(&mut self, _barrier: &mut Barrier) {}

    fn visit_store_mut(
        &mut self,
        _pointer: &mut Handle<Expression>,
        _value: &mut Handle<Expression>,
    ) {
    }

    fn visit_image_store_mut(
        &mut self,
        _image: &mut Handle<Expression>,
        _coordinate: &mut Handle<Expression>,
        _array_index: &mut Option<Handle<Expression>>,
        _value: &mut Handle<Expression>,
    ) {
    }

    fn visit_atomic_mut(
        &mut self,
        _pointer: &mut Handle<Expression>,
        _fun: &mut AtomicFunction,
        _value: &mut Handle<Expression>,
        _result: &mut Handle<Expression>,
    ) {
    }

    fn visit_call_mut(
        &mut self,
        _function: &mut Handle<Function>,
        _arguments: &mut Vec<Handle<Expression>>,
        _result: &mut Option<Handle<Expression>>,
    ) {
    }
}
*/