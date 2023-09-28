use crate::{def::AstId, syntax::ast};

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum ConstExpr {
    Unknown,
    Expr(AstId<ast::Expr>),
}
