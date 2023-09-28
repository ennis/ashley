use crate::{def::InFile, syntax::ast};
use rowan::ast::AstPtr;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum BodyDiagnostic {
    ExpectedFunctionName { callee: InFile<AstPtr<ast::Expr>> },
    ParseIntError { lit_expr: InFile<AstPtr<ast::LitExpr>> },
    ParseFloatError { lit_expr: InFile<AstPtr<ast::LitExpr>> },
}
