use crate::{
    diagnostic::Diagnostic,
    item,
    item::{
        body::{BodyDiagnostic, BodyMap},
        FunctionId, InFile,
    },
    syntax::{ast, SyntaxNode},
    ty::{
        body::{Body, Expr},
        Type,
    },
    utils::CommaSeparated,
    CompilerDb, SourceFileId,
};
use ashley_data_structures::Id;
use std::sync::Arc;

type ItemExprId = Id<item::body::Expr>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum ComponentSyntaxError {
    InvalidSyntax,
    TooManyComponents,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TyBodyDiagnostic {
    // ------ Type-checking, name resolution ------
    NoMatchingConstructor {
        expr: ItemExprId,
        ty: Type,
        arg_tys: Vec<Type>,
    },
    NoSuchTypeConstructor {
        expr: ItemExprId,
        ty: Type,
    },

    /*"indexing into a type that is not an array or vector"*/
    InvalidIndexing {
        expr: ItemExprId,
        base_ty: Type,
    },

    /*self.typed_body
    .diagnostics
    .push(diag_span_error!(index_expr, "index must be of type int"));*/
    InvalidIndexType {
        index: Id<Expr>,
        ty: Type,
    },
    UnresolvedFunction {
        call: ItemExprId,
    },
    ExpectedFunctionName {
        call: ItemExprId,
    },
    ExpectedValue {
        expr: ItemExprId,
    },
    NoMatchingOverload {
        call: ItemExprId,
        arg_types: Vec<Type>,
        candidates: Vec<FunctionId>,
    },
    AmbiguousOverload {
        func_name: String,
        candidates: Vec<FunctionId>,
    },
    InvalidTypesForPrefixOp {
        expr: ItemExprId,
        operand_ty: Type,
    },
    InvalidTypesForPostfixOp {
        op: ast::UnaryOp,
        expr: ItemExprId,
        operand_ty: Type,
    },
    InvalidTypesForBinaryOp {
        source: ItemExprId,
        left_ty: Type,
        right_ty: Type,
    },

    NoImplicitConversion {
        expr: ItemExprId,
        from: Type,
        to: Type,
    },
    InvalidComponentSelection {
        expr: ItemExprId,
        error: ComponentSyntaxError,
        receiver: Type,
    },
    UnresolvedField {
        expr: ItemExprId,
        name: String,
        receiver: Type,
    },
    UnresolvedPath {
        expr: ItemExprId,
    },
    ReceiverNotAStructOrVector {
        expr: ItemExprId,
    },
    ParseIntError {
        expr: ItemExprId,
    },
    ParseFloatError {
        expr: ItemExprId,
    },
}

impl TyBodyDiagnostic {
    pub fn render(
        &self,
        compiler: &dyn CompilerDb,
        body: &Body,
        body_map: &BodyMap,
        source_file: SourceFileId,
        syntax_root: &SyntaxNode,
    ) -> Diagnostic {
        let get_expr_syntax = |expr: &ItemExprId| InFile::new(source_file, body_map[*expr].to_node(syntax_root));

        let diag = match self {
            TyBodyDiagnostic::NoMatchingConstructor { expr, arg_tys, ty } => {
                let expr_syntax = get_expr_syntax(expr);
                Diagnostic::error(format!("no matching constructor for `{ty}`"))
                    .span(&expr_syntax.span())
                    .note(format!("argument types are {}", CommaSeparated(&arg_tys)))
            }
            TyBodyDiagnostic::NoSuchTypeConstructor { ty, expr } => {
                let expr_syntax = get_expr_syntax(expr);
                Diagnostic::error(format!("no constructor for type `{ty}`")).span(&expr_syntax.span())
            }
            TyBodyDiagnostic::InvalidIndexing { .. } => {
                todo!()
            }
            TyBodyDiagnostic::InvalidIndexType { .. } => {
                todo!()
            }
            TyBodyDiagnostic::UnresolvedFunction { .. } => {
                todo!()
            }
            TyBodyDiagnostic::ExpectedFunctionName { .. } => {
                todo!()
            }
            TyBodyDiagnostic::ExpectedValue { .. } => {
                todo!()
            }
            TyBodyDiagnostic::NoMatchingOverload { .. } => {
                todo!()
            }
            TyBodyDiagnostic::AmbiguousOverload { .. } => {
                todo!()
            }
            TyBodyDiagnostic::InvalidTypesForPrefixOp { .. } => {
                todo!()
            }
            TyBodyDiagnostic::InvalidTypesForPostfixOp { .. } => {
                todo!()
            }
            TyBodyDiagnostic::InvalidTypesForBinaryOp { .. } => {
                todo!()
            }
            TyBodyDiagnostic::NoImplicitConversion { .. } => {
                todo!()
            }
            TyBodyDiagnostic::InvalidComponentSelection { .. } => {
                todo!()
            }
            TyBodyDiagnostic::UnresolvedField { .. } => {
                todo!()
            }
            TyBodyDiagnostic::UnresolvedPath { .. } => {
                todo!()
            }
            TyBodyDiagnostic::ReceiverNotAStructOrVector { .. } => {
                todo!()
            }
            TyBodyDiagnostic::ParseIntError { .. } => {
                todo!()
            }
            TyBodyDiagnostic::ParseFloatError { .. } => {
                todo!()
            }
        };
        diag
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ConstEvalDiagnostic {
    CouldNotEvaluateAsConstant { expr: Id<Expr> },
}
