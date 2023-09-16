use crate::{
    diagnostic::{Diagnostic2, Span},
    session::DefId,
    syntax::{
        ast,
        ast::{BinExpr, BinaryOp, ConstructorExpr, FieldExpr, IndexExpr, LitExpr, Name, PostfixExpr, UnaryOp},
    },
    tast::{
        consteval::ConstEvalError, layout::BlockLayoutDiagnostic, swizzle::ComponentSyntaxError, ExprId, ExprSource,
        InFile, Type,
    },
    utils::CommaSeparated,
};
use ashley::syntax::ast::PrefixExpr;
use rowan::ast::AstPtr;
use std::{num::ParseIntError, sync::Arc};

// Some of those ExprSource could be ExprIDs
// Problem: sometimes (often) the diag is emitted before the Expr is allocated.
// In r-a type checking is separate from lowering to ExprId, so there's an ExprId available
// for most diagnostics of type-checking.
//
// However, we do both lowering and typechecking in a single pass (Exprs have a type when allocated)
// since we don't need to do inference.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TyDiagnostic {
    // ------ Type-checking, name resolution ------
    NoMatchingConstructor {
        expr: InFile<AstPtr<ConstructorExpr>>,
        ty: Type,
        arg_tys: Vec<Type>,
    },
    NoSuchTypeConstructor {
        expr: InFile<AstPtr<ConstructorExpr>>,
        ty: Type,
    },
    InvalidIndexing {
        expr: InFile<AstPtr<IndexExpr>>,
        base_ty: Type,
    },
    InvalidIndexType {
        index: InFile<AstPtr<ast::Expr>>,
        ty: Type,
    },
    UnresolvedFunction {
        callee: ExprSource,
        func_name: String,
    },
    ExpectedFunctionName {
        callee: ExprSource,
    },
    ExpectedValue {
        path: InFile<AstPtr<ast::PathExpr>>,
    },
    NoMatchingOverload {
        call: InFile<AstPtr<ast::CallExpr>>,
        arg_types: Vec<Type>,
        candidates: Arc<Vec<DefId>>,
    },
    AmbiguousOverload {
        func_name: String,
        candidates: Vec<DefId>,
    },
    InvalidTypesForPrefixOp {
        op: UnaryOp,
        op_span: Span,
        expr: InFile<AstPtr<PrefixExpr>>,
        operand_ty: Type,
    },
    InvalidTypesForPostfixOp {
        op: UnaryOp,
        op_span: Span,
        expr: InFile<AstPtr<PostfixExpr>>,
        operand_ty: Type,
    },
    InvalidTypesForBinaryOp {
        op: BinaryOp,
        op_span: Span,
        source: InFile<AstPtr<BinExpr>>,
        left_ty: Type,
        right_ty: Type,
    },

    NoImplicitConversion {
        source: ExprSource,
        from: Type,
        to: Type,
    },
    InvalidComponentSelection {
        field: InFile<AstPtr<Name>>,
        error: ComponentSyntaxError,
        receiver: Type,
    },
    UnresolvedField {
        field: InFile<AstPtr<Name>>,
        receiver: Type,
    },
    UnresolvedPath {
        path: InFile<AstPtr<ast::PathExpr>>,
    },
    ReceiverNotAStructOrVector {
        field_expr: InFile<AstPtr<FieldExpr>>,
    },
    ParseIntError {
        lit_expr: InFile<AstPtr<LitExpr>>,
    },
    ParseFloatError {
        lit_expr: InFile<AstPtr<LitExpr>>,
    },

    // ------ Type resolution ------
    UnresolvedType {
        name: InFile<AstPtr<ast::Name>>,
    },
    InvalidArrayStride {
        stride_qualifier: InFile<AstPtr<ast::StrideQualifier>>,
        elem_ty_size: u32,
        stride: u32,
    },
    ArrayStrideOnOpaqueType {
        stride_qualifier: InFile<AstPtr<ast::StrideQualifier>>,
        element_type: Type,
    },
    Unimplemented {
        qualifier: InFile<AstPtr<ast::TypeQualifier>>,
    },

    // ------ Consteval ------
    ConstEvalError(ConstEvalError),

    // ------ Layout check ------
    BlockLayoutError(BlockLayoutDiagnostic),
}

impl From<ConstEvalError> for TyDiagnostic {
    fn from(value: ConstEvalError) -> Self {
        TyDiagnostic::ConstEvalError(value)
    }
}

impl From<BlockLayoutDiagnostic> for TyDiagnostic {
    fn from(value: BlockLayoutDiagnostic) -> Self {
        TyDiagnostic::BlockLayoutError(value)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl From<&TyDiagnostic> for Diagnostic2 {
    fn from(value: &TyDiagnostic) -> Self {
        match value {
            TyDiagnostic::NoMatchingConstructor { expr, ty, arg_tys } => {
                Diagnostic2::error(format!("no matching constructor for `{ty}`"))
                    .label(expr, "")
                    .note(format!("argument types are: ({})", CommaSeparated(arg_tys)))
            }
            TyDiagnostic::NoSuchTypeConstructor { expr, ty } => {
                Diagnostic2::error("invalid type constructor").label(expr).emit();
            }
            TyDiagnostic::InvalidIndexing { .. } => {}
            TyDiagnostic::InvalidIndexType { .. } => {}
            TyDiagnostic::UnresolvedFunction { .. } => {}
            TyDiagnostic::ExpectedFunctionName { .. } => {}
            TyDiagnostic::ExpectedValue { .. } => {}
            TyDiagnostic::NoMatchingOverload { .. } => {}
            TyDiagnostic::AmbiguousOverload { .. } => {}
            TyDiagnostic::InvalidTypesForPrefixOp { .. } => {}
            TyDiagnostic::InvalidTypesForPostfixOp { .. } => {}
            TyDiagnostic::InvalidTypesForBinaryOp { .. } => {}
            TyDiagnostic::NoImplicitConversion { .. } => {}
            TyDiagnostic::InvalidComponentSelection { .. } => {}
            TyDiagnostic::UnresolvedField { .. } => {}
            TyDiagnostic::UnresolvedPath { .. } => {}
            TyDiagnostic::ReceiverNotAStructOrVector { .. } => {}
            TyDiagnostic::ParseIntError { .. } => {}
            TyDiagnostic::ParseFloatError { .. } => {}
            TyDiagnostic::UnresolvedType { .. } => {}
            TyDiagnostic::InvalidArrayStride { .. } => {}
            TyDiagnostic::ArrayStrideOnOpaqueType { .. } => {}
            TyDiagnostic::Unimplemented { .. } => {}
            TyDiagnostic::ConstEvalError(_) => {}
            TyDiagnostic::BlockLayoutError(_) => {}
        }
    }
}
