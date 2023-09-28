use crate::{
    def,
    def::{body::BodyMap, DefAstId, DefLoc, FunctionLoc, HasSource, InFile},
    diagnostic::Diagnostic,
    syntax::{ast, SyntaxNode},
    ty::{body::Body, Type},
    utils::CommaSeparated,
    CompilerDb, SourceFileId,
};
use ashley_data_structures::Id;

type ItemExprId = Id<def::body::Expr>;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum ComponentSyntaxError {
    InvalidSyntax,
    TooManyComponents,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TyDiagnostic {
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
        index: Id<def::body::Expr>,
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
        candidates: Vec<FunctionLoc>,
    },
    AmbiguousOverload {
        func_name: String,
        candidates: Vec<FunctionLoc>,
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

    // ------ Type resolution ------
    UnresolvedType {
        ty_loc: DefAstId<ast::Type>,
        name: String,
    },
    InvalidArrayStride {
        ty_loc: DefAstId<ast::Type>,
        elem_ty_size: u32,
        stride: u32,
    },
    ArrayStrideOnOpaqueType {
        ty_loc: DefAstId<ast::Type>,
        element_type: Type,
    },
    Unimplemented {
        ty_loc: DefAstId<ast::Type>,
    },
}

impl TyDiagnostic {
    pub fn render(
        &self,
        db: &dyn CompilerDb,
        //body: &Body,
        body_map: &BodyMap,
        source_file: SourceFileId,
        syntax_root: &SyntaxNode,
    ) -> Diagnostic {
        let get_expr_syntax = |expr: &ItemExprId| InFile::new(source_file, body_map[*expr].to_node(syntax_root));

        let diag = match self {
            TyDiagnostic::NoMatchingConstructor { expr, arg_tys, ty } => {
                let expr_syntax = get_expr_syntax(expr);
                Diagnostic::error(format!("no matching constructor for `{ty}`"))
                    .span(&expr_syntax.span())
                    .note(format!("argument types are {}", CommaSeparated(&arg_tys)))
            }
            TyDiagnostic::NoSuchTypeConstructor { ty, expr } => {
                let expr_syntax = get_expr_syntax(expr);
                Diagnostic::error(format!("no constructor for type `{ty}`")).span(&expr_syntax.span())
            }
            TyDiagnostic::InvalidIndexing { .. } => {
                todo!()
            }
            TyDiagnostic::InvalidIndexType { .. } => {
                todo!()
            }
            TyDiagnostic::UnresolvedFunction { .. } => {
                todo!()
            }
            TyDiagnostic::ExpectedFunctionName { .. } => {
                todo!()
            }
            TyDiagnostic::ExpectedValue { .. } => {
                todo!()
            }
            TyDiagnostic::NoMatchingOverload { .. } => {
                todo!()
            }
            TyDiagnostic::AmbiguousOverload { .. } => {
                todo!()
            }
            TyDiagnostic::InvalidTypesForPrefixOp { .. } => {
                todo!()
            }
            TyDiagnostic::InvalidTypesForPostfixOp { .. } => {
                todo!()
            }
            TyDiagnostic::InvalidTypesForBinaryOp { .. } => {
                todo!()
            }
            TyDiagnostic::NoImplicitConversion { .. } => {
                todo!()
            }
            TyDiagnostic::InvalidComponentSelection { .. } => {
                todo!()
            }
            TyDiagnostic::UnresolvedField { .. } => {
                todo!()
            }
            TyDiagnostic::UnresolvedPath { .. } => {
                todo!()
            }
            TyDiagnostic::ReceiverNotAStructOrVector { .. } => {
                todo!()
            }
            TyDiagnostic::ParseIntError { .. } => {
                todo!()
            }
            TyDiagnostic::ParseFloatError { .. } => {
                todo!()
            }
            TyDiagnostic::UnresolvedType { ty_loc, ref name } => {
                let source = ty_loc.source(db);
                Diagnostic::error(format!("unresolved type name: `{name}`")).span(source.span())
            }
            TyDiagnostic::InvalidArrayStride {
                ty_loc,
                elem_ty_size,
                stride,
            } => {
                todo!()
            }
            TyDiagnostic::ArrayStrideOnOpaqueType { ty_loc, element_type } => {
                todo!()
            }
            TyDiagnostic::Unimplemented { ty_loc } => {
                todo!()
            }
        };
        diag
    }
}

/*#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ConstEvalDiagnostic {
    CouldNotEvaluateAsConstant { expr: Id<Expr> },
}


impl TyDiagnostic {
    pub fn render(&self, db: &dyn CompilerDb) -> Diagnostic {
        match self {

        }
    }
}*/
