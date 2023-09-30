use crate::{
    def,
    def::{AstId, AstMapOwnerId, FunctionId},
    diagnostic::Diagnostic,
    syntax::{ast, SyntaxNode},
    ty::{body::ExprAstId, FunctionSignature, Type},
    utils::CommaSeparated,
    CompilerDb, SourceFileId,
};
use ashley_data_structures::Id;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ComponentSyntaxError {
    InvalidSyntax,
    TooManyComponents,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TyDiagnostic {
    // ------ Type-checking, name resolution ------
    NoMatchingConstructor {
        expr: ExprAstId,
        ty: Type,
        arg_tys: Vec<Type>,
    },
    NoSuchTypeConstructor {
        expr: ExprAstId,
        ty: Type,
    },

    /*"indexing into a type that is not an array or vector"*/
    InvalidIndexing {
        expr: ExprAstId,
        base_ty: Type,
    },

    /*self.typed_body
    .diagnostics
    .push(diag_span_error!(index_expr, "index must be of type int"));*/
    InvalidIndexType {
        /// AST ID of the index expression (the thing in the square brackets, not the whole _indexing_ expression).
        expr: ExprAstId,
        ty: Type,
    },
    UnresolvedFunction {
        expr: ExprAstId,
        name: String,
    },
    ExpectedFunctionName {
        expr: ExprAstId,
    },
    ExpectedValue {
        expr: ExprAstId,
    },
    ExpectedBoolean {
        expr: ExprAstId,
        ty: Type,
    },
    NoMatchingOverload {
        expr: ExprAstId,
        func_name: String,
        arg_types: Vec<Type>,
        candidates: Vec<FunctionId>,
    },
    AmbiguousOverload {
        expr: ExprAstId,
        func_name: String,
        arg_types: Vec<Type>,
        matches: Vec<FunctionId>,
    },
    /*InvalidTypesForUnaryOp {
        expr: ExprAstId,
        op: ast::UnaryOp,
        operand_ty: Type,
    },*/
    InvalidTypesForBinaryOp {
        expr: ExprAstId,
        left_ty: Type,
        right_ty: Type,
    },
    InvalidNumberOfArguments {
        expr: ExprAstId,
        expected: u32,
        got: u32,
    },
    /*InvalidArguments {
        expr: ExprAstId,
        func_name: String,
        signature: FunctionSignature,
        arg_types: Vec<Type>,
    },*/
    NoImplicitConversion {
        expr: ExprAstId,
        from: Type,
        to: Type,
    },
    InvalidComponentSelection {
        expr: ExprAstId,
        selection: String,
        error: ComponentSyntaxError,
        receiver: Type,
    },
    ExpectedPlace {
        expr: ExprAstId,
        ty: Type,
    },
    UnresolvedField {
        expr: ExprAstId,
        name: String,
        receiver: Type,
    },
    UnresolvedPath {
        expr: ExprAstId,
    },
    ReceiverNotAStructOrVector {
        expr: ExprAstId,
        receiver: Type,
    },
    ParseIntError {
        expr: ExprAstId,
    },
    ParseFloatError {
        expr: ExprAstId,
    },

    // ------ Type resolution ------
    UnresolvedType {
        ty_loc: AstId<ast::Type>,
        name: String,
    },
    InvalidArrayStride {
        ty_loc: AstId<ast::Type>,
        elem_ty_size: u32,
        stride: u32,
    },
    ArrayStrideOnOpaqueType {
        ty_loc: AstId<ast::Type>,
        element_type: Type,
    },
    Unimplemented {
        ty_loc: AstId<ast::Type>,
    },
}

impl TyDiagnostic {
    pub fn render(&self, db: &dyn CompilerDb, ast_map_owner: AstMapOwnerId) -> Diagnostic {
        let module = ast_map_owner.module(db);
        let ast_map = ast_map_owner.ast_map(db);

        let syntax = |ast_id: &AstId<ast::Expr>| ast_map.node_in_file(db, module, *ast_id);
        let ty_syntax = |ast_id: &AstId<ast::Type>| ast_map.node_in_file(db, module, *ast_id);

        let diag = match self {
            TyDiagnostic::NoMatchingConstructor { expr, arg_tys, ty } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("no matching constructor for `{ty}`"))
                    .span(syn.span())
                    .note(format!("argument types are {}", CommaSeparated(&arg_tys)))
            }
            TyDiagnostic::NoSuchTypeConstructor { ty, expr } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("no constructor for type `{ty}`")).span(syn.span())
            }
            TyDiagnostic::InvalidIndexing { expr, base_ty } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("values of type `{base_ty}` cannot be indexed")).span(syn.span())
            }
            TyDiagnostic::InvalidIndexType { expr, ty } => {
                let syn = syntax(expr);
                Diagnostic::error("index expressions should be integers")
                    .label(syn.span(), format!("this has type `{ty}`"))
            }
            TyDiagnostic::UnresolvedFunction { expr, name } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("unresolved function name `{name}`")).span(syn.span())
            }
            TyDiagnostic::ExpectedFunctionName { expr } => {
                let syn = syntax(expr);
                Diagnostic::error("expected function name").span(syn.span())
            }
            TyDiagnostic::ExpectedValue { expr } => {
                let syn = syntax(expr);
                Diagnostic::error("expected value").span(syn.span())
            }
            TyDiagnostic::ExpectedPlace { expr, ty } => {
                let syn = syntax(expr);
                Diagnostic::error("expected a place")
                    .span(syn.span())
                    .note(format!("expression is an rvalue of type `{ty}`"))
            }
            TyDiagnostic::NoMatchingOverload {
                expr,
                func_name,
                arg_types,
                candidates,
            } => {
                let syn = syntax(expr);
                let mut diag = Diagnostic::error(format!("no matching overload for call to function `{func_name}`"))
                    .span(syn.span())
                    .note(format!("argument types are {}", CommaSeparated(&arg_types)));
                if !candidates.is_empty() {
                    diag = diag.note("candidates are:");
                    for candidate in candidates {
                        let data = candidate.data(db);
                        let signature = candidate.signature(db);
                        let name = &data.name;
                        let return_type = &signature.return_type;
                        let argument_types = CommaSeparated(&signature.parameter_types);
                        diag = diag.note(format!("{return_type} {name}({argument_types})"))
                    }
                }
                diag
            }
            TyDiagnostic::AmbiguousOverload {
                expr,
                func_name,
                arg_types,
                matches,
            } => {
                let syn = syntax(expr);
                let mut diag = Diagnostic::error(format!("ambiguous call to overloaded function `{func_name}`"))
                    .span(syn.span())
                    .note(format!("argument types are {}", CommaSeparated(&arg_types)));
                assert!(!matches.is_empty());
                diag = diag.note("could be:");
                for func in matches {
                    let data = func.data(db);
                    let signature = func.signature(db);
                    let name = &data.name;
                    let return_type = &signature.return_type;
                    let argument_types = CommaSeparated(&signature.parameter_types);
                    diag = diag.note(format!("{return_type} {name}({argument_types})"))
                }
                diag
            }
            /*TyDiagnostic::InvalidTypesForPrefixOp { .. } => {
                todo!()
            }
            TyDiagnostic::InvalidTypesForPostfixOp { .. } => {
                todo!()
            }*/
            TyDiagnostic::InvalidTypesForBinaryOp {
                expr,
                left_ty,
                right_ty,
            } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("invalid types to binary operation"))
                    .span(syn.span())
                    .note(format!("operand types are {left_ty}, {right_ty}"))
            }
            TyDiagnostic::NoImplicitConversion { expr, to, from } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("cannot implicitly convert `{from}` to `{to}`"))
                    .label(syn.span(), format!("this is of type `{from}`"))
            }
            TyDiagnostic::InvalidComponentSelection {
                expr,
                error,
                selection,
                receiver,
            } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("invalid component selection: `{selection}`"))
                    .span(syn.span())
                    .note(format!("base expression is of type `{receiver}`"))
            }
            TyDiagnostic::UnresolvedField { expr, name, receiver } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("unresolved field: `{name}`"))
                    .span(syn.span())
                    .note(format!("base expression is of type `{receiver}`"))
            }
            TyDiagnostic::UnresolvedPath { expr } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("unresolved path")).span(syn.span())
            }
            TyDiagnostic::ReceiverNotAStructOrVector { expr, receiver } => {
                let syn = syntax(expr);
                Diagnostic::error("invalid field or component selection")
                    .span(syn.span())
                    .note(format!("base expression is of type `{receiver}`"))
            }
            TyDiagnostic::ParseIntError { .. } => {
                todo!()
            }
            TyDiagnostic::ParseFloatError { .. } => {
                todo!()
            }
            TyDiagnostic::UnresolvedType { ty_loc, ref name } => {
                let syn = ty_syntax(ty_loc);
                Diagnostic::error(format!("unresolved type name: `{name}`")).span(syn.span())
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
            TyDiagnostic::InvalidNumberOfArguments { .. } => {
                todo!()
            }
            TyDiagnostic::ExpectedBoolean { expr, ty } => {
                let syn = syntax(expr);
                Diagnostic::error(format!("expected boolean expression"))
                    .label(syn.span(), format!("the expression is of type `{ty}`"))
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
