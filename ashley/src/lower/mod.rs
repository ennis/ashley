//! AST lowering to SPIR-V
mod builtin;
mod consteval;
mod mangle;
mod swizzle;
mod typecheck;
mod types;

use crate::{
    diagnostic::{DiagnosticBuilder, Diagnostics, SourceFileProvider, SourceId, SourceLocation},
    hir,
    hir::{
        types::{FunctionType, ScalarType, StructType},
        Constant, ConstantData, Function, FunctionBuilder, FunctionData, GlobalVariable, GlobalVariableData, Module,
        Operation, TypeData, ValueOrConstant,
    },
    lower::{
        builtin::BuiltinTypes,
        consteval::try_evaluate_constant_expression,
        typecheck::{typecheck_builtin_operation, BuiltinOperation},
    },
    syntax::{ast, ast::AstNode, ArithOp, BinaryOp, CmpOp, LogicOp, SyntaxNode, SyntaxToken},
};
use ashley::{hir::Value, lower::typecheck::check_signature, syntax::ast::Expr};
use codespan_reporting::{diagnostic::Diagnostic, term, term::termcolor::WriteColor};
use rspirv::{dr::Builder, spirv};
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use spirv::CLOp::exp;
use std::{
    any::Any,
    cell::Cell,
    collections::HashMap,
    io::ErrorKind,
    panic::Location,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

trait DiagnosticsExt {
    /// "missing generic argument"
    fn missing_generic_argument(
        &mut self,
        instantiation_loc: SourceLocation,
        expected_forms: &str,
    ) -> DiagnosticBuilder;

    /// "extra generic argument(s)"
    fn extra_generic_arguments(
        &mut self,
        instantiation_loc: SourceLocation,
        expected: usize,
        got: usize,
    ) -> DiagnosticBuilder;

    /// "an item with the same name has already been defined"
    fn already_defined(
        &mut self,
        name: &str,
        loc: SourceLocation,
        prev_loc: Option<SourceLocation>,
    ) -> DiagnosticBuilder;

    /// "cannot find type `{name}` in this scope"
    fn cannot_find_type(&mut self, name: &str, loc: SourceLocation) -> DiagnosticBuilder;

    /// "cannot find value `{name}` in this scope"
    fn cannot_find_value(&mut self, name: &str, loc: SourceLocation) -> DiagnosticBuilder;

    /// "could not evaluate constant expression"
    fn consteval_failure(&mut self, loc: SourceLocation) -> DiagnosticBuilder;
}

impl DiagnosticsExt for Diagnostics {
    fn missing_generic_argument(&self, instantiation_loc: SourceLocation, expected_forms: &str) -> DiagnosticBuilder {
        self.error("missing generic argument")
            .primary_label(instantiation_loc.to_source_location(), "")
            .note(format!("expected: `{expected_forms}`"))
    }

    fn extra_generic_arguments(
        &self,
        instantiation_loc: SourceLocation,
        expected: usize,
        got: usize,
    ) -> DiagnosticBuilder {
        self.error("too many generic arguments").primary_label(
            instantiation_loc.to_source_location(),
            format!("expected {expected} argument(s), got {got} argument(s)"),
        )
    }

    fn already_defined(&self, name: &str, loc: SourceLocation, prev_loc: Option<SourceLocation>) -> DiagnosticBuilder {
        self.error(format!(
            "`{name}`an item with the same name has already been defined in this scope"
        ))
        .primary_label(loc, "")
        .secondary_label(prev_loc, "previous definition here")
    }

    fn cannot_find_type(&mut self, name: &str, loc: SourceLocation) -> DiagnosticBuilder {
        self.error(format!("cannot find type `{name}` in this scope"))
            .primary_label(loc, "")
    }

    fn cannot_find_value(&mut self, name: &str, loc: SourceLocation) -> DiagnosticBuilder {
        self.error(format!("cannot find value `{name}` in this scope"))
            .primary_label(loc, "")
    }

    fn consteval_failure(&mut self, loc: SourceLocation) -> DiagnosticBuilder {
        ctxt.diag
            .error("could not evaluate constant expression")
            .primary_label(loc, "")
    }
}

//--------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum DefNameKind {
    Function,
    Type,
    Variable,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct DefName {
    kind: DefNameKind,
    name: String,
}

enum DefKind {
    Type(hir::Type),
    Function(FuncRef),
    Constant(hir::Constant),
    GlobalVariable(hir::GlobalVariable),
    BlockVariable(hir::Value),
}

struct Def {
    location: Option<SourceLocation>,
    kind: DefKind,
}

struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    types: HashMap<String, Def>,
    variables: HashMap<String, Def>,
    functions: HashMap<String, Def>,
}

impl<'a> Scope<'a> {
    fn new(parent: Option<&'a Scope<'a>>) -> Scope {
        Scope {
            parent,
            types: Default::default(),
            variables: Default::default(),
            functions: Default::default(),
        }
    }

    fn resolve(&self, kind: DefNameKind, name: &str) -> Option<&Def> {
        let def = match kind {
            DefNameKind::Function => self.functions.get(name),
            DefNameKind::Type => self.types.get(name),
            DefNameKind::Variable => self.variables.get(name),
        };

        def.or_else(|| self.parent.and_then(|parent| parent.resolve(kind, name)))
    }

    fn define_type(&mut self, name: String, location: Option<SourceLocation>, ty: hir::Type) -> Option<Def> {
        self.types.insert(
            name,
            Def {
                kind: DefKind::Type(ty),
                location,
            },
        )
    }

    fn define_function(&mut self, name: String, location: Option<SourceLocation>, func: FuncRef) -> Option<Def> {
        self.functions.insert(
            name,
            Def {
                kind: DefKind::Function(func),
                location,
            },
        )
    }

    fn define_constant(
        &mut self,
        name: String,
        location: Option<SourceLocation>,
        constant: hir::Constant,
    ) -> Option<Def> {
        self.variables.insert(
            name,
            Def {
                kind: DefKind::Constant(constant),
                location,
            },
        )
    }

    fn define_global_variable(
        &mut self,
        name: String,
        location: Option<SourceLocation>,
        var: hir::GlobalVariable,
    ) -> Option<Def> {
        self.variables.insert(
            name,
            Def {
                kind: DefKind::GlobalVariable(var),
                location,
            },
        )
    }

    fn define_block_variable(
        &mut self,
        name: String,
        location: Option<SourceLocation>,
        var: hir::Value,
    ) -> Option<Def> {
        self.variables.insert(
            name,
            Def {
                kind: DefKind::BlockVariable(var),
                location,
            },
        )
    }
}

//--------------------------------------------------------------------------------------------------

/// HIR value with its type.
///
/// Avoids an indirection to the `types` array in the module.
struct TypedValue {
    ty: hir::Type,
    val: hir::Value,
}

impl TypedValue {
    fn new(val: hir::Value, ty: hir::Type) -> TypedValue {
        TypedValue { ty, val }
    }
}

struct LowerCtxt<'a> {
    current_file: SourceId,
    diag: &'a Diagnostics,
    m: &'a mut hir::Module,
    types: BuiltinTypes,
    error_type: hir::Type,
    error_place_type: hir::Type,
}

impl<'a> LowerCtxt<'a> {
    fn token_loc(&self, token: &SyntaxToken) -> SourceLocation {
        SourceLocation::new(self.current_file, token.text_range())
    }

    fn node_loc(&self, node: &SyntaxNode) -> SourceLocation {
        SourceLocation::new(self.current_file, node.text_range())
    }
}

/// Converts an Ident to a String, or generates one if the Ident is empty.
fn ident_to_string_opt(ident: Option<ast::Ident>) -> String {
    static ANON_IDENT_COUNTER: AtomicUsize = AtomicUsize::new(0);
    ident
        .map(|ident| ident.text().to_string())
        .unwrap_or_else(|| format!("__anon_{}", ANON_IDENT_COUNTER.fetch_add(1, Ordering::Relaxed)))
}

/// Resolved reference to a function.
enum FuncRef {
    /// User-defined function
    User(Function),
    /// Built-in
    Builtin(BuiltinOperation),
}

fn lower_function_call(ctxt: &mut LowerCtxt, func_ref: FuncRef) {}

/// Emits IR for a module.
fn lower_module(ctxt: &mut LowerCtxt, module: ast::Module, builtin_scope: &Scope<'_>) {
    let mut global_scope = Scope::new(Some(&builtin_scope));
    for item in module.items() {
        lower_item(ctxt, item, &mut global_scope)
    }
}

fn lower_item(ctxt: &mut LowerCtxt, item: ast::Item, scope: &mut Scope<'_>) {
    match item {
        ast::Item::FnDef(def) => {
            lower_fn_def(ctxt, def, scope);
        }
        ast::Item::FnDecl(decl) => {
            lower_fn_decl(ctxt, decl, scope);
        }
        ast::Item::Global(global) => {
            let loc = ctxt.node_loc(global.syntax());

            // no diagnostic, this is a syntax error
            let name = ident_to_string_opt(global.name());
            let Some(kind) = global.qualifier().and_then(|q| q.global_kind()) else { return; };
            let ty = lower_type_opt(ctxt, global.ty(), scope);

            match kind {
                Some(ast::QualifierKind::Const) => {
                    let value = if let Some(initializer) = global.initializer() {
                        let Some(initializer) = initializer.expr() else {return;};
                        let loc = ctxt.node_loc(initializer.syntax());
                        if let Some(value) = try_evaluate_constant_expression(ctxt, &initializer, scope, Some(ty)) {
                            ctxt.m.define_constant(value)
                        } else {
                            ctxt.diag
                                .error("could not evaluate constant expression")
                                .primary_label(loc, "")
                                .emit();
                            // dummy
                            ctxt.m.define_constant(ConstantData::Bool(false))
                        }
                    } else {
                        ctxt.diag
                            .error("no initializer provided in constant declaration")
                            .primary_label(loc, "")
                            .emit();
                        // dummy
                        ctxt.m.define_constant(ConstantData::Bool(false))
                    };
                    if let Some(prev_def) = scope.define_constant(name.clone(), Some(loc), value) {
                        ctxt.diag.already_defined(&name, loc, prev_def.location).emit();
                    }
                }
                Some(kind @ ast::QualifierKind::In | ast::QualifierKind::Out | ast::QualifierKind::Uniform) => {
                    if let Some(_) = global.initializer() {
                        ctxt.diag
                            .error("cannot initialize in/out/uniform variables")
                            .primary_label(loc, "")
                            .emit();
                    }
                    // Define global var in HIR
                    let var = ctxt.m.define_global_variable(GlobalVariableData {
                        name: name.text().to_string(),
                        ty,
                        storage_class: match kind {
                            ast::QualifierKind::In => spirv::StorageClass::Input,
                            ast::QualifierKind::Out => spirv::StorageClass::Output,
                            ast::QualifierKind::Uniform => spirv::StorageClass::Uniform,
                        },
                        linkage: None,
                    });
                    // Define global var in scope
                    if let Some(prev_def) = scope.define_global_variable(name.text(), Some(loc), var) {
                        ctxt.diag.already_defined(name.text(), loc, prev_def.location).emit();
                    }
                }
                None => {
                    todo!("global variable with no qualifiers")
                }
            }
        }
        ast::Item::StructDef(def) => {
            todo!()
        }
        ast::Item::ImportDecl(imp) => {
            todo!()
        }
    }
}

#[derive(Debug)]
struct ImplicitConversionError;

enum ImplicitConversionContext {
    Assignment,
    Operation,
}

/// Generates IR for an implicit conversion.
fn lower_implicit_conversion(
    ctxt: &mut LowerCtxt,
    b: &mut hir::FunctionBuilder,
    value: TypedValue,
    ty: hir::Type,
) -> TypedValue {
    if ty == value.ty {
        // same types, no conversion
        return value;
    }

    // all implicit conversions
    let v = match (&ctxt.m.types[value.ty(&ctxt.m)], &ctxt.m.types[ty]) {
        (TypeData::Scalar(tsrc), TypeData::Scalar(tdst)) => match (tsrc, tdst) {
            (ScalarType::Int, ScalarType::UnsignedInt) => b.emit_bitcast(ty, value),
            (ScalarType::Int, ScalarType::Float | ScalarType::Double) => b.emit_convert_s_to_f(ty, value),
            (ScalarType::UnsignedInt, ScalarType::Float | ScalarType::Double) => b.emit_convert_u_to_f(ty, value),
            (ScalarType::Float, ScalarType::Double) => b.emit_f_convert(ty, value),
            _ => {
                // TODO better error message
                ctxt.diag.error("mismatched types").emit();
                b.emit_undef(ty)
            }
        },
        (TypeData::Vector(tsrc, n2), TypeData::Vector(tdst, n1)) if n1 == n2 => match (tsrc, tdst) {
            (ScalarType::Int, ScalarType::UnsignedInt) => b.emit_bitcast(ty, value),
            (ScalarType::Int, ScalarType::Float | ScalarType::Double) => b.emit_convert_s_to_f(ty, value),
            (ScalarType::UnsignedInt, ScalarType::Float | ScalarType::Double) => b.emit_convert_u_to_f(ty, value),
            (ScalarType::Float, ScalarType::Double) => b.emit_f_convert(ty, value),
            _ => {
                ctxt.diag.error("mismatched types").emit();
                b.emit_undef(ty)
            }
        },
        (TypeData::Matrix(tsrc, r1, c1), TypeData::Matrix(tdst, r2, c2)) if r1 == r2 && c1 == c2 => {
            match (tsrc, tdst) {
                (ScalarType::Float, ScalarType::Double) => b.emit_f_convert(ty, value),
                _ => {
                    ctxt.diag.error("mismatched types").emit();
                    b.emit_undef(ty)
                }
            }
        }
        _ => {
            ctxt.diag.error("mismatched types").emit();
            b.emit_undef(ty)
        }
    };

    TypedValue::new(v, ty)
}

/// Generates IR for an expression node, or returns undef if the expression is `None`.
fn lower_expr_opt(
    ctxt: &mut LowerCtxt,
    b: &mut hir::FunctionBuilder,
    expr: Option<ast::Expr>,
    scope: &Scope,
) -> TypedValue {
    if let Some(expr) = expr {
        lower_expr(ctxt, b, expr, scope)
    } else {
        let unk = ctxt.m.ty_unknown();
        b.emit_undef(unk)
    }
}

/// Emits IR for a rvalue binary operation.
///
/// # Arguments
///
/// * op_token the token of the operator; for the same BuiltinOperation, it can be either the operator (e.g. '+'), or the assignment-operator (e.g. '+=')
fn lower_rvalue_bin_expr(
    ctxt: &mut LowerCtxt,
    b: &mut hir::FunctionBuilder,
    operation: BuiltinOperation,
    lhs: TypedValue,
    rhs: TypedValue,
    op_token: &SyntaxToken,
) -> TypedValue {
    match typecheck_builtin_operation(ctxt, operation, &[lhs.ty, rhs.ty]) {
        Ok(result) => {
            // emit implicit conversions
            let converted_lhs = lower_implicit_conversion(ctxt, b, lhs, result.parameter_types[0]);
            let converted_rhs = lower_implicit_conversion(ctxt, b, rhs, result.parameter_types[1]);
            lower_builtin_operation(ctxt, b, operation, converted_lhs, converted_rhs)
        }
        Err(_) => {
            // TODO: show operand types and expected signatures
            let loc = ctxt.token_loc(op_token);
            ctxt.diag
                .error(format!(
                    "invalid operand types to binary operation `{}`",
                    op_token.text()
                ))
                .primary_label("", loc)
                .emit();
            TypedValue::new(b.emit_undef(lhs.ty), lhs.ty)
        }
    }
}

fn lower_bin_expr(
    ctxt: &mut LowerCtxt,
    b: &mut hir::FunctionBuilder,
    bin_expr: ast::BinExpr,
    scope: &Scope,
) -> Option<TypedValue> {
    let Some((op_token, ast_operator)) = bin_expr.op_details() else {
        let unk = ctxt.m.ty_unknown();
        return b.emit_undef(unk).into();
    };

    let conv_arith_op = |arith_op| match arith_op {
        ArithOp::Add => BuiltinOperation::Add,
        ArithOp::Mul => BuiltinOperation::Mul,
        ArithOp::Sub => BuiltinOperation::Sub,
        ArithOp::Div => BuiltinOperation::Div,
        ArithOp::Rem => BuiltinOperation::Rem,
        ArithOp::Shl => BuiltinOperation::Shl,
        ArithOp::Shr => BuiltinOperation::Shr,
        ArithOp::BitXor => BuiltinOperation::BitXor,
        ArithOp::BitOr => BuiltinOperation::BitOr,
        ArithOp::BitAnd => BuiltinOperation::BitAnd,
    };

    // determine if this is an assignment, and the associated operation if there's one
    let is_assignment;
    let operation;
    match ast_operator {
        BinaryOp::LogicOp(logic_op) => {
            is_assignment = false;
            operation = match logic_op {
                LogicOp::And => Some(BuiltinOperation::And),
                LogicOp::Or => Some(BuiltinOperation::Or),
            };
        }
        BinaryOp::ArithOp(arith_op) => {
            is_assignment = false;
            operation = Some(conv_arith_op(arith_op));
        }
        BinaryOp::CmpOp(cmp_op) => {
            is_assignment = false;
            operation = match cmp_op {
                CmpOp::Eq => Some(BuiltinOperation::Eq),
                CmpOp::Ne => Some(BuiltinOperation::Ne),
                CmpOp::Gt => Some(BuiltinOperation::Gt),
                CmpOp::Ge => Some(BuiltinOperation::Ge),
                CmpOp::Lt => Some(BuiltinOperation::Lt),
                CmpOp::Le => Some(BuiltinOperation::Le),
            };
        }
        BinaryOp::Assignment(assign_op) => {
            is_assignment = true;
            operation = match assign_op {
                None => None,
                Some(arith_op) => {
                    operator = Some(conv_arith_op(arith_op));
                }
            };
        }
    };

    let mut rhs = lower_expr_opt(ctxt, b, bin_expr.rhs(), scope);
    if is_assignment {
        let place = lower_place_expr_opt(ctxt, b, bin_expr.lhs(), scope);
        let place_ptr = b.access_chain(place.ty, place.base, &place.indices);
        if let Some(operation) = operation {
            // emit the arithmetic part of the assignment
            // load lhs
            // rhs = lhs + rhs
            let lhs = b.emit_load(place.ty, place_ptr, None);
            rhs = lower_rvalue_bin_expr(ctxt, b, operation, TypedValue::new(lhs, place.ty), rhs, &op_token);
        }
        // implicit conversion to lhs type of assignment
        rhs = lower_implicit_conversion(ctxt, b, rhs, place.ty);
        // emit assignment
        b.emit_store(place_ptr, rhs, None);
        // this expression produces no value
        None
    } else if let Some(operation) = operation {
        Some(lower_rvalue_bin_expr(ctxt, b, operation, lhs, rhs, &op_token))
    } else {
        // should not happen
        unreachable!("no assignment and no operation")
    }
}

fn lower_index_expr(
    ctxt: &mut LowerCtxt,
    b: &mut hir::FunctionBuilder,
    expr: ast::IndexExpr,
    lvalue: bool,
    scope: &Scope,
) -> hir::Value {
    todo!()
}

enum AccessIndex {
    Constant(hir::Constant),
    Value(hir::Value),
}

// TODO this might move to the HIR builder
struct Place {
    base: hir::Value,
    /// Access chain
    indices: Vec<ValueOrConstant>,
    ty: hir::Type,
}

fn error_place(ctxt: &mut LowerCtxt, b: &mut hir::FunctionBuilder) -> Place {
    let err_ty = ctxt.m.error_type();
    let base = b.emit_undef(ctxt.error_place_type);
    Place {
        base,
        indices: vec![],
        ty: err_ty,
    }
}

/// Generates IR for a place expression (lvalue).
///
/// `expr` should be a `IndexExpr`, `PathExpr`, or `FieldExpr`.
///
/// # Return value
///
/// A pointer-typed HIR value representing the place.
fn lower_place_expr(ctxt: &mut LowerCtxt, b: &mut hir::FunctionBuilder, expr: ast::Expr, scope: &Scope) -> Place {
    match expr {
        ast::Expr::IndexExpr(index_expr) => {
            let Some(array) = index_expr.array() else { return error_place(ctxt, b); };
            let Some(index) = index_expr.index() else { return error_place(ctxt, b); };

            let mut array_place = lower_place_expr(ctxt, b, array, scope);

            let elem_type = match ctxt.m.types[array_place.ty] {
                TypeData::Array(t, _) => t,
                TypeData::RuntimeArray(t) => t,
                _ => {
                    ctxt.diag
                        .error("cannot index into value")
                        .primary_label(ctxt.node_loc(&array.syntax()), "")
                        .emit();
                    return error_place(ctxt, b);
                }
            };

            if let Some(const_index) = try_evaluate_constant_expression(ctx, &index, scope, None) {
                let i = ctxt.m.define_constant(const_index);
                array_place.indices.push(ValueOrConstant::Constant(i));
                array_place.ty = elem_type;
            } else {
                // Non-const
                let i = lower_expr(ctxt, b, index, scope);
                eprintln!("**TODO** check type of indices");
                array_place.indices.push(ValueOrConstant::Value(i.val));
                array_place.ty = elem_type;
            }

            array_place
        }
        ast::Expr::PathExpr(path_expr) => {
            let loc = ctxt.node_loc(path_expr.syntax());
            let Some(ident) = path_expr.ident() else { return error_place(ctxt, b); };
            let decl = match scope.resolve(DefNameKind::Variable, ident.text()) {
                Some(def) => {
                    match def.kind {
                        DefKind::Constant(_) => {
                            ctxt.diag
                                .error("cannot use a constant as an lvalue")
                                .primary_label(loc, "")
                                .emit();
                            return error_place(ctxt, b);
                        }
                        DefKind::GlobalVariable(global_variable) => {
                            eprintln!("**TODO** check global variables as place expressions");
                            let ty = ctxt.m.globals[global_variable].ty;
                            // TODO storage class?
                            let result_type = ctxt.m.define_type(TypeData::Pointer {
                                pointee_type: ty,
                                storage_class: spirv::StorageClass::Uniform,
                            });
                            let ptr = b.access_global(result_type, global_variable);
                            Place {
                                ty,
                                indices: vec![],
                                base: ptr,
                            }
                        }
                        DefKind::BlockVariable(_) => {}
                        _ => unreachable!(),
                    }
                }
                None => {
                    ctxt.diag
                        .error(format!("could not find variable `{}` in scope", ident.text()))
                        .primary_label(loc, "")
                        .emit();
                }
            };

            todo!("path place expr")
        }
        ast::Expr::FieldExpr(field_expr) => {
            todo!("field place expr")
        }
        _ => {
            // not a valid place expression, create a value of type "pointer to unknown" so that compilation can proceed
            todo!("dummy place expr")
        }
    }
}

fn lower_place_expr_opt(
    ctxt: &mut LowerCtxt,
    b: &mut hir::FunctionBuilder,
    expr: Option<ast::Expr>,
    scope: &Scope,
) -> Place {
    if let Some(expr) = expr {
        lower_place_expr(ctxt, b, expr, scope)
    } else {
        error_place(ctxt, b)
    }
}

/// Generates IR for an expression node.
///
/// # Return value
///
/// The IR value of the expression
fn lower_expr(ctxt: &mut LowerCtxt, b: &mut hir::FunctionBuilder, expr: ast::Expr, scope: &Scope) -> TypedValue {
    let loc = ctxt.node_loc(expr.syntax());
    match expr {
        ast::Expr::BinExpr(bin_expr) => lower_bin_expr(ctxt, b, bin_expr, scope),
        ast::Expr::CallExpr(call_expr) => {
            let func = lower_expr_opt(ctxt, b, call_expr.func(), scope);
            let mut args = Vec::new();
            if let Some(arg_list) = call_expr.arg_list() {
                for arg in arg_list.arguments() {
                    args.push(lower_expr(ctxt, b, arg, scope));
                }
            }
            b.emit_call(func, args, ctxt.node_loc(call_expr.syntax()))
        }
        ast::Expr::IndexExpr(_) | ast::Expr::PathExpr(_) | ast::Expr::FieldExpr(_) => {
            // lower place expression and dereference
            let place = lower_place_expr(ctxt, b, expr, scope);
            let (ty, _storage_class) = place
                .ty(ctxt.m)
                .pointee_type(ctxt.m)
                .expect("lower_place_expr did not produce a pointer value");
            b.emit_load(ty, place, None)
        }
        ast::Expr::ParenExpr(expr) => lower_expr_opt(ctxt, b, expr.expr(), scope),
        ast::Expr::LitExpr(lit) => match lit.kind() {
            ast::LiteralKind::String(str) => {
                todo!()
            }
            ast::LiteralKind::IntNumber(v) => {
                if let Some(value) = v.value() {
                    let c = b.int_const(value as i64);
                    b.emit_constant(c, loc)
                } else {
                    b.undef()
                }
            }
            ast::LiteralKind::FloatNumber(v) => {
                if let Some(value) = v.value() {
                    let c = b.fp_const(value);
                    b.emit_constant(c, loc)
                } else {
                    b.undef()
                }
            }
            ast::LiteralKind::Bool(v) => {
                let c = b.bool_const(v);
                b.emit_constant(c, loc)
            }
        },
        ast::Expr::TupleExpr(_) => {
            todo!("tuple expressions")
        }
        ast::Expr::ArrayExpr(_) => {
            todo!("array expressions")
        }
        ast::Expr::PrefixExpr(_) => {}
    }
}

fn evaluate_constant_expression(
    ctx: &mut LowerCtxt,
    expr: &ast::Expr,
    scope: &Scope,
    expected_type: Option<hir::Type>,
) -> ConstantData {
    if let Some(data) = try_evaluate_constant_expression(ctx, expr, scope, expected_type) {
        data
    } else {
        let node_loc = ctx.node_loc(expr.syntax());
        ctx.diag.consteval_failure(node_loc)
    }
}

/// Emits IR for a type reference.
fn lower_type(ctxt: &mut LowerCtxt, ty: ast::Type, scope: &Scope) -> hir::Type {
    match ty {
        ast::Type::TypeRef(tyref) => {
            let Some(ident) = tyref.ident() else { return ctxt.m.error_type(); };
            if let Some(def) = scope.resolve(DefNameKind::Type, ident.text()) {
                match def.kind {
                    DefKind::Type(ty) => ty,
                    _ => unreachable!(),
                }
            } else {
                ctxt.diag.cannot_find_type(ident.text(), ctxt.node_loc(tyref.syntax()));
                m.error_type()
            }
        }
        ast::Type::TupleType(tuple_ty) => {
            let fields = tuple_ty.fields().map(|f| lower_type(ctxt, f, scope));
            m.define_struct_type(None, fields)
        }
        ast::Type::ArrayType(array_type) => {
            let element_type = lower_type_opt(ctxt, array_type.element_type(), scope);
            if let Some(expr) = array_type.length() {
                let len_ty = m.define_type(TypeData::Scalar(ScalarType::UnsignedInt));
                let length = evaluate_constant_expression(ctxt, &expr, scope, Some(len_ty));
                let length = match length {
                    ConstantData::U32(len) => len,
                    _ => {
                        return m.error_type();
                    }
                };
                m.define_type(TypeData::Array(element_type, length))
            } else {
                // no length
                m.define_type(TypeData::RuntimeArray(element_type))
            }
        }
        ast::Type::ClosureType(closure_type) => {
            todo!("closure types")
        }
    }
}

fn lower_type_opt(ctxt: &mut LowerCtxt, ty: Option<ast::Type>, scope: &Scope) -> hir::Type {
    if let Some(ty) = ty {
        lower_type(ctxt, ty, scope)
    } else {
        b.ty_unknown()
    }
}

/// Emits IR for a block statement.
fn lower_stmt(ctxt: &mut LowerCtxt, b: &mut hir::FunctionBuilder, stmt: ast::Stmt, scope: &mut Scope) {
    match stmt {
        ast::Stmt::ExprStmt(expr) => {
            lower_expr_opt(ctxt, b, expr.expr(), scope);
        }
        ast::Stmt::ReturnStmt(stmt) => {
            if let Some(expr) = stmt.expr() {
                let value = lower_expr(ctxt, b, expr, scope);
                b.ret_value(value);
            } else {
                // return void
                b.ret();
            }
        }
        ast::Stmt::WhileStmt(stmt) => {}
        ast::Stmt::BreakStmt(stmt) => {}
        ast::Stmt::ContinueStmt(stmt) => {}
        ast::Stmt::DiscardStmt(stmt) => {}
        ast::Stmt::LocalVariable(v) => {}
        ast::Stmt::IfStmt(if_stmt) => {}
    }
}

/// Emits IR for a block.
fn lower_block(ctxt: &mut LowerCtxt, b: &mut hir::FunctionBuilder, block: ast::Block, scope: &Scope) {
    // start block scope
    let mut block_scope = Scope::new(Some(scope));
    for stmt in block.stmts() {
        lower_stmt(ctxt, b, stmt, &mut block_scope)
    }
}

/// Lowers a function definition or declaration
fn lower_function(
    ctxt: &mut LowerCtxt,
    name: String,
    loc: SourceLocation,
    extern_: Option<ast::Extern>,
    param_list: Option<ast::ParamList>,
    ret_type: Option<ast::RetType>,
    block: Option<ast::Block>,
    scope: &mut Scope,
) -> hir::Function {
    // build parameter list
    let mut params = vec![];
    let mut arg_types = vec![];
    if let Some(param_list) = param_list {
        for param in param_list.parameters() {
            let arg_ty = lower_type_opt(ctxt, param.ty(), scope);
            arg_types.push(arg_ty);
            params.push(hir::FunctionParameter {
                name: param.ident().map(|id| id.text().to_string()).unwrap_or_default(),
                ty,
            });
        }
    }

    // return type
    let return_type = if let Some(ret_type) = ret_type {
        lower_type_opt(ctxt, ret_type.ty(), scope)
    } else {
        // no trailing return type in the AST (`-> Type`), so return unit.
        ctxt.types.void
    };

    // create function type
    // TODO: we're allocating an Arc even if the function type is already interned; if that has
    // a measurable impact, figure out a way to intern a type from a non-owned TypeData.
    let func_type = ctxt
        .m
        .define_type(TypeData::Function(Arc::new(FunctionType { arg_types, return_type })));

    // function linkage
    let linkage = if extern_.is_some() {
        if block.is_some() {
            // "extern" on a function definition => export
            Some(spirv::LinkageType::Export)
        } else {
            // "extern" on a function declaration => import
            Some(spirv::LinkageType::Import)
        }
    } else {
        None
    };

    // if there's a block, then it's a function definition, otherwise it's just a declaration
    let func_data = if let Some(block) = block {
        let (mut func_data, entry_block_id) =
            FunctionData::new(name.clone(), func_type, params, linkage, spirv::FunctionControl::NONE);
        let mut builder = FunctionBuilder::new(&mut func_data, entry_block_id);
        lower_block(ctxt, &mut builder, block, scope);
        func_data
    } else {
        FunctionData::new_declaration(name.clone(), func_type, params, linkage, spirv::FunctionControl::NONE)
    };

    let func = ctxt.m.add_function(func_data);

    // insert function in scope
    scope.define_function(name, Some(loc), FuncRef::User(func));
    func
}

/// Lowers a function declaration.
fn lower_fn_decl(ctxt: &mut LowerCtxt, fn_decl: ast::FnDecl, scope: &mut Scope) -> hir::Function {
    let loc = ctxt.node_loc(fn_decl.syntax());
    let name = ident_to_string_opt(fn_decl.name());
    lower_function(
        ctxt,
        name,
        loc,
        fn_decl.extern_(),
        fn_decl.param_list(),
        fn_decl.ret_type(),
        fn_decl.block(),
        scope,
    )
}

/// Emits IR for a function definition.
fn lower_fn_def(ctxt: &mut LowerCtxt, fn_def: ast::FnDef, scope: &mut Scope) -> hir::Function {
    let loc = ctxt.node_loc(fn_def.syntax());
    let name = ident_to_string_opt(fn_def.name());
    lower_function(
        ctxt,
        name,
        loc,
        fn_def.extern_(),
        fn_def.param_list(),
        fn_def.ret_type(),
        fn_def.block(),
        scope,
    )
}

pub fn lower(
    m: &mut hir::Module,
    ast: ast::Module,
    file: SourceId,
    source_files: SourceFileProvider,
    diag_writer: &mut dyn WriteColor,
) {
    let diag = Diagnostics::new(source_files, diag_writer, term::Config::default());
    let mut root_scope = Scope::new(None);
    let builtin_types = register_builtins(m, &mut root_scope);
    let mut ctxt = LowerCtxt {
        m,
        diag: &diag,
        current_file: file,
        types: builtin_types,
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_lower() {}
}
