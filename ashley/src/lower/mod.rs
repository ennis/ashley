//! AST lowering to SPIR-V
mod builtin;
mod consteval;
mod mangle;
mod swizzle;
mod typecheck;
mod tast;

use crate::{
    diagnostic::{AsSourceLocation, DiagnosticBuilder, Diagnostics, SourceFileProvider, SourceId, SourceLocation},
    hir,
    hir::{
        types::{Field, FunctionType, ScalarType, StructType},
        ConstantData, FunctionBuilder, FunctionData, IdRef, Module, TypeData,
    },
    lower::{
        builtin::{register_builtin_types, BuiltinTypes},
        consteval::try_evaluate_constant_expression,
        typecheck::{
            check_signature, lower_builtin_operation, register_builtin_operations, typecheck_builtin_operation,
            BuiltinOperation,
        },
    },
    syntax::{ast, ast::AstNode, ArithOp, BinaryOp, CmpOp, LogicOp, SyntaxToken},
};
use ashley::{
    lower::typecheck::{OverloadCandidate, SignatureMismatch},
    syntax::UnaryOp,
};
use codespan_reporting::{term, term::termcolor::WriteColor};
use rspirv::spirv;
use smallvec::SmallVec;
use std::{
    borrow::Cow,
    collections::HashMap,
    num::ParseFloatError,
    sync::atomic::{AtomicUsize, Ordering},
};
use spirv::StorageClass;
use ashley::syntax::ast::Stmt;
use crate::lower::swizzle::ComponentIndices;

trait DiagnosticsExt {
    /// "missing generic argument"
    fn missing_generic_argument(&self, instantiation_loc: SourceLocation, expected_forms: &str) -> DiagnosticBuilder;

    /// "extra generic argument(s)"
    fn extra_generic_arguments(
        &self,
        instantiation_loc: SourceLocation,
        expected: usize,
        got: usize,
    ) -> DiagnosticBuilder;

    /// "an item with the same name has already been defined"
    fn already_defined(&self, name: &str, loc: SourceLocation, prev_loc: Option<SourceLocation>) -> DiagnosticBuilder;

    /// "cannot find type `{name}` in this scope"
    fn cannot_find_type(&self, name: &str, loc: SourceLocation) -> DiagnosticBuilder;

    /// "cannot find value `{name}` in this scope"
    fn cannot_find_value(&self, name: &str, loc: SourceLocation) -> DiagnosticBuilder;

    /// "could not evaluate constant expression"
    fn consteval_failure(&self, loc: SourceLocation) -> DiagnosticBuilder;
}

impl DiagnosticsExt for Diagnostics {
    fn missing_generic_argument(&self, instantiation_loc: SourceLocation, expected_forms: &str) -> DiagnosticBuilder {
        self.error("missing generic argument")
            .primary_label(instantiation_loc, "")
            .note(format!("expected: `{expected_forms}`"))
    }

    fn extra_generic_arguments(
        &self,
        instantiation_loc: SourceLocation,
        expected: usize,
        got: usize,
    ) -> DiagnosticBuilder {
        self.error("too many generic arguments").primary_label(
            instantiation_loc,
            format!("expected {expected} argument(s), got {got} argument(s)"),
        )
    }

    fn already_defined(&self, name: &str, loc: SourceLocation, prev_loc: Option<SourceLocation>) -> DiagnosticBuilder {
        let mut diag = self
            .error(format!(
                "`{name}`an item with the same name has already been defined in this scope"
            ))
            .primary_label(loc, "");
        if let Some(prev_loc) = prev_loc {
            diag = diag.secondary_label(prev_loc, "previous definition here");
        }
        diag
    }

    fn cannot_find_type(&self, name: &str, loc: SourceLocation) -> DiagnosticBuilder {
        self.error(format!("cannot find type `{name}` in this scope"))
            .primary_label(loc, "")
    }

    fn cannot_find_value(&self, name: &str, loc: SourceLocation) -> DiagnosticBuilder {
        self.error(format!("cannot find value `{name}` in this scope"))
            .primary_label(loc, "")
    }

    fn consteval_failure(&self, loc: SourceLocation) -> DiagnosticBuilder {
        self.error("could not evaluate constant expression")
            .primary_label(loc, "")
    }
}

//--------------------------------------------------------------------------------------------------

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum DefNameKind {
    Type,
    Variable,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct DefName {
    kind: DefNameKind,
    name: String,
}

#[derive(Copy, Clone, Debug)]
enum DefKind {
    Type(hir::Type),
    Function(FuncRef),
    Constant(hir::Constant),
    GlobalVariable(hir::GlobalVariable),
    BlockVariable(hir::Local),
}

struct Def {
    location: Option<SourceLocation>,
    kind: DefKind,
}

struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    types: HashMap<String, Def>,
    variables_and_functions: HashMap<String, Def>,
}

impl<'a> Scope<'a> {
    fn new(parent: Option<&'a Scope<'a>>) -> Scope {
        Scope {
            parent,
            types: Default::default(),
            variables_and_functions: Default::default(),
        }
    }

    fn resolve(&self, kind: DefNameKind, name: &str) -> Option<&Def> {
        let def = match kind {
            DefNameKind::Type => self.types.get(name),
            DefNameKind::Variable => self.variables_and_functions.get(name),
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
        self.variables_and_functions.insert(
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
        self.variables_and_functions.insert(
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
        self.variables_and_functions.insert(
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
        var: hir::Local,
    ) -> Option<Def> {
        self.variables_and_functions.insert(
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
// TODO: move into HIR builder?
#[derive(Copy, Clone)]
struct TypedValue {
    ty: hir::Type,
    val: hir::IdRef,
}

impl TypedValue {
    fn new(val: impl Into<IdRef>, ty: hir::Type) -> TypedValue {
        TypedValue { ty, val: val.into() }
    }
}

#[derive(Debug)]
struct LowerError;

type ValueResult = Result<TypedValue, LowerError>;

/// Lowers a `ValueResult` into a hir::Value or hir::Constant, or synthesizes an undef value in case
/// the value is an error.
fn lower_value_opt(fb: &mut FunctionBuilder, v: Option<TypedValue>, loc: SourceLocation) -> TypedValue {
    match v {
        Some(typed_value) => typed_value,
        None => {
            let ty = fb.error_type();
            let undef = fb.emit_undef(ty);
            TypedValue::new(undef, ty)
        }
    }
}

struct LowerCtxt<'a> {
    diag: &'a Diagnostics,
    types: BuiltinTypes,
    error_type: hir::Type,
    error_place_type: hir::Type,
}

/// Converts an Ident to a String, or generates one if the Ident is empty.
fn ident_to_string_opt(ident: Option<ast::Ident>) -> String {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    ident
        .map(|ident| ident.text().to_string())
        .unwrap_or_else(|| format!("__anon_{}", COUNTER.fetch_add(1, Ordering::Relaxed)))
}

#[derive(Copy, Clone, Debug)]
/// Resolved reference to a function.
enum FuncRef {
    /// User-defined function
    User(hir::Function),
    /// Built-in
    Builtin(BuiltinOperation),
}

fn error_value(fb: &mut FunctionBuilder) -> TypedValue {
    let ty = fb.ty_unknown();
    TypedValue::new(fb.emit_undef(ty), ty)
}

/// Emits IR for a module.
fn lower_module(ctxt: &mut LowerCtxt, m: &mut hir::Module, ast: ast::Module, builtin_scope: &Scope<'_>) {
    let mut global_scope = Scope::new(Some(&builtin_scope));
    for item in ast.items() {
        lower_item(ctxt, m, item, &mut global_scope)
    }
}

fn lower_item(ctxt: &mut LowerCtxt, m: &mut hir::Module, item: ast::Item, scope: &mut Scope<'_>) {
    match item {
        ast::Item::FnDef(def) => {
            lower_fn_def(ctxt, m, def, scope);
        }
        ast::Item::FnDecl(decl) => {
            lower_fn_decl(ctxt, m, decl, scope);
        }
        ast::Item::Global(global) => {
            // no diagnostic, this is a syntax error
            let name = ident_to_string_opt(global.name());
            let Some(kind) = global.qualifier().and_then(|q| q.global_kind()) else { return; };
            let ty = lower_type_opt(ctxt, m, global.ty(), scope);

            match kind {
                ast::QualifierKind::Const => {
                    let value = if let Some(initializer) = global.initializer() {
                        let Some(initializer) = initializer.expr() else {return;};
                        if let Some(value) = try_evaluate_constant_expression(ctxt.diag, &initializer, scope, Some(ty))
                        {
                            m.define_constant(value)
                        } else {
                            ctxt.diag
                                .error("could not evaluate constant expression")
                                .primary_label(&initializer, "")
                                .emit();
                            // dummy
                            m.define_constant(ConstantData::Bool(false))
                        }
                    } else {
                        ctxt.diag
                            .error("no initializer provided in constant declaration")
                            .primary_label(&global, "")
                            .emit();
                        // dummy
                        m.define_constant(ConstantData::Bool(false))
                    };
                    if let Some(prev_def) = scope.define_constant(name.clone(), Some(global.source_location()), value) {
                        ctxt.diag
                            .already_defined(&name, global.source_location(), prev_def.location)
                            .emit();
                    }
                }
                ast::QualifierKind::In | ast::QualifierKind::Out | ast::QualifierKind::Uniform => {
                    if let Some(_) = global.initializer() {
                        ctxt.diag
                            .error("cannot initialize in/out/uniform variables")
                            .primary_label(&global, "")
                            .emit();
                    }
                    // Define global var in HIR
                    let var = m.define_global_variable(hir::GlobalVariableData {
                        name: name.clone(),
                        ty,
                        storage_class: match kind {
                            ast::QualifierKind::In => spirv::StorageClass::Input,
                            ast::QualifierKind::Out => spirv::StorageClass::Output,
                            ast::QualifierKind::Uniform => spirv::StorageClass::Uniform,
                            _ => unreachable!(),
                        },
                        linkage: None,
                    });
                    // Define global var in scope
                    if let Some(prev_def) =
                        scope.define_global_variable(name.clone(), Some(global.source_location()), var)
                    {
                        ctxt.diag
                            .already_defined(&name, global.source_location(), prev_def.location)
                            .emit();
                    }
                }
            }
        }
        ast::Item::StructDef(_def) => {
            todo!()
        }
        ast::Item::ImportDecl(_imp) => {
            todo!()
        }
    }
}

/// Generates IR for an implicit conversion.
///
/// TODO: provide context for error messages
fn lower_implicit_conversion(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    value: TypedValue,
    loc: SourceLocation,
    ty: hir::Type,
) -> TypedValue {
    if ty == value.ty {
        // same types, no conversion
        return value;
    }

    // all implicit conversions
    use hir::TypeData as TD;

    let v = match (&fb.types[value.ty], &fb.types[ty]) {
        (TD::Scalar(tsrc), TD::Scalar(tdst)) => match (tsrc, tdst) {
            (ScalarType::Int, ScalarType::UnsignedInt) => fb.emit_bitcast(ty, value.val),
            (ScalarType::Int, ScalarType::Float | ScalarType::Double) => fb.emit_convert_s_to_f(ty, value.val),
            (ScalarType::UnsignedInt, ScalarType::Float | ScalarType::Double) => fb.emit_convert_u_to_f(ty, value.val),
            (ScalarType::Float, ScalarType::Double) => fb.emit_f_convert(ty, value.val),
            _ => {
                // TODO better error message
                ctxt.diag.error("mismatched types").primary_label(loc, "").emit();
                fb.emit_undef(ty)
            }
        },
        (TD::Vector(tsrc, n2), TD::Vector(tdst, n1)) if n1 == n2 => match (tsrc, tdst) {
            (ScalarType::Int, ScalarType::UnsignedInt) => fb.emit_bitcast(ty, value.val),
            (ScalarType::Int, ScalarType::Float | ScalarType::Double) => fb.emit_convert_s_to_f(ty, value.val),
            (ScalarType::UnsignedInt, ScalarType::Float | ScalarType::Double) => fb.emit_convert_u_to_f(ty, value.val),
            (ScalarType::Float, ScalarType::Double) => fb.emit_f_convert(ty, value.val),
            _ => {
                ctxt.diag.error("mismatched types").primary_label(loc, "").emit();
                fb.emit_undef(ty)
            }
        },
        (
            TD::Matrix {
                component_type: tsrc,
                rows: r1,
                columns: c1,
            },
            TD::Matrix {
                component_type: tdst,
                rows: r2,
                columns: c2,
            },
        ) if r1 == r2 && c1 == c2 => match (tsrc, tdst) {
            (ScalarType::Float, ScalarType::Double) => fb.emit_f_convert(ty, value.val),
            _ => {
                ctxt.diag.error("mismatched types").primary_label(loc, "").emit();
                fb.emit_undef(ty)
            }
        },
        _ => {
            ctxt.diag.error("mismatched types").primary_label(loc, "").emit();
            fb.emit_undef(ty)
        }
    };

    TypedValue::new(v, ty)
}

/// Generates IR for an expression node, or returns undef if the expression is `None`.
fn lower_expr_opt(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    expr: Option<&ast::Expr>,
    scope: &Scope,
) -> Option<TypedValue> {
    if let Some(expr) = expr {
        lower_expr(ctxt, fb, expr, scope)
    } else {
        Some(error_value(fb))
    }
}

fn lower_expr_opt_non_void(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    expr: Option<&ast::Expr>,
    scope: &Scope,
) -> TypedValue {
    if let Some(expr) = expr {
        let loc = expr.source_location();
        let result = lower_expr(ctxt, fb, expr, scope);
        match result {
            None => {
                ctxt.diag
                    .error("expression cannot have void type")
                    .primary_label(loc, "")
                    .emit();
                error_value(fb)
            }
            Some(value) => value,
        }
    } else {
        error_value(fb)
    }
}

/*fn lower_builtin_operation(ctxt: &mut LowerCtxt,
                           fb: &mut FunctionBuilder,
                           operation: BuiltinOperation,
                           args: &[TypedValue],
                           arg_locations: &[SourceLocation])
{
    let mut types = SmallVec::<[hir::Type;2]>::new();
    for arg in args.iter() {
        types.push(arg.ty);
    }

    match typecheck_builtin_operation(fb, &ctxt.types, operation, &types) {
        Ok(result) => {
            // emit implicit conversions
            let mut imù
            let _converted_lhs = lower_implicit_conversion(ctxt, fb, lhs, lhs_loc, result.parameter_types[0])?;
            let _converted_rhs = lower_implicit_conversion(ctxt, fb, rhs, rhs_loc, result.parameter_types[1])?;
            todo!("lower builtin op")
            //lower_builtin_operation(ctxt, b, operation, converted_lhs, converted_rhs)
        }
        Err(_) => {
            // TODO: show operand types and expected signatures
            ctxt.diag
                .error(format!(
                    "invalid operand types to binary operation `{}`",
                    op_token.text()
                ))
                .primary_label(op_token, "")
                .emit();
            Err(LowerError)
        }
    }
}*/

/// Emits IR for a rvalue binary operation.
///
/// # Arguments
///
/// * op_token the token of the operator; for the same BuiltinOperation, it can be either the operator (e.g. '+'), or the assignment-operator (e.g. '+=')
fn lower_rvalue_bin_expr(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    operation: BuiltinOperation,
    lhs: TypedValue,
    lhs_loc: SourceLocation,
    rhs: TypedValue,
    rhs_loc: SourceLocation,
    op_token: &SyntaxToken,
) -> TypedValue {
    match typecheck_builtin_operation(fb, &ctxt.types, operation, &[lhs.ty, rhs.ty]) {
        Ok(result) => {
            // emit implicit conversions
            let converted_lhs = lower_implicit_conversion(ctxt, fb, lhs, lhs_loc, result.parameter_types[0]);
            let converted_rhs = lower_implicit_conversion(ctxt, fb, rhs, rhs_loc, result.parameter_types[1]);
            lower_builtin_operation(
                ctxt,
                fb,
                operation,
                result.index,
                &[converted_lhs.val, converted_rhs.val],
                &[result.parameter_types[0], result.parameter_types[1]],
                result.result_type,
            )
        }
        Err(_) => {
            // TODO: show operand types and expected signatures
            ctxt.diag
                .error(format!(
                    "invalid operand types to binary operation `{}`",
                    op_token.text()
                ))
                .primary_label(op_token, "")
                .emit();
            error_value(fb)
        }
    }
}


fn lower_prefix_expr(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    prefix_expr: &ast::PrefixExpr,
    scope: &Scope,
) -> TypedValue {
    let Some((op_token, ast_operator)) = prefix_expr.op_details() else {
        return error_value(fb);
    };

    let operation = match ast_operator {
        UnaryOp::Not => BuiltinOperation::Not,
        UnaryOp::Neg => BuiltinOperation::UnaryMinus,
    };

    let Some(ast_expr) = prefix_expr.expr() else {
        return error_value(fb);
    };
    let operand = lower_expr_opt_non_void(ctxt, fb, Some(&ast_expr), scope);

    match typecheck_builtin_operation(fb, &ctxt.types, operation, &[operand.ty]) {
        Ok(result) => {
            let converted_operand = lower_implicit_conversion(ctxt, fb, operand, ast_expr.source_location(), operand.ty);
            lower_builtin_operation(
                ctxt,
                fb,
                operation,
                result.index,
                &[converted_operand.val],
                &[converted_operand.ty],
                result.result_type,
            )
        }
        Err(_) => {
            ctxt.diag
                .error(format!(
                    "invalid operand types to unary operation `{}`",
                    op_token.text()
                ))
                .primary_label(op_token, "")
                .emit();
            error_value(fb)
        }
    }
}

fn lower_bin_expr(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    bin_expr: &ast::BinExpr,
    scope: &Scope,
) -> Option<TypedValue> {
    let Some((op_token, ast_operator)) = bin_expr.op_details() else {
        // syntax error
        return Some(error_value(fb));
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
                Some(arith_op) => Some(conv_arith_op(arith_op)),
            };
        }
    };

    let Some(ast_lhs) = bin_expr.lhs() else { return Some(error_value(fb)) };
    let Some(ast_rhs) = bin_expr.rhs() else { return Some(error_value(fb)) };
    let lhs_loc = ast_lhs.source_location();
    let rhs_loc = ast_rhs.source_location();

    let mut rhs = lower_expr_opt_non_void(ctxt, fb, Some(&ast_rhs), scope);

    if is_assignment {
        let place = lower_place_expr(ctxt, fb, &ast_lhs, scope);
        let (deref_ty, _) = place.deref_ty(fb);
        let place_ptr = fb.access_chain(place.ptr_ty, place.base, &place.indices);
        if let Some(operation) = operation {
            // emit the arithmetic part of the assignment
            // load lhs
            // rhs = lhs + rhs
            let lhs = fb.emit_load(deref_ty, place_ptr.into(), None);
            rhs = lower_rvalue_bin_expr(
                ctxt,
                fb,
                operation,
                TypedValue::new(lhs, deref_ty),
                lhs_loc,
                rhs,
                rhs_loc,
                &op_token,
            );
        }
        // implicit conversion to lhs type of assignment
        rhs = lower_implicit_conversion(ctxt, fb, rhs, ast_rhs.source_location(), deref_ty);
        // emit assignment
        fb.emit_store(place_ptr.into(), rhs.val, None);
        // this expression produces no value
        None
    } else if let Some(operation) = operation {
        let lhs = lower_expr_opt_non_void(ctxt, fb, Some(&ast_lhs), scope);
        Some(lower_rvalue_bin_expr(
            ctxt, fb, operation, lhs, lhs_loc, rhs, rhs_loc, &op_token,
        ))
    } else {
        // should not happen
        unreachable!("no assignment and no operation")
    }
}

fn lower_index_expr(
    _ctxt: &mut LowerCtxt,
    _func: &mut FunctionBuilder,
    _expr: ast::IndexExpr,
    _lvalue: bool,
    _scope: &Scope,
) -> hir::Value {
    todo!()
}

enum AccessIndex {
    Constant(hir::Constant),
    Value(hir::Value),
}

#[derive(Copy, Clone)]
enum PlaceBase {
    /// Pointer value
    Value(hir::Value),
    Global(hir::GlobalVariable),
    // FIXME: not sure there are pointer constants
    Constant(hir::Constant),
}

impl From<hir::Value> for PlaceBase {
    fn from(value: hir::Value) -> Self {
        PlaceBase::Value(value)
    }
}

impl From<hir::GlobalVariable> for PlaceBase {
    fn from(value: hir::GlobalVariable) -> Self {
        PlaceBase::Global(value)
    }
}

impl From<hir::Constant> for PlaceBase {
    fn from(value: hir::Constant) -> Self {
        PlaceBase::Constant(value)
    }
}

// TODO this might move to the HIR builder
struct Place {
    base: hir::IdRef,
    /// Access chain
    indices: Vec<hir::IdRef>,
    /// The type of base + indices access chain (a pointer type).
    ptr_ty: hir::Type,
}

impl Place {
    fn deref_ty(&self, m: &mut Module) -> (hir::Type, spirv::StorageClass) {
        self.ptr_ty.pointee_type(m).expect("expected pointer type")
    }
}

fn error_place(fb: &mut FunctionBuilder) -> Place {
    let err_ty = fb.error_type();
    let err_ptr_ty = fb.define_type(TypeData::Pointer {
        pointee_type: err_ty,
        storage_class: spirv::StorageClass::Function,
    });
    let base = fb.emit_undef(err_ptr_ty);
    Place {
        base: base.into(),
        indices: vec![],
        ptr_ty: err_ptr_ty,
    }
}

enum FieldProjection {
    Index(usize),
    Swizzle()
}

fn resolve_field(ctxt: &mut LowerCtxt, m: &Module, ty: hir::Type, ident: &str) -> ComponentIndices {
    match m.types[ty] {
        TypeData::Vector(scalar_type, len) => {
            // swizzle expr

        }
        TypeData::Struct()
    }
}

/// Generates IR for a place expression (lvalue).
///
/// `expr` should be a `IndexExpr`, `PathExpr`, or `FieldExpr`.
///
/// # Return value
///
/// A pointer-typed HIR value representing the place.
fn lower_place_expr(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    expr: &ast::Expr,
    scope: &Scope,
) -> Place {
    match expr {
        // `place[index]`
        ast::Expr::IndexExpr(index_expr) => {
            let Some(array) = index_expr.array() else { return error_place(fb) };
            let Some(index) = index_expr.index()else { return error_place(fb) };

            let mut array_place = lower_place_expr(ctxt, fb, &array, scope);
            let (array_ty, storage_class) = array_place.deref_ty(fb);

            let elem_type = match fb.types[array_ty] {
                TypeData::Array(t, _) => t,
                TypeData::RuntimeArray(t) => t,
                // TODO: index into vectors
                _ => {
                    ctxt.diag
                        .error("cannot index into value")
                        .location(&array)
                        .emit();
                    return error_place(fb);
                }
            };
            let elem_ptr_type = fb.pointer_type(elem_type, storage_class);

            if let Some(const_index) = try_evaluate_constant_expression(ctxt.diag, &index, scope, None) {
                let i = fb.define_constant(const_index);
                array_place.indices.push(i.into());
                array_place.ptr_ty = elem_ptr_type;
            } else {
                // Non-const
                let i = lower_expr_opt_non_void(ctxt, fb, Some(&index), scope);
                eprintln!("**TODO** check type of indices");
                // can't be void
                array_place.indices.push(i.val);
                array_place.ptr_ty = elem_ptr_type;
            }

            array_place
        }
        // path reference
        ast::Expr::PathExpr(path_expr) => {
            let Some(ident) = path_expr.ident() else { return error_place(fb) };
            match scope.resolve(DefNameKind::Variable, ident.text()) {
                Some(def) => {
                    match def.kind {
                        DefKind::Constant(_) => {
                            ctxt.diag
                                .error("cannot use a constant as an lvalue")
                                .location(&path_expr)
                                .emit();
                            error_place(fb)
                        }
                        DefKind::GlobalVariable(global_variable) => {
                            eprintln!("**TODO** check global variables as place expressions");
                            let ty = fb.globals[global_variable].ty;
                            let storage_class = fb.globals[global_variable].storage_class;
                            // TODO: global
                            let ptr_ty = fb.pointer_type(ty, storage_class);

                            Place {
                                ptr_ty,
                                indices: vec![],
                                base: global_variable.into(),
                            }
                        }
                        DefKind::BlockVariable(var) => {
                            eprintln!("**TODO** check block variables as place expressions");
                            // already of pointer type
                            let ty = fb.local_type(var);
                            let ptr_ty = fb.pointer_type(ty, StorageClass::Function);
                            Place {
                                base: var.into(),
                                indices: vec![],
                                ptr_ty,
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                None => {
                    ctxt.diag
                        .error(format!("could not find variable `{}` in scope", ident.text()))
                        .primary_label(&ident, "")
                        .emit();
                    error_place(fb)
                }
            }
        }
        // `value.field` or swizzle patterns
        ast::Expr::FieldExpr(field_expr) => {

            let Some(expr) = field_expr.expr() else { return error_place(fb) };
            let Some(field_ident) = field_expr.field() else {return error_place(fb) };

            let left_place = lower_place_expr(ctxt, fb, &expr, scope);
            let place_ty =

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
    fb: &mut FunctionBuilder,
    expr: Option<&ast::Expr>,
    scope: &Scope,
) -> Place {
    if let Some(expr) = expr {
        lower_place_expr(ctxt, fb, &expr, scope)
    } else {
        error_place(fb)
    }
}

/// Lowers a function reference.
fn resolve_function_ref(ctxt: &mut LowerCtxt, expr: Option<&ast::Expr>, scope: &Scope) -> Option<FuncRef> {
    let Some(expr) = expr else { return None };
    match expr {
        ast::Expr::PathExpr(p) => {
            let ident_str = ident_to_string_opt(p.ident());
            let Some(def) = scope.resolve(DefNameKind::Variable, &ident_str) else {
                ctxt.diag.error(format!("unresolved identifier: `{}`", &ident_str)).primary_label(&p, "").emit();
                return None
            };
            let DefKind::Function(func) = def.kind else {
                ctxt.diag.error(format!("`{}` did not resolve to a function", &ident_str)).primary_label(&p, "").emit();
                return None
            };
            Some(func)
        }
        _ => {
            ctxt.diag
                .error("invalid function reference")
                .primary_label(&expr, "")
                .emit();
            None
        }
    }
}

fn lower_function_call_expr(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    call_expr: &ast::CallExpr,
    scope: &Scope,
) -> Option<TypedValue> {
    // TODO do not return early if function is unresolved, also check the validity of argument expressions
    let Some(func_ref) = resolve_function_ref(ctxt, call_expr.func().as_ref(), scope) else {
        return Some(error_value(fb))
    };

    // TODO there are 4 vector allocations in this function, remove them all

    // lower arguments
    let mut args = Vec::new();
    let mut arg_types = Vec::new();
    let mut arg_locations = Vec::new();
    if let Some(arg_list) = call_expr.arg_list() {
        for arg in arg_list.arguments() {
            // TODO: check all argument expressions
            let location = arg.source_location();
            let arg_val = lower_expr_opt_non_void(ctxt, fb, Some(&arg), scope);
            arg_types.push(arg_val.ty);
            args.push(arg_val.val);
            arg_locations.push(location);
        }
    }

    let (result_type, parameter_types, overload_index) = match func_ref {
        FuncRef::User(f) => {
            // no overloads for user functions, extract signature and check
            let func_type = fb.functions[f].function_type;
            let TypeData::Function(ref func_type) = fb.types[func_type] else { panic!("expected function type") };
            (func_type.return_type, func_type.arg_types.clone().into_owned(), 0)
        }
        FuncRef::Builtin(op) => {
            match typecheck_builtin_operation(fb, &ctxt.types, op, &arg_types) {
                Ok(overload) => {
                    // TODO avoid into_vec
                    (
                        overload.result_type,
                        overload.parameter_types.into_vec(),
                        overload.index,
                    )
                }
                Err(_) => {
                    ctxt.diag
                        .error(format!("no matching overload found for `{op:?}`"))
                        .primary_label(&call_expr, "")
                        .emit();
                    return Some(error_value(fb));
                }
            }
        }
    };

    // apply implicit conversions
    for i in 0..args.len() {
        let value = lower_implicit_conversion(
            ctxt,
            fb,
            TypedValue::new(args[i], arg_types[i]),
            arg_locations[i],
            parameter_types[i],
        );
        args[i] = value.val;
        arg_types[i] = value.ty;
    }

    match func_ref {
        FuncRef::User(func) => Some(TypedValue::new(
            fb.emit_function_call(result_type, func.into(), &args),
            result_type,
        )),
        FuncRef::Builtin(builtin) => Some(lower_builtin_operation(
            ctxt,
            fb,
            builtin,
            overload_index,
            &args,
            &arg_types,
            result_type,
        )),
    }
}

/// Generates IR for an expression node.
///
/// # Return value
///
/// The IR value of the expression
fn lower_expr(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    expr: &ast::Expr,
    scope: &Scope,
) -> Option<TypedValue> {
    match expr {
        ast::Expr::BinExpr(bin_expr) => lower_bin_expr(ctxt, fb, bin_expr, scope),
        ast::Expr::CallExpr(call_expr) => lower_function_call_expr(ctxt, fb, call_expr, scope),
        ast::Expr::IndexExpr(_) | ast::Expr::PathExpr(_) | ast::Expr::FieldExpr(_) => {
            // lower place expression and dereference
            // TODO move into a separate function?
            // TODO functions should not be dereferenced
            let place = lower_place_expr(ctxt, fb, &expr, scope);
            let (ty, storage_class) = place
                .ptr_ty
                .pointee_type(fb)
                .expect("lower_place_expr did not produce a pointer value");
            let proj = fb.access_chain(place.ptr_ty, place.base, &place.indices);
            let val = fb.emit_load(ty, proj.into(), None);
            Some(TypedValue::new(val, ty))
        }
        ast::Expr::ParenExpr(expr) => lower_expr_opt(ctxt, fb, expr.expr().as_ref(), scope),
        ast::Expr::LitExpr(lit) => match lit.kind() {
            ast::LiteralKind::String(_str) => {
                todo!("literal string")
            }
            ast::LiteralKind::IntNumber(v) => {
                match v.value() {
                    Ok(v) => {
                        // TODO warn about overflow
                        // TODO unsigned suffixes
                        let c = fb.const_i32(v as i32);
                        Some(TypedValue::new(c, ctxt.types.int))
                    }
                    Err(err) => {
                        ctxt.diag
                            .error("error parsing integer value: {err}")
                            .primary_label(&v, "")
                            .emit();
                        let ty = ctxt.types.int;
                        Some(TypedValue::new(fb.emit_undef(ty), ty))
                    }
                }
            }
            ast::LiteralKind::FloatNumber(v) => {
                match v.value() {
                    Ok(v) => {
                        // TODO warn about non-representable floats
                        let c = fb.const_f32(v as f32);
                        Some(TypedValue::new(c, ctxt.types.float))
                    }
                    Err(err) => {
                        ctxt.diag
                            .error("error parsing floating-point value: {err}")
                            .primary_label(&v, "")
                            .emit();
                        let ty = ctxt.types.float;
                        Some(TypedValue::new(fb.emit_undef(ty), ty))
                    }
                }
            }
            ast::LiteralKind::Bool(v) => {
                let b = fb.const_bool(v);
                Some(TypedValue::new(b, ctxt.types.bool))
            }
        },
        ast::Expr::TupleExpr(_) => {
            todo!("tuple expressions")
        }
        ast::Expr::ArrayExpr(_) => {
            todo!("array expressions")
        }
        ast::Expr::PrefixExpr(prefix_expr) => Some(lower_prefix_expr(ctxt, fb, &prefix_expr, scope)),
    }
}

fn evaluate_constant_expression(
    ctx: &mut LowerCtxt,
    expr: &ast::Expr,
    scope: &Scope,
    expected_type: Option<hir::Type>,
) -> ConstantData {
    if let Some(data) = try_evaluate_constant_expression(ctx.diag, expr, scope, expected_type) {
        data
    } else {
        ctx.diag.consteval_failure(expr.source_location()).emit();
        // TODO
        ConstantData::Bool(false)
    }
}

/// Emits IR for a type reference.
fn lower_type(ctxt: &mut LowerCtxt, m: &mut hir::Module, ty: ast::Type, scope: &Scope) -> hir::Type {
    match ty {
        ast::Type::TypeRef(tyref) => {
            let Some(ident) = tyref.ident() else { return m.error_type(); };
            if let Some(def) = scope.resolve(DefNameKind::Type, ident.text()) {
                match def.kind {
                    DefKind::Type(ty) => ty,
                    _ => unreachable!(),
                }
            } else {
                ctxt.diag.cannot_find_type(ident.text(), tyref.source_location()).emit();
                m.error_type()
            }
        }
        ast::Type::TupleType(tuple_ty) => {
            let fields: Vec<_> = tuple_ty
                .fields()
                .map(|f| Field {
                    name: None,
                    ty: lower_type(ctxt, m, f, scope),
                })
                .collect();
            // TODO remove?
            m.define_type(TypeData::Struct(StructType {
                name: None,
                fields: Cow::Owned(fields),
            }))
        }
        ast::Type::ArrayType(array_type) => {
            let element_type = lower_type_opt(ctxt, m, array_type.element_type(), scope);
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
        ast::Type::ClosureType(_closure_type) => {
            todo!("closure types")
        }
    }
}

fn lower_type_opt(ctxt: &mut LowerCtxt, m: &mut hir::Module, ty: Option<ast::Type>, scope: &Scope) -> hir::Type {
    if let Some(ty) = ty {
        lower_type(ctxt, m, ty, scope)
    } else {
        ctxt.error_type
    }
}

/// Emits IR for a block statement.
fn lower_stmt(ctxt: &mut LowerCtxt, fb: &mut FunctionBuilder, stmt: &ast::Stmt, scope: &mut Scope) -> bool {
    let terminated = match stmt {
        ast::Stmt::ExprStmt(expr) => {
            lower_expr_opt(ctxt, fb, expr.expr().as_ref(), scope);
            false
        }
        ast::Stmt::ReturnStmt(stmt) => {
            if let Some(expr) = stmt.expr() {
                let value = lower_expr(ctxt, fb, &expr, scope);
                if let Some(value) = value {
                    fb.ret_value(value.val);
                } else {
                    // TODO emit warning here (return with unit-valued expression)
                    fb.ret();
                }
            } else {
                // return void
                fb.ret();
            }
            true
        }
        ast::Stmt::BlockStmt(stmt) => {
            if let Some(block) = stmt.block() {
                lower_block(ctxt, fb, &block, scope);
            }
            false
        }
        ast::Stmt::WhileStmt(_stmt) => false,
        ast::Stmt::BreakStmt(_stmt) => true,
        ast::Stmt::ContinueStmt(_stmt) => true,
        ast::Stmt::DiscardStmt(_stmt) => true,
        ast::Stmt::LocalVariable(v) => {
            let ty = lower_type_opt(ctxt, fb, v.ty(), scope);
            //let ptr_ty = b.pointer_type(ty, spirv::StorageClass::Function);

            let initializer = if let Some(initializer) = v.initializer() {
                Some(lower_expr_opt_non_void(ctxt, fb, initializer.expr().as_ref(), scope).val)
            } else {
                None
            };

            let name = ident_to_string_opt(v.name());
            let id = fb.add_local(ty, &name);
            if let Some(initializer) = initializer {
                fb.emit_store(id.into(), initializer, None);
            }
            scope.define_block_variable(ident_to_string_opt(v.name()), Some(v.source_location()), id);
            false
        }
        ast::Stmt::IfStmt(if_stmt) => {
            let Some(ast_condition) = if_stmt.condition() else { return false };
            let Some(ast_condition) = ast_condition.expr() else { return false };
            let Some(stmt) = if_stmt.stmt() else { return false };

            let condition = lower_expr_opt_non_void(ctxt, fb, Some(&ast_condition), scope);
            if condition.ty != ctxt.types.bool {
                ctxt.diag.error("expected boolean expression").location(&ast_condition).emit();
            }

            let merge_block = fb.create_block();
            let true_block = fb.create_block();
            let false_block = fb.create_block();

            fb.emit_selection_merge(merge_block, spirv::SelectionControl::NONE);
            fb.branch_conditional(condition.val, true_block, false_block);
            fb.select_block(true_block);
            lower_stmt(ctxt, fb, &stmt, scope);
            fb.branch(merge_block);
            fb.select_block(false_block);
            if let Some(else_branch) = if_stmt.else_branch() {
                if let Some(stmt) = else_branch.stmt() {
                    lower_stmt(ctxt, fb, &stmt, scope);
                }
            }
            fb.branch(merge_block);
            fb.select_block(merge_block);
            false
        }
        Stmt::ForStmt(_) => {
            todo!("for loops")
        }
    };
    terminated
}

/// Emits IR for a block.
fn lower_block(ctxt: &mut LowerCtxt, b: &mut FunctionBuilder, block: &ast::Block, scope: &Scope) {
    // start block scope
    let mut block_scope = Scope::new(Some(scope));
    //let mut terminated = false;
    for stmt in block.stmts() {
        /*if terminated {
            ctxt.diag.error("unreachable statement").primary_label(&stmt, "").emit();
        }*/
        lower_stmt(ctxt, b, &stmt, &mut block_scope);
    }
    /*if !terminated {
        // auto-return
        b.ret();
    }*/
}

/// Lowers a function definition or declaration
fn lower_function(
    ctxt: &mut LowerCtxt,
    m: &mut hir::Module,
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
            let arg_ty = lower_type_opt(ctxt, m, param.ty(), scope);
            arg_types.push(arg_ty);
            params.push(hir::FunctionParameter {
                name: param.ident().map(|id| id.text().to_string()).unwrap_or_default(),
                ty: arg_ty,
            });
        }
    }

    // return type
    let return_type = if let Some(ret_type) = ret_type {
        lower_type_opt(ctxt, m, ret_type.ty(), scope)
    } else {
        // no trailing return type in the AST (`-> Type`), so return unit.
        ctxt.types.void
    };

    // create function type
    let func_type = m.define_type(TypeData::Function(FunctionType {
        arg_types: Cow::Owned(arg_types),
        return_type,
    }));

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
        let mut builder = FunctionBuilder::new(m, &mut func_data, entry_block_id);
        lower_block(ctxt, &mut builder, &block, scope);
        if !builder.is_block_terminated() {
            // TODO check for return type
            builder.ret()
        }
        func_data
    } else {
        FunctionData::new_declaration(name.clone(), func_type, params, linkage, spirv::FunctionControl::NONE)
    };

    let func = m.add_function(func_data);

    // insert function in scope
    scope.define_function(name, Some(loc), FuncRef::User(func));
    func
}

/// Lowers a function declaration.
fn lower_fn_decl(ctxt: &mut LowerCtxt, m: &mut hir::Module, fn_decl: ast::FnDecl, scope: &mut Scope) -> hir::Function {
    let loc = fn_decl.source_location();
    let name = ident_to_string_opt(fn_decl.name());
    lower_function(
        ctxt,
        m,
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
fn lower_fn_def(ctxt: &mut LowerCtxt, m: &mut hir::Module, fn_def: ast::FnDef, scope: &mut Scope) -> hir::Function {
    let loc = fn_def.source_location();
    let name = ident_to_string_opt(fn_def.name());
    lower_function(
        ctxt,
        m,
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
    diag_writer: impl WriteColor + 'static,
) {
    let diag = Diagnostics::new(source_files, file, diag_writer, term::Config::default());
    let mut builtin_scope = Scope::new(None);
    let builtin_types = register_builtin_types(m, &mut builtin_scope);
    register_builtin_operations(m, &mut builtin_scope);
    let error_type = m.ty_unknown();
    let error_place_type = m.ty_unknown();

    let mut ctxt = LowerCtxt {
        diag: &diag,
        types: builtin_types,
        error_type,
        error_place_type,
    };
    lower_module(&mut ctxt, m, ast, &builtin_scope);
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_lower() {}
}
