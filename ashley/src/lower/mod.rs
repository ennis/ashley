//! AST lowering to SPIR-V
mod builtin;
mod consteval;
mod mangle;
mod swizzle;
mod typecheck;

use crate::{
    diagnostic::{AsSourceLocation, DiagnosticBuilder, Diagnostics, SourceFileProvider, SourceId, SourceLocation},
    hir,
    hir::{
        types::{Field, FunctionType, ScalarType, StructType},
        ConstantData, FunctionBuilder, FunctionData, IdRef, TypeData,
    },
    lower::{
        builtin::{register_builtin_types, BuiltinTypes},
        consteval::try_evaluate_constant_expression,
        typecheck::{typecheck_builtin_operation, BuiltinOperation},
    },
    syntax::{ast, ast::AstNode, ArithOp, BinaryOp, CmpOp, LogicOp, SyntaxToken},
};
use codespan_reporting::{term, term::termcolor::WriteColor};
use rspirv::spirv;
use std::{
    borrow::Cow,
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

// REFACTORS:
// - return Result<TypedValue,XXX> instead of creating error values when they are not needed
//   - synthesize undefs only when needed
// - integrate Constants into TypedValue

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
// TODO: move into HIR builder?
struct TypedValue {
    ty: hir::Type,
    // no values for void
    val: Option<hir::IdRef>,
}

impl TypedValue {
    fn new(val: impl Into<IdRef>, ty: hir::Type) -> TypedValue {
        TypedValue {
            ty,
            val: Some(val.into()),
        }
    }

    fn void(ty: hir::Type) -> TypedValue {
        TypedValue { val: None, ty }
    }
}

#[derive(Debug)]
struct LowerError;

type ValueResult = Result<TypedValue, LowerError>;

/// Lowers a `ValueResult` into a hir::Value or hir::Constant, or synthesizes an undef value in case
/// the value is an error.
fn lower_value(fb: &mut FunctionBuilder, v: ValueResult) -> Option<hir::IdRef> {
    match v {
        Ok(typed_value) => typed_value.val,
        Err(_) => {
            let ty = fb.ty_unknown();
            let undef = fb.emit_undef(ty);
            Some(undef.into())
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
    static ANON_IDENT_COUNTER: AtomicUsize = AtomicUsize::new(0);
    ident
        .map(|ident| ident.text().to_string())
        .unwrap_or_else(|| format!("__anon_{}", ANON_IDENT_COUNTER.fetch_add(1, Ordering::Relaxed)))
}

/// Resolved reference to a function.
enum FuncRef {
    /// User-defined function
    User(hir::Function),
    /// Built-in
    Builtin(BuiltinOperation),
}

//fn lower_function_call(ctxt: &mut LowerCtxt, func_ref: FuncRef) {}

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

#[derive(Debug)]
struct ImplicitConversionError;

enum ImplicitConversionContext {
    Assignment,
    Operation,
}

/// Generates IR for an implicit conversion.
fn lower_implicit_conversion(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    value: TypedValue,
    loc: SourceLocation,
    ty: hir::Type,
) -> ValueResult {
    if ty == value.ty {
        // same types, no conversion
        return Ok(value);
    }

    // all implicit conversions
    use hir::TypeData as TD;

    let raw_val = if let Some(val) = value.val {
        val
    } else {
        ctxt.diag
            .bug("requested implicit conversion of unit type")
            .primary_label(loc, "")
            .emit();
        return Ok(value);
    };

    let v = match (&fb.types[value.ty], &fb.types[ty]) {
        (TD::Scalar(tsrc), TD::Scalar(tdst)) => match (tsrc, tdst) {
            (ScalarType::Int, ScalarType::UnsignedInt) => fb.emit_bitcast(ty, raw_val),
            (ScalarType::Int, ScalarType::Float | ScalarType::Double) => fb.emit_convert_s_to_f(ty, raw_val),
            (ScalarType::UnsignedInt, ScalarType::Float | ScalarType::Double) => fb.emit_convert_u_to_f(ty, raw_val),
            (ScalarType::Float, ScalarType::Double) => fb.emit_f_convert(ty, raw_val),
            _ => {
                // TODO better error message
                ctxt.diag.error("mismatched types").primary_label(loc, "").emit();
                fb.emit_undef(ty)
            }
        },
        (TD::Vector(tsrc, n2), TD::Vector(tdst, n1)) if n1 == n2 => match (tsrc, tdst) {
            (ScalarType::Int, ScalarType::UnsignedInt) => fb.emit_bitcast(ty, raw_val),
            (ScalarType::Int, ScalarType::Float | ScalarType::Double) => fb.emit_convert_s_to_f(ty, raw_val),
            (ScalarType::UnsignedInt, ScalarType::Float | ScalarType::Double) => fb.emit_convert_u_to_f(ty, raw_val),
            (ScalarType::Float, ScalarType::Double) => fb.emit_f_convert(ty, raw_val),
            _ => {
                ctxt.diag.error("mismatched types").primary_label(loc, "").emit();
                fb.emit_undef(ty)
            }
        },
        (TD::Matrix(tsrc, r1, c1), TD::Matrix(tdst, r2, c2)) if r1 == r2 && c1 == c2 => match (tsrc, tdst) {
            (ScalarType::Float, ScalarType::Double) => fb.emit_f_convert(ty, raw_val),
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

    Ok(TypedValue::new(v, ty))
}

/// Generates IR for an expression node, or returns undef if the expression is `None`.
fn lower_expr_opt(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    expr: Option<ast::Expr>,
    scope: &Scope,
) -> ValueResult {
    if let Some(expr) = expr {
        lower_expr(ctxt, fb, expr, scope)
    } else {
        Err(LowerError)
    }
}

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
) -> ValueResult {
    match typecheck_builtin_operation(fb, &ctxt.types, operation, &[lhs.ty, rhs.ty]) {
        Ok(result) => {
            // emit implicit conversions
            let _converted_lhs = lower_implicit_conversion(ctxt, fb, lhs, lhs_loc, result.parameter_types[0]);
            let _converted_rhs = lower_implicit_conversion(ctxt, fb, rhs, rhs_loc, result.parameter_types[1]);
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
}

fn error_value(fb: &mut FunctionBuilder) -> TypedValue {
    let ty = fb.ty_unknown();
    TypedValue::new(fb.emit_undef(ty), ty)
}

fn lower_bin_expr(
    ctxt: &mut LowerCtxt,
    fb: &mut FunctionBuilder,
    bin_expr: ast::BinExpr,
    scope: &Scope,
) -> ValueResult {
    let (op_token, ast_operator) = bin_expr.op_details().ok_or(LowerError)?;

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

    let ast_lhs = bin_expr.lhs().ok_or(LowerError)?;
    let ast_rhs = bin_expr.rhs().ok_or(LowerError)?;
    let lhs_loc = ast_lhs.source_location();
    let rhs_loc = ast_rhs.source_location();

    let mut rhs = lower_expr_opt(ctxt, fb, bin_expr.rhs(), scope)?;
    if is_assignment {
        let place = lower_place_expr_opt(ctxt, fb, bin_expr.lhs(), scope)?;
        let place_ptr = fb.access_chain(place.ty, place.base, &place.indices);
        if let Some(operation) = operation {
            // emit the arithmetic part of the assignment
            // load lhs
            // rhs = lhs + rhs
            let lhs = fb.emit_load(place.ty, place_ptr, None);
            rhs = lower_rvalue_bin_expr(
                ctxt,
                fb,
                operation,
                TypedValue::new(lhs, place.ty),
                lhs_loc,
                rhs,
                rhs_loc,
                &op_token,
            )?;
        }
        // implicit conversion to lhs type of assignment
        rhs = lower_implicit_conversion(ctxt, fb, rhs, ast_rhs.source_location(), place.ty)?;
        // emit assignment
        fb.emit_store(place_ptr, rhs.val.expect("expected value"), None);
        // this expression produces no value
        Ok(TypedValue::void(fb.ty_unit()))
    } else if let Some(operation) = operation {
        let lhs = lower_expr_opt(ctxt, fb, bin_expr.rhs(), scope)?;
        lower_rvalue_bin_expr(ctxt, fb, operation, lhs, lhs_loc, rhs, rhs_loc, &op_token)
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
    /// The type of base + indices access chain
    /// It's always a pointer.
    ///
    /// TODO: should it be the dereferenced type?
    ty: hir::Type,
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
        ty: err_ptr_ty,
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
) -> Result<Place, LowerError> {
    match expr {
        // `place[index]`
        ast::Expr::IndexExpr(index_expr) => {
            let array = index_expr.array().ok_or(LowerError)?;
            let index = index_expr.index().ok_or(LowerError)?;

            let mut array_place = lower_place_expr(ctxt, fb, &array, scope)?;

            let elem_type = match fb.types[array_place.ty] {
                TypeData::Array(t, _) => t,
                TypeData::RuntimeArray(t) => t,
                _ => {
                    ctxt.diag
                        .error("cannot index into value")
                        .primary_label(&array, "")
                        .emit();
                    return Err(LowerError);
                }
            };

            if let Some(const_index) = try_evaluate_constant_expression(ctxt.diag, &index, scope, None) {
                let i = fb.define_constant(const_index);
                array_place.indices.push(i.into());
                array_place.ty = elem_type;
            } else {
                // Non-const
                let i = lower_expr(ctxt, fb, index, scope)?;
                eprintln!("**TODO** check type of indices");
                // can't be void
                let i = i.val.ok_or(LowerError)?;
                array_place.indices.push(i);
                array_place.ty = elem_type;
            }

            Ok(array_place)
        }
        // path reference
        ast::Expr::PathExpr(path_expr) => {
            let ident = path_expr.ident().ok_or(LowerError)?;
            let _decl = match scope.resolve(DefNameKind::Variable, ident.text()) {
                Some(def) => {
                    match def.kind {
                        DefKind::Constant(_) => {
                            ctxt.diag
                                .error("cannot use a constant as an lvalue")
                                .primary_label(&path_expr, "")
                                .emit();
                            return Err(LowerError);
                        }
                        DefKind::GlobalVariable(global_variable) => {
                            eprintln!("**TODO** check global variables as place expressions");
                            let ty = fb.globals[global_variable].ty;
                            // TODO storage class?
                            let result_type = fb.define_type(TypeData::Pointer {
                                pointee_type: ty,
                                storage_class: spirv::StorageClass::Uniform,
                            });
                            let ptr = fb.access_global(result_type, global_variable);
                            Ok(Place {
                                ty,
                                indices: vec![],
                                base: ptr.into(),
                            })
                        }
                        DefKind::BlockVariable(_) => {
                            todo!()
                        }
                        _ => unreachable!(),
                    }
                }
                None => {
                    ctxt.diag
                        .error(format!("could not find variable `{}` in scope", ident.text()))
                        .primary_label(&ident, "")
                        .emit();
                    Err(LowerError)
                }
            };

            todo!("path place expr")
        }
        // `value.field` or swizzle patterns
        ast::Expr::FieldExpr(_field_expr) => {
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
    func: &mut FunctionBuilder,
    expr: Option<ast::Expr>,
    scope: &Scope,
) -> Result<Place, LowerError> {
    if let Some(expr) = expr {
        lower_place_expr(ctxt, func, &expr, scope)
    } else {
        Err(LowerError)
    }
}

/// Generates IR for an expression node.
///
/// # Return value
///
/// The IR value of the expression
fn lower_expr(ctxt: &mut LowerCtxt, fb: &mut FunctionBuilder, expr: ast::Expr, scope: &Scope) -> ValueResult {
    match expr {
        ast::Expr::BinExpr(bin_expr) => lower_bin_expr(ctxt, fb, bin_expr, scope),
        ast::Expr::CallExpr(call_expr) => {
            let f = lower_expr_opt(ctxt, fb, call_expr.func(), scope)?;
            let Some(f_id) = f.val else {
                ctxt.diag.error("expression did not evaluate to a function").primary_label(&call_expr, "").emit();
                return Err(LowerError);
            };
            let Some(f_id) = f_id.to_function() else {
                ctxt.diag.error("expression did not evaluate to a function").primary_label(&call_expr, "").emit();
                return Err(LowerError);
            };
            let result_type = match fb.types[f.ty] {
                TypeData::Function(ref f) => f.return_type,
                _ => {
                    ctxt.diag.error("expression did not evaluate to a function").primary_label(&call_expr, "").emit();
                    return Err(LowerError);
                }
            };

            let mut args = Vec::new();
            if let Some(arg_list) = call_expr.arg_list() {
                for arg in arg_list.arguments() {
                    let result = lower_expr(ctxt, fb, arg, scope)?;
                    let Some(val) = result.val else {
                        // TODO diag
                        return Err(LowerError);
                    };
                    args.push(val);
                }
            }
            // TODO check args against signature

            let v = fb.emit_function_call(result_type, f_id, &args);
            Ok(TypedValue::new(v, result_type))
        }
        ast::Expr::IndexExpr(_) | ast::Expr::PathExpr(_) | ast::Expr::FieldExpr(_) => {
            // lower place expression and dereference
            // TODO move into a separate function?
            let place = lower_place_expr(ctxt, fb, &expr, scope)?;
            let (ty, _storage_class) = place
                .ty
                .pointee_type(fb)
                .expect("lower_place_expr did not produce a pointer value");
            let proj = fb.access_chain(place.ty, place.base, &place.indices);
            let val = fb.emit_load(ty, proj, None);
            Ok(TypedValue::new(val, ty))
        }
        ast::Expr::ParenExpr(expr) => lower_expr_opt(ctxt, fb, expr.expr(), scope),
        ast::Expr::LitExpr(lit) => match lit.kind() {
            ast::LiteralKind::String(_str) => {
                todo!("literal string")
            }
            ast::LiteralKind::IntNumber(_v) => {
                todo!("literal int")
            }
            ast::LiteralKind::FloatNumber(_v) => {
                todo!("literal float")
            }
            ast::LiteralKind::Bool(_v) => {
                todo!("literal bool")
            }
        },
        ast::Expr::TupleExpr(_) => {
            todo!("tuple expressions")
        }
        ast::Expr::ArrayExpr(_) => {
            todo!("array expressions")
        }
        ast::Expr::PrefixExpr(_) => {
            todo!("unary expr")
        }
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
                ctxt.diag.cannot_find_type(ident.text(), tyref.source_location());
                m.error_type()
            }
        }
        ast::Type::TupleType(tuple_ty) => {
            let fields : Vec<_> = tuple_ty.fields().map(|f| Field {
                name: None,
                ty: lower_type(ctxt, m, f, scope),
            }).collect();
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
fn lower_stmt(ctxt: &mut LowerCtxt, b: &mut FunctionBuilder, stmt: ast::Stmt, scope: &mut Scope) {
    match stmt {
        ast::Stmt::ExprStmt(expr) => {
            lower_expr_opt(ctxt, b, expr.expr(), scope);
        }
        ast::Stmt::ReturnStmt(stmt) => {
            if let Some(expr) = stmt.expr() {
                let Ok(value) = lower_expr(ctxt, b, expr, scope) else { return; };
                if let Some(value) = value.val {
                    b.ret_value(value);
                } else {
                    // TODO emit warning here (return with unit-valued expression)
                    b.ret();
                }
            } else {
                // return void
                b.ret();
            }
        }
        ast::Stmt::WhileStmt(_stmt) => {}
        ast::Stmt::BreakStmt(_stmt) => {}
        ast::Stmt::ContinueStmt(_stmt) => {}
        ast::Stmt::DiscardStmt(_stmt) => {}
        ast::Stmt::LocalVariable(_v) => {}
        ast::Stmt::IfStmt(_if_stmt) => {}
    }
}

/// Emits IR for a block.
fn lower_block(ctxt: &mut LowerCtxt, b: &mut FunctionBuilder, block: ast::Block, scope: &Scope) {
    // start block scope
    let mut block_scope = Scope::new(Some(scope));
    for stmt in block.stmts() {
        lower_stmt(ctxt, b, stmt, &mut block_scope)
    }
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
        lower_block(ctxt, &mut builder, block, scope);
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
