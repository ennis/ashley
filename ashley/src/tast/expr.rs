use crate::{
    builtins,
    builtins::{BuiltinOperation, BuiltinSignature},
    diagnostic::{AsSourceLocation, SourceLocation},
    syntax::{ast, BinaryOp, UnaryOp},
    tast::{
        consteval::ConstantValue,
        def::{DefKind, FunctionDef, StructDef},
        overload::{check_signature, OverloadCandidate, OverloadResolutionError, SignatureMismatch},
        scope::Res,
        swizzle::{get_component_indices, ComponentIndices},
        ty::Type,
        Def, DefId, ExprId, LocalVarId, PlaceId, ScalarType, TypeCheckBodyCtxt, TypeKind, Types,
    },
};
use ordered_float::OrderedFloat;
use std::{ops::Deref, sync::Arc};

/*pub enum FunctionRef {
    Builtin(BuiltinOperation, u32),
    /// Function defined in the current module.
    Module(Arc<FunctionDef>),
    /// Imported
    Imported,
}*/
/*
pub enum Operation {
    Builtin {
        op: BuiltinOperation,
        overload_index: usize,
    },
    /// Function call in the current module.
    FunctionCall(FunctionRef),
}*/

pub struct Expr {
    pub ast: Option<ast::Expr>,
    pub ty: Type,
    pub kind: ExprKind,
}

#[derive(Clone, Debug)]
pub struct Place {
    pub ast: Option<ast::Expr>,
    /// Type of the value stored in this place. Not the pointer type.
    pub ty: Type,
    pub kind: PlaceKind,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ImplicitConversionKind {
    IntBitcast,
    SignedIntToFloat,
    UnsignedIntToFloat,
    /// Float to double
    FloatConvert,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Binary {
        op: BinaryOp,
        /// Signature of the binary operation.
        // TODO replace this with a DefId to the operator function
        signature: BuiltinSignature,
        lhs: ExprId,
        rhs: ExprId,
    },
    BinaryAssign {
        op: BinaryOp,
        /// Signature of the operation that is used to compute the value to assign.
        // TODO replace this with a DefId to the operator function
        signature: BuiltinSignature,
        lhs: ExprId,
        rhs: ExprId,
    },
    Unary {
        op: UnaryOp,
        /// Signature of the unary operation.
        // TODO replace this with a DefId to the operator function
        signature: BuiltinSignature,
        expr: ExprId,
    },
    Assign {
        lhs: ExprId,
        rhs: ExprId,
    },
    Prefix {
        expr: ExprId,
    },
    Ternary {
        condition: ExprId,
        true_expr: ExprId,
        false_expr: ExprId,
    },
    Call {
        function: DefId,
        args: Vec<ExprId>,
    },
    Builtin {
        builtin: BuiltinOperation,
        args: Vec<ExprId>,
    },
    LocalVar {
        var: LocalVarId,
    },
    GlobalVar {
        var: DefId,
    },
    FunctionRef {},
    Index {
        array: ExprId,
        index: ExprId,
    },
    Field {
        expr: ExprId,
        //def: Arc<StructDef>,
        index: usize,
    },
    ComponentAccess {
        expr: ExprId,
        components: ComponentIndices,
    },
    ImplicitConversion {
        expr: ExprId,
        kind: ImplicitConversionKind,
    },
    Literal {
        value: ConstantValue,
    },
    Undef,
}

pub(crate) struct TypedExpr {
    pub(crate) expr: ExprKind,
    pub(crate) ty: Type,
}

impl TypedExpr {
    pub(crate) fn new(expr: ExprKind, ty: Type) -> Self {
        TypedExpr { expr, ty }
    }

    /*pub(crate) fn error(types: &mut Types) -> Self {
        TypedExpr::new(ExprKind::Undef, types.intern(TypeKind::Error))
    }*/
}

pub(crate) struct TypedPlace {
    pub(crate) place: PlaceKind,
    pub(crate) ty: TypeKind,
}

impl TypedPlace {
    pub(crate) fn new(place: PlaceKind, ty: TypeKind) -> Self {
        TypedPlace { place, ty }
    }

    pub(crate) fn error() -> Self {
        TypedPlace::new(PlaceKind::Error, TypeKind::Error)
    }
}

enum AccessKind {
    Index(usize),
    Field(usize),
    Component(ComponentIndices),
}

struct Access {
    ty: TypeKind,
    kind: AccessKind,
}

impl TypeCheckBodyCtxt<'_, '_> {
    /*pub(crate) fn error_expr(&mut self) -> TypedExpr {
        let err_ty = self.tyctxt.ty(TypeKind::Error);
        let id = self.typed_body.exprs.push(Expr {
            ast: None,
            ty: err_ty.clone(),
            kind: ExprKind::Undef,
        });
        TypedExpr { id, ty: err_ty }
    }*/

    fn error_expr(&mut self) -> TypedExpr {
        TypedExpr::new(ExprKind::Undef, self.tyctxt.ty(TypeKind::Error))
    }

    fn add_place(&mut self, place: TypedPlace) -> PlaceId {
        self.typed_body.places.push(Place {
            ast: None, // TODO
            ty: self.tyctxt.ty(place.ty),
            kind: place.place,
        })
    }

    fn add_expr(&mut self, expr: TypedExpr) -> ExprId {
        self.typed_body.exprs.push(Expr {
            ast: None,
            ty: expr.ty,
            kind: expr.expr,
        })
    }

    pub(crate) fn typecheck_expr(&mut self, expr: &ast::Expr) -> TypedExpr {
        match expr {
            ast::Expr::BinExpr(bin_expr) => self.typecheck_bin_expr(bin_expr),
            ast::Expr::CallExpr(call_expr) => self.typecheck_call_expr(call_expr),
            ast::Expr::IndexExpr(index_expr) => self.typecheck_index_expr(index_expr),
            ast::Expr::PathExpr(path_expr) => self.typecheck_path_expr(path_expr),
            ast::Expr::FieldExpr(field_expr) => self.typecheck_field_expr(field_expr),
            ast::Expr::ParenExpr(expr) => expr
                .expr()
                .map(|expr| self.typecheck_expr(&expr))
                .unwrap_or_else(|| self.error_expr()),
            ast::Expr::LitExpr(lit) => self.typecheck_lit_expr(lit),
            ast::Expr::TupleExpr(_) => {
                todo!("tuple expressions")
            }
            ast::Expr::ArrayExpr(_) => {
                todo!("array expressions")
            }
            ast::Expr::PrefixExpr(prefix_expr) => self.typecheck_prefix_expr(prefix_expr),
        }
    }

    fn typecheck_call_expr(&mut self, call_expr: &ast::CallExpr) -> TypedExpr {
        let Some(ast_callee) = call_expr.callee() else {return self.error_expr() };
        let Some(arg_list) = call_expr.arg_list() else {return self.error_expr() };

        //let callee = self.typecheck_expr(&ast_callee);

        let overloads = match ast_callee {
            ast::Expr::PathExpr(func_path) => {
                // resolve function
                let Some(ident) = func_path.ident() else { return self.error_expr() };
                match self.resolve_name(ident.text()) {
                    Some(Res::OverloadSet(overloads)) => overloads.clone(),
                    _ => {
                        // TODO better error message
                        self.diag.error("unresolved function").location(&ast_callee).emit();
                        return self.error_expr();
                    }
                }
            }
            _ => {
                // TODO better error message
                self.diag.error("expected function name").location(&ast_callee).emit();
                return self.error_expr();
            }
        };

        let mut args = Vec::new();
        let mut arg_types = Vec::new();
        for arg in arg_list.arguments() {
            let expr = self.typecheck_expr(&arg);
            arg_types.push(expr.ty.clone());
            args.push(expr);
        }

        match self.resolve_overload(&overloads, &arg_types) {
            Ok(i) => {
                let function_ty = self
                    .module
                    .def(overloads[i])
                    .as_function()
                    .unwrap()
                    .function_type
                    .as_function()
                    .unwrap();
                let args: Vec<_> = args
                    .into_iter()
                    .zip(function_ty.arg_types.iter())
                    .map(|(arg, target)| {
                        let conv_arg = self.apply_implicit_conversion(arg, None, target.clone());
                        self.add_expr(conv_arg)
                    })
                    .collect();

                TypedExpr {
                    expr: ExprKind::Call {
                        function: overloads[i],
                        args,
                    },
                    ty: function_ty.return_type.clone(),
                }
            }
            Err(OverloadResolutionError::NoMatch) => {
                // TODO better error message (print function name and overloads)
                self.diag
                    .error("no matching function overload")
                    .location(&call_expr)
                    .emit();
                return self.error_expr();
            }
            Err(OverloadResolutionError::Ambiguous) => {
                // TODO better error message
                self.diag
                    .error("ambiguous call to overloaded function")
                    .location(&call_expr)
                    .emit();
                return self.error_expr();
            }
        }
    }

    fn typecheck_prefix_expr(&mut self, prefix_expr: &ast::PrefixExpr) -> TypedExpr {
        let Some(op) = prefix_expr.op_details() else {return self.error_expr() };
        let Some(expr) = prefix_expr.expr() else {return self.error_expr() };
        let value = self.typecheck_expr(&expr);
        // TODO replace with resolve_overload (typecheck operators as if they were a special kind of function)
        let operation = match op.1 {
            ast::UnaryOp::Neg => &builtins::operations::UnaryMinus,
            ast::UnaryOp::Not => &builtins::operations::Not,
        };
        match self.typecheck_builtin_operation(operation, &[value.ty.clone()]) {
            Ok(overload) => {
                let conv_expr = self.apply_implicit_conversion(value, None, overload.parameter_types[0].clone());
                TypedExpr {
                    expr: ExprKind::Unary {
                        op: op.1,
                        signature: operation[overload.index],
                        expr: self.add_expr(conv_expr),
                    },
                    ty: overload.return_type,
                }
            }
            Err(_) => {
                // TODO display operand types
                self.emit_error(
                    expr.source_location(),
                    format!("no overload for unary operation `{}`", op.0),
                );
                self.error_expr()
            }
        }
    }

    fn typecheck_expr_with_implicit_conversion(&mut self, expr: &ast::Expr, target_ty: Type) -> TypedExpr {
        let e = self.typecheck_expr(expr);
        self.apply_implicit_conversion(e, expr.source_location(), target_ty)
    }

    fn typecheck_path_expr(&mut self, path_expr: &ast::PathExpr) -> TypedExpr {
        let Some(path) = path_expr.ident() else { return self.error_expr(); };
        let res = self.resolve_name(path.text());
        match res {
            Some(res) => {
                match res {
                    Res::OverloadSet(func) => {
                        self.diag
                            .error("cannot use a function as a place")
                            .location(path_expr)
                            .emit();
                        TypedExpr::error()
                    }
                    Res::Global(def) => {
                        let ty = match &self.module.def(def).kind {
                            DefKind::Function(_) => unreachable!(), // would have been an overload set
                            DefKind::Global(global) => global.ty.clone(),
                            DefKind::Struct(_) => {
                                // TODO better error message
                                self.diag
                                    .error("name did not resolve to a value")
                                    .location(path_expr)
                                    .emit();
                                return self.error_expr();
                            }
                        };
                        TypedExpr {
                            expr: ExprKind::GlobalVar { var: def },
                            ty,
                        }
                    }
                    Res::Local(local) => {
                        let ty = self.typed_body.local_vars[local].ty.clone();
                        TypedExpr {
                            expr: ExprKind::LocalVar { var: local },
                            ty,
                        }
                    }
                    Res::PrimTy(_) => {
                        // TODO better error message
                        self.diag
                            .error("name did not resolve to a value")
                            .location(path_expr)
                            .emit();
                        TypedExpr::error()
                    }
                }
            }
            None => {
                // TODO better error message
                self.diag.error("unresolved name").location(path_expr).emit();
                TypedExpr::error()
            }
        }
    }

    fn typecheck_field_expr(&mut self, field_expr: &ast::FieldExpr) -> TypedExpr {
        // typecheck base expression
        let Some(base) = field_expr.expr() else { return self.error_expr() };
        let base = self.typecheck_expr(&base);

        let Some(field_name) = field_expr.field() else { return self.error_expr() };
        match base.ty {
            TypeKind::Struct(def) => {
                let def = self.module.def(def);
                let struct_def = def.as_struct().unwrap();
                if let Some(field_index) = struct_def
                    .fields
                    .iter()
                    .position(|field| field.name == field_name.text())
                {
                    let ty = struct_def.fields[field_index].ty.clone();
                    // base expression is not a place
                    TypedExpr {
                        expr: ExprKind::Field {
                            expr: self.add_expr(base),
                            index: field_index,
                        },
                        ty,
                    }
                } else {
                    self.diag
                        .error(format!(
                            "struct `{}` has no field named `{}`",
                            def.name,
                            field_name.text()
                        ))
                        .location(&field_name)
                        .emit();
                    TypedExpr::error()
                }
            }
            TypeKind::Vector(scalar_type, size) => {
                match get_component_indices(field_name.text(), size as usize) {
                    Ok(components) => {
                        let ty = self.tyctxt.ty(TypeKind::Vector(scalar_type, components.len() as u8));
                        // base expression is not a place
                        TypedExpr {
                            expr: ExprKind::ComponentAccess {
                                expr: self.add_expr(base),
                                components,
                            },
                            ty,
                        }
                    }
                    Err(_) => {
                        self.diag
                            .error(format!("invalid component selection: `{}`", field_name.text()))
                            .location(&field_name)
                            .emit();
                        TypedExpr::error()
                    }
                }
            }
            _ => {
                // TODO better error message
                self.diag
                    .error("invalid field or component selection")
                    .location(field_expr)
                    .emit();
                TypedExpr::error()
            }
        }
    }

    fn typecheck_builtin(&mut self, op: &BuiltinOperation) {}

    fn typecheck_place(&mut self, place_expr: &ast::Expr) -> TypedExpr {
        let expr = self.typecheck_expr(place_expr);
        // TODO check value category
        expr
    }

    fn apply_implicit_conversion(&mut self, value: TypedExpr, loc: Option<SourceLocation>, ty: Type) -> TypedExpr {
        use ExprKind as EK;
        use ImplicitConversionKind as ICK;
        use ScalarType as ST;
        use TypeKind as TK;

        if ty == value.ty {
            // same types, no conversion
            return value;
        }

        match (value.ty.deref(), ty.deref()) {
            (TK::Scalar(tsrc), TK::Scalar(tdst)) => match (tsrc, tdst) {
                (ST::Int, ST::UnsignedInt) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::IntBitcast,
                    },
                    ty: ty.clone(),
                },
                (ST::Int, ST::Float | ST::Double) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::SignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::UnsignedInt, ST::Float | ST::Double) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::UnsignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::Float, ST::Double) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },
                _ => {
                    // TODO better error message
                    if let Some(loc) = loc {
                        self.diag.error("mismatched types").location(loc).emit();
                    }
                    TypedExpr::error()
                }
            },
            (TK::Vector(tsrc, n2), TK::Vector(tdst, n1)) if n1 == n2 => match (tsrc, tdst) {
                (ST::Int, ST::UnsignedInt) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::IntBitcast,
                    },
                    ty: ty.clone(),
                },
                (ST::Int, ST::Float | ST::Double) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::SignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::UnsignedInt, ST::Float | ST::Double) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::UnsignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::Float, ST::Double) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },
                _ => {
                    // TODO better error message
                    if let Some(loc) = loc {
                        self.diag.error("mismatched types").location(loc).emit();
                    }
                    TypedExpr::error()
                }
            },
            (
                TK::Matrix {
                    component_type: tsrc,
                    rows: r1,
                    columns: c1,
                },
                TK::Matrix {
                    component_type: tdst,
                    rows: r2,
                    columns: c2,
                },
            ) if r1 == r2 && c1 == c2 => match (tsrc, tdst) {
                (ST::Float, ST::Double) => TypedExpr {
                    expr: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },
                _ => {
                    // TODO better error message
                    if let Some(loc) = loc {
                        self.diag.error("mismatched types").location(loc).emit();
                    }
                    TypedExpr::error()
                }
            },
            _ => {
                // TODO better error message
                if let Some(loc) = loc {
                    self.diag.error("mismatched types").location(loc).emit();
                }
                TypedExpr::error()
            }
        }
    }

    fn typecheck_bin_expr(&mut self, bin_expr: &ast::BinExpr) -> TypedExpr {
        let Some((op_token, ast_operator)) = bin_expr.op_details() else {
            // syntax error
            return TypedExpr::error();
        };

        let conv_arith_op = |arith_op| match arith_op {
            ast::ArithOp::Add => &builtins::operations::Add,
            ast::ArithOp::Mul => &builtins::operations::Mul,
            ast::ArithOp::Sub => &builtins::operations::Sub,
            ast::ArithOp::Div => &builtins::operations::Div,
            ast::ArithOp::Rem => &builtins::operations::Rem,
            ast::ArithOp::Shl => &builtins::operations::Shl,
            ast::ArithOp::Shr => &builtins::operations::Shr,
            ast::ArithOp::BitXor => &builtins::operations::BitXor,
            ast::ArithOp::BitOr => &builtins::operations::BitOr,
            ast::ArithOp::BitAnd => &builtins::operations::BitAnd,
        };

        // TODO typecheck operators as if they were a special kind of function

        // determine if this is an assignment, and the associated operation if there's one
        let is_assignment;
        let operation;
        match ast_operator {
            ast::BinaryOp::LogicOp(logic_op) => {
                is_assignment = false;
                operation = match logic_op {
                    ast::LogicOp::And => Some(&builtins::operations::And),
                    ast::LogicOp::Or => Some(&builtins::operations::Or),
                };
            }
            ast::BinaryOp::ArithOp(arith_op) => {
                is_assignment = false;
                operation = Some(conv_arith_op(arith_op));
            }
            ast::BinaryOp::CmpOp(cmp_op) => {
                is_assignment = false;
                operation = match cmp_op {
                    ast::CmpOp::Eq => Some(&builtins::operations::Eq),
                    ast::CmpOp::Ne => Some(&builtins::operations::Ne),
                    ast::CmpOp::Gt => Some(&builtins::operations::Gt),
                    ast::CmpOp::Ge => Some(&builtins::operations::Ge),
                    ast::CmpOp::Lt => Some(&builtins::operations::Lt),
                    ast::CmpOp::Le => Some(&builtins::operations::Le),
                };
            }
            ast::BinaryOp::Assignment(assign_op) => {
                is_assignment = true;
                operation = match assign_op {
                    None => None,
                    Some(arith_op) => Some(conv_arith_op(arith_op)),
                };
            }
        };

        let Some(ast_lhs) = bin_expr.lhs() else { return TypedExpr::error() };
        let Some(ast_rhs) = bin_expr.rhs() else { return TypedExpr::error() };
        let lhs_loc = ast_lhs.source_location();
        let rhs_loc = ast_rhs.source_location();

        let lhs = self.typecheck_expr(&ast_lhs);
        let rhs = self.typecheck_expr(&ast_rhs);

        if let Some(operation) = operation {
            match self.typecheck_builtin_operation(operation, &[lhs.ty.clone(), rhs.ty.clone()]) {
                Ok(overload) => {
                    let lhs_conv = self.apply_implicit_conversion(lhs, None, overload.parameter_types[0].clone());
                    let rhs_conv = self.apply_implicit_conversion(rhs, None, overload.parameter_types[1].clone());
                    if is_assignment {
                        TypedExpr {
                            expr: ExprKind::BinaryAssign {
                                op: ast_operator,
                                signature: operation.signatures[overload.index],
                                lhs: self.add_expr(lhs_conv),
                                rhs: self.add_expr(rhs_conv),
                            },
                            ty: self.tyctxt.builtins.void.clone(),
                        }
                    } else {
                        TypedExpr {
                            expr: ExprKind::Binary {
                                op: ast_operator,
                                signature: operation.signatures[overload.index],
                                lhs: self.add_expr(lhs_conv),
                                rhs: self.add_expr(rhs_conv),
                            },
                            ty: todo!(),
                        }
                    }
                }
                Err(_) => {
                    self.diag
                        .error(format!("no overload for binary operation `{op_token}`"))
                        .location(op_token.source_location())
                        .emit();
                }
            }
        }

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

    /// Typechecks a literal expression.
    pub(crate) fn typecheck_lit_expr(&mut self, lit_expr: &ast::LitExpr) -> TypedExpr {
        use TypeKind as TK;
        match lit_expr.kind() {
            ast::LiteralKind::String(_str) => {
                todo!("literal string")
            }
            ast::LiteralKind::IntNumber(v) => {
                match v.value() {
                    Ok(v) => {
                        // TODO warn about overflow
                        // TODO unsigned suffixes
                        TypedExpr {
                            ty: self.tyctxt.builtins.int.clone(),
                            expr: ExprKind::Literal {
                                value: ConstantValue::Int(v as i32),
                            },
                        }
                    }
                    Err(err) => {
                        self.diag
                            .error(format!("error parsing integer value: {err}"))
                            .location(&v)
                            .emit();
                        TypedExpr {
                            ty: self.tyctxt.builtins.int.clone(),
                            expr: ExprKind::Undef,
                        }
                    }
                }
            }
            ast::LiteralKind::FloatNumber(v) => {
                match v.value() {
                    Ok(v) => {
                        // TODO warn about non-representable floats
                        TypedExpr {
                            ty: self.tyctxt.builtins.float.clone(),
                            expr: ExprKind::Literal {
                                value: ConstantValue::Float(OrderedFloat::from(v as f32)),
                            },
                        }
                    }
                    Err(err) => {
                        self.diag
                            .error(format!("error parsing floating-point value: {err}"))
                            .location(&v)
                            .emit();
                        TypedExpr {
                            ty: self.tyctxt.builtins.float.clone(),
                            expr: ExprKind::Undef,
                        }
                    }
                }
            }
            ast::LiteralKind::Bool(v) => TypedExpr {
                ty: self.tyctxt.builtins.bool.clone(),
                expr: ExprKind::Literal {
                    value: ConstantValue::Bool(v),
                },
            },
        }
    }
}
