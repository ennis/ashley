use crate::{
    builtins,
    builtins::{BuiltinOperation, BuiltinSignature, Constructor},
    diagnostic::{AsSourceLocation, SourceLocation},
    syntax::{
        ast,
        ast::{AstNode, UnaryOp},
        SyntaxNode, SyntaxToken,
    },
    tast::{
        consteval::ConstantValue,
        def::DefKind,
        overload::OverloadResolutionError,
        scope::Res,
        swizzle::{get_component_indices, ComponentIndices},
        ty::Type,
        DefId, ExprId, LocalVarId, ScalarType, TypeCheckBodyCtxt, TypeKind,
    },
    utils::DisplayCommaSeparated,
};
use ordered_float::OrderedFloat;
use std::ops::Deref;

/// Represents an expression with its inferred type.
#[derive(Clone, Debug)]
pub struct Expr {
    pub syntax: Option<SyntaxNode>,
    pub ty: Type,
    pub kind: ExprKind,
}

impl Expr {
    pub(crate) fn new(kind: ExprKind, ty: Type) -> Self {
        Expr { kind, ty, syntax: None }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConversionKind {
    IntBitcast,
    SignedIntToFloat,
    UnsignedIntToFloat,
    /// Float to double
    FloatConvert,
    FloatToSignedInt,
    FloatToUnsignedInt,
    /// Different layouts (e.g. different array stride or matrix stride or order)
    Layout,
}

/*#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConstructorKind {
    /// Identity constructor (scalar or vector)
    /// Conversion constructors are handled by "ImplicitConversion" subexpressions.
    Identity,
    /// Vector component constructor
    Vector,
    /// Vector splat.
    VectorSplat,
    /// Matrix diagonal.
    MatrixDiagonal,
    /// matCxR(C0, C1, C2, ...)
    MatrixFromColumns,
    /// matCxR(m00, .., m0R-1, m10, ...)
    MatrixFromComponents,
    /// Array constructor
    Array,
    /// Struct constructor
    Struct,
}*/

#[derive(Clone, Debug)]
pub enum ExprKind {
    Binary {
        op: ast::BinaryOp,
        /// Signature of the binary operation.
        // TODO replace this with a DefId to the operator function
        signature: BuiltinSignature,
        lhs: ExprId,
        rhs: ExprId,
    },
    BinaryAssign {
        op: ast::BinaryOp,
        /// Signature of the operation that is used to compute the value to assign.
        signature: BuiltinSignature,
        lhs: ExprId,
        rhs: ExprId,
    },
    Unary {
        op: ast::UnaryOp,
        /// Signature of the unary operation.
        signature: BuiltinSignature,
        expr: ExprId,
    },
    Assign {
        lhs: ExprId,
        rhs: ExprId,
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
    LocalVar {
        var: LocalVarId,
    },
    GlobalVar {
        var: DefId,
    },
    //FunctionRef {},
    Index {
        array_or_vector: ExprId,
        index: ExprId,
    },
    Field {
        expr: ExprId,
        //def: Arc<StructDef>,
        index: usize,
    },
    /// Vector component access & shuffle (e.g. `pos.y` or `offset.yx`).
    ComponentAccess {
        expr: ExprId,
        components: ComponentIndices,
    },
    ImplicitConversion {
        expr: ExprId,
        kind: ConversionKind,
    },
    BuiltinConstructor {
        ctor: &'static Constructor,
        args: Vec<ExprId>,
    },
    Literal {
        value: ConstantValue,
    },
    Undef,
}

/*enum AccessKind {
    Index(usize),
    Field(usize),
    Component(ComponentIndices),
}

struct Access {
    ty: TypeKind,
    kind: AccessKind,
}*/

impl TypeCheckBodyCtxt<'_, '_> {
    /*pub(crate) fn error_expr(&mut self) -> TypedExpr {
        let err_ty = self.sess.tyctxt.ty(TypeKind::Error);
        let id = self.typed_body.exprs.push(Expr {
            ast: None,
            ty: err_ty.clone(),
            kind: ExprKind::Undef,
        });
        Expr { id, ty: err_ty }
    }*/

    fn error_expr(&mut self) -> Expr {
        Expr::new(ExprKind::Undef, self.sess.tyctxt.ty(TypeKind::Error))
    }

    pub(crate) fn add_expr(&mut self, expr: Expr) -> ExprId {
        self.typed_body.exprs.push(expr)
    }

    /*pub(crate) fn add_expr_with_syntax(&mut self, expr: Expr, syntax: &SyntaxNode) -> ExprId {
        self.typed_body.exprs.push(Expr {
            syntax: Some(syntax.clone()),
            ty: expr.ty,
            kind: expr.expr,
        })
    }*/

    //
    /*fn typecheck_constructor_components(
        &mut self,
        arglist: &ast::ArgList,
        target_scalar_type: ScalarType,
    ) -> (Vec<Expr>, usize) {
        //let mut has_invalid_components = false;
        let mut args = vec![];
        let mut num_components = 0;
        for arg_expr in arglist.arguments() {
            let arg = self.typecheck_expr(&arg_expr);
            if !arg.ty.is_scalar_or_vector() {
                self.sess.diag.error("invalid component type in constructor").emit();
                //has_invalid_components = true;
            } else {
                num_components += arg.ty.num_components().unwrap() as usize;
                let conv_ty = self.sess.tyctxt.ty(arg.ty.with_scalar_type(target_scalar_type));
                let converted = self.apply_implicit_conversion(arg, Some(arg_expr.source_location()), conv_ty);
                args.push(converted);
            }
        }
        (args, num_components)
    }*/

    pub(crate) fn typecheck_constructor_expr(&mut self, expr: &ast::ConstructorExpr) -> Expr {
        let Some(ty) = expr.ty().map(|ty| self.convert_type(ty)) else { return self.error_expr(); };
        let Some(ast_args) = expr.args() else { return self.error_expr(); };

        let mut arg_loc = vec![];
        let mut args = vec![];
        for arg in ast_args.arguments() {
            arg_loc.push(arg.source_location());
            args.push(self.typecheck_expr(&arg));
        }

        let arg_tys = args.iter().map(|arg| arg.ty.clone()).collect::<Vec<_>>();

        use ScalarType as ST;
        use TypeKind as TK;

        let has_scalar_conversion = |from: &TypeKind, to: &TypeKind| match (from, to) {
            (TK::Scalar(from_scalar), TK::Scalar(to_scalar)) => match (from_scalar, to_scalar) {
                (a, b) if a == b => true,
                (ST::UnsignedInt, ST::Int) => true,
                (ST::Bool, ST::Int) => true,
                (ST::Float, ST::Int) => true,
                (ST::Double, ST::Int) => true,
                (ST::Int, ST::UnsignedInt) => true,
                (ST::Bool, ST::UnsignedInt) => true,
                (ST::Float, ST::UnsignedInt) => true,
                (ST::Double, ST::UnsignedInt) => true,
                (ST::Int, ST::Bool) => true,
                (ST::UnsignedInt, ST::Bool) => true,
                (ST::Float, ST::Bool) => true,
                (ST::Double, ST::Bool) => true,
                (ST::Int, ST::Float) => true,
                (ST::UnsignedInt, ST::Float) => true,
                (ST::Bool, ST::Float) => true,
                (ST::Double, ST::Float) => true,
                (ST::Int, ST::Double) => true,
                (ST::UnsignedInt, ST::Double) => true,
                (ST::Bool, ST::Double) => true,
                (ST::Float, ST::Double) => true,
                _ => false,
            },
            _ => false,
        };

        match *ty.deref() {
            TK::Scalar(_) | TK::Vector(_, _) | TK::Matrix { .. } => {
                for &ctor in builtins::CONSTRUCTORS {
                    if ctor.ty == *ty
                        && ctor.args.len() == arg_tys.len()
                        && arg_tys
                            .iter()
                            .zip(ctor.args.iter())
                            .all(|(from, to)| **from == *to || has_scalar_conversion(&*from.0, to))
                    {
                        let mut conv_args = Vec::with_capacity(arg_tys.len());
                        for (arg, ctor_arg_tk) in args.into_iter().zip(ctor.args.iter()) {
                            let ctor_arg_ty = self.sess.tyctxt.ty(ctor_arg_tk.clone());
                            let arg_loc = arg.syntax.as_ref().map(AsSourceLocation::source_location);
                            let conv_arg = self.apply_implicit_conversion(arg, arg_loc, ctor_arg_ty);
                            conv_args.push(self.add_expr(conv_arg));
                        }
                        return Expr::new(ExprKind::BuiltinConstructor { ctor, args: conv_args }, ty);
                    }
                }
                self.sess
                    .diag
                    .error(format!("no matching constructor for `{ty}`"))
                    .location(expr)
                    .note(format!("argument types are: ({})", DisplayCommaSeparated(&arg_tys)))
                    .emit();
                self.error_expr()
            }
            TK::Array { .. } => {
                todo!("array constructors")
            }
            _ => {
                self.sess.diag.error("invalid type constructor").location(expr).emit();
                self.error_expr()
            }
        }
    }

    pub(crate) fn typecheck_expr(&mut self, expr: &ast::Expr) -> Expr {
        let expr = match expr {
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
            ast::Expr::PostfixExpr(postfix_expr) => self.typecheck_postfix_expr(postfix_expr),
            ast::Expr::ConstructorExpr(constructor) => self.typecheck_constructor_expr(constructor),
        };
        if let Some(ref syntax) = expr.syntax {
            eprintln!("typecheck_expr {} : {}", syntax.text(), expr.ty);
        } else {
            eprintln!("typecheck_expr : {}", expr.ty);
        }
        expr
    }

    fn typecheck_index_expr(&mut self, index_expr: &ast::IndexExpr) -> Expr {
        let Some(array_expr) = index_expr.array() else { return self.error_expr() };
        let Some(index_expr) = index_expr.index() else { return self.error_expr() };

        let array = self.typecheck_expr(&array_expr);
        let index = self.typecheck_expr(&index_expr);

        let ty = match array.ty.deref() {
            TypeKind::Vector(scalar_type, _) => self.sess.tyctxt.ty(TypeKind::Scalar(*scalar_type)),
            TypeKind::Array { element_type, .. } | TypeKind::RuntimeArray { element_type, .. } => element_type.clone(),
            _ => {
                self.sess
                    .diag
                    .error("indexing into non-array type")
                    .location(&array_expr)
                    .emit();
                return self.error_expr();
            }
        };

        match index.ty.deref() {
            TypeKind::Scalar(ScalarType::Int) => {}
            _ => {
                self.sess
                    .diag
                    .error("index must be of type int")
                    .location(&index_expr)
                    .emit();
                return self.error_expr();
            }
        };

        // TODO check for out of bounds access if

        Expr {
            syntax: Some(index_expr.syntax().clone()),
            kind: ExprKind::Index {
                array_or_vector: self.add_expr(array),
                index: self.add_expr(index),
            },
            ty,
        }
    }

    fn typecheck_call_expr(&mut self, call_expr: &ast::CallExpr) -> Expr {
        let Some(ast_callee) = call_expr.callee() else {return self.error_expr() };
        let Some(arg_list) = call_expr.arg_list() else {return self.error_expr() };

        //let callee = self.typecheck_expr(&ast_callee);

        let func_name;
        let overloads = match ast_callee {
            ast::Expr::PathExpr(ref func_path) => {
                // resolve function
                let Some(ident) = func_path.ident() else { return self.error_expr() };
                func_name = ident.text().to_string();
                match self.resolve_name(ident.text()) {
                    Some(Res::OverloadSet(overloads)) => overloads.clone(),
                    _ => {
                        self.sess
                            .diag
                            .error(format!("unresolved function: {func_name}"))
                            .location(&ast_callee)
                            .emit();
                        return self.error_expr();
                    }
                }
            }
            _ => {
                self.sess
                    .diag
                    .error("expected function name")
                    .location(&ast_callee)
                    .emit();
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
            Ok(candidate) => {
                let args: Vec<_> = args
                    .into_iter()
                    .zip(candidate.parameter_types.iter())
                    .map(|(arg, target)| {
                        let conv_arg = self.apply_implicit_conversion(arg, None, target.clone());
                        self.add_expr(conv_arg)
                    })
                    .collect();

                Expr {
                    syntax: Some(call_expr.syntax().clone()),
                    kind: ExprKind::Call {
                        function: overloads[candidate.index],
                        args,
                    },
                    ty: candidate.result_type.clone(),
                }
            }
            Err(OverloadResolutionError::NoMatch) => {
                let mut diag = self
                    .sess
                    .diag
                    .error(format!("no matching function overload for call to `{func_name}`"))
                    .location(&call_expr)
                    .note(format!("argument types are: ({})", DisplayCommaSeparated(&arg_types)));
                for overload in overloads.iter() {
                    let def = self.sess.pkgs.def(*overload);
                    if def.as_function().unwrap().parameters.len() != args.len() {
                        continue;
                    }
                    diag = diag.note(format!("candidate: `{}`", def.display_declaration()));
                }
                diag.emit();
                return self.error_expr();
            }
            Err(OverloadResolutionError::Ambiguous(candidates)) => {
                // TODO better error message
                let mut diag = self
                    .sess
                    .diag
                    .error(format!("ambiguous call to overloaded function `{func_name}`"))
                    .location(&call_expr);
                for candidate in candidates.iter() {
                    let def = self.sess.pkgs.def(overloads[candidate.index]);
                    diag = diag.note(format!("candidate: `{}`", def.display_declaration()));
                }
                diag.emit();
                return self.error_expr();
            }
        }
    }

    fn typecheck_unary_expr(&mut self, syntax: SyntaxNode, token: SyntaxToken, op: UnaryOp, expr: &ast::Expr) -> Expr {
        let value = self.typecheck_expr(&expr);
        let operation;
        let is_assignment;
        match op {
            UnaryOp::Neg => {
                is_assignment = false;
                operation = &builtins::UnaryMinus;
            }
            UnaryOp::Not => {
                is_assignment = false;
                operation = &builtins::Not;
            }
            UnaryOp::PrefixInc => {
                is_assignment = true;
                operation = &builtins::PrefixInc;
            }
            UnaryOp::PrefixDec => {
                is_assignment = true;
                operation = &builtins::PrefixDec;
            }
            UnaryOp::PostfixInc => {
                is_assignment = true;
                operation = &builtins::PostfixInc;
            }
            UnaryOp::PostfixDec => {
                is_assignment = true;
                operation = &builtins::PostfixDec;
            }
        };

        if is_assignment {
            // TODO check that expression is a place
        }

        match self.typecheck_builtin_operation(operation, &[value.ty.clone()]) {
            Ok(overload) => {
                let conv_expr = if is_assignment {
                    // there are no "implicit conversions" of the value for `value++` and `value--` (and the prefix variants)
                    value
                } else {
                    self.apply_implicit_conversion(value, None, overload.parameter_types[0].clone())
                };

                Expr {
                    syntax: Some(syntax),
                    kind: ExprKind::Unary {
                        op,
                        signature: operation.signatures[overload.index],
                        expr: self.add_expr(conv_expr),
                    },
                    ty: overload.result_type.clone(),
                }
            }
            Err(_) => {
                let op_type = &value.ty;
                self.sess
                    .diag
                    .error(format!("no overload for unary operation `{}`", token))
                    .location(&syntax)
                    .note(format!("operand has type {op_type}"))
                    .emit();
                self.error_expr()
            }
        }
    }

    fn typecheck_prefix_expr(&mut self, prefix_expr: &ast::PrefixExpr) -> Expr {
        let Some(op) = prefix_expr.op_details() else {return self.error_expr() };
        let Some(expr) = prefix_expr.expr() else {return self.error_expr() };
        self.typecheck_unary_expr(prefix_expr.syntax().clone(), op.0, op.1, &expr)
    }

    fn typecheck_postfix_expr(&mut self, postfix_expr: &ast::PostfixExpr) -> Expr {
        let Some(op) = postfix_expr.op_details() else {return self.error_expr() };
        let Some(expr) = postfix_expr.expr() else {return self.error_expr() };
        self.typecheck_unary_expr(postfix_expr.syntax().clone(), op.0, op.1, &expr)
    }

    /*fn typecheck_expr_with_implicit_conversion(&mut self, expr: &ast::Expr, target_ty: Type) -> Expr {
        let e = self.typecheck_expr(expr);
        self.apply_implicit_conversion(e, Some(expr.source_location()), target_ty)
    }*/

    fn typecheck_path_expr(&mut self, path_expr: &ast::PathExpr) -> Expr {
        let Some(path) = path_expr.ident() else { return self.error_expr(); };
        let res = self.resolve_name(path.text());
        match res {
            Some(res) => {
                match res {
                    Res::OverloadSet(_func) => {
                        self.sess
                            .diag
                            .error("cannot use a function as a place")
                            .location(path_expr)
                            .emit();
                        self.error_expr()
                    }
                    Res::Global(def) => {
                        let ty = match &self.sess.pkgs.def(def).kind {
                            DefKind::Function(_) => unreachable!(), // would have been an overload set
                            DefKind::Global(global) => global.ty.clone(),
                            DefKind::Struct(_) => {
                                // TODO better error message
                                self.sess
                                    .diag
                                    .error("name did not resolve to a value")
                                    .location(path_expr)
                                    .emit();
                                return self.error_expr();
                            }
                        };
                        Expr {
                            syntax: Some(path_expr.syntax().clone()),
                            kind: ExprKind::GlobalVar { var: def },
                            ty,
                        }
                    }
                    Res::Local(local) => {
                        let ty = self.typed_body.local_vars[local].ty.clone();
                        Expr {
                            syntax: Some(path_expr.syntax().clone()),
                            kind: ExprKind::LocalVar { var: local },
                            ty,
                        }
                    }
                    Res::PrimTy { .. } => {
                        // TODO better error message
                        self.sess
                            .diag
                            .error("name did not resolve to a value")
                            .location(path_expr)
                            .emit();
                        self.error_expr()
                    }
                }
            }
            None => {
                // TODO better error message
                self.sess.diag.error("unresolved name").location(path_expr).emit();
                self.error_expr()
            }
        }
    }

    fn typecheck_field_expr(&mut self, field_expr: &ast::FieldExpr) -> Expr {
        // typecheck base expression
        let Some(base) = field_expr.expr() else { return self.error_expr() };
        let base = self.typecheck_expr(&base);

        let Some(field_name) = field_expr.field() else { return self.error_expr() };
        match base.ty.deref() {
            TypeKind::Struct { name, fields, .. } => {
                if let Some(field_index) = fields.iter().position(|field| field.name == field_name.text()) {
                    let ty = fields[field_index].ty.clone();
                    // base expression is not a place
                    Expr {
                        syntax: Some(field_expr.syntax().clone()),
                        kind: ExprKind::Field {
                            expr: self.add_expr(base),
                            index: field_index,
                        },
                        ty,
                    }
                } else {
                    if !name.is_empty() {
                        self.sess
                            .diag
                            .error(format!("struct `{name}` has no field named `{}`", field_name.text()))
                            .location(&field_name)
                            .emit();
                    } else {
                        self.sess
                            .diag
                            .error(format!(
                                "anonymous struct type has no field named `{}`",
                                field_name.text()
                            ))
                            .location(&field_name)
                            .emit();
                    }
                    self.error_expr()
                }
            }
            TypeKind::Vector(scalar_type, size) => {
                match get_component_indices(field_name.text(), *size as usize) {
                    Ok(components) => {
                        let ty = self.sess.tyctxt.ty(if components.len() == 1 {
                            TypeKind::Scalar(*scalar_type)
                        } else {
                            TypeKind::Vector(*scalar_type, components.len() as u8)
                        });
                        // base expression is not a place
                        Expr {
                            syntax: Some(field_expr.syntax().clone()),
                            kind: ExprKind::ComponentAccess {
                                expr: self.add_expr(base),
                                components,
                            },
                            ty,
                        }
                    }
                    Err(_) => {
                        self.sess
                            .diag
                            .error(format!("invalid component selection: `{}`", field_name.text()))
                            .location(&field_name)
                            .emit();
                        self.error_expr()
                    }
                }
            }
            _ => {
                // TODO better error message
                self.sess
                    .diag
                    .error("invalid field or component selection")
                    .location(field_expr)
                    .emit();
                self.error_expr()
            }
        }
    }

    //fn typecheck_builtin(&mut self, op: &BuiltinOperation) {}

    fn typecheck_place(&mut self, place_expr: &ast::Expr) -> Expr {
        let expr = self.typecheck_expr(place_expr);
        // TODO check value category
        expr
    }

    fn apply_implicit_conversion(&mut self, value: Expr, loc: Option<SourceLocation>, ty: Type) -> Expr {
        use ConversionKind as ICK;
        use ExprKind as EK;
        use ScalarType as ST;
        use TypeKind as TK;

        if ty == value.ty {
            // same types, no conversion
            return value;
        }

        match (value.ty.deref(), ty.deref()) {
            (TK::Scalar(tsrc), TK::Scalar(tdst)) => match (tsrc, tdst) {
                (ST::Int, ST::UnsignedInt) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::IntBitcast,
                    },
                    ty: ty.clone(),
                },
                (ST::Int, ST::Float | ST::Double) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::SignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::UnsignedInt, ST::Float | ST::Double) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::UnsignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::Float, ST::Double) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },
                _ => {
                    // TODO better error message
                    if let Some(loc) = loc {
                        self.sess.diag.error("mismatched types").location(loc).emit();
                    }
                    self.error_expr()
                }
            },
            (TK::Vector(tsrc, n2), TK::Vector(tdst, n1)) if n1 == n2 => match (tsrc, tdst) {
                (ST::Int, ST::UnsignedInt) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::IntBitcast,
                    },
                    ty: ty.clone(),
                },
                (ST::Int, ST::Float | ST::Double) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::SignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::UnsignedInt, ST::Float | ST::Double) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::UnsignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::Float, ST::Double) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },
                _ => {
                    // TODO better error message
                    if let Some(loc) = loc {
                        self.sess.diag.error("mismatched types").location(loc).emit();
                    }
                    self.error_expr()
                }
            },
            (
                TK::Matrix {
                    component_type: tsrc,
                    rows: r1,
                    columns: c1,
                    stride: _,
                },
                TK::Matrix {
                    component_type: tdst,
                    rows: r2,
                    columns: c2,
                    stride: _,
                },
            ) if r1 == r2 && c1 == c2 => match (tsrc, tdst) {
                (ST::Float, ST::Float) | (ST::Double, ST::Double) => {
                    // the component type is the same, but the stride may be different
                    Expr {
                        syntax: value.syntax.clone(),
                        kind: EK::ImplicitConversion {
                            expr: self.add_expr(value),
                            kind: ICK::Layout,
                        },
                        ty: ty.clone(),
                    }
                }
                (ST::Float, ST::Double) => Expr {
                    syntax: value.syntax.clone(),
                    kind: EK::ImplicitConversion {
                        expr: self.add_expr(value),
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },

                _ => {
                    // TODO better error message
                    if let Some(loc) = loc {
                        self.sess.diag.error("mismatched types").location(loc).emit();
                    }
                    self.error_expr()
                }
            },
            _ => {
                // TODO better error message
                if let Some(loc) = loc {
                    self.sess.diag.error("mismatched types").location(loc).emit();
                }
                self.error_expr()
            }
        }
    }

    fn typecheck_bin_expr(&mut self, bin_expr: &ast::BinExpr) -> Expr {
        let Some((op_token, ast_operator)) = bin_expr.op_details() else {
            // syntax error
            return self.error_expr()
        };

        let conv_arith_op = |arith_op| match arith_op {
            ast::ArithOp::Add => &builtins::Add,
            ast::ArithOp::Mul => &builtins::Mul,
            ast::ArithOp::Sub => &builtins::Sub,
            ast::ArithOp::Div => &builtins::Div,
            ast::ArithOp::Rem => &builtins::Rem,
            ast::ArithOp::Shl => &builtins::Shl,
            ast::ArithOp::Shr => &builtins::Shr,
            ast::ArithOp::BitXor => &builtins::BitXor,
            ast::ArithOp::BitOr => &builtins::BitOr,
            ast::ArithOp::BitAnd => &builtins::BitAnd,
        };

        // TODO typecheck operators as if they were a special kind of function

        // determine if this is an assignment, and the associated operation if there's one
        let is_assignment;
        let operation;
        match ast_operator {
            ast::BinaryOp::LogicOp(logic_op) => {
                is_assignment = false;
                operation = match logic_op {
                    ast::LogicOp::And => Some(&builtins::And),
                    ast::LogicOp::Or => Some(&builtins::Or),
                };
            }
            ast::BinaryOp::ArithOp(arith_op) => {
                is_assignment = false;
                operation = Some(conv_arith_op(arith_op));
            }
            ast::BinaryOp::CmpOp(cmp_op) => {
                is_assignment = false;
                operation = match cmp_op {
                    ast::CmpOp::Eq => Some(&builtins::Eq),
                    ast::CmpOp::Ne => Some(&builtins::Ne),
                    ast::CmpOp::Gt => Some(&builtins::Gt),
                    ast::CmpOp::Ge => Some(&builtins::Ge),
                    ast::CmpOp::Lt => Some(&builtins::Lt),
                    ast::CmpOp::Le => Some(&builtins::Le),
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

        let Some(ast_lhs) = bin_expr.lhs() else { return self.error_expr() };
        let Some(ast_rhs) = bin_expr.rhs() else { return self.error_expr() };
        //let lhs_loc = ast_lhs.source_location();
        let rhs_loc = ast_rhs.source_location();

        let lhs = self.typecheck_expr(&ast_lhs);
        let rhs = self.typecheck_expr(&ast_rhs);

        // TODO: check that LHS is a place if this is an assignment

        if let Some(operation) = operation {
            match self.typecheck_builtin_operation(operation, &[lhs.ty.clone(), rhs.ty.clone()]) {
                Ok(overload) => {
                    let lhs_conv = self.apply_implicit_conversion(lhs, None, overload.parameter_types[0].clone());
                    let rhs_conv = self.apply_implicit_conversion(rhs, None, overload.parameter_types[1].clone());
                    if is_assignment {
                        Expr {
                            syntax: Some(bin_expr.syntax().clone()),
                            kind: ExprKind::BinaryAssign {
                                op: ast_operator,
                                signature: operation.signatures[overload.index],
                                lhs: self.add_expr(lhs_conv),
                                rhs: self.add_expr(rhs_conv),
                            },
                            ty: self.sess.tyctxt.prim_tys.void.clone(),
                        }
                    } else {
                        Expr {
                            syntax: Some(bin_expr.syntax().clone()),
                            kind: ExprKind::Binary {
                                op: ast_operator,
                                signature: operation.signatures[overload.index],
                                lhs: self.add_expr(lhs_conv),
                                rhs: self.add_expr(rhs_conv),
                            },
                            ty: overload.result_type.clone(),
                        }
                    }
                }
                Err(_) => {
                    let lhs_ty = &lhs.ty;
                    let rhs_ty = &rhs.ty;
                    self.sess
                        .diag
                        .error(format!("no overload for binary operation `{op_token}`"))
                        .location(op_token.source_location())
                        .note(format!("operand types are: {lhs_ty} {op_token} {rhs_ty}"))
                        .emit();
                    self.error_expr()
                }
            }
        } else {
            let rhs_conv = self.apply_implicit_conversion(rhs, Some(rhs_loc), lhs.ty.clone());
            Expr {
                syntax: Some(bin_expr.syntax().clone()),
                kind: ExprKind::Assign {
                    lhs: self.add_expr(lhs),
                    rhs: self.add_expr(rhs_conv),
                },
                ty: self.sess.tyctxt.prim_tys.void.clone(),
            }
        }
    }

    /// Typechecks a literal expression.
    pub(crate) fn typecheck_lit_expr(&mut self, lit_expr: &ast::LitExpr) -> Expr {
        match lit_expr.kind() {
            ast::LiteralKind::String(_str) => {
                todo!("literal string")
            }
            ast::LiteralKind::IntNumber(v) => {
                match v.value() {
                    Ok(v) => {
                        // TODO warn about overflow
                        // TODO unsigned suffixes
                        Expr {
                            syntax: Some(lit_expr.syntax().clone()),
                            ty: self.sess.tyctxt.prim_tys.int.clone(),
                            kind: ExprKind::Literal {
                                value: ConstantValue::Int(v as i32),
                            },
                        }
                    }
                    Err(err) => {
                        self.sess
                            .diag
                            .error(format!("error parsing integer value: {err}"))
                            .location(&v)
                            .emit();
                        Expr {
                            syntax: Some(lit_expr.syntax().clone()),
                            ty: self.sess.tyctxt.prim_tys.int.clone(),
                            kind: ExprKind::Undef,
                        }
                    }
                }
            }
            ast::LiteralKind::FloatNumber(v) => {
                match v.value() {
                    Ok(v) => {
                        // TODO warn about non-representable floats
                        Expr {
                            syntax: Some(lit_expr.syntax().clone()),
                            ty: self.sess.tyctxt.prim_tys.float.clone(),
                            kind: ExprKind::Literal {
                                value: ConstantValue::Float(OrderedFloat::from(v as f32)),
                            },
                        }
                    }
                    Err(err) => {
                        self.sess
                            .diag
                            .error(format!("error parsing floating-point value: {err}"))
                            .location(&v)
                            .emit();
                        Expr {
                            syntax: Some(lit_expr.syntax().clone()),
                            ty: self.sess.tyctxt.prim_tys.float.clone(),
                            kind: ExprKind::Undef,
                        }
                    }
                }
            }
            ast::LiteralKind::Bool(v) => Expr {
                syntax: Some(lit_expr.syntax().clone()),
                ty: self.sess.tyctxt.prim_tys.bool.clone(),
                kind: ExprKind::Literal {
                    value: ConstantValue::Bool(v),
                },
            },
        }
    }
}
