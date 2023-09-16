use crate::{
    builtins,
    builtins::{BuiltinSignature, Constructor},
    diagnostic::Span,
    syntax::{
        ast,
        ast::{AstNode, AstPtr, UnaryOp},
    },
    tast::{
        consteval::ConstantValue,
        def::DefKind,
        diagnostics::TyDiagnostic::*,
        overload::OverloadResolutionError,
        scope::Res,
        swizzle::{get_component_indices, ComponentIndices},
        ty::Type,
        DefId, ExprId, ExprSource, InFile, LocalVarId, ScalarType, TypeCheckBodyCtxt, TypeKind,
    },
    utils::CommaSeparated,
};
use ordered_float::OrderedFloat;
use std::ops::Deref;

/// Represents an expression with its inferred type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub ty: Type,
    pub kind: ExprKind,
}

impl Expr {
    pub(crate) fn new(kind: ExprKind, ty: Type) -> Self {
        Expr { kind, ty }
    }
}

pub(crate) struct TypedExprId {
    pub(crate) ty: Type,
    pub(crate) id: ExprId,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConversionKind {
    /// int32 -> uint32 bit casts (no-op)
    IntBitcast,
    /// Integer to float or double
    SignedIntToFloat,
    /// Unsigned integer to float or double
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

#[derive(Clone, Debug, PartialEq, Eq)]
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

impl TypeCheckBodyCtxt<'_> {
    fn error_expr(&mut self) -> Expr {
        Expr::new(ExprKind::Undef, self.compiler.tyctxt().ty(TypeKind::Error))
    }

    pub(crate) fn add_expr(&mut self, source: ExprSource, expr: Expr) -> TypedExprId {
        let ty = expr.ty.clone();
        let id = self.typed_body.exprs.push(expr);
        self.typed_body.expr_map.insert(source.clone(), id);
        self.typed_body.expr_map_back.insert(id, source);
        TypedExprId { ty, id }
    }

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
        let Some(ty) = expr.ty().map(|ty| self.convert_type(ty)) else {
            return self.error_expr();
        };
        let Some(ast_args) = expr.args() else {
            return self.error_expr();
        };

        let mut arg_locations = vec![];
        let mut args = vec![];
        for arg in ast_args.arguments() {
            arg_locations.push(Span::new(self.source_file, arg.syntax().text_range()));
            args.push(self.typecheck_expr(&arg));
        }

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
                        && ctor.args.len() == args.len()
                        && args
                            .iter()
                            .zip(ctor.args.iter())
                            .all(|(from, to)| *from.ty == *to || has_scalar_conversion(&*from.ty, to))
                    {
                        let mut conv_args = Vec::with_capacity(args.len());
                        for (i, arg) in args.into_iter().enumerate() {
                            let ctor_arg_ty = self.compiler.tyctxt().ty(ctor.args[i].clone());
                            let arg_source = self.typed_body.expr_map_back.get(arg.id).unwrap().clone();
                            let conv_arg = self.apply_implicit_conversion(arg, arg_source, ctor_arg_ty);
                            conv_args.push(conv_arg.id);
                        }
                        return Expr::new(ExprKind::BuiltinConstructor { ctor, args: conv_args }, ty);
                    }
                }

                let arg_tys = args.iter().map(|arg| arg.ty.clone()).collect::<Vec<_>>();

                self.errors.push(NoMatchingConstructor {
                    expr: InFile::new_ast_ptr(self.source_file, expr),
                    ty: ty.clone(),
                    arg_tys,
                });

                /*self.compiler
                .diag_error(format!("no matching constructor for `{ty}`"))
                .location(expr)
                .note(format!("argument types are: ({})", CommaSeparated(&arg_tys)))
                .emit();*/
                self.error_expr()
            }
            TK::Array { .. } => {
                todo!("array constructors")
            }
            _ => {
                self.errors.push(NoSuchTypeConstructor {
                    expr: InFile::new_ast_ptr(self.source_file, expr),
                    ty: ty.clone(),
                });
                /*self.compiler
                .diag_error("invalid type constructor")
                .location(expr)
                .emit();*/
                self.error_expr()
            }
        }
    }

    pub(crate) fn typecheck_expr(&mut self, expr_ast: &ast::Expr) -> TypedExprId {
        let source = self.in_file_syntax_ptr(expr_ast);

        let expr = match expr_ast {
            ast::Expr::BinExpr(bin_expr) => self.typecheck_bin_expr(bin_expr),
            ast::Expr::CallExpr(call_expr) => self.typecheck_call_expr(call_expr),
            ast::Expr::IndexExpr(index_expr) => self.typecheck_index_expr(index_expr),
            ast::Expr::PathExpr(path_expr) => self.typecheck_path_expr(path_expr),
            ast::Expr::FieldExpr(field_expr) => self.typecheck_field_expr(field_expr),
            ast::Expr::ParenExpr(expr) => {
                if let Some(expr) = expr.expr() {
                    let inner_expr = self.typecheck_expr(&expr);
                    self.typed_body.expr_map.insert(source, inner_expr.id);
                    return inner_expr;
                } else {
                    self.error_expr()
                }
            }
            ast::Expr::LitExpr(lit) => self.typecheck_lit_expr(lit),
            ast::Expr::TupleExpr(_) => {
                todo!("tuple expressions")
            }
            ast::Expr::ArrayExpr(_) => {
                todo!("array expressions")
            }
            ast::Expr::TernaryExpr(_) => {
                todo!("ternary expressions")
            }
            ast::Expr::PrefixExpr(prefix_expr) => self.typecheck_prefix_expr(prefix_expr),
            ast::Expr::PostfixExpr(postfix_expr) => self.typecheck_postfix_expr(postfix_expr),
            ast::Expr::ConstructorExpr(constructor) => self.typecheck_constructor_expr(constructor),
        };

        self.add_expr(source, expr)
    }

    fn typecheck_index_expr(&mut self, indexing_expr: &ast::IndexExpr) -> Expr {
        let Some(array_expr) = indexing_expr.array() else {
            return self.error_expr();
        };
        let Some(index_expr) = indexing_expr.index() else {
            return self.error_expr();
        };

        let array = self.typecheck_expr(&array_expr);
        let index = self.typecheck_expr(&index_expr);

        let ty = match array.ty.deref() {
            TypeKind::Vector(scalar_type, _) => self.compiler.tyctxt().ty(TypeKind::Scalar(*scalar_type)),
            TypeKind::Array { element_type, .. } | TypeKind::RuntimeArray { element_type, .. } => element_type.clone(),
            _ => {
                self.errors.push(InvalidIndexing {
                    expr: InFile::new_ast_ptr(self.source_file, indexing_expr),
                    base_ty: array.ty.clone(),
                });
                /*self.typed_body.diagnostics.push(diag_span_error!(
                    array_expr,
                    "indexing into a type that is not an array or vector"
                ));*/
                return self.error_expr();
            }
        };

        match index.ty.deref() {
            // TODO unsigned int?
            TypeKind::Scalar(ScalarType::Int) => {}
            _ => {
                self.errors.push(InvalidIndexType {
                    index: InFile::new_ast_ptr(self.source_file, &index_expr),
                    ty: index.ty.clone(),
                });
                /*self.typed_body
                .diagnostics
                .push(diag_span_error!(index_expr, "index must be of type int"));*/
                return self.error_expr();
            }
        };

        // TODO check for out of bounds access if index is constant
        Expr {
            kind: ExprKind::Index {
                array_or_vector: array.id,
                index: index.id,
            },
            ty,
        }
    }

    fn typecheck_call_expr(&mut self, call_expr: &ast::CallExpr) -> Expr {
        let Some(ast_callee) = call_expr.callee() else {
            return self.error_expr();
        };
        let Some(arg_list) = call_expr.arg_list() else {
            return self.error_expr();
        };

        //let callee = self.typecheck_expr(&ast_callee);
        let span = self.span(call_expr);
        let callee_span = self.span(&ast_callee);
        let callee_ptr = self.in_file_syntax_ptr(&ast_callee);

        let func_name;
        let overloads = match ast_callee {
            ast::Expr::PathExpr(ref func_path) => {
                // resolve function
                let Some(name) = func_path.name() else {
                    return self.error_expr();
                };
                func_name = name.text().to_string();
                match self.resolve_name(&func_name) {
                    Some(Res::OverloadSet(overloads)) => overloads.clone(),
                    _ => {
                        self.typed_body.errors.push(UnresolvedFunction {
                            callee: callee_ptr,
                            func_name,
                        });
                        /*self.typed_body
                        .diagnostics
                        .push(diag_span_error!(callee_span, "unresolved function: {func_name}"));*/
                        return self.error_expr();
                    }
                }
            }
            _ => {
                self.typed_body.errors.push(ExpectedFunctionName { callee: callee_ptr });
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

        eprintln!("--- overload resolution ---");
        for o in overloads.iter() {
            let def = self.compiler.definition(*o).as_function().unwrap();
            if let Some(builtin) = def.builtin {
                eprintln!("{}", builtin.description);
            }
        }

        match self.resolve_overload(&overloads, &arg_types) {
            Ok(candidate) => {
                let args: Vec<_> = args
                    .into_iter()
                    .zip(candidate.parameter_types.iter())
                    .map(|(arg, target)| {
                        let arg_source = self.typed_body.expr_map_back.get(arg.id).unwrap().clone();
                        self.apply_implicit_conversion(arg, arg_source, target.clone()).id
                    })
                    .collect();

                Expr {
                    kind: ExprKind::Call {
                        function: overloads[candidate.index],
                        args,
                    },
                    ty: candidate.result_type.clone(),
                }
            }
            Err(OverloadResolutionError::NoMatch) => {
                /*let mut diag = diag_span_error!(call_expr, "no matching function overload for call to `{func_name}`")
                    .note(format!("argument types are: ({})", CommaSeparated(&arg_types)));

                for &overload in overloads.iter() {
                    let def = self.compiler.definition(overload);
                    if def.as_function().unwrap().parameters.len() != args.len() {
                        continue;
                    }
                    diag = diag.note(format!("candidate: `{}`", def.display_declaration()));
                }
                diag.emit();*/

                self.errors.push(NoMatchingOverload {
                    call: InFile::new_ast_ptr(self.source_file, call_expr),
                    arg_types,
                    candidates: overloads,
                });

                return self.error_expr();
            }
            Err(OverloadResolutionError::Ambiguous(candidates)) => {
                self.errors.push(AmbiguousOverload {
                    func_name,
                    candidates: candidates.iter().map(|c| overloads[c.index]).collect(),
                });
                /*// TODO better error message
                let mut diag = self
                    .compiler
                    .diag_error(format!("ambiguous call to overloaded function `{func_name}`"))
                    .location(&call_expr);
                for candidate in candidates.iter() {
                    let def = self.compiler.definition(overloads[candidate.index]);
                    diag = diag.note(format!("candidate: `{}`", def.display_declaration()));
                }
                diag.emit();*/
                return self.error_expr();
            }
        }
    }

    fn typecheck_unary_expr(&mut self, op: UnaryOp, expr: &ast::Expr) -> Result<Expr, Type> {
        let value = self.typecheck_expr(&expr);
        let value_source = self.typed_body.expr_map_back.get(value.id).unwrap().clone();

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
                    self.apply_implicit_conversion(value, value_source, overload.parameter_types[0].clone())
                };

                Ok(Expr {
                    kind: ExprKind::Unary {
                        op,
                        signature: operation.signatures[overload.index],
                        expr: conv_expr.id,
                    },
                    ty: overload.result_type.clone(),
                })
            }
            Err(_) => Err(value.ty.clone()),
        }
    }

    fn typecheck_prefix_expr(&mut self, prefix_expr: &ast::PrefixExpr) -> Expr {
        let Some(op) = prefix_expr.op_details() else {
            return self.error_expr();
        };
        let Some(expr) = prefix_expr.expr() else {
            return self.error_expr();
        };
        match self.typecheck_unary_expr(op.1, &expr) {
            Ok(expr) => expr,
            Err(operand_ty) => {
                self.errors.push(InvalidTypesForPrefixOp {
                    op: op.1,
                    op_span: Span::new(self.source_file, op.0.text_range()),
                    expr: InFile::new_ast_ptr(self.source_file, prefix_expr),
                    operand_ty,
                });
                self.error_expr()
            }
        }
    }

    fn typecheck_postfix_expr(&mut self, postfix_expr: &ast::PostfixExpr) -> Expr {
        let Some(op) = postfix_expr.op_details() else {
            return self.error_expr();
        };
        let Some(expr) = postfix_expr.expr() else {
            return self.error_expr();
        };
        match self.typecheck_unary_expr(op.1, &expr) {
            Ok(expr) => expr,
            Err(operand_ty) => {
                self.errors.push(InvalidTypesForPostfixOp {
                    op: op.1,
                    op_span: Span::new(self.source_file, op.0.text_range()),
                    expr: InFile::new_ast_ptr(self.source_file, postfix_expr),
                    operand_ty,
                });
                self.error_expr()
            }
        }
    }

    /*fn typecheck_expr_with_implicit_conversion(&mut self, expr: &ast::Expr, target_ty: Type) -> Expr {
        let e = self.typecheck_expr(expr);
        self.apply_implicit_conversion(e, Some(expr.source_location()), target_ty)
    }*/

    fn typecheck_path_expr(&mut self, path_expr: &ast::PathExpr) -> Expr {
        let Some(path) = path_expr.name() else {
            return self.error_expr();
        };
        let res = self.resolve_name(&path.text());
        match res {
            Some(res) => {
                match res {
                    Res::OverloadSet(_func) => {
                        self.errors.push(ExpectedValue {
                            path: InFile::new_ast_ptr(self.source_file, path_expr),
                        });
                        self.error_expr()
                    }
                    Res::Global(def) => {
                        let ty = match &self.compiler.definition(def).kind {
                            DefKind::Function(_) => unreachable!("resolution should have returned an overload set"),
                            DefKind::Global(global) => global.ty.clone(),
                            DefKind::Struct(_) => {
                                self.errors.push(ExpectedValue {
                                    path: InFile::new_ast_ptr(self.source_file, path_expr),
                                });
                                /*self.compiler
                                .diag_error("name did not resolve to a value")
                                .location(path_expr)
                                .emit();*/
                                return self.error_expr();
                            }
                        };
                        Expr {
                            kind: ExprKind::GlobalVar { var: def },
                            ty,
                        }
                    }
                    Res::Local(local) => {
                        let ty = self.typed_body.local_vars[local].ty.clone();
                        Expr {
                            kind: ExprKind::LocalVar { var: local },
                            ty,
                        }
                    }
                    Res::PrimTy { .. } => {
                        self.errors.push(ExpectedValue {
                            path: InFile::new_ast_ptr(self.source_file, path_expr),
                        });
                        self.error_expr()
                    }
                }
            }
            None => {
                self.errors.push(UnresolvedPath {
                    path: InFile::new_ast_ptr(self.source_file, path_expr),
                });
                self.error_expr()
            }
        }
    }

    fn typecheck_field_expr(&mut self, field_expr: &ast::FieldExpr) -> Expr {
        // typecheck base expression
        let Some(base) = field_expr.expr() else {
            return self.error_expr();
        };
        let base = self.typecheck_expr(&base);

        let Some(field_name) = field_expr.field() else {
            return self.error_expr();
        };
        match base.ty.deref() {
            // --- Field access ---
            TypeKind::Struct { name, fields, .. } => {
                if let Some(field_index) = fields.iter().position(|field| field.name == field_name.text()) {
                    let ty = fields[field_index].ty.clone();
                    // base expression is not a place
                    Expr {
                        kind: ExprKind::Field {
                            expr: base.id,
                            index: field_index,
                        },
                        ty,
                    }
                } else {
                    self.errors.push(UnresolvedField {
                        field: InFile::new_ast_ptr(self.source_file, &field_name),
                        receiver: base.ty.clone(),
                    });

                    /*if !name.is_empty() {

                        self.compiler
                            .diag_error(format!("struct `{name}` has no field named `{}`", field_name.text()))
                            .location(&field_name)
                            .emit();
                    } else {
                        self.compiler
                            .diag_error(format!(
                                "anonymous struct type has no field named `{}`",
                                field_name.text()
                            ))
                            .location(&field_name)
                            .emit();
                    }*/

                    self.error_expr()
                }
            }
            // --- Vector component access ---
            TypeKind::Vector(scalar_type, size) => {
                match get_component_indices(&field_name.text(), *size as usize) {
                    Ok(components) => {
                        let ty = self.compiler.tyctxt().ty(if components.len() == 1 {
                            TypeKind::Scalar(*scalar_type)
                        } else {
                            TypeKind::Vector(*scalar_type, components.len() as u8)
                        });
                        Expr {
                            kind: ExprKind::ComponentAccess {
                                expr: base.id,
                                components,
                            },
                            ty,
                        }
                    }
                    Err(error) => {
                        self.errors.push(InvalidComponentSelection {
                            field: InFile::new_ast_ptr(self.source_file, &field_name),
                            error,
                            receiver: base.ty.clone(),
                        });
                        /*self.compiler
                        .diag_error(format!("invalid component selection: `{}`", field_name.text()))
                        .location(&field_name)
                        .emit();*/
                        self.error_expr()
                    }
                }
            }
            _ => {
                self.errors.push(ReceiverNotAStructOrVector {
                    field_expr: InFile::new_ast_ptr(self.source_file, field_expr),
                });
                /*self.compiler
                .diag_error("invalid field or component selection")
                .location(field_expr)
                .emit();*/
                self.error_expr()
            }
        }
    }

    fn diag_no_implicit_conversion(&mut self, source: &ExprSource, from_ty: &Type, to_ty: &Type) {
        /*self.diagnostics.push(diag_span_error!(
            source,
            "could not find an implicit conversion from `{}` to `{}`",
            from_ty,
            to_ty
        ));*/
        self.errors.push(NoImplicitConversion {
            source: source.clone(),
            from: from_ty.clone(),
            to: to_ty.clone(),
        })
    }

    fn apply_implicit_conversion(&mut self, value: TypedExprId, source: ExprSource, ty: Type) -> TypedExprId {
        use ConversionKind as ICK;
        use ExprKind as EK;
        use ScalarType as ST;
        use TypeKind as TK;

        if ty == value.ty {
            // same types, no conversion
            return value;
        }

        let expr = match (value.ty.deref(), ty.deref()) {
            (TK::Scalar(tsrc), TK::Scalar(tdst)) => match (tsrc, tdst) {
                (ST::Int, ST::UnsignedInt) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::IntBitcast,
                    },
                    ty: ty.clone(),
                },
                (ST::Int, ST::Float | ST::Double) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::SignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::UnsignedInt, ST::Float | ST::Double) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::UnsignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::Float, ST::Double) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },
                _ => {
                    self.diag_no_implicit_conversion(&source, &value.ty, &ty);
                    self.error_expr()
                }
            },
            (TK::Vector(tsrc, n2), TK::Vector(tdst, n1)) if n1 == n2 => match (tsrc, tdst) {
                (ST::Int, ST::UnsignedInt) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::IntBitcast,
                    },
                    ty: ty.clone(),
                },
                (ST::Int, ST::Float | ST::Double) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::SignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::UnsignedInt, ST::Float | ST::Double) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::UnsignedIntToFloat,
                    },
                    ty: ty.clone(),
                },
                (ST::Float, ST::Double) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },
                (_a, _b) => {
                    self.diag_no_implicit_conversion(&source, &value.ty, &ty);
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
                        kind: EK::ImplicitConversion {
                            expr: value.id,
                            kind: ICK::Layout,
                        },
                        ty: ty.clone(),
                    }
                }
                (ST::Float, ST::Double) => Expr {
                    kind: EK::ImplicitConversion {
                        expr: value.id,
                        kind: ICK::FloatConvert,
                    },
                    ty: ty.clone(),
                },

                (_a, _b) => {
                    self.diag_no_implicit_conversion(&source, &value.ty, &ty);
                    self.error_expr()
                }
            },
            _ => {
                self.diag_no_implicit_conversion(&source, &value.ty, &ty);
                self.error_expr()
            }
        };
        self.add_expr(source, expr)
    }

    fn typecheck_bin_expr(&mut self, bin_expr: &ast::BinExpr) -> Expr {
        let Some((op_token, ast_operator)) = bin_expr.op_details() else {
            // syntax error
            return self.error_expr();
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

        let Some(ast_lhs) = bin_expr.lhs() else {
            return self.error_expr();
        };
        let Some(ast_rhs) = bin_expr.rhs() else {
            return self.error_expr();
        };

        let lhs = self.typecheck_expr(&ast_lhs);
        let rhs = self.typecheck_expr(&ast_rhs);
        let lhs_source = self.typed_body.expr_map_back.get(lhs.id).unwrap().clone();
        let rhs_source = self.typed_body.expr_map_back.get(rhs.id).unwrap().clone();

        // TODO: check that LHS is a place if this is an assignment

        if let Some(operation) = operation {
            // binary operation, possibly with assignment (e.g `x * y` or `x *= y`)
            match self.typecheck_builtin_operation(operation, &[lhs.ty.clone(), rhs.ty.clone()]) {
                Ok(overload) => {
                    let lhs_conv = self.apply_implicit_conversion(lhs, lhs_source, overload.parameter_types[0].clone());
                    let rhs_conv = self.apply_implicit_conversion(rhs, rhs_source, overload.parameter_types[1].clone());
                    if is_assignment {
                        Expr {
                            kind: ExprKind::BinaryAssign {
                                op: ast_operator,
                                signature: operation.signatures[overload.index],
                                lhs: lhs_conv.id,
                                rhs: rhs_conv.id,
                            },
                            ty: self.compiler.tyctxt().prim_tys.void.clone(),
                        }
                    } else {
                        Expr {
                            kind: ExprKind::Binary {
                                op: ast_operator,
                                signature: operation.signatures[overload.index],
                                lhs: lhs_conv.id,
                                rhs: rhs_conv.id,
                            },
                            ty: overload.result_type.clone(),
                        }
                    }
                }
                Err(_) => {
                    self.errors.push(InvalidTypesForBinaryOp {
                        op: ast_operator,
                        op_span: Span::new(self.source_file, op_token.text_range()),
                        source: InFile::new_ast_ptr(self.source_file, bin_expr),
                        left_ty: lhs.ty.clone(),
                        right_ty: rhs.ty.clone(),
                    });
                    /*self.compiler
                    .diag_error(format!("no overload for binary operation `{op_token}`"))
                    .location(&op_token)
                    .note(format!("operand types are: {lhs_ty} {op_token} {rhs_ty}"))
                    .emit();*/
                    self.error_expr()
                }
            }
        } else {
            // assignment only (`x = y`)
            let rhs_conv = self.apply_implicit_conversion(rhs, rhs_source, lhs.ty.clone());
            Expr {
                kind: ExprKind::Assign {
                    lhs: lhs.id,
                    rhs: rhs_conv.id,
                },
                ty: self.compiler.tyctxt().prim_tys.void.clone(),
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
                // TODO we assume that all literals are i32 (a.k.a. "int").
                // Thus, something like `uint v = 12;` will fail (since "12" is interpreted as "int")
                //
                match v.value() {
                    Ok(v) => {
                        // TODO warn about overflow
                        // TODO unsigned suffixes
                        Expr {
                            ty: self.compiler.tyctxt().prim_tys.int.clone(),
                            kind: ExprKind::Literal {
                                value: ConstantValue::Int((v as i32) as u32),
                            },
                        }
                    }
                    Err(err) => {
                        self.errors.push(ParseIntError {
                            lit_expr: InFile::new_ast_ptr(self.source_file, lit_expr),
                        });
                        Expr {
                            ty: self.compiler.tyctxt().prim_tys.int.clone(),
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
                            ty: self.compiler.tyctxt().prim_tys.float.clone(),
                            kind: ExprKind::Literal {
                                value: ConstantValue::Float(OrderedFloat::from(v as f32)),
                            },
                        }
                    }
                    Err(err) => {
                        self.errors.push(ParseFloatError {
                            lit_expr: InFile::new_ast_ptr(self.source_file, lit_expr),
                        });
                        Expr {
                            ty: self.compiler.tyctxt().prim_tys.float.clone(),
                            kind: ExprKind::Undef,
                        }
                    }
                }
            }
            ast::LiteralKind::Bool(v) => Expr {
                ty: self.compiler.tyctxt().prim_tys.bool.clone(),
                kind: ExprKind::Literal {
                    value: ConstantValue::Bool(v),
                },
            },
        }
    }
}
