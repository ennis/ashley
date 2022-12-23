//! AST lowering to HIR
use crate::{
    diagnostic::{Diagnostics, SourceFileProvider, SourceId, SourceLocation},
    dialect::{
        base,
        base::{
            FunctionType, MatrixType, ScalarType, ScalarTypeKind, TupleType, UnknownType,
            VectorType,
        },
    },
    hir,
    hir::{Attr, Cursor, HirCtxt, IntegerAttr, Location, OperationId, RegionId, ValueId},
    syntax::{ast, ast::*, ArithOp, BinaryOp, LogicOp, SyntaxNode, SyntaxToken},
    utils::DowncastExt,
};
use codespan_reporting::{diagnostic::Diagnostic, term, term::termcolor::WriteColor};
use std::{any::Any, cell::Cell, collections::HashMap, io::ErrorKind};
use crate::hir::Builder;

trait DiagnosticsExt {
    /// Emits a "missing generic argument" diagnostic.
    fn missing_generic_argument(&mut self, instantiation_loc: Location, expected_forms: &str);

    /// Emits a "extra generic argument(s)" diagnostic.
    fn extra_generic_arguments(&mut self, instantiation_loc: Location, expected: usize, got: usize);
}

impl DiagnosticsExt for Diagnostics {
    fn missing_generic_argument(&mut self, instantiation_loc: Location, expected_forms: &str) {
        self.error("missing generic argument")
            .primary_label(instantiation_loc.to_source_location(), "")
            .note(format!("expected: `{expected_forms}`"))
            .emit();
    }

    fn extra_generic_arguments(&mut self, instantiation_loc: Location, expected: usize, got: usize) {
        self.error("too many generic arguments")
            .primary_label(
                instantiation_loc.to_source_location(),
                format!("expected {expected} argument(s), got {got} argument(s)"),
            )
            .emit();
    }
}

//--------------------------------------------------------------------------------------------------

/// Attribute with an attached location
struct AttrWithLoc<'hir>(Attr<'hir>, Location);

//--------------------------------------------------------------------------------------------------

/// Type definition.
trait TypeDefinition<'hir> {
    /// Instantiate the type with the given arguments.
    fn instantiate(
        &self,
        ctx: &mut LowerCtxt,
        args: &[AttrWithLoc<'hir>],
        instantiation_loc: Location,
    ) -> Option<Attr<'hir>>;
}

//--------------------------------------------------------------------------------------------------

/// All builtin scalar types, like `f32`, `i32`, etc.
struct BuiltinScalarTypeDefinition<'hir> {
    kind: ScalarTypeKind,
    cached: Cell<Option<Attr<'hir>>>,
}

impl<'hir> TypeDefinition<'hir> for BuiltinScalarTypeDefinition<'hir> {
    fn instantiate(
        &self,
        ctx: &mut LowerCtxt,
        args: &[AttrWithLoc<'hir>],
        instantiation_loc: Location,
    ) -> Option<Attr<'hir>> {
        if !args.is_empty() {
            // Should not have arguments
            ctx.diag.extra_generic_arguments(instantiation_loc, 0, args.len());
            return None;
        }

        if let Some(cached) = self.cached.get() {
            return Some(cached);
        }

        let ty = ctx.ctxt.intern_attr(ScalarType(self.kind)).upcast();
        self.cached.set(Some(ty));
        Some(ty)
    }
}

//--------------------------------------------------------------------------------------------------

/// Vector types: `vec<ty,len>`
struct BuiltinVectorTypeDefinition;

impl<'hir> TypeDefinition<'hir> for BuiltinVectorTypeDefinition {
    fn instantiate(
        &self,
        b: &mut Builder<'a, 'hir>,
        ctx: &mut LowerCtxt,
        args: &[AttrWithLoc<'hir>],
        instantiation_loc: Location,
    ) -> Option<Attr<'hir>> {
        let inst_loc = instantiation_loc.to_source_location();

        let diag_args_invalid = |ctx| {
            ctx.diag
                .error("invalid number of generic arguments")
                .primary_label(
                    inst_loc,
                    "expected arguments of the form `<element-type, vector-length>`",
                )
                .emit();
        };

        let elem_type = if let Some(AttrWithLoc(scalar_type, loc)) = args.get(0) {
            if let Some(ScalarType(elem_type)) = scalar_type.cast() {
                *elem_type
            } else {
                ctx.diag
                    .error("invalid vector element type")
                    .primary_label(loc.to_source_location(), "")
                    .note("expected a scalar type: `f32`, `i32`, `u32`, `bool`")
                    .emit();
                ScalarTypeKind::Float
            }
        } else {
            diag_args_invalid(ctx);
            return None;
        };

        let vector_length = if let Some(AttrWithLoc(vector_length, loc)) = args.get(1) {
            if let Some(IntegerAttr(len)) = vector_length.cast() {
                len as usize
            } else {
                ctx.diag
                    .error("invalid vector size")
                    .primary_label(loc.to_source_location(), "")
                    .emit();
                1
            }
        } else {
            diag_args_invalid(ctx);
            return None;
        };

        if args.len() > 2 {
            diag_args_invalid(ctx);
            return None;
        }

        let ty = match vector_length {
            2 => ctx.ctxt.intern_attr(VectorType(elem_type, 2)),
            3 => ctx.ctxt.intern_attr(VectorType(elem_type, 3)),
            4 => ctx.ctxt.intern_attr(VectorType(elem_type, 4)),
            _ => {
                ctx.diag
                    .error("invalid vector size")
                    .primary_label(inst_loc, "expected 2, 3, or 4")
                    .emit();
                return None;
            }
        };

        Some(ty.upcast())
    }
}

/// Matrix types: `matrix<ty,rows,cols>`
struct BuiltinMatrixTypeDefinition;

impl<'hir> TypeDefinition<'hir> for BuiltinMatrixTypeDefinition {
    fn instantiate(
        &self,
        ctx: &mut LowerCtxt,
        args: &[AttrWithLoc<'hir>],
        instantiation_loc: Location,
    ) -> Option<Attr<'hir>> {
        let inst_loc = instantiation_loc.to_source_location();

        let diag_args_invalid = |ctx| {
            ctx.diag
                .error("invalid number of generic arguments")
                .primary_label(
                    inst_loc,
                    "expected arguments of the form `<element-type, row-count, column-count>`",
                )
                .emit();
        };

        let elem_type = if let Some(AttrWithLoc(scalar_type, loc)) = args.get(0) {
            if let Some(ScalarType(elem_type)) = scalar_type.cast() {
                *elem_type
            } else {
                ctx.diag
                    .error("invalid matrix element type")
                    .primary_label(loc.to_source_location(), "")
                    .note("expected a scalar type: `f32`, `i32`, `u32`")
                    .emit();
                ScalarTypeKind::Float
            }
        } else {
            diag_args_invalid(ctx);
            return None;
        };

        let (row_count, row_loc) = if let Some(AttrWithLoc(count, row_loc)) = args.get(1) {
            if let Some(IntegerAttr(count)) = count.cast() {
                (*count, *row_loc)
            } else {
                ctx.diag
                    .error("invalid matrix row count")
                    .primary_label(row_loc.to_source_location(), "")
                    .emit();
                (1, *row_loc)
            }
        } else {
            diag_args_invalid(ctx);
            return None;
        };

        let (column_count, column_loc) = if let Some(AttrWithLoc(count, column_loc)) = args.get(2) {
            if let Some(IntegerAttr(count)) = count.cast() {
                (*count, *column_loc)
            } else {
                ctx.diag
                    .error("invalid matrix column count")
                    .primary_label(column_loc.to_source_location(), "")
                    .emit();
                (1, *column_loc)
            }
        } else {
            diag_args_invalid(ctx);
            return None;
        };

        if args.len() > 3 {
            diag_args_invalid(ctx);
            return None;
        }

        let row_count_ok = row_count >= 2 && row_count <= 4;
        let col_count_ok = column_count >= 2 && column_count <= 4;

        if !row_count_ok || !col_count_ok {
            if !row_count_ok {
                ctx.diag
                    .error("invalid matrix row count")
                    .primary_label(row_loc, "expected 2,3, or 4")
                    .emit();
                return None;
            }
            if !col_count_ok {
                ctx.diag
                    .error("invalid matrix row count")
                    .primary_label(column_loc, "expected 2,3, or 4")
                    .emit();
                return None;
            }
        }

        let ty = ctx
            .ctxt
            .intern_attr(MatrixType(elem_type, row_count as u8, column_count as u8))
            .upcast();
        Some(ty)
    }
}

//--------------------------------------------------------------------------------------------------

trait Scope<'hir> {
    /// Resolves a type or type constructor name.
    fn resolve_type(&self, name: &str) -> Option<&dyn TypeDefinition<'hir>>;
}

/// Root scope containing primitive types & constants.
struct BuiltinScope<'hir> {
    ty_f32: hir::Attr<'hir>,
    ty_f64: hir::Attr<'hir>,
    ty_u32: hir::Attr<'hir>,
    ty_i32: hir::Attr<'hir>,
}

impl<'hir> BuiltinScope<'hir> {
    fn new(b: &mut hir::Builder<'_, 'hir>) -> BuiltinScope<'hir> {
        BuiltinScope {
            ty_f32: b.f32_ty().upcast(),
            ty_f64: b.f64_ty().upcast(),
            ty_u32: b.u32_ty().upcast(),
            ty_i32: b.i32_ty().upcast(),
        }
    }
}

/// Simple scope, contains global variables.
struct SimpleScope<'a, 'hir> {
    parent: Option<&'a dyn Scope<'hir>>,
    types: HashMap<String, Box<dyn TypeDefinition<'hir>>>,
    //variables: HashMap<String, VariableDefinition>,
}

impl<'a, 'hir> SimpleScope<'a, 'hir> {
    fn new(parent: Option<&'a dyn Scope<'hir>>) -> SimpleScope<'a, 'hir> {
        SimpleScope {
            parent,
            types: Default::default(),
            //variables: Default::default(),
        }
    }
}

impl<'a, 'hir> Scope<'hir> for SimpleScope<'a, 'hir> {
    fn resolve_type(&self, name: &str) -> Option<&dyn TypeDefinition<'hir>> {
        self.types
            .get(name)
            .map(|ty| &**ty)
            .or_else(|| self.parent.and_then(|parent| parent.resolve_type(name)))
    }
}

/// Function argument.
struct FunctionArgument {
    /// AST node of the corresponding function parameter.
    node: FnParam,
    /// HIR value holding the value of the function argument.
    hir_value: hir::ValueId,
}

/// The scope of the body of a function.
struct FunctionBodyScope<'a, 'hir> {
    parent: Option<&'a dyn Scope<'hir>>,
    arguments: HashMap<String, FunctionArgument>,
}

impl<'a, 'hir> FunctionBodyScope<'a, 'hir> {
    fn new(parent: Option<&'a dyn Scope<'hir>>) -> FunctionBodyScope<'a, 'hir> {
        FunctionBodyScope {
            parent,
            arguments: Default::default(),
        }
    }
}

/// Definition of a local variable in a block.
struct LocalVariableDefinition {
    /// Corresponding syntax node of the definition.
    node: SyntaxNode,
    /// HIR value holding the logical pointer to the variable.
    hir_value: hir::ValueId,
}

/// Scope defined by a block inside a function.
struct BlockScope<'a, 'hir> {
    parent: Option<&'a dyn Scope<'hir>>,
    variables: HashMap<String, LocalVariableDefinition>,
}

impl<'a, 'hir> BlockScope<'a, 'hir> {
    fn new(parent: Option<&'a dyn Scope<'hir>>) -> BlockScope<'a, 'hir> {
        BlockScope {
            parent,
            variables: Default::default(),
        }
    }

    /// Declares local variable in this scope, Possibly shadows an existing one.
    fn declare_variable(&mut self, def: LocalVariableDefinition) {}
}

impl<'a, 'hir> Scope<'hir> for BlockScope<'a, 'hir> {
    fn resolve_type(&self, name: &str) -> Option<&dyn TypeDefinition<'hir>> {
        self.parent.and_then(|parent| parent.resolve_type(name))
    }
}

struct LowerCtxt<'a> {
    current_file: SourceId,
    diag: &'a Diagnostics,
}

impl<'a> LowerCtxt<'a> {
    fn new(file: SourceId, diag: &'a Diagnostics) -> LowerCtxt<'a> {
        LowerCtxt {
            current_file: file,
            diag,
        }
    }

    fn instantiate_user_type(&mut self, syntax: Item) -> Option<hir::Attr> {
        eprintln!("TODO instantiate_user_type");
        None
    }

    /*fn instantiate_builtin_type(
        &mut self,
        kind: BuiltinTypeKind,
        args: &[(Attr, Location)],
        instantiation_loc: Location,
    ) -> Option<hir::Attr<'ir>> {
        if matches!(
            kind,
            BuiltinTypeKind::Int(_) | BuiltinTypeKind::UnsignedInt(_) | BuiltinTypeKind::Float(_)
        ) && !args.is_empty()
        {
            self.diag
                .error("extra generic arguments on builtin type")
                .primary_label(instantiation_loc.to_source_location(), "")
                .emit();
        }

        match kind {
            BuiltinTypeKind::Int(bits) => match bits {
                8 => {
                    todo!()
                }
                16 => {
                    todo!()
                }
                32 => Some(self.ty_i32),
                _ => None,
            },
            BuiltinTypeKind::UnsignedInt(bits) => match bits {
                8 => {
                    todo!()
                }
                16 => {
                    todo!()
                }
                32 => Some(self.ty_u32),
                _ => None,
            },
            BuiltinTypeKind::Float(bits) => match bits {
                32 => Some(self.ty_f32),
                64 => {
                    todo!()
                }
                _ => None,
            },
            BuiltinTypeKind::Vector => {}
            BuiltinTypeKind::Matrix => {
                todo!()
            }
        }
    }*/

    /// Instantiate a type given parameters
    fn instantiate_type_or_unknown(
        &mut self,
        ty: &dyn TypeDefinition,
        args: &[(Attr, Location)],
        loc: Location,
    ) -> Attr {
        match ty {
            TypeDefinition::Builtin { kind, ty } => {
                if let Some(ty) = ty.get() {
                    ty
                } else {
                    if let Some(instantiated) = self.instantiate_builtin_type(*kind, args, loc) {
                        if args.is_empty() {
                            // no args, we can cache the resulting type instance
                            ty.set(Some(instantiated));
                        }
                        instantiated
                    } else {
                        // failed to instantiate
                        self.ty_unknown
                    }
                }
            }
            TypeDefinition::User { def, ty } => {
                if let Some(instantiated) = self.instantiate_user_type(def.clone()) {
                    instantiated
                } else {
                    self.ty_unknown
                }
            }
        }
    }

    fn token_loc(&self, token: &SyntaxToken) -> Location {
        Location::Source(SourceLocation::new(self.current_file, token.text_range()))
    }

    fn node_loc(&self, node: &SyntaxNode) -> Location {
        Location::Source(SourceLocation::new(self.current_file, node.text_range()))
    }

    fn emit_module<'ir>(&mut self, b: &mut hir::Builder<'_, 'ir>, module: Module) {
        let mut root_scope = BuiltinScope::new(b);
        let mut global_scope = SimpleScope::new(Some(&root_scope));
        for item in module.items() {
            self.emit_item(b, item, &mut global_scope)
        }
    }

    fn emit_item<'ir>(&mut self, b: &mut hir::Builder<'_, 'ir>, item: Item, scope: &mut SimpleScope<'_, 'ir>) {
        match item {
            Item::FnDef(def) => {
                self.emit_fn_def(b, def, scope);
            }
            Item::Global(global) => {
                //todo!()
            }
        }
    }

    /// Generates IR for an expression node, or returns undef if the expression is `None`.
    ///
    /// Helper for
    fn emit_expr_or_undef<'ir>(
        &mut self,
        b: &mut hir::Builder<'_, 'ir>,
        expr: Option<Expr>,
        scope: &dyn Scope<'ir>,
    ) -> hir::ValueId {
        if let Some(expr) = expr {
            self.emit_expr(b, expr, scope)
        } else {
            b.undef()
        }
    }

    /// Generates IR for an expression node.
    ///
    /// # Return value
    ///
    /// The IR value of the expression
    fn emit_expr<'ir>(&mut self, b: &mut hir::Builder<'_, 'ir>, expr: Expr, scope: &dyn Scope<'ir>) -> hir::ValueId {
        let loc = self.node_loc(expr.syntax());
        match expr {
            Expr::BinExpr(bin_expr) => {
                let lhs = self.emit_expr_or_undef(b, bin_expr.lhs(), scope);
                let rhs = self.emit_expr_or_undef(b, bin_expr.rhs(), scope);

                if let Some((_, op)) = bin_expr.op_details() {
                    match op {
                        BinaryOp::LogicOp(logic_op) => match logic_op {
                            LogicOp::And => {
                                todo!()
                            }
                            LogicOp::Or => {
                                todo!()
                            }
                        },
                        BinaryOp::ArithOp(arith_op) => match arith_op {
                            ArithOp::Add => base::Add::build(b, lhs, rhs, loc).result,
                            ArithOp::Mul => base::Mul::build(b, lhs, rhs, loc).result,
                            ArithOp::Sub => base::Sub::build(b, lhs, rhs, loc).result,
                            ArithOp::Div => base::Div::build(b, lhs, rhs, loc).result,
                            ArithOp::Rem => {
                                todo!()
                            }
                            ArithOp::Shl => {
                                todo!()
                            }
                            ArithOp::Shr => {
                                todo!()
                            }
                            ArithOp::BitXor => {
                                todo!()
                            }
                            ArithOp::BitOr => {
                                todo!()
                            }
                            ArithOp::BitAnd => {
                                todo!()
                            }
                        },
                        BinaryOp::CmpOp(_) => {
                            todo!()
                        }
                        BinaryOp::Assignment(_) => b.undef(),
                    }
                } else {
                    b.undef()
                }
            }
            Expr::CallExpr(call_expr) => {
                let func = self.emit_expr_or_undef(b, call_expr.func(), scope);
                let mut args = Vec::new();
                if let Some(arg_list) = call_expr.arg_list() {
                    for arg in arg_list.arguments() {
                        args.push(self.emit_expr(b, arg, scope));
                    }
                }
                base::Call::build(b, func, &args, self.node_loc(call_expr.syntax())).result
            }
            Expr::IndexExpr(index_expr) => {
                todo!()
            }
            Expr::ParenExpr(expr) => self.emit_expr_or_undef(b, expr.expr(), scope),
            Expr::LitExpr(lit) => match lit.kind() {
                LiteralKind::String(str) => {
                    todo!()
                }
                LiteralKind::IntNumber(v) => {
                    if let Some(value) = v.value() {
                        let attr = b.int_const(value as i128).upcast();
                        base::Constant::build(b, attr, loc).result
                    } else {
                        b.undef()
                    }
                }
                LiteralKind::FloatNumber(v) => {
                    if let Some(value) = v.value() {
                        let attr = b.fp_const(value);
                        base::Constant::build(b, attr, loc)
                    } else {
                        b.undef()
                    }
                }
                LiteralKind::Bool(v) => {
                    let attr = b.bool_const(v);
                    base::Constant::build(b, attr, loc)
                }
            },
            Expr::PathExpr(path_expr) => b.undef(),
            Expr::TupleExpr(_) => {
                todo!()
            }
            Expr::ArrayExpr(_) => {
                todo!()
            }
        }
    }

    /// Emits IR for a type reference.
    fn emit_type<'ir>(&mut self, b: &mut hir::Builder<'_, 'ir>, ty: Type) -> hir::Attr<'ir> {
        match ty {
            Type::TypeRef(tyref) => {
                match tyref.ident() {
                    Some(ident) => {
                        // resolve builtin types
                        if let Some(hir_ty) = self.emit_builtin_type(ident.text()) {
                            hir_ty
                        } else {
                            self.ty_unknown
                        }
                    }
                    _ => self.ty_unknown,
                }
            }
            Type::TupleType(tuple_ty) => {
                let mut fields = vec![];
                for t in tuple_ty.fields() {
                    fields.push(self.emit_type(b, t));
                }
                let fields = b.ctxt_mut().alloc_slice_copy(&fields);
                b.ctxt_mut().intern_attr(TupleType(fields)).upcast()
            }
        }
    }

    fn emit_type_or_unknown<'ir>(&mut self, b: &mut hir::Builder<'_, 'ir>, ty: Option<Type>) -> hir::Attr<'ir> {
        if let Some(ty) = ty {
            self.emit_type(b, ty)
        } else {
            self.ty_unknown
        }
    }

    /// Emits IR for a block statement.
    fn emit_stmt<'ir>(&mut self, b: &mut hir::Builder<'_, 'ir>, stmt: Stmt, scope: &mut BlockScope<'_, 'ir>) {
        match stmt {
            Stmt::ExprStmt(expr) => {
                self.emit_expr_or_undef(b, expr.expr(), scope);
            }
            Stmt::ReturnStmt(_) => {}
            Stmt::WhileStmt(_) => {}
            Stmt::BreakStmt(_) => {}
            Stmt::ContinueStmt(_) => {}
            Stmt::DiscardStmt(_) => {}
            Stmt::LocalVariable(_) => {}
            Stmt::IfStmt(if_stmt) => {}
        }
    }

    /// Emits IR for a block.
    fn emit_block<'ir>(&mut self, b: &mut hir::Builder<'_, 'ir>, block: Block, scope: &dyn Scope<'ir>) {
        // start block scope
        let mut block_scope = BlockScope::new(Some(scope));
        for stmt in block.stmts() {
            self.emit_stmt(b, stmt, &mut block_scope)
        }
    }

    /// Emits IR for a function declaration.
    fn emit_fn_def<'ir>(&mut self, b: &mut hir::Builder<'_, 'ir>, fn_def: FnDef, scope: &mut SimpleScope<'_, 'ir>) {
        let loc = self.node_loc(fn_def.syntax());
        // generate IR for all parameter types and generate the function type
        let mut param_types = vec![];
        if let Some(param_list) = fn_def.param_list() {
            for param in param_list.parameters() {
                param_types.push(self.emit_type_or_unknown(b, param.ty()));
            }
        }
        // generate return type
        let arg_types = self.ctxt.alloc_slice_copy(&param_types);
        let return_ty = if let Some(ret_type) = fn_def.ret_type() {
            self.emit_type_or_unknown(b, ret_type.ty())
        } else {
            // no trailing return type in the AST (`-> Type`), so return unit.
            self.ctxt.intern_attr(base::UnitType).upcast()
        };

        // intern the function type
        let function_type = FunctionType { arg_types, return_ty };
        let function_type = b.ctxt_mut().intern_attr(function_type);

        let base::Function {
            op,
            result,
            body,
            arguments,
        } = base::Function::build(b, function_type, loc);

        // generate IR for the function body
        if let Some(block) = fn_def.block() {
            // build the scope containing the function's arguments
            let mut func_scope = FunctionBodyScope::new(Some(scope));
            // TODO unwrap
            if let Some(param_list) = fn_def.param_list() {
                for (value, node) in arguments.iter().zip(param_list.parameters()) {
                    if let Some(ident) = node.ident() {
                        let arg = FunctionArgument {
                            node,
                            hir_value: *value,
                        };
                        func_scope.arguments.insert(ident.text().to_string(), arg);
                    }
                }
            }

            let mut body_builder = b.build_region(body);
            self.emit_block(&mut body_builder, block, &func_scope);
        }
    }
}

pub fn lower_module(
    hir: &mut HirCtxt,
    m: ast::Module,
    file: SourceId,
    source_files: SourceFileProvider,
    diag_writer: &mut dyn WriteColor,
) -> RegionId {
    let diag = Diagnostics::new(source_files, diag_writer, term::Config::default());
    let root_region = hir.create_region();
    let mut lower_ctxt = LowerCtxt::new(hir, file, diag);
    lower_ctxt.emit_module(Cursor::End(root_region), m);
    root_region
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_lower() {}
}
