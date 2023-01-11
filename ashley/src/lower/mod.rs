//! AST lowering to SPIR-V
use crate::{diagnostic::{Diagnostics, SourceFileProvider, SourceId, SourceLocation}, hir, syntax::{ast, ast::*, ArithOp, BinaryOp, LogicOp, SyntaxNode, SyntaxToken}};
use codespan_reporting::{diagnostic::Diagnostic, term, term::termcolor::WriteColor};
use smallvec::SmallVec;
use std::{any::Any, cell::Cell, collections::HashMap, io::ErrorKind, sync::Arc};

use rspirv::dr::Builder;
use crate::hir::{Constant, Function, TypeData};
use crate::hir::types::ScalarType;

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

#[derive(Copy, Clone, Debug)]
enum GenericArgument {
    Type(rspirv),
    Constant(hir::ConstantImpl),
}

//--------------------------------------------------------------------------------------------------

/*/// Function on an atomic value.
#[derive(Clone, Copy, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub enum AtomicFunction {
    Add,
    Subtract,
    And,
    ExclusiveOr,
    InclusiveOr,
    Min,
    Max,
    Exchange { compare: Option<Handle<Expression>> },
}*/

/// Built-in shader function for testing relation between values.
#[derive(Clone, Copy, Debug, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub enum RelationalFunction {
    All,
    Any,
    IsNan,
    IsInf,
    IsFinite,
    IsNormal,
}

/// Represents the result of function resolution: either a user-defined function or a builtin function.
enum FunctionOrBuiltin {
    Function(FunctionId),
    BuiltinMath(MathFunction),
    BuiltinRelational(RelationalFunction),
}

trait Scope {
    /// Resolves a type name to a HIR type.
    fn resolve_type(&self, name: &str) -> Option<hir::Type>;
    /// Resolves a function name to a HIR function.
    fn resolve_func(&self, name: &str) -> Option<hir::FunctionId>;
    /// Resolves a constant name to a HIR constant.
    fn resolve_const(&self, name: &str) -> Option<hir::Constant>;
}

/// Root scope containing primitive types & constants.
struct BuiltinScope {
    ty_f32: hir::Type,
    ty_f64: hir::Type,
    ty_u32: hir::Type,
    ty_i32: hir::Type,
    ty_bool: hir::Type,
    ty_f32x2: hir::Type,
    ty_f32x3: hir::Type,
    ty_f32x4: hir::Type,
    ty_i32x2: hir::Type,
    ty_i32x3: hir::Type,
    ty_i32x4: hir::Type,
    ty_u32x2: hir::Type,
    ty_u32x3: hir::Type,
    ty_u32x4: hir::Type,
    ty_b1x2: hir::Type,
    ty_b1x3: hir::Type,
    ty_b1x4: hir::Type,
    ty_texture_1d: hir::Type,
    ty_texture_2d: hir::Type,
    ty_texture_3d: hir::Type,
    ty_texture_1d_array: hir::Type,
    ty_texture_2d_array: hir::Type,
    ty_texture_cube: hir::Type,
    ty_texture_1d_ms: hir::Type,
    ty_texture_2d_ms: hir::Type,
    ty_image_1d: hir::Type,
    ty_image_2d: hir::Type,
    ty_image_3d: hir::Type,
    ty_image_1d_array: hir::Type,
    ty_image_2d_array: hir::Type,
    ty_image_cube: hir::Type,
    ty_image_1d_ms: hir::Type,
    ty_image_2d_ms: hir::Type,
}

macro_rules! make_image_type {
    ($hir:expr, texture $d:ident $t:ident) => {
        module.define_type(TypeData::SampledImage(Arc::new(SampledImageType {
            sampled_ty: ScalarType::$t,
            dim: ImageDimension::$d,
            ms: false,
        })))
    };
    ($hir:expr, texture $d:ident $t:ident MS) => {
        module.define_type(TypeData::SampledImage(Arc::new(SampledImageType {
            sampled_ty: ScalarType::$t,
            dim: ImageDimension::$d,
            ms: true,
        })))
    };
    ($hir:expr, image $d:ident $t:ident) => {
        module.define_type(TypeData::SampledImage(Arc::new(SampledImageType {
            element_ty: ScalarType::$t,
            dim: ImageDimension::$d,
            ms: false,
        })))
    };
    ($hir:expr, image $d:ident $t:ident MS) => {
        module.define_type(TypeData::SampledImage(Arc::new(SampledImageType {
            sampled_ty: ScalarType::$t,
            dim: ImageDimension::$d,
            ms: true,
        })))
    };
}

impl BuiltinScope {
    fn new(module: &mut hir::Module) -> BuiltinScope {
        let ty_f32 = module.define_type(TypeData::Scalar(ScalarType::Float));
        let ty_f64 = module.define_type(TypeData::Scalar(ScalarType::Double));
        let ty_u32 = module.define_type(TypeData::Scalar(ScalarType::UnsignedInt));
        let ty_i32 = module.define_type(TypeData::Scalar(ScalarType::Int));

        let ty_f32x2 = module.define_type(TypeData::Vector(ScalarType::Float, 2));
        let ty_f32x3 = module.define_type(TypeData::Vector(ScalarType::Float, 3));
        let ty_f32x4 = module.define_type(TypeData::Vector(ScalarType::Float, 4));

        let ty_i32x2 = module.define_type(TypeData::Vector(ScalarType::Int, 2));
        let ty_i32x3 = module.define_type(TypeData::Vector(ScalarType::Int, 3));
        let ty_i32x4 = module.define_type(TypeData::Vector(ScalarType::Int, 4));

        let ty_u32x2 = module.define_type(TypeData::Vector(ScalarType::UnsignedInt, 2));
        let ty_u32x3 = module.define_type(TypeData::Vector(ScalarType::UnsignedInt, 3));
        let ty_u32x4 = module.define_type(TypeData::Vector(ScalarType::UnsignedInt, 4));

        let ty_b1x2 = module.define_type(TypeData::Vector(ScalarType::Bool, 2));
        let ty_b1x3 = module.define_type(TypeData::Vector(ScalarType::Bool, 3));
        let ty_b1x4 = module.define_type(TypeData::Vector(ScalarType::Bool, 4));

        // sampled images
        let ty_texture_1d = make_image_type!(hir, texture Dim1D       Float   );
        let ty_texture_2d = make_image_type!(hir, texture Dim2D       Float   );
        let ty_texture_3d = make_image_type!(hir, texture Dim3D       Float   );
        let ty_texture_1d_array = make_image_type!(hir, texture Dim1DArray  Float   );
        let ty_texture_2d_array = make_image_type!(hir, texture Dim2DArray  Float   );
        let ty_texture_cube = make_image_type!(hir, texture DimCube     Float   );
        let ty_texture_1d_ms = make_image_type!(hir, texture Dim1D       Float MS);
        let ty_texture_2d_ms = make_image_type!(hir, texture Dim2D       Float MS);

        // storage images
        let ty_image_1d = make_image_type!(hir, image   Dim1D       Float   );
        let ty_image_2d = make_image_type!(hir, image   Dim2D       Float   );
        let ty_image_3d = make_image_type!(hir, image   Dim3D       Float   );
        let ty_image_1d_array = make_image_type!(hir, image   Dim1DArray  Float   );
        let ty_image_2d_array = make_image_type!(hir, image   Dim2DArray  Float   );
        let ty_image_cube = make_image_type!(hir, image   DimCube     Float   );
        let ty_image_1d_ms = make_image_type!(hir, image   Dim1D       Float MS);
        let ty_image_2d_ms = make_image_type!(hir, image   Dim2D       Float MS);

        BuiltinScope {
            ty_f32,
            ty_f64,
            ty_u32,
            ty_i32,
            ty_bool,
            ty_f32x2,
            ty_f32x3,
            ty_f32x4,
            ty_i32x2,
            ty_i32x3,
            ty_i32x4,
            ty_u32x2,
            ty_u32x3,
            ty_u32x4,
            ty_b1x2,
            ty_b1x3,
            ty_b1x4,
            ty_texture_1d,
            ty_texture_2d,
            ty_texture_3d,
            ty_texture_1d_array,
            ty_texture_2d_array,
            ty_texture_cube,
            ty_texture_1d_ms,
            ty_texture_2d_ms,
            ty_image_1d,
            ty_image_2d,
            ty_image_3d,
            ty_image_1d_array,
            ty_image_2d_array,
            ty_image_cube,
            ty_image_1d_ms,
            ty_image_2d_ms,
        }
    }
}

impl Scope for BuiltinScope {
    fn resolve_type(&self, name: &str) -> Option<hir::Type> {
        match name {
            "f32" => Some(self.ty_f32),
            "f64" => Some(self.ty_f64),
            "u32" => Some(self.ty_u32),
            "i32" => Some(self.ty_i32),
            "bool" => Some(self.ty_bool),
            "f32x2" => Some(self.ty_f32x2),
            "f32x3" => Some(self.ty_f32x3),
            "f32x4" => Some(self.ty_f32x4),
            "i32x2" => Some(self.ty_i32x2),
            "i32x3" => Some(self.ty_i32x3),
            "i32x4" => Some(self.ty_i32x4),
            "u32x2" => Some(self.ty_u32x2),
            "u32x3" => Some(self.ty_u32x3),
            "u32x4" => Some(self.ty_u32x4),
            "b1x2" => Some(self.ty_b1x2),
            "b1x3" => Some(self.ty_b1x3),
            "b1x4" => Some(self.ty_b1x4),
            "Texture1D" => Some(self.ty_texture_1d),
            "Texture2D" => Some(self.ty_texture_2d),
            "Texture3D" => Some(self.ty_texture_3d),
            "Texture1DArray" => Some(self.ty_texture_1d_array),
            "Texture2DArray" => Some(self.ty_texture_2d_array),
            "TextureCube" => Some(self.ty_texture_cube),
            "Texture1DMS" => Some(self.ty_texture_1d_ms),
            "Texture2DMS" => Some(self.ty_texture_2d_ms),
            "Image1D" => Some(self.ty_image_1d),
            "Image2D" => Some(self.ty_image_2d),
            "Image3D" => Some(self.ty_image_3d),
            "Image1DArray" => Some(self.ty_image_1d_array),
            "Image2DArray" => Some(self.ty_image_2d_array),
            "ImageCube" => Some(self.ty_image_cube),
            "Image1DMS" => Some(self.ty_image_1d_ms),
            "Image2DMS" => Some(self.ty_image_2d_ms),
            _ => None,
        }
    }

    fn resolve_func(&self, name: &str) -> Option<Function> {
        None
    }

    fn resolve_const(&self, name: &str) -> Option<Constant> {
        None
    }
}

/// Simple scope, contains global variables.
struct SimpleScope<'a> {
    parent: Option<&'a dyn Scope>,
    types: HashMap<String, Type>,
    functions: HashMap<String, Function>,
    constants: HashMap<String, Constant>
    //variables: HashMap<String, VariableDefinition>,
}

impl<'a> SimpleScope<'a> {
    fn new(parent: Option<&'a dyn Scope>) -> SimpleScope<'a> {
        SimpleScope {
            parent,
            types: Default::default(),
            //variables: Default::default(),
        }
    }
}

impl<'a> Scope for SimpleScope<'a> {
    fn resolve_type(&self, name: &str) -> Option<Type> {
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
struct FunctionBodyScope<'a> {
    parent: Option<&'a dyn Scope>,
    arguments: HashMap<String, FunctionArgument>,
}

impl<'a> FunctionBodyScope<'a> {
    fn new(parent: Option<&'a dyn Scope>) -> FunctionBodyScope<'a> {
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
    hir_value: hir::Value,
}

/// Scope defined by a block inside a function.
struct BlockScope<'a> {
    parent: Option<&'a dyn Scope>,
    variables: HashMap<String, LocalVariableDefinition>,
}

impl<'a> BlockScope<'a> {
    fn new(parent: Option<&'a dyn Scope>) -> BlockScope<'a> {
        BlockScope {
            parent,
            variables: Default::default(),
        }
    }

    /// Declares local variable in this scope, Possibly shadows an existing one.
    fn declare_variable(&mut self, def: LocalVariableDefinition) {}
}

impl<'a> Scope for BlockScope<'a> {
    fn resolve_type(&self, name: &str) -> Option<Type> {
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

    fn token_loc(&self, token: &SyntaxToken) -> SourceLocation {
        SourceLocation::new(self.current_file, token.text_range())
    }

    fn node_loc(&self, node: &SyntaxNode) -> SourceLocation {
        SourceLocation::new(self.current_file, node.text_range())
    }

    /// Emits IR for a module.
    ///
    /// Essentially, creates a HIR program.
    fn emit_module(&mut self, b: &mut hir::FunctionBuilder, module: Module) {
        let mut root_scope = BuiltinScope::new(b);
        let mut global_scope = SimpleScope::new(Some(&root_scope));
        for item in module.items() {
            self.emit_item(b, item, &mut global_scope)
        }
    }

    fn emit_item(&mut self, b: &mut hir::FunctionBuilder, item: Item, scope: &mut SimpleScope<'_>) {
        match item {
            Item::FnDef(def) => {
                self.emit_fn_def(b, def, scope);
            }
            Item::Global(global) => {
                let Some(name) = global.name() else {
                    // no diagnostic, this is a syntax error
                    return;
                };
                let Some(kind) = global.qualifier().and_then(|q| q.global_kind()) else {
                    // no diagnostic, this is a syntax error
                    return;
                };
                let ty = self.emit_type_or_unknown(b, global.ty());

                match kind {
                    Some(GlobalKind::Uniform) => self.program_inputs.push(RequiredValue {
                        name: "",
                        ty,
                        domain: None,
                        fulfillment: None,
                    }),
                    Some(GlobalKind::Const) => {}
                    Some(GlobalKind::In) => {}
                    Some(GlobalKind::Out) => {}
                    None => {}
                }
            }
        }
    }


    /// Generates IR for a function.
    fn emit_function(&mut self,
                     b: &mut hir::FunctionBuilder, func: Option<FnDef>, scope: &mut SimpleScope<'_>) {

    }


    /// Generates IR for an expression node, or returns undef if the expression is `None`.
    fn emit_expr_or_undef(
        &mut self,
        b: &mut hir::FunctionBuilder,
        expr: Option<Expr>,
        scope: &dyn Scope,
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
    fn emit_expr(&mut self, b: &mut hir::FunctionBuilder, expr: Expr, scope: &dyn Scope) -> hir::ValueId {
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
                            ArithOp::Add => b.emit_f_add(lhs, rhs),
                            ArithOp::Mul => b.emit_mul(lhs, rhs, loc),
                            ArithOp::Sub => b.emit_sub(lhs, rhs, loc),
                            ArithOp::Div => b.emit_div(lhs, rhs, loc),
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
                b.emit_call(func, args, self.node_loc(call_expr.syntax()))
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
                        let c = b.int_const(value as i64);
                        b.emit_constant(c, loc)
                    } else {
                        b.undef()
                    }
                }
                LiteralKind::FloatNumber(v) => {
                    if let Some(value) = v.value() {
                        let c = b.fp_const(value);
                        b.emit_constant(c, loc)
                    } else {
                        b.undef()
                    }
                }
                LiteralKind::Bool(v) => {
                    let c = b.bool_const(v);
                    b.emit_constant(c, loc)
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
    fn emit_type(&mut self, b: &mut hir::FunctionBuilder, ty: Type) -> hir::Type {
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
            Type::ArrayType(array_type) => {

            }
            Type::ClosureType(closure_type) => {

            }
        }
    }

    fn emit_type_or_unknown(&mut self, b: &mut hir::FunctionBuilder, ty: Option<Type>) -> hir::Type {
        if let Some(ty) = ty {
            self.emit_type(b, ty)
        } else {
            b.ty_unknown()
        }
    }

    /// Emits IR for a block statement.
    fn emit_stmt(&mut self, b: &mut hir::FunctionBuilder, stmt: Stmt, scope: &mut BlockScope) {
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
    fn emit_block(&mut self, b: &mut hir::FunctionBuilder, block: Block, scope: &dyn Scope) {
        // start block scope
        let mut block_scope = BlockScope::new(Some(scope));
        for stmt in block.stmts() {
            self.emit_stmt(b, stmt, &mut block_scope)
        }
    }

    /// Emits IR for a function declaration.
    fn emit_fn_def(&mut self, b: &mut hir::FunctionBuilder, fn_def: FnDef, scope: &mut SimpleScope) {
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
