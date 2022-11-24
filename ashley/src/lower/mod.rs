//! AST lowering to HIR
use crate::{
    diagnostic::{Diagnostics, SourceFileProvider, SourceId, SourceLocation},
    dialect::{
        base,
        base::{BaseDialectBuilder, FunctionType, ScalarType, ScalarTypeKind, TupleType, UnknownType, VectorType},
    },
    hir,
    hir::{Cursor, HirCtxt, Location, OperationId, RegionId, TypeOrAttr, ValueId},
    syntax::{ast, ast::*, ArithOp, BinaryOp, LogicOp, SyntaxNode, SyntaxToken},
};
use codespan_reporting::{term, term::termcolor::WriteColor};
use std::{any::Any, cell::Cell, collections::HashMap};

// Lowering global decls:
// -> process syntax nodes as they arrive
// -> add to symbol table

/*struct TypeDefinition<'hir> {
    /// Syntax node of the item definition.
    item: Item,
    /// HIR type, if this is not a type constructor.
    hir_type: Option<hir::Type<'hir>>,
}*/

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
enum BuiltinTypeKind {
    Int(u8),
    UnsignedInt(u8),
    Float(u8),
    Vector,
    Matrix,
}

enum TypeDefinition<'hir> {
    ///
    Builtin {
        kind: BuiltinTypeKind,
        ty: Cell<Option<hir::Type<'hir>>>,
    },
    /// User-defined type
    User {
        /// Definition syntax node
        def: Item,
        /// If the type is not generic, contains the single instantiation of this type in HIR
        ty: Cell<Option<hir::Type<'hir>>>,
    },
}

/*trait TypeConstructor<'hir> {
    /// Returns the definition site of this type.
    fn def_location(&self) -> Location;

    /// Tries to instantiate this type with the specified parameters.
    // For builtins, instantiates vector, matrix types
    // For definitions:
    fn instantiate(&self, ctxt: &mut HirCtxt<'hir>, diag: &mut Diagnostics, args: &[TypeOrAttr]) -> Option<hir::Type<'hir>>;
}*/

struct VariableDefinition {
    node: SyntaxNode,
    /// HIR value
    hir_value: hir::ValueId,
}

pub struct SymbolTable<'a, 'hir> {
    parent: Option<&'a SymbolTable<'a, 'hir>>,
    /// Types visible in scope.
    types: HashMap<&'hir str, TypeDefinition<'hir>>,
    /// Variables visible in scope.
    variables: HashMap<&'hir str, VariableDefinition>,
}

impl<'a, 'hir> SymbolTable<'a, 'hir> {
    // holds:
    // - type map: name -> TypeDefinition (node + Type<'hir>)
    // - variable map: name -> VariableDefinition (node + ValueId)
    //

    /*pub fn define_type(&mut self, name: Ident, item: Item, hir: hir::Type<'hir>) -> hir::Type<'hir> {

    }*/
}

// trait Scope:
//
// implementors:
// - BuiltinScope, defines builtin types (f32, etc.)
//
// Generic types? e.g. vec
// -> given type parameters (types & attributes), build a concrete HIR type instance

pub trait Scope<'hir> {
    /// Resolves a type or type constructor name.
    fn resolve_type(&mut self, name: &str) -> Option<&TypeDefinition>;
}

/// Root scope containing primitive types & constants.
struct BuiltinScope<'hir> {
    ty_f32: hir::Type<'hir>,
    ty_f64: hir::Type<'hir>,
    ty_u8: hir::Type<'hir>,
    ty_u16: hir::Type<'hir>,
    ty_u32: hir::Type<'hir>,
    ty_i8: hir::Type<'hir>,
    ty_i16: hir::Type<'hir>,
    ty_i32: hir::Type<'hir>,
}

struct LowerCtxt<'a, 'hir, 'diag> {
    ctxt: &'a mut HirCtxt<'hir>,
    current_file: SourceId,
    diag: Diagnostics<'diag>,
    ty_unknown: hir::Type<'hir>,
    ty_f32: hir::Type<'hir>,
    ty_i32: hir::Type<'hir>,
    ty_u32: hir::Type<'hir>,
}

type HirBuilder<'a, 'hir> = hir::RegionBuilder<'a, 'hir>;

impl<'a, 'ir, 'd> LowerCtxt<'a, 'ir, 'd> {
    fn new(ctxt: &'a mut HirCtxt<'ir>, file: SourceId, diag: Diagnostics<'d>) -> LowerCtxt<'a, 'ir, 'd> {
        let ty_unknown = ctxt.intern_type(UnknownType);
        let ty_f32 = ctxt.intern_type(ScalarType(ScalarTypeKind::Float));
        let ty_i32 = ctxt.intern_type(ScalarType(ScalarTypeKind::Int));
        let ty_u32 = ctxt.intern_type(ScalarType(ScalarTypeKind::UnsignedInt));

        LowerCtxt {
            current_file: file,
            diag,
            ty_unknown,
            ty_f32,
            ty_i32,
            ty_u32,
            ctxt,
        }
    }

    fn instantiate_user_type(&mut self, syntax: Item) -> Option<hir::Type<'ir>> {
        eprintln!("TODO instantiate_user_type");
        None
    }

    fn instantiate_builtin_type(
        &mut self,
        kind: BuiltinTypeKind,
        args: &[(TypeOrAttr, Location)],
        instantiation_loc: Location,
    ) -> Option<hir::Type<'ir>> {
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
            BuiltinTypeKind::Float(_) => match bits {
                32 => Some(self.ty_f32),
                64 => {
                    todo!()
                }
                _ => None,
            },
            BuiltinTypeKind::Vector => {
                // element type & len
                let scalar_type = match args.get(0) {
                    Some((TypeOrAttr::Type(ty), loc)) => {
                        if let Some(scalar_type) = ty.cast::<base::ScalarType>() {
                            *scalar_type.0
                        } else {
                            self.diag
                                .error("invalid vector element type")
                                .primary_label(loc.to_source_location(), "")
                                .emit();
                            return None;
                        }
                    }
                    Some((_, loc)) => {
                        self.diag
                            .error("invalid generic arguments or whatever")
                            .primary_label(loc.to_source_location(), "around here")
                            .emit();
                        return None;
                    }
                    None => {
                        self.diag
                            .error("missing generic arguments")
                            .primary_label(instantiation_loc.to_source_location(), "")
                            .emit();
                        return None;
                    }
                };

                match args.get(1) {
                    // it's already annoying to match attribute patterns
                    // with closed-form attribute types, could do `[Attribute::Type(Type::Scalar(kind) Attribute::Integer(value)]`
                    Some((TypeOrAttr::Attribute(len_attr), loc)) => {
                        if let Some(hir::IntegerAttr(val)) = len_attr.cast() {
                            match val {
                                1 => self.ctxt.intern_type(VectorType(scalar_type, 1)),
                                2 => self.ctxt.intern_type(VectorType(scalar_type, 2)),
                                3 => self.ctxt.intern_type(VectorType(scalar_type, 3)),
                                4 => self.ctxt.intern_type(VectorType(scalar_type, 4)),
                                _ => {
                                    self.diag
                                        .error("unsupported vector length")
                                        .primary_label(loc.to_source_location(), "")
                                        .emit();
                                    return None;
                                }
                            }
                        }
                    }
                    Some((_, loc)) => {
                        self.diag
                            .error("invalid generic arguments or whatever")
                            .primary_label(loc.to_source_location(), "around here")
                            .emit();
                        None
                    }
                    None => {
                        self.diag
                            .error("missing generic arguments")
                            .primary_label(instantiation_loc.to_source_location())
                            .emit();
                        None
                    }
                }
            }
            BuiltinTypeKind::Matrix => {
                todo!();
                None
            },
        }
    }


    fn instantiate_type_or_unknown(
        &mut self,
        ty: &TypeDefinition<'ir>,
        args: &[(TypeOrAttr, Location)],
        loc: Location,
    ) -> hir::Type<'ir> {
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

    fn emit_module(&mut self, at: hir::Cursor, module: Module) {
        for item in module.items() {
            self.emit_item(at, item)
        }
    }

    fn emit_item(&mut self, at: hir::Cursor, item: Item) {
        match item {
            Item::FnDef(def) => {
                self.emit_fn_def(at, def);
            }
            Item::Global(global) => {
                //todo!()
            }
        }
    }

    fn emit_expr_or_undef(&mut self, at: hir::Cursor, expr: Option<Expr>) -> hir::ValueId {
        if let Some(expr) = expr {
            self.emit_expr(at, expr)
        } else {
            self.ctxt.undef(at)
        }
    }

    fn emit_expr(&mut self, at: hir::Cursor, expr: Expr) -> hir::ValueId {
        let loc = self.node_loc(expr.syntax());
        match expr {
            Expr::BinExpr(bin_expr) => {
                let lhs = self.emit_expr_or_undef(at, bin_expr.lhs());
                let rhs = self.emit_expr_or_undef(at, bin_expr.rhs());

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
                            ArithOp::Add => self.ctxt.base_add(at, lhs, rhs, loc),
                            ArithOp::Mul => {
                                todo!()
                            }
                            ArithOp::Sub => self.ctxt.base_sub(at, lhs, rhs, loc),
                            ArithOp::Div => {
                                todo!()
                            }
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
                        BinaryOp::Assignment(_) => {
                            todo!()
                        }
                    }
                } else {
                    self.ctxt.undef(at)
                }
            }
            Expr::CallExpr(call_expr) => {
                todo!()
            }
            Expr::IndexExpr(index_expr) => {
                todo!()
            }
            Expr::ParenExpr(expr) => self.emit_expr_or_undef(at, expr.expr()),
            Expr::LitExpr(lit) => match lit.kind() {
                LiteralKind::String(str) => {
                    todo!()
                }
                LiteralKind::IntNumber(v) => {
                    if let Some(value) = v.value() {
                        let attr = self.ctxt.int_const(value as i128);
                        self.ctxt.base_constant(at, attr, loc)
                    } else {
                        self.ctxt.undef(at)
                    }
                }
                LiteralKind::FloatNumber(v) => {
                    if let Some(value) = v.value() {
                        let attr = self.ctxt.fp_const(value);
                        self.ctxt.base_constant(at, attr, loc)
                    } else {
                        self.ctxt.undef(at)
                    }
                }
                LiteralKind::Bool(v) => self.ctxt.bool_const(v),
            },
            Expr::PathExpr(_) => {
                todo!()
            }
            Expr::TupleExpr(_) => {
                todo!()
            }
            Expr::ArrayExpr(_) => {
                todo!()
            }
        }
    }

    /// Emits a primitive builtin type.
    fn emit_builtin_type(&mut self, ident: &str) -> Option<hir::Type<'ir>> {
        match ident {
            "f32" => Some(self.ty_f32),
            "i32" => Some(self.ty_i32),
            "u32" => Some(self.ty_u32),
            _ => None,
        }
    }

    /// Emits IR for a type reference.
    fn emit_type(&mut self, ty: Type) -> hir::Type<'ir> {
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
                    fields.push(self.emit_type(t));
                }
                let fields = self.ctxt.alloc_slice_copy(&fields);
                self.ctxt.intern_type(TupleType(fields))
            }
        }
    }

    fn emit_type_or_unknown(&mut self, ty: Option<Type>) -> hir::Type<'ir> {
        if let Some(ty) = ty {
            self.emit_type(ty)
        } else {
            self.ty_unknown
        }
    }

    /// Emits IR for a block statement.
    fn emit_stmt(&mut self, at: hir::Cursor, stmt: Stmt) {
        match stmt {
            Stmt::ExprStmt(expr) => {
                self.emit_expr_or_undef(at, expr.expr());
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
    fn emit_block(&mut self, at: hir::Cursor, block: Block) {
        for stmt in block.stmts() {
            self.emit_stmt(at, stmt)
        }
    }

    /// Emits IR for a function declaration.
    fn emit_fn_def(&mut self, at: hir::Cursor, fn_def: FnDef) {
        let loc = self.node_loc(fn_def.syntax());
        // build the function type
        let mut param_types = vec![];
        if let Some(param_list) = fn_def.param_list() {
            for param in param_list.parameters() {
                if let Some(ty) = param.ty() {
                    param_types.push(self.emit_type(ty));
                } else {
                    param_types.push(self.ty_unknown);
                }
            }
        }
        let arg_types = self.ctxt.alloc_slice_copy(&param_types);

        let return_ty = if let Some(ret_type) = fn_def.ret_type() {
            self.emit_type_or_unknown(ret_type.ty())
        } else {
            self.ctxt.intern_type(base::UnitType)
        };

        let base::FunctionDefinition {
            op,
            result,
            body,
            arguments,
        } = self.ctxt.base_func(at, FunctionType { arg_types, return_ty }, loc);
        if let Some(block) = fn_def.block() {
            self.emit_block(Cursor::End(body), block);
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
