//! AST lowering to HIR
use ashley::dialect::base::ScalarTypeKind;
use ashley::syntax::{BinaryOp, LogicOp, SyntaxToken};
use crate::diagnostic::Diagnostics;
use crate::dialect::base;
use crate::dialect::base::{BaseDialectBuilder, FunctionType, ScalarType, TupleType, UnknownType};
use crate::hir;
use crate::hir::{HirCtxt};
use crate::syntax::ast::*;
use crate::syntax::{FileId, Span};

// Lowering global decls:
// -> process syntax nodes as they arrive
// -> add to symbol table


struct Definition {
    /// Syntax node of the item definition.
    item: Item,
    /// Definition ID in
    ssa: hir::ValueId,
}

pub struct SymbolTable<'a> {
    parent: Option<&'a SymbolTable<'a>>,

}



impl<'a> SymbolTable<'a> {

    // holds:
    // - type map: name -> TypeDefinition (node + Type<'hir>)
    // - variable map: name -> VariableDefinition (node + ValueId)
    //

    //pub fn define(&mut self, name: Ident, item: Item) ->

}

struct LowerCtxt<'hir, 'diag> {
    current_file: FileId,
    current_file_attr: &'hir hir::StringAttr<'hir>,
    diag: Diagnostics<'diag>,

    ty_unknown: hir::Type<'hir>,
    ty_f32: hir::Type<'hir>,
    ty_i32: hir::Type<'hir>,
    ty_u32: hir::Type<'hir>,
}

type HirBuilder<'a,'hir> = hir::RegionBuilder<'a,'hir>;

impl<'ir, 'd> LowerCtxt<'ir, 'd> {

    fn new(ctxt: &mut HirCtxt<'ir>, diag: Diagnostics<'d>) -> LowerCtxt<'ir, 'd> {
        let ty_unknown = ctxt.intern_type(UnknownType);
        let ty_f32 = ctxt.intern_type(ScalarType(ScalarTypeKind::Float));
        let ty_i32 = ctxt.intern_type(ScalarType(ScalarTypeKind::Int));
        let ty_u32 = ctxt.intern_type(ScalarType(ScalarTypeKind::UnsignedInt));

        LowerCtxt {
            current_file: 0,
            current_file_attr: todo!(),
            diag,
            ty_unknown,
            ty_f32,
            ty_i32,
            ty_u32
        }
    }

    fn span_for_token(&self, token: &SyntaxToken) -> Span {
        Span {
            file_id: self.current_file,
            start: token.text_range().start().into(),
            end: token.text_range().end().into()
        }
    }

    fn emit_item(&mut self, hir: &mut HirBuilder<'_, 'ir>, item: Item) {
        match item {
            Item::FnDef(def) => {
                self.emit_fn_def(hir, def);
            }
            Item::Global(global) => {
                todo!()
            }
        }
    }

    /*fn emit_expr(&mut self, region: RegionAppender, hir: &mut HirCtxt<'hir>, expr: Expr) -> hir::ValueId {
        match expr {
            Expr::BinExpr(bin_expr) => {

                let lhs = if let Some(lhs) = bin_expr.lhs() {

                } else {
                    hir.undef()
                    //self.emit_expr(region, hir, bin_expr.lhs())
                };

                if let Some((_, op)) = bin_expr.op_details() {
                    match op {
                        BinaryOp::LogicOp(logic_op) => {
                            match logic_op {
                                LogicOp::And => {
                                    //hir.base_and()
                                }
                                LogicOp::Or => {}
                            }
                        }
                        BinaryOp::ArithOp(arith_op) => {

                        }
                        BinaryOp::CmpOp(_) => {}
                        BinaryOp::Assignment(_) => {}
                    }
                }
            }
            Expr::CallExpr(_) => {}
            Expr::IndexExpr(_) => {}
            Expr::ParenExpr(_) => {}
            Expr::LitExpr(_) => {}
            Expr::PathExpr(_) => {}
            Expr::TupleExpr(_) => {}
            Expr::ArrayExpr(_) => {}
        }
    }*/

    /// Emits a primitive builtin type.
    fn emit_builtin_type(&mut self, hir: &mut HirBuilder<'_, 'ir>, ident: &str) -> Option<hir::Type<'ir>> {
        match ident {
            "f32" => {
                Some(self.ty_f32)
            }
            "i32" => {
                Some(self.ty_i32)
            }
            "u32" => {
                Some(self.ty_u32)
            }
            _ => None,
        }
    }

    /// Emits IR for a type reference.
    fn emit_type(&mut self, hir: &mut HirBuilder<'_, 'ir>, ty: Type) -> hir::Type<'ir> {
        match ty {
            Type::TypeRef(tyref) => {
                match tyref.ident() {
                    Some(ident) => {
                        // resolve builtin types
                        if let Some(hir_ty) = self.emit_builtin_type(hir, ident.text()) {
                            hir_ty
                        } else {
                            hir.ctxt().intern_type(UnknownType)
                        }

                    }
                    _ => {
                        hir.ctxt().intern_type(UnknownType)
                    }
                }
            }
            Type::TupleType(tuple_ty) => {
                let mut fields = vec![];
                for t in tuple_ty.fields() {
                    fields.push(self.emit_type(hir, t));
                }
                let fields = hir.ctxt().alloc_slice_copy(&fields);
                hir.ctxt().intern_type(TupleType(fields))
            }
        }
    }

    /// Emits IR for a block statement.
    fn emit_stmt(&mut self, hir: &mut HirBuilder<'_, 'ir>, stmt: Stmt) {
        match stmt {
            Stmt::ExprStmt(_) => {}
            Stmt::ReturnStmt(_) => {}
            Stmt::WhileStmt(_) => {}
            Stmt::BreakStmt(_) => {}
            Stmt::ContinueStmt(_) => {}
            Stmt::DiscardStmt(_) => {}
            Stmt::LocalVariable(_) => {}
            Stmt::IfStmt(if_stmt) => {

            }
        }
    }

    /// Emits IR for a block.
    fn emit_block(&mut self, hir: &mut HirBuilder<'_, 'ir>, block: Block) {
        for stmt in block.stmts() {
            //self.emit_stmt()
        }
    }

    /// Emits IR for a function declaration.
    fn emit_fn_def(&mut self, hir: &mut HirBuilder<'_, 'ir>, fn_def: FnDef) {
        // build the function type
        let mut param_types = vec![];
        if let Some(param_list) = fn_def.param_list() {
            for param in param_list.parameters() {
                if let Some(ty) = param.ty() {
                    param_types.push(self.emit_type(hir, ty));
                } else {
                    param_types.push(self.ty_unknown);
                }
            }
        }
        let arg_types = hir.ctxt().alloc_slice_copy(&param_types);
        let return_ty = fn_def.ret_type().and_then(|ty| ty.ty()).map(|ty| self.emit_type(hir, ty)).unwrap_or(self.ty_unknown);
        let base::FunctionDefinition { op, result, body, arguments } = hir.base_func(FunctionType { arg_types, return_ty }, Span::default());

        if let Some(block) = fn_def.block() {
            let mut hir = hir.subregion(body);
            self.emit_block(&mut hir, block);

        }
    }

}