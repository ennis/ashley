//! Lowering of ASTs to "def" representations.

use crate::{
    db::DebugWithDb,
    def::{
        diagnostic::ItemDiagnostic, ty::TypeKind, AstId, AstMap, DefLoc, Function, FunctionLoc, FunctionParam, Global,
        GlobalLoc, Import, InFile, Linkage, ModuleIndex, ModuleIndexMap, Qualifier, Struct, StructField, StructLoc,
        Type, Visibility,
    },
    syntax::{ast, ast::AstToken, Lang, SyntaxNode, SyntaxNodePtr},
    CompilerDb, ModuleId,
};
use ashley_data_structures::{Id, IndexVec};
use rowan::{
    ast::{AstNode, AstPtr},
    GreenNode,
};
use std::{collections::HashMap, marker::PhantomData, ops::Index};

////////////////////////////////////////////////////////////////////////////////////////////////////
pub struct ItemLowerCtxt<'a> {
    compiler: &'a dyn CompilerDb,
}

fn visibility(vis: &Option<ast::Visibility>) -> Visibility {
    vis.as_ref().and_then(|q| q.visibility()).unwrap_or(Visibility::Private)
}

pub(crate) fn lower_type_opt(db: &dyn CompilerDb, def_map: &mut AstMap, ty: &Option<ast::Type>) -> Type {
    match ty {
        Some(ty) => lower_type(db, def_map, ty),
        None => Type {
            ast_id: None,
            kind: TypeKind::Error,
        },
    }
}

pub(crate) fn lower_type(db: &dyn CompilerDb, def_map: &mut AstMap, ty: &ast::Type) -> Type {
    match ty {
        ast::Type::TypeRef(name) => {
            let Some(name) = name.name() else {
                return Type::error();
            };
            Type {
                ast_id: Some(def_map.push(ty)),
                kind: TypeKind::Name { name: name.text() },
            }
        }
        ast::Type::TupleType(_) => {
            todo!("tuple types")
        }
        ast::Type::ArrayType(array_ty) => {
            let element = Box::new(lower_type_opt(db, def_map, &array_ty.element_type()));
            /*let size = if let Some(expr) = array_ty.length() {
                Some(self.ast_id_map.push(&expr))
            } else {
                None
            };*/

            //let mut stride = None;
            for qualifier in array_ty.qualifiers() {
                match qualifier {
                    ast::TypeQualifier::StrideQualifier(stride_qualifier) => {
                        if let Some(expr) = stride_qualifier.stride() {
                            /*let ast_id = self.ast_id_map.push(&expr);
                            let body_id = self.compiler.body_id(BodyLoc {
                                owner: BodyOwnerId::TypeConst(self),
                                kind: BodyKind::Expr(),
                            })*/
                        }
                    }
                    _ => {
                        // TODO other qualifiers?
                        warn!(
                            "unhandled type qualifier: `{}` ({:?})",
                            qualifier.syntax().text(),
                            qualifier.syntax().kind()
                        );
                    }
                }
            }

            /*if let Some(size) = size {
                Type::Array { element, size, stride }
            } else {
                Type::RuntimeArray { element, stride }
            }*/
            todo!("array types")
        }
        ast::Type::ClosureType(_) => {
            todo!("closure types")
        }
    }
}

impl<'a> ItemLowerCtxt<'a> {
    pub(crate) fn new(compiler: &'a dyn CompilerDb) -> ItemLowerCtxt<'a> {
        ItemLowerCtxt { compiler }
    }

    /*pub(crate) fn lower_module(mut self) -> (ModuleIndex, ModuleItemMap) {
        let module = ast::Module::cast(SyntaxNode::new_root(self.syntax.clone())).expect("invalid module syntax");
        for item in module.items() {
            self.lower_item(&item)
        }

        (self.module, self.map)
    }

    fn lower_item(&mut self, item: &ast::Item) {
        match item {
            ast::Item::Global(global_var) => {
                self.lower_global_variable(global_var);
            }
            ast::Item::FnDef(fn_def) => {
                self.lower_function(fn_def);
            }
            ast::Item::StructDef(struct_def) => {
                self.lower_struct(struct_def);
            }
            ast::Item::ImportDecl(import) => {
                self.lower_import(import);
            }
        }
    }*/

    /*fn lower_import(&mut self, import: &ast::ImportDecl) -> Option<Id<Import>> {
        //let ast_id = self.ast_id_map.push(import);
        // TODO parse URI string (unescape, etc.)
        // TODO parse aliases, module parameters
        let uri = import.uri()?.string()?.text().to_string();
        let id = self.module.imports.push(Import { uri });
        let id2 = self.map.imports.push(AstPtr::new(import));
        Some(id)
    }*/

    fn lower_field(&mut self, def_map: &mut AstMap, field: &ast::StructField) -> Option<StructField> {
        let name = field.name()?.text();
        let ty = self.lower_type_opt(def_map, &field.ty());
        Some(StructField {
            ast: def_map.push(field),
            name,
            ty,
        })
    }

    pub(super) fn lower_struct(mut self, strukt: &ast::StructDef) -> (Struct, AstMap) {
        let mut ast_map = AstMap::new();
        // If there's a syntax error and no name could be parsed, we shouldn't even attempt to
        // lower the fields. Same with functions & globals.
        let name = strukt.name().expect("expected a name").text();
        let mut fields = IndexVec::new();
        for f in strukt.fields() {
            let Some(field) = self.lower_field(&mut ast_map, &f) else {
                continue;
            };
            fields.push(field);
        }

        let attributes = self.lower_attributes(&mut ast_map, strukt.attrs());
        let visibility = visibility(&strukt.visibility());

        (
            Struct {
                attrs: attributes,
                name,
                visibility,
                fields,
            },
            ast_map,
        )
    }

    pub(super) fn lower_function(mut self, func: &ast::FnDef) -> (Function, AstMap) {
        //let ast_id = self.ast_id_map.push(func);
        let mut ast_map = AstMap::new();
        let name = func.name().expect("expected a name").text();
        let mut parameters = IndexVec::new();
        if let Some(param_list) = func.param_list() {
            for p in param_list.parameters() {
                let Some(param) = self.lower_function_param(&mut ast_map, &p) else {
                    continue;
                };
                parameters.push(param);
            }
        }
        let body = func.block().map(|block| ast_map.push(&block));
        let visibility = visibility(&func.visibility());
        let extern_ = func.extern_().map(|linkage| linkage.is_extern()).unwrap_or(false);
        let linkage = if visibility == Visibility::Public || extern_ {
            Linkage::Extern
        } else {
            Linkage::Internal
        };
        let attributes = self.lower_attributes(&mut ast_map, func.attrs());

        let return_type = func.return_type().map(|ty| self.lower_type(&mut ast_map, &ty));

        (
            Function {
                attributes,
                name,
                visibility,
                body,
                linkage,
                parameters,
                return_type,
            },
            ast_map,
        )
    }

    fn lower_attributes(
        &mut self,
        def_map: &mut AstMap,
        attributes: impl Iterator<Item = ast::Attribute>,
    ) -> Vec<AstId<ast::Attribute>> {
        attributes.map(|attr| def_map.push(&attr)).collect()
    }

    fn lower_function_param(&mut self, def_map: &mut AstMap, func_param: &ast::FnParam) -> Option<FunctionParam> {
        let ast_id = def_map.push(func_param);
        let name = func_param.name().map(|name| name.text()).unwrap_or(String::new());
        let ty = self.lower_type_opt(def_map, &func_param.ty());
        Some(FunctionParam { ast: ast_id, name, ty })
    }

    fn lower_type_opt(&mut self, ast_map: &mut AstMap, ty: &Option<ast::Type>) -> Type {
        lower_type_opt(self.compiler, ast_map, ty)
    }

    fn lower_type(&mut self, ast_map: &mut AstMap, ty: &ast::Type) -> Type {
        lower_type(self.compiler, ast_map, ty)
    }

    pub(super) fn lower_global_variable(mut self, global: &ast::Global) -> (Global, AstMap) {
        let mut ast_map = AstMap::new();
        let name = global.name().expect("expected a name").text().to_string();
        let visibility = visibility(&global.visibility());
        let extern_ = global.extern_().map(|linkage| linkage.is_extern()).unwrap_or(false);
        let linkage = if visibility == Visibility::Public || extern_ {
            Linkage::Extern
        } else {
            Linkage::Internal
        };
        let attributes = self.lower_attributes(&mut ast_map, global.attrs());
        let storage_class = global.qualifier().and_then(|q| q.qualifier());

        let ty = self.lower_type_opt(&mut ast_map, &global.ty());

        (
            Global {
                ty,
                attrs: attributes,
                name,
                visibility,
                linkage,
                storage_class,
            },
            ast_map,
        )
    }
}

/*
fn dump_module_items(compiler: &dyn CompilerDb, module: ModuleId, items: &ModuleIndex) {
    use std::fmt::Write;
    let mut o = String::new();

    writeln!(o, "=== imports ===");
    for (i, _s) in items.structs.iter_full() {
        let id = StructLoc { module, strukt: i };
        writeln!(o, "{:?}", id.debug_with(compiler));
    }
    writeln!(o, "=== structs ===");
    for (i, _s) in items.structs.iter_full() {
        let id = StructLoc { module, strukt: i };
        writeln!(o, "{:?}", id.debug_with(compiler));
    }
    writeln!(o, "=== functions ===");
    for (i, _f) in items.functions.iter_full() {
        let id = FunctionLoc { module, function: i };
        writeln!(o, "{:?}", id.debug_with(compiler));
    }
    writeln!(o, "=== globals ===");
    for (i, _g) in items.globals.iter_full() {
        let id = GlobalLoc { module, global: i };
        writeln!(o, "{:?}", id.debug_with(compiler));
    }

    trace!("{}", o);
}
*/
