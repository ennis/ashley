//! Lowering of ASTs to "def" representations.

use crate::{
    db::DebugWithDb,
    def::{
        diagnostic::ItemDiagnostic, ty::TypeKind, AstId, AstIdMap, DefLoc, DefMap, Function, FunctionLoc,
        FunctionParam, Global, GlobalLoc, Import, InFile, Linkage, ModuleItemMap, ModuleItems, Qualifier, Struct,
        StructField, StructLoc, Type, Visibility,
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
    module: ModuleItems,
    map: ModuleItemMap,
    module_id: ModuleId,
    syntax: GreenNode,
}

fn visibility(vis: &Option<ast::Visibility>) -> Visibility {
    vis.as_ref().and_then(|q| q.visibility()).unwrap_or(Visibility::Private)
}

pub(crate) fn lower_type_opt(db: &dyn CompilerDb, def_map: &mut DefMap, ty: &Option<ast::Type>) -> Type {
    match ty {
        Some(ty) => lower_type(db, def_map, ty),
        None => Type {
            ast_id: None,
            kind: TypeKind::Error,
        },
    }
}

pub(crate) fn lower_type(db: &dyn CompilerDb, def_map: &mut DefMap, ty: &ast::Type) -> Type {
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
    pub(crate) fn new(compiler: &'a dyn CompilerDb, syntax: GreenNode, module_id: ModuleId) -> ItemLowerCtxt<'a> {
        ItemLowerCtxt {
            syntax,
            compiler,
            module: ModuleItems::new(),
            map: ModuleItemMap::new(),
            module_id,
        }
    }

    pub(crate) fn lower_module(mut self) -> (ModuleItems, ModuleItemMap) {
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
    }

    fn lower_import(&mut self, import: &ast::ImportDecl) -> Option<Id<Import>> {
        //let ast_id = self.ast_id_map.push(import);
        // TODO parse URI string (unescape, etc.)
        // TODO parse aliases, module parameters
        let uri = import.uri()?.string()?.text().to_string();
        let id = self.module.imports.push(Import { uri });
        let id2 = self.map.imports.push(AstPtr::new(import));
        Some(id)
    }

    fn lower_field(&mut self, def_map: &mut DefMap, field: &ast::StructField) -> Option<StructField> {
        let name = field.name()?.text();
        let ty = self.lower_type_opt(def_map, &field.ty());
        Some(StructField {
            ast: def_map.push(field),
            name,
            ty,
        })
    }

    fn lower_struct(&mut self, strukt: &ast::StructDef) -> Option<Id<Struct>> {
        let mut def_map = DefMap::new();
        let name = strukt.name()?.text();
        let mut fields = IndexVec::new();
        for f in strukt.fields() {
            let Some(field) = self.lower_field(&mut def_map, &f) else {
                continue;
            };
            fields.push(field);
        }

        let attributes = self.lower_attributes(&mut def_map, strukt.attrs());
        let visibility = visibility(&strukt.visibility());

        let id = self.module.structs.push(Struct {
            attrs: attributes,
            name,
            visibility,
            fields,
        });
        let id2 = self.map.structs.push((AstPtr::new(strukt), def_map));
        assert_eq!(id, id2);
        Some(id)
    }

    fn lower_function(&mut self, func: &ast::FnDef) -> Option<Id<Function>> {
        //let ast_id = self.ast_id_map.push(func);
        let mut def_map = DefMap::new();
        let name = func.name()?.text();
        let mut parameters = IndexVec::new();
        for p in func.param_list()?.parameters() {
            let Some(param) = self.lower_function_param(&mut def_map, &p) else {
                continue;
            };
            parameters.push(param);
        }
        let body = func.block().map(|block| def_map.push(&block));
        let visibility = visibility(&func.visibility());
        let extern_ = func.extern_().map(|linkage| linkage.is_extern()).unwrap_or(false);
        let linkage = if visibility == Visibility::Public || extern_ {
            Linkage::Extern
        } else {
            Linkage::Internal
        };
        let attributes = self.lower_attributes(&mut def_map, func.attrs());

        let return_type = func.return_type().map(|ty| self.lower_type(&mut def_map, &ty));

        let id = self.module.functions.push(Function {
            attributes,
            name,
            visibility,
            body,
            linkage,
            parameters,
            return_type,
        });
        let id2 = self.map.functions.push((AstPtr::new(func), def_map));
        assert_eq!(id, id2);
        Some(id)
    }

    fn lower_attributes(
        &mut self,
        def_map: &mut DefMap,
        attributes: impl Iterator<Item = ast::Attribute>,
    ) -> Vec<AstId<ast::Attribute>> {
        attributes.map(|attr| def_map.push(&attr)).collect()
    }

    fn lower_function_param(&mut self, def_map: &mut DefMap, func_param: &ast::FnParam) -> Option<FunctionParam> {
        let ast_id = def_map.push(func_param);
        let name = func_param.name()?.text();
        // TODO lower even if type is missing or failed to parse?
        let ty = self.lower_type_opt(def_map, &func_param.ty());
        Some(FunctionParam { ast: ast_id, name, ty })
    }

    fn lower_type_opt(&mut self, def_map: &mut DefMap, ty: &Option<ast::Type>) -> Type {
        lower_type_opt(self.compiler, def_map, ty)
    }

    fn lower_type(&mut self, def_map: &mut DefMap, ty: &ast::Type) -> Type {
        lower_type(self.compiler, def_map, ty)
    }

    fn lower_global_variable(&mut self, global: &ast::Global) -> Option<Id<Global>> {
        let mut def_map = DefMap::new();
        let name = global.name()?.text().to_string();
        let visibility = visibility(&global.visibility());
        let extern_ = global.extern_().map(|linkage| linkage.is_extern()).unwrap_or(false);
        let linkage = if visibility == Visibility::Public || extern_ {
            Linkage::Extern
        } else {
            Linkage::Internal
        };
        let attributes = self.lower_attributes(&mut def_map, global.attrs());
        let storage_class = global.qualifier().and_then(|q| q.qualifier());

        let ty = self.lower_type_opt(&mut def_map, &global.ty());

        let id = self.module.globals.push(Global {
            ty,
            attrs: attributes,
            name,
            visibility,
            linkage,
            storage_class,
        });
        let id2 = self.map.globals.push((AstPtr::new(global), def_map));
        assert_eq!(id, id2);
        Some(id)
    }
}

fn dump_module_items(compiler: &dyn CompilerDb, module: ModuleId, items: &ModuleItems) {
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
