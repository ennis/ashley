use crate::{
    item::{
        AstId, AstIdMap, ConstExpr, Function, FunctionParam, Global, GlobalId, Import, Linkage, ModuleItems, Qualifier,
        Type, Visibility,
    },
    syntax::{ast, ast::AstToken, SyntaxNode, SyntaxNodePtr},
    CompilerDb,
};
use ashley_data_structures::{Id, IndexVec};
use rowan::{ast::AstNode, GreenNode};

pub struct ItemLowerCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    module: ModuleItems,
    ast_id_map: AstIdMap,
}

fn visibility(vis: &Option<ast::Visibility>) -> Visibility {
    vis.as_ref().and_then(|q| q.visibility()).unwrap_or(Visibility::Private)
}

impl<'a> ItemLowerCtxt<'a> {
    pub(crate) fn new(compiler: &'a dyn CompilerDb, syntax: GreenNode) -> ItemLowerCtxt<'a> {
        ItemLowerCtxt {
            compiler,
            module: ModuleItems::new(),
            ast_id_map: AstIdMap {
                syntax,
                map: Default::default(),
            },
        }
    }

    pub(super) fn lower_module(mut self) -> (ModuleItems, AstIdMap) {
        let module =
            ast::Module::cast(SyntaxNode::new_root(self.ast_id_map.syntax.clone())).expect("invalid module syntax");
        for item in module.items() {
            self.lower_item(&item)
        }
        (self.module, self.ast_id_map)
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
                todo!("lower struct")
                //self.lower_struct(struct_def);
            }
            ast::Item::ImportDecl(import) => {
                self.lower_import(import);
            }
        }
    }

    fn lower_import(&mut self, import: &ast::ImportDecl) -> Option<Id<Import>> {
        let ast_id = self.ast_id_map.push(import);
        // TODO parse URI string (unescape, etc.)
        // TODO parse aliases, module parameters
        let uri = import.uri()?.string()?.text().to_string();
        Some(self.module.imports.push(Import { ast: ast_id, uri }))
    }

    fn lower_function(&mut self, func: &ast::FnDef) -> Option<Id<Function>> {
        let ast_id = self.ast_id_map.push(func);
        let name = func.name()?.text();
        let mut parameters = IndexVec::new();
        for p in func.param_list()?.parameters() {
            let Some(param) = self.lower_function_param(&p) else {
                continue;
            };
            parameters.push(param);
        }
        let has_body = func.block().is_some();
        let visibility = visibility(&func.visibility());
        let extern_ = func.extern_().map(|linkage| linkage.is_extern()).unwrap_or(false);
        let linkage = if visibility == Visibility::Public || extern_ {
            Linkage::Extern
        } else {
            Linkage::Internal
        };
        let attributes = self.lower_attributes(func.attrs());

        let return_type = func.return_type().map(|ty| self.lower_type(&ty));

        Some(self.module.functions.push(Function {
            attributes,
            name,
            visibility,
            ast: ast_id,
            has_body,
            linkage,
            parameters,
            return_type,
        }))
    }

    fn lower_attributes(&mut self, attributes: impl Iterator<Item = ast::Attribute>) -> Vec<AstId<ast::Attribute>> {
        attributes.map(|attr| self.ast_id_map.push(&attr)).collect()
    }

    fn lower_function_param(&mut self, func_param: &ast::FnParam) -> Option<FunctionParam> {
        let ast_id = self.ast_id_map.push(func_param);
        let name = func_param.name()?.text();
        // TODO lower even if type is missing or failed to parse?
        let ty = self.lower_type_opt(&func_param.ty());
        Some(FunctionParam { ast: ast_id, name, ty })
    }

    fn lower_type_opt(&mut self, ty: &Option<ast::Type>) -> Type {
        match ty {
            Some(ty) => self.lower_type(ty),
            None => Type::Error,
        }
    }

    fn lower_type(&mut self, ty: &ast::Type) -> Type {
        match ty {
            ast::Type::TypeRef(name) => {
                let Some(name) = name.name() else { return Type::Error };
                Type::Name { name: name.text() }
            }
            ast::Type::TupleType(_) => {
                todo!("tuple types")
            }
            ast::Type::ArrayType(array_ty) => {
                let element = Box::new(self.lower_type_opt(&array_ty.element_type()));
                let size = if let Some(expr) = array_ty.length() {
                    Some(self.ast_id_map.push(&expr))
                } else {
                    None
                };

                let mut stride = None;
                for qualifier in array_ty.qualifiers() {
                    match qualifier {
                        ast::TypeQualifier::StrideQualifier(stride_qualifier) => {
                            if let Some(expr) = stride_qualifier.stride() {
                                stride = Some(self.ast_id_map.push(&expr));
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

                if let Some(size) = size {
                    Type::Array { element, size, stride }
                } else {
                    Type::RuntimeArray { element, stride }
                }
            }
            ast::Type::ClosureType(_) => {
                todo!("closure types")
            }
        }
    }

    fn lower_const_expr_opt(&mut self, expr: &Option<ast::Expr>) -> Option<AstId<ast::Expr>> {
        match expr {
            None => None,
            Some(expr) => Some(self.ast_id_map.push(expr)),
        }
    }

    fn lower_global_variable(&mut self, global: &ast::Global) -> Option<Id<Global>> {
        let name = global.name()?.text().to_string();
        let visibility = visibility(&global.visibility());
        let extern_ = global.extern_().map(|linkage| linkage.is_extern()).unwrap_or(false);
        let linkage = if visibility == Visibility::Public || extern_ {
            Linkage::Extern
        } else {
            Linkage::Internal
        };
        let attributes = self.lower_attributes(global.attrs());
        let storage_class = global.qualifier().and_then(|q| q.qualifier());

        Some(self.module.globals.push(Global {
            ast: self.ast_id_map.push(global),
            attrs: attributes,
            name,
            visibility,
            linkage,
            storage_class,
        }))
    }
}
