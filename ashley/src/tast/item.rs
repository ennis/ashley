use crate::{
    diagnostic::AsSourceLocation,
    syntax::ast,
    tast,
    tast::{
        consteval::ConstantValue,
        def::{DefKind, FunctionDef, FunctionParam, GlobalDef, StructDef, StructField},
        scope::Scope,
        Def, DefId, FunctionType, IdentExt, TypeCheckCtxt, TypeKind,
    },
};
use ashley::tast::{scope::Res, ScalarType, Type, Visibility};
use std::{hash::Hash, mem, sync::Arc};

enum Item {
    GlobalVariable(GlobalDef),
    Function(FunctionDef),
    StructDef(StructDef),
}

impl<'a, 'diag> TypeCheckCtxt<'a, 'diag> {
    pub(super) fn typecheck_module(&mut self, module: &ast::Module) {
        let mut scope = Scope::new();
        self.scopes.push(scope);
        for item in module.items() {
            self.typecheck_item(&item);
        }
        self.scopes.pop();
    }

    pub(super) fn typecheck_item(&mut self, item: &ast::Item) {
        match item {
            ast::Item::Global(global_var) => {
                self.typecheck_global_variable(global_var);
            }
            ast::Item::FnDef(fn_def) => {
                self.typecheck_function(fn_def);
            }
            ast::Item::StructDef(struct_def) => {
                self.define_struct(struct_def);
            }
            ast::Item::ImportDecl(import_decl) => {
                todo!("import declarations")
            }
        }
    }

    fn typecheck_global_variable(&mut self, global: &ast::Global) -> Option<DefId> {
        let name = global.name().to_string_opt();
        let qualifier = global.qualifier().and_then(|q| q.qualifier());
        let visibility = global
            .visibility()
            .and_then(|q| q.visibility())
            .unwrap_or(Visibility::Private);
        let extern_ = global.extern_().map(|linkage| linkage.is_extern()).unwrap_or(false);
        let ty = global
            .ty()
            .map(|ty| self.convert_type(ty))
            .unwrap_or_else(|| self.tyctxt.error.clone());

        //
        let external_linkage = visibility == tast::Visibility::Public || extern_;
        // TODO can't determine whether this is an import or export; can't rely on the presence of an initializer
        // because it's valid to define a global variable without an initializer
        // this should be LinkOnceODR

        if external_linkage {
            eprintln!("TODO variable external linkage")
            //todo!("variable external linkage")
        }
        let linkage = None;

        let def = Def {
            package: None,
            location: Some(global.source_location()),
            builtin: false,
            name: name.clone(),
            visibility,
            kind: DefKind::Global(GlobalDef {
                ast: Some(global.clone()),
                linkage,
                ty,
                qualifier,
            }),
        };

        let def_id: DefId = self.module.defs.push(def).into();
        // TODO check for duplicate definitions
        self.scopes.last_mut().unwrap().add(name, Res::Global(def_id));
        Some(def_id)
    }

    /// Lowers a function definition or declaration
    fn typecheck_function(&mut self, fn_def: &ast::FnDef) -> Option<DefId> {
        let name = fn_def.name().to_string_opt();
        let visibility = fn_def
            .visibility()
            .and_then(|q| q.visibility())
            .unwrap_or(Visibility::Private);
        let extern_ = fn_def.extern_().map(|linkage| linkage.is_extern()).unwrap_or(false);
        let param_list = fn_def.param_list();
        let ret_type = fn_def.ret_type();
        let block = fn_def.block();

        // build parameter list
        let mut params = vec![];
        let mut arg_types = vec![];
        if let Some(param_list) = param_list {
            for param in param_list.parameters() {
                let ty = param
                    .ty()
                    .map(|ty| self.convert_type(ty))
                    .unwrap_or_else(|| self.tyctxt.error.clone());
                arg_types.push(ty.clone());
                params.push(FunctionParam {
                    ast: Some(param.clone()),
                    name: param.ident().map(|id| id.text().to_string()).unwrap_or_default(),
                    ty,
                });
            }
        }

        // return type
        let return_type = if let Some(ret_type) = ret_type {
            ret_type
                .ty()
                .map(|ty| self.convert_type(ty))
                .unwrap_or_else(|| self.tyctxt.error.clone())
        } else {
            self.tyctxt.builtins.void.clone()
        };

        // create function type
        let func_type = self.tyctxt.ty(TypeKind::Function(FunctionType {
            arg_types,
            return_type: return_type.clone(),
        }));

        // determine SPIR-V function linkage
        let external_linkage = visibility == tast::Visibility::Public || extern_;

        let linkage = match (external_linkage, block.is_some()) {
            (true, true) => {
                // "extern" on a function definition => export
                Some(spirv::LinkageType::Export)
            }
            (true, false) => {
                // "extern" on a function declaration => import
                Some(spirv::LinkageType::Import)
            }
            (false, true) => {
                // no "extern" on a function definition => internal
                None
            }
            (false, false) => {
                // forward declaration of internal function
                None
            }
        };

        let def = Def {
            package: None,
            location: Some(fn_def.source_location()),
            builtin: false,
            name: name.clone(),
            visibility,
            kind: DefKind::Function(FunctionDef {
                ast: Some(fn_def.clone()),
                linkage,
                function_control: spirv::FunctionControl::NONE,
                function_type: func_type.clone(),
                parameters: params,
            }),
        };
        // TODO: check for duplicate definitions
        let def_id: DefId = self.module.defs.push(def).into();
        self.scopes.last_mut().unwrap().add_function_overload(name, def_id);
        Some(def_id)
    }

    /// Converts an AST type to a TAST type.
    fn convert_type(&mut self, ty: ast::Type) -> Type {
        match ty {
            ast::Type::TypeRef(tyref) => {
                let Some(ident) = tyref.ident() else { return self.tyctxt.error.clone(); };
                if let Some(res) = self.resolve_name(ident.text()) {
                    match res {
                        Res::Global(def_id) => {
                            let def = self.module.def(def_id);
                            match def.kind {
                                DefKind::Struct(ref struct_def) => struct_def.ty.clone(),
                                _ => {
                                    self.diag.error("expected a type").location(ident).emit();
                                    self.tyctxt.error.clone()
                                }
                            }
                        }
                        Res::PrimTy(ty) => ty,
                        _ => {
                            self.diag.error("expected a type").location(ident).emit();
                            self.tyctxt.error.clone()
                        }
                    }
                } else {
                    // TODO better error message
                    self.diag.error("expected a type").location(ident).emit();
                    self.tyctxt.error.clone()
                }
            }
            ast::Type::TupleType(_tuple_ty) => {
                todo!("tuple types");
                /*let fields: Vec<_> = tuple_ty
                    .fields()
                    .enumerate()
                    .map(|(i,f)| StructField {
                        ast: None,
                        name: format!("field{}", i),
                        ty: self.convert_type(f),
                    })
                    .collect();
                self.module.ty(TypeKind::Struct(Arc::new(StructDef {
                    ast: None,
                    name: "".to_string(), // TODO generate a name for tuples (or remove tuples entirely)
                    fields,
                    ty: (),
                })))*/
            }
            ast::Type::ArrayType(array_type) => {
                let element_type = array_type
                    .element_type()
                    .map(|ty| self.convert_type(ty))
                    .unwrap_or_else(|| self.tyctxt.error.clone());

                if let Some(expr) = array_type.length() {
                    if let Some(len) = self.try_evaluate_constant_expression(&expr) {
                        match len {
                            ConstantValue::Int(len) => {
                                let len = len as u32;
                                self.tyctxt.ty(TypeKind::Array(element_type, len))
                            }
                            ConstantValue::Float(_) => {
                                self.diag.error("array length must be an integer").location(expr).emit();
                                self.tyctxt.error.clone()
                            }
                            ConstantValue::Bool(_) => {
                                self.diag.error("array length must be an integer").location(expr).emit();
                                self.tyctxt.error.clone()
                            }
                        }
                    } else {
                        // TODO better error message
                        self.diag
                            .error("array length must be a constant expression")
                            .location(expr)
                            .emit();
                        self.tyctxt.error.clone()
                    }
                } else {
                    // no length
                    self.tyctxt.ty(TypeKind::RuntimeArray(element_type))
                }
            }
            ast::Type::ClosureType(_closure_type) => {
                todo!("closure types")
            }
        }
    }

    fn define_struct(&mut self, struct_def: &ast::StructDef) -> DefId {
        let name = struct_def.ident().to_string_opt();
        let visibility = struct_def
            .visibility()
            .and_then(|v| v.visibility())
            .unwrap_or(Visibility::Private);
        let mut fields = vec![];
        for field in struct_def.fields() {
            let ty = field
                .ty()
                .map(|ty| self.convert_type(ty.clone()))
                .unwrap_or_else(|| self.tyctxt.error.clone());
            fields.push(StructField {
                ast: Some(field.clone()),
                name: field.ident().to_string_opt(),
                ty,
            });
        }

        // define the type before the struct (there's a circular reference)
        let next_def_id = self.module.defs.next_id();
        let ty = self.tyctxt.ty(TypeKind::Struct(DefId::from(next_def_id)));

        let def_id = self.module.defs.push(Def {
            package: None,
            location: Some(struct_def.source_location()),
            builtin: false,
            name,
            visibility,
            kind: DefKind::Struct(StructDef {
                ast: Some(struct_def.clone()),
                fields,
                ty,
            }),
        });
        assert_eq!(def_id, next_def_id);
        def_id.into()
    }
}
