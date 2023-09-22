use crate::{
    diagnostic::Span,
    ir::Interpolation,
    session::Namespace,
    syntax::{ast, SyntaxNode, SyntaxNodePtr},
    tast,
    tast::{
        attributes::{check_attributes, AttributeTarget, KnownAttribute, KnownAttributeKind},
        def::{DefKind, FunctionDef, FunctionParam, GlobalDef, StructDef},
        layout::{check_struct_layout, StdLayoutRules::Std430},
        scope::{Res, Scope},
        ty::StructField,
        Def, DefId, FunctionType, InFile, NameExt, TypeCheckItemCtxt, TypeKind, Visibility,
    },
};
use ashley_data_structures::{Id, IndexVec};
use ashley_db::new_key_type;
use indexmap::IndexMap;
use rowan::ast::{AstNode, AstPtr};
use std::{collections::HashMap, marker::PhantomData};

////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Attributes on struct fields or variables.
#[derive(Default, Clone, Debug, Eq, PartialEq, Hash)]
struct VariableAttributes {
    location: Option<u32>,
    interpolation: Option<Interpolation>,
    builtin: Option<spirv::BuiltIn>,
    offset: Option<u32>,
    align: Option<u32>,
}

impl VariableAttributes {
    fn from_attributes(attrs: &[KnownAttribute]) -> VariableAttributes {
        let mut location = None;
        let mut interpolation = None;
        let mut builtin = None;
        let mut offset = None;
        let mut align = None;

        for attr in attrs {
            match attr.kind {
                KnownAttributeKind::Interpolate { kind, sampling } => {
                    interpolation = Some(Interpolation {
                        kind,
                        sampling: sampling.unwrap_or_default(),
                    });
                }
                KnownAttributeKind::Location(l) => {
                    location = Some(l);
                }
                KnownAttributeKind::Offset(o) => {
                    offset = Some(o);
                }
                KnownAttributeKind::Align(a) => {
                    align = Some(a);
                }
                KnownAttributeKind::Position => {
                    builtin = Some(spirv::BuiltIn::Position);
                }
                KnownAttributeKind::PointSize => {
                    builtin = Some(spirv::BuiltIn::PointSize);
                }
                _ => {}
            }
        }

        VariableAttributes {
            location,
            interpolation,
            builtin,
            offset,
            align,
        }
    }
}
////////////////////////////////////////////////////////////////////////////////////////////////////

/// Context for item-level type-checking.
pub(crate) struct TypeCheckItemCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    module: ModuleId,
    source_file: SourceFileId,
    typed_module: &'a mut Module,
    scopes: Vec<Scope>,
    errors: Vec<TyDiagnostic>,
}

impl<'a> TypeCheckItemCtxt<'a> {
    pub(crate) fn convert_type(&mut self, ty: ast::Type) -> Type {
        let mut lower_ctxt = TypeLoweringCtxt::new(self.compiler, &self.scopes, &mut self.errors);
        lower_ctxt.lower_type(ty, self.source_file)
    }
}

pub(crate) fn typecheck_items(
    compiler: &dyn CompilerDb,
    module: ModuleId,
    source_file: SourceFileId,
    ast: ast::Module,
) -> Module {
    let mut typed_module = Module::new();

    let mut ctxt = TypeCheckItemCtxt {
        compiler,
        module,
        source_file,
        typed_module: &mut typed_module,
        scopes: Vec::new(),
        errors: vec![],
    };
    ctxt.define_builtin_functions();
    //compiler.push_diagnostic_source_file(source_file);
    ctxt.typecheck_module(&ast);
    //compiler.pop_diagnostic_source_file();

    typed_module.errors = ctxt.errors;
    typed_module
}

impl<'a> TypeCheckItemCtxt<'a> {
    pub(super) fn typecheck_module(&mut self, module: &ast::Module) {
        self.scopes.push(Scope::new());
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
            ast::Item::ImportDecl(_import_decl) => {
                todo!("import declarations")
            }
        }
    }

    fn check_attributes(
        &mut self,
        target: AttributeTarget,
        attrs: impl Iterator<Item = ast::Attribute>,
    ) -> Vec<KnownAttribute> {
        check_attributes(self.compiler, target, attrs)
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
            .unwrap_or_else(|| self.compiler.tyctxt().error.clone());

        //
        let external_linkage = visibility == tast::Visibility::Public || extern_;
        // TODO can't determine whether this is an import or export; can't rely on the presence of an initializer
        // because it's valid to define a global variable without an initializer
        // this should be LinkOnceODR

        // process attributes
        let attrs = self.check_attributes(AttributeTarget::GLOBAL, global.attrs());
        let var_attrs = VariableAttributes::from_attributes(&attrs);

        if external_linkage {
            eprintln!("TODO variable external linkage")
            //todo!("variable external linkage")
        }
        let linkage = None;

        // TODO check validity of qualifiers and attributes for the type

        let def = Def {
            //package: None,
            span: Some(Span::new(self.source_file, global.syntax().text_range())),
            builtin: false,
            name: name.clone(),
            visibility,
            kind: DefKind::Global(GlobalDef {
                ast: Some(InFile::new(self.source_file, AstPtr::new(global))),
                linkage,
                ty,
                qualifier,
                location: var_attrs.location,
                interpolation: var_attrs.interpolation,
                builtin: var_attrs.builtin,
            }),
        };

        let def_id = self.compiler.def_id(self.module, name.clone(), Namespace::Value);
        self.typed_module.defs.insert(def_id, def);
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
        let return_type = fn_def.return_type();
        let block = fn_def.block();

        // build parameter list
        // TODO check for unique parameter names
        let mut params = vec![];
        let mut arg_types = vec![];
        if let Some(param_list) = param_list {
            for param in param_list.parameters() {
                let ty = param
                    .ty()
                    .map(|ty| self.convert_type(ty))
                    .unwrap_or_else(|| self.compiler.tyctxt().error.clone());
                arg_types.push(ty.clone());
                params.push(FunctionParam {
                    ast: Some(InFile::new(self.source_file, AstPtr::new(&param))),
                    name: param
                        .name()
                        .map(|param_name| param_name.text().to_string())
                        .unwrap_or_default(),
                    ty,
                });
            }
        }

        // return type
        let return_type = if let Some(return_type) = return_type {
            self.convert_type(return_type)
        } else {
            self.compiler.tyctxt().prim_tys.void.clone()
        };

        // process attributes
        let known_attributes = self.check_attributes(AttributeTarget::FUNCTION, fn_def.attrs());

        let mut execution_model = None;
        for attr in known_attributes {
            match attr.kind {
                KnownAttributeKind::Vertex => execution_model = Some(spirv::ExecutionModel::Vertex),
                KnownAttributeKind::Fragment => execution_model = Some(spirv::ExecutionModel::Fragment),
                _ => {}
            }
        }

        // create function type
        let func_type = self.compiler.tyctxt().ty(TypeKind::Function(FunctionType {
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
            //package: None,
            span: Some(Span::new(self.source_file, fn_def.syntax().text_range())),
            builtin: false,
            name: name.clone(),
            visibility,
            kind: DefKind::Function(FunctionDef {
                ast: Some(InFile::new(self.source_file, AstPtr::new(fn_def))),
                has_body: fn_def.block().is_some(),
                linkage,
                function_control: spirv::FunctionControl::NONE,
                function_type: func_type.clone(),
                parameters: params,
                builtin: None,
                execution_model,
            }),
        };
        // TODO: check for duplicate definitions
        let def_id = self.compiler.def_id(self.module, name.clone(), Namespace::Value);
        self.typed_module.defs.insert(def_id, def);
        self.scopes.last_mut().unwrap().add_function_overload(name, def_id);
        Some(def_id)
    }

    fn define_struct(&mut self, struct_def: &ast::StructDef) -> DefId {
        let name = struct_def.name().to_string_opt();
        let visibility = struct_def
            .visibility()
            .and_then(|v| v.visibility())
            .unwrap_or(Visibility::Private);
        let mut fields = vec![];
        for field in struct_def.fields() {
            let ty = field
                .ty()
                .map(|ty| self.convert_type(ty.clone()))
                .unwrap_or_else(|| self.compiler.tyctxt().error.clone());

            // process field attributes
            let attrs = self.check_attributes(AttributeTarget::MEMBER, field.attrs());
            let var_attrs = VariableAttributes::from_attributes(&attrs);

            fields.push(StructField {
                name: field.name().to_string_opt(),
                ty,
                syntax: Some(InFile::new_ast_ptr(self.source_file, &field)),
                location: var_attrs.location,
                interpolation: var_attrs.interpolation,
                builtin: var_attrs.builtin,
                offset: var_attrs.offset,
                align: var_attrs.align,
            });
        }

        let layout = if fields.iter().all(|f| !f.ty.is_opaque()) {
            // compute layout if the struct does not have opaque fields
            // TODO attributes for std layout rules
            // TODO move layout check to a separate query/pass; IDE diagnostics don't need layout information immediately.
            let result = check_struct_layout(self.compiler, &name, &fields, Std430, None, &mut self.errors);
            Some(result)
        } else {
            None
        };

        let def_id = self.compiler.def_id(self.module, name.clone(), Namespace::Type);

        // create the struct type
        let ty = self.compiler.tyctxt().ty(TypeKind::Struct {
            name: name.clone(),
            def: Some(def_id),
            fields,
            layout,
        });

        // create the definition
        self.typed_module.defs.insert(
            def_id,
            Def {
                span: Some(Span::new(self.source_file, struct_def.syntax().text_range())),
                builtin: false,
                name,
                visibility,
                kind: DefKind::Struct(StructDef {
                    ast: Some(InFile::new(self.source_file, AstPtr::new(struct_def))),
                    ty,
                }),
            },
        );
        def_id
    }
}
