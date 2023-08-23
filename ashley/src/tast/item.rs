use crate::{
    diagnostic::{AsSourceLocation, Diagnostics},
    hir,
    hir::{Interpolation, Layout},
    syntax::ast,
    tast,
    tast::{
        attributes::{check_attributes, AttributeTarget, KnownAttribute, KnownAttributeKind},
        def::{DefKind, FunctionDef, FunctionParam, FunctionQualifiers, GlobalDef, StructDef},
        layout::{
            std_struct_layout,
            StdLayoutRules::{Std140, Std430},
        },
        scope::{Res, Scope},
        ty::StructField,
        Def, DefId, FunctionType, IdentExt, Type, TypeCheckItemCtxt, TypeKind, Visibility,
    },
    utils::round_up,
};
use rowan::ast::AstPtr;

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
            ast::Item::ImportDecl(import_decl) => {
                todo!("import declarations")
            }
        }
    }

    fn check_attributes(
        &mut self,
        target: AttributeTarget,
        attrs: impl Iterator<Item = ast::Attribute>,
    ) -> Vec<KnownAttribute> {
        check_attributes(&self.sess.custom_attributes, target, attrs, &mut self.sess.diag)
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
            .unwrap_or_else(|| self.sess.tyctxt.error.clone());

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
            location: Some(global.source_location()),
            builtin: false,
            name: name.clone(),
            visibility,
            kind: DefKind::Global(GlobalDef {
                ast: Some(ast::AstPtr::new(global)),
                linkage,
                ty,
                qualifier,
                location: var_attrs.location,
                interpolation: var_attrs.interpolation,
                builtin: var_attrs.builtin,
            }),
        };

        let def_id = DefId::new(self.package, self.module.defs.push(def));
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
                    .unwrap_or_else(|| self.sess.tyctxt.error.clone());
                arg_types.push(ty.clone());
                params.push(FunctionParam {
                    ast: Some(AstPtr::new(&param)),
                    name: param.ident().map(|id| id.text().to_string()).unwrap_or_default(),
                    ty,
                });
            }
        }

        // return type
        let return_type = if let Some(return_type) = return_type {
            self.convert_type(return_type)
        } else {
            self.sess.tyctxt.prim_tys.void.clone()
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
        let func_type = self.sess.tyctxt.ty(TypeKind::Function(FunctionType {
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
            location: Some(fn_def.source_location()),
            builtin: false,
            name: name.clone(),
            visibility,
            kind: DefKind::Function(FunctionDef {
                ast: Some(ast::AstPtr::new(fn_def)),
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
        let def_id = DefId::new(self.package, self.module.defs.push(def));
        self.scopes.last_mut().unwrap().add_function_overload(name, def_id);
        Some(def_id)
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
                .unwrap_or_else(|| self.sess.tyctxt.error.clone());

            // process field attributes
            let attrs = self.check_attributes(AttributeTarget::MEMBER, field.attrs());
            let var_attrs = VariableAttributes::from_attributes(&attrs);

            fields.push(StructField {
                name: field.ident().to_string_opt(),
                ty,
                location: var_attrs.location,
                interpolation: var_attrs.interpolation,
                builtin: var_attrs.builtin,
                offset: var_attrs.offset,
                align: var_attrs.align,
            });
        }

        // compute field offsets
        let offsets = match std_struct_layout(
            &fields,
            // TODO attributes for std layouts
            Std430,
            &mut self.sess.diag,
        ) {
            Ok((_layout, offsets)) => Some(offsets),
            Err(_err) => None, // error already reported by std_struct_layout
        };

        // define the type before the struct (there's a circular reference)
        let next_def_id = self.module.defs.next_id();
        let ty = self.sess.tyctxt.ty(TypeKind::Struct {
            name: name.clone(),
            def: Some(DefId::new(self.package, next_def_id)),
            fields,
            offsets,
        });

        let def_id = self.module.defs.push(Def {
            //package: None,
            location: Some(struct_def.source_location()),
            builtin: false,
            name,
            visibility,
            kind: DefKind::Struct(StructDef {
                ast: Some(AstPtr::new(struct_def)),
                ty,
            }),
        });
        assert_eq!(def_id, next_def_id);
        DefId::new(self.package, def_id)
    }
}
