//! Registration of builtin functions and variables during type checking.

// FIXME: how about moving this alongside "builtin.rs"? That's where I have the reflex to look.

use crate::{
    builtins::{pseudo_type_to_concrete_type, ImageClass, PseudoType},
    session::Namespace,
    tast::{
        def::{DefKind, FunctionDef, FunctionParam},
        Def, FunctionType, TypeCheckItemCtxt, Visibility,
    },
    utils::CommaSeparated,
};

impl TypeCheckItemCtxt<'_> {
    pub(crate) fn define_builtin_functions(&mut self) {
        let tyctxt = self.compiler.tyctxt();
        for builtin in crate::builtins::OPERATION_SIGNATURES {
            for sig in builtin.signatures {
                // convert generic builtin signatures to actual function signatures
                let is_vector_generic = sig.parameter_types.iter().any(PseudoType::is_vector_generic);
                let is_image_type_generic = sig.parameter_types.iter().any(PseudoType::is_image_type_generic);
                let max_vec_len = if is_vector_generic { 4 } else { 1 };
                let image_classes = if is_image_type_generic {
                    &[ImageClass::F, ImageClass::SI, ImageClass::UI][..]
                } else {
                    &[ImageClass::F][..]
                };

                for ic in image_classes {
                    for vec_len in 1..=max_vec_len {
                        let arg_types: Vec<_> = sig
                            .parameter_types
                            .iter()
                            .map(|ty| pseudo_type_to_concrete_type(*ty, &tyctxt.prim_tys, vec_len, *ic))
                            .collect();
                        let return_type = pseudo_type_to_concrete_type(sig.result_type, &tyctxt.prim_tys, vec_len, *ic);
                        let parameters: Vec<FunctionParam> = arg_types
                            .iter()
                            .map(|arg| FunctionParam {
                                ast: None,
                                ty: arg.clone(),
                                name: "".to_string(),
                            })
                            .collect();

                        // there's an entry named `r#mod` in the builtin signatures because it clashes with the rust keyword, so we need to trim the `r#` prefix
                        let real_name = builtin.name.trim_start_matches("r#").to_string();

                        // use a name unique across all overloads for the DefId
                        let overload_unique_name =
                            format!("{real_name}({})->{}", CommaSeparated(&arg_types), return_type);

                        let def_id = self
                            .compiler
                            .def_id(self.module, overload_unique_name, Namespace::Value);

                        if self.typed_module.defs.contains_key(&def_id) {
                            // It's possible to produce two overloads with the same real signature when resolving pseudo-types.
                            // E.g:
                            //      `vecN clamp(vecN,vecN,vecN)`
                            // and
                            //      `vecN clamp(vecN,float,float)`
                            // produce the same overload (`float clamp(float,float,float)`)
                            // with the pseudo-type substitution: vecN => float`.
                            //
                            // We ignore all subsequent overloads with the same signature. They are supposed to be functionally equivalent
                            // (although they might have different lowerings: see the comment on `builtin_operations!` about that).
                            continue;
                        }

                        let function_type = tyctxt.ty(FunctionType { arg_types, return_type });

                        self.typed_module.defs.insert(
                            def_id,
                            Def {
                                //package: None,
                                location: None,
                                builtin: true,
                                name: real_name.clone(),
                                visibility: Visibility::Public,
                                kind: DefKind::Function(FunctionDef {
                                    ast: None,
                                    has_body: false,
                                    linkage: None,
                                    function_control: spirv::FunctionControl::NONE,
                                    function_type,
                                    parameters,
                                    builtin: Some(sig),
                                    execution_model: None,
                                }),
                            },
                        );
                    }
                }
            }
        }
    }
}
