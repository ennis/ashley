//! Registration of builtin functions and variables during type checking.
use crate::{
    builtins::{pseudo_type_to_concrete_type, ImageClass, PseudoType},
    tast::{
        def::{DefKind, FunctionDef, FunctionParam},
        Def, FunctionType, TypeCheckItemCtxt, Visibility,
    },
};

impl TypeCheckItemCtxt<'_, '_> {
    pub(crate) fn define_builtin_functions(&mut self) {
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
                            .map(|ty| pseudo_type_to_concrete_type(*ty, &self.sess.tyctxt.prim_tys, vec_len, *ic))
                            .collect();
                        let return_type =
                            pseudo_type_to_concrete_type(sig.result_type, &self.sess.tyctxt.prim_tys, vec_len, *ic);
                        let parameters: Vec<FunctionParam> = arg_types
                            .iter()
                            .map(|arg| FunctionParam {
                                ast: None,
                                ty: arg.clone(),
                                name: "".to_string(),
                            })
                            .collect();
                        let function_type = self.sess.tyctxt.ty(FunctionType { arg_types, return_type });

                        self.module.defs.push(Def {
                            //package: None,
                            location: None,
                            builtin: true,
                            // there's an entry named `r#mod` in the builtin signatures because it clashes with the rust keyword, so we need to trim the `r#` prefix
                            name: builtin.name.trim_start_matches("r#").to_string(),
                            visibility: Visibility::Public,
                            kind: DefKind::Function(FunctionDef {
                                ast: None,
                                linkage: None,
                                function_control: spirv::FunctionControl::NONE,
                                function_type,
                                parameters,
                                builtin: Some(sig),
                                execution_model: None,
                            }),
                        });
                    }
                }
            }
        }
    }
}
