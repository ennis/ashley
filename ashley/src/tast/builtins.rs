//! Registration of builtin functions and variables during type checking.
use ashley::builtins::expand_generic_builtin_signature;
use crate::{
    builtins::{BuiltinTypes, PseudoType},
    tast::{
        def::{DefKind, FunctionDef},
        Def, FunctionType, Type, TypeCheckCtxt, TypeKind, Visibility,
    },
};


impl TypeCheckCtxt<'_, '_> {
    pub(crate) fn define_builtin_functions(&mut self) {
        for builtin in crate::builtins::OPERATION_SIGNATURES {
            for sig in builtin.signatures {
                expand_generic_builtin_signature(sig, &self.tyctxt.builtins, |args, ret| {
                    // convert generic builtin signatures to actual function signatures
                    let function_type = self.tyctxt.ty(FunctionType { arg_types, return_type });

                })

                        let arg_types: Vec<_> = sig
                            .parameter_types
                            .iter()
                            .map(|ty| pseudo_type_to_concrete_type(*ty, &self.tyctxt.builtins, vec_len, *ic))
                            .collect();
                        let return_type =
                            pseudo_type_to_concrete_type(sig.result_type, &self.tyctxt.builtins, vec_len, *ic);
                        self.module.defs.push(Def {
                            package: None,
                            location: None,
                            builtin: true,
                            name: builtin.name.to_string(),
                            visibility: Visibility::Public,
                            kind: DefKind::Function(FunctionDef {
                                ast: None,
                                linkage: None,
                                function_control: spirv::FunctionControl::NONE,
                                function_type,
                                parameters: vec![],
                                builtin: Some(sig),
                            }),
                        });
                    }
                }
            }
        }
    }
}
