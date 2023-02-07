//! Registration of builtin functions and variables during type checking.
use crate::{
    builtins::{BuiltinTypes, PseudoType},
    tast::{
        def::{DefKind, FunctionDef},
        Def, FunctionType, Type, TypeCheckCtxt, TypeKind, Visibility,
    },
};
use crate::builtins::ImageClass;


impl TypeCheckCtxt<'_, '_> {
    pub(crate) fn define_builtin_functions(&mut self) {
        for builtin in crate::builtins::OPERATION_SIGNATURES {
            for sig in builtin.signatures {
                // convert generic builtin signatures to actual function signatures
                // TODO factor this out
                let is_vector_generic = sig.parameter_types.iter().any(|ty| {
                    use PseudoType::*;
                    matches!(ty, vecN | bvecN | ivecN | uvecN | dvecN)
                });
                let is_image_type_generic = sig.parameter_types.iter().any(|ty| {
                    use PseudoType::*;
                    matches!(
                        ty,
                        gimage1D
                            | gimage1DArray
                            | gimage2D
                            | gimage2DArray
                            | gimage2DMS
                            | gimage2DMSArray
                            | gimage2DRect
                            | gimage3D
                            | gimageCube
                            | gimageCubeArray
                            | gimageBuffer
                            | gtexture1D
                            | gtexture1DArray
                            | gtexture2D
                            | gtexture2DArray
                            | gtexture2DMS
                            | gtexture2DMSArray
                            | gtexture2DRect
                            | gtexture3D
                            | gtextureCube
                            | gtextureCubeArray
                            | gtextureBuffer
                            | gvec4
                    )
                });
                let max_vec_len = if is_vector_generic {
                    4
                } else {
                    /*dummy*/
                    1
                };
                let image_classes = if is_image_type_generic {
                    &[ImageClass::F, ImageClass::SI, ImageClass::UI][..]
                } else {
                    /*dummy*/
                    &[ImageClass::F][..]
                };

                for ic in image_classes {
                    for vec_len in 1..=max_vec_len {
                        let arg_types: Vec<_> = sig
                            .parameter_types
                            .iter()
                            .map(|ty| pseudo_type_to_concrete_type(*ty, &self.tyctxt.builtins, vec_len, *ic))
                            .collect();
                        let return_type =
                            pseudo_type_to_concrete_type(sig.result_type, &self.tyctxt.builtins, vec_len, *ic);
                        let function_type = self.tyctxt.ty(FunctionType { arg_types, return_type });
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
