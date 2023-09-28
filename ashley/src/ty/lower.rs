//! Lowering of `def::TypeRef`s to `ty::Type`s

use crate::{
    def,
    def::{AstId, AstIdMap, BodyOwnerId, DefLoc, Resolver, Scope, TypeRes, ValueRes},
    syntax::ast,
    ty::{ScalarType, TyDiagnostic, TyOwnerId, Type, TypeCtxt, TypeKind},
    CompilerDb, SourceFileId,
};
use ashley::def::DefAstId;
use std::sync::Arc;

/*fn check_int_or_uint(
    compiler: &dyn CompilerDb,
    source_file: SourceFileId,
    expr: &ast::Expr,
    ty: &Type,
) -> Option<TyDiagnostic> {
    match &*ty.0 {
        TypeKind::Scalar(ScalarType::Int | ScalarType::UnsignedInt) => None,
        _ => {
            // TODO it should be "int or uint", not "uint" only => create a special diagnostic for array sizes
            Some(TyDiagnostic::ConstEvalError(ConstEvalError::TypeMismatch {
                expr: InFile::new_ast_ptr(source_file, expr),
                expected: compiler.tyctxt().prim_tys.uint.clone(),
                resolved: ty.clone(),
            }))
        }
    }
}*/

pub(crate) struct TypeLoweringCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    tyctxt: Arc<TypeCtxt>,
    owner: DefLoc,
    resolver: &'a Resolver<'a>,
    diags: &'a mut Vec<TyDiagnostic>,
}

/// Converts an AST type to a TAST type.
// TODO: rename this to "check_type", it can emit error diagnostics
impl<'a> TypeLoweringCtxt<'a> {
    pub(crate) fn new(
        compiler: &'a dyn CompilerDb,
        resolver: &'a Resolver<'a>,
        owner: DefLoc,
        diags: &'a mut Vec<TyDiagnostic>,
    ) -> TypeLoweringCtxt<'a> {
        let tyctxt = compiler.tyctxt();
        TypeLoweringCtxt {
            compiler,
            tyctxt,
            owner,
            resolver,
            diags,
        }
    }

    pub(crate) fn lower_type(&mut self, ty: &def::Type) -> Type {
        // Don't bother if there's no ast_id: the only case where it happens is if
        // there was no type at all.
        if ty.kind == def::TypeKind::Error || ty.ast_id.is_none() {
            return self.tyctxt.error.clone();
        }

        let ty_loc = DefAstId::new(self.owner, ty.ast_id.unwrap());

        match ty.kind {
            def::TypeKind::Name { ref name } => match self.resolver.resolve_type_name(name) {
                None => {
                    self.diags.push(TyDiagnostic::UnresolvedType {
                        ty_loc,
                        name: name.clone(),
                    });
                    self.tyctxt.error.clone()
                }
                Some(res) => match res {
                    TypeRes::Struct(struct_id) => {
                        todo!("lower struct ref ty")
                        //return self.compiler.def_ty(struct_id.into()).clone()
                    }
                    TypeRes::Primitive(ty) => return ty,
                },
            },
            def::TypeKind::Array {
                ref element,
                size,
                stride,
            } => {
                let element_type = self.lower_type(element);

                //let mut stride_val = None;
                if let Some(stride) = stride {
                    //self.compiler.const_eval(BodyOwnerId::ConstArrayStrideSpecifier())
                }

                todo!("type lowering")

                /*// process type qualifiers
                for qual in array_type.qualifiers() {
                    match qual {
                        TypeQualifier::StrideQualifier(ref q) => {
                            if let Some(stride_expr) = q.stride() {
                                // const-eval the stride expression
                                let stride_val = eval_non_specialization_constant_expr(
                                    self.compiler,
                                    source_file,
                                    &stride_expr,
                                    self.scopes,
                                    &check_int_or_uint,
                                    &mut self.diags,
                                );
                                let Some(stride_val) = stride_val else { continue };
                                let user_stride = stride_val
                                    .to_u32()
                                    .expect("constant evaluation result did not have the expected type");

                                if let Some(elem_layout) = elem_layout {
                                    // stride must not be less than the element type size
                                    let elem_ty_size = elem_layout.size;
                                    if user_stride < elem_ty_size {
                                        self.diags.push(InvalidArrayStride {
                                            stride_qualifier: InFile::new_ast_ptr(source_file, q),
                                            elem_ty_size,
                                            stride: user_stride,
                                        });
                                        // ignore user-specified stride if it is invalid
                                        stride = Some(elem_ty_size);
                                    } else {
                                        stride = Some(user_stride);
                                    }
                                } else {
                                    // cannot specify stride on opaque array
                                    self.diags.push(ArrayStrideOnOpaqueType {
                                        stride_qualifier: InFile::new_ast_ptr(source_file, q),
                                        element_type: element_type.clone(),
                                    });
                                    stride = None;
                                }
                            }
                        }
                        TypeQualifier::RowMajorQualifier(_) => {
                            self.compiler
                                .diag_warn(format!("unimplemented: row_major qualifier"))
                                .location(&qual)
                                .emit();
                        }
                        TypeQualifier::ColumnMajorQualifier(_) => {
                            self.compiler
                                .diag_warn(format!("unimplemented: column_major qualifier"))
                                .location(&qual)
                                .emit();
                        }
                    }
                }*/

                /*if let Some(elem_layout) = element_type.layout() {
                    // compute default stride if the user did not specify it
                    if stride.is_none() {
                        stride = Some(round_up(elem_layout.size, elem_layout.align));
                    }
                }

                // compute stride or check user-specified stride
                if let Some(elem_layout) = element_type.layout() {
                    if let Some(ref mut stride) = stride {
                    } else {
                        // compute stride
                        stride = Some(round_up(elem_layout.size, elem_layout.align));
                    };
                }

                if let Some(expr) = array_type.length() {
                    let length = eval_non_specialization_constant_expr(
                        self.compiler,
                        source_file,
                        &expr,
                        self.scopes,
                        check_int_or_uint,
                        &mut self.diags,
                    );

                    match length {
                        Some(length) => {
                            let length = length
                                .to_u32()
                                .expect("constant evaluation result did not have the expected type");
                            tyctxt.ty(TypeKind::Array {
                                element_type,
                                size: length,
                                stride,
                            })
                        }
                        None => tyctxt.error.clone(),
                    }
                } else {
                    // no length
                    tyctxt.ty(TypeKind::RuntimeArray { element_type, stride })
                }*/
            }
            _ => {
                todo!("type lowering")
            }
        }
    }
}
