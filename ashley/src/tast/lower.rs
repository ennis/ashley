use crate::{
    builtins::BuiltinSignature,
    diagnostic::Diagnostics,
    hir,
    hir::{GlobalVariableData, IdRef},
    tast::{def::GlobalDef, Def, ExprId, LocalDefId, LocalVar, LocalVarId, Module, TypeCtxt, TypedBody},
};
use ashley::tast::{def::DefKind, expr::ExprKind, DefId, Qualifier, TypeKind};
use std::{borrow::Cow, collections::HashMap, ops::Deref};

enum HirDef {
    Variable(hir::GlobalVariable),
    Function(hir::Function),
    Constant(hir::Constant),
}

struct LowerCtxt<'a, 'diag> {
    tyctxt: &'a mut TypeCtxt,
    module: &'a Module,
    diag: &'a mut Diagnostics<'diag>,
    def_map: HashMap<DefId, HirDef>,
    local_map: HashMap<LocalVarId, hir::Local>,
}

struct TypedIdRef {
    ty: hir::Type,
    id: hir::IdRef,
}

// TODO this might move to the HIR builder
struct Place {
    base: hir::IdRef,
    /// Access chain
    indices: Vec<hir::IdRef>,
    /// The type of the place.
    ty: hir::Type,
    ///
    storage_class: spirv::StorageClass,
}

impl<'a, 'diag> LowerCtxt<'a, 'diag> {
    fn lower_module(&mut self, hir: &mut hir::Module) {
        for (def_id, def) in self.module.definitions() {
            if def.builtin {
                continue;
            }
            self.lower_def(hir, def, def_id);
        }
    }

    fn convert_type(&mut self, hir: &mut hir::Module, ty: &crate::tast::Type) -> hir::Type {
        let tydata = match *ty.0.deref() {
            crate::tast::TypeKind::Error => hir::TypeData::Unknown,
            crate::tast::TypeKind::Unit => hir::TypeData::Unit,
            crate::tast::TypeKind::Scalar(scalar_type) => hir::TypeData::Scalar(scalar_type),
            crate::tast::TypeKind::Vector(scalar_type, size) => hir::TypeData::Vector(scalar_type, size),
            crate::tast::TypeKind::Matrix {
                component_type,
                columns,
                rows,
            } => hir::TypeData::Matrix {
                component_type,
                columns,
                rows,
            },
            crate::tast::TypeKind::Array(ref element_type, size) => {
                hir::TypeData::Array(self.convert_type(hir, element_type), size)
            }
            crate::tast::TypeKind::Struct {
                ref name,
                ref fields,
                def,
            } => hir::TypeData::Struct(hir::types::StructType {
                name: Some(Cow::Owned(name.clone())),
                fields: Cow::Owned(
                    fields
                        .iter()
                        .map(|f| hir::types::Field {
                            ty: self.convert_type(hir, &f.1),
                            name: None,
                        })
                        .collect(),
                ),
            }),
            crate::tast::TypeKind::Pointer {
                ref pointee_type,
                storage_class,
            } => hir::TypeData::Pointer {
                pointee_type: self.convert_type(hir, pointee_type),
                storage_class,
            },
            crate::tast::TypeKind::Function(ref fty) => hir::TypeData::Function(hir::types::FunctionType {
                return_type: self.convert_type(hir, &fty.return_type),
                arg_types: Cow::Owned(fty.arg_types.iter().map(|p| self.convert_type(hir, p)).collect()),
            }),
            crate::tast::TypeKind::Image(image_type) => hir::TypeData::Image(image_type),
            crate::tast::TypeKind::Sampler => hir::TypeData::Sampler,
            crate::tast::TypeKind::RuntimeArray(_) => {
                todo!("RuntimeArray")
            }
            crate::tast::TypeKind::SamplerShadow => {
                todo!("SamplerShadow")
            }
            crate::tast::TypeKind::String => {
                panic!("unrepresentable type: string");
            }
            crate::tast::TypeKind::Unknown => {
                panic!("unknown type encountered during lowering");
            }
        };
        hir.define_type(tydata)
    }

    fn lower_global(&mut self, hir: &mut hir::Module, def: &Def, global_def: &GlobalDef) -> hir::GlobalVariable {
        let storage_class = match global_def.qualifier {
            Some(q) => match q {
                Qualifier::Const => {
                    // lower constant
                    todo!("constant lowering")
                }
                Qualifier::Buffer => spirv::StorageClass::StorageBuffer,
                Qualifier::Uniform => spirv::StorageClass::Uniform,
                Qualifier::In => spirv::StorageClass::Input,
                Qualifier::Out => spirv::StorageClass::Output,
                Qualifier::Shared => spirv::StorageClass::Workgroup,
            },
            None => spirv::StorageClass::Private,
        };
        let ty = self.convert_type(hir, &global_def.ty);
        hir.define_global_variable(hir::GlobalVariableData {
            name: def.name.clone(),
            ty,
            storage_class,
            linkage: global_def.linkage,
        })
    }

    fn lower_unary_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        sig: &BuiltinSignature,
        expr: ExprId,
        result_type: hir::Type,
    ) -> IdRef {
        let expr = self.lower_expr(fb, body, expr).expect("expected non-void expression");
        let val = (sig.lower)(&mut (), fb, &[expr.id], &[expr.ty], result_type);
        val.into()
    }

    fn lower_bin_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        sig: &BuiltinSignature,
        lhs: ExprId,
        rhs: ExprId,
        result_type: hir::Type,
    ) -> IdRef {
        let lhs = self.lower_expr(fb, body, lhs).expect("expected non-void expression");
        let rhs = self.lower_expr(fb, body, rhs).expect("expected non-void expression");
        let val = (sig.lower)(&mut (), fb, &[lhs.id, rhs.id], &[lhs.ty, rhs.ty], result_type);
        val.into()
    }

    fn lower_assign_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        lhs: ExprId,
        rhs: ExprId,
        result_type: hir::Type,
    ) {
        let rhs = self.lower_expr(fb, body, rhs).expect("expected non-void expression");
        let lhs = self.lower_place(fb, body, lhs);
        let lhs_ptr_ty = fb.pointer_type(lhs.ty, lhs.storage_class);
        let lhs_ptr = IdRef::from(fb.access_chain(lhs_ptr_ty, lhs.base, &lhs.indices));
        fb.emit_store(lhs_ptr, rhs.id, None)
    }

    fn lower_bin_assign_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        sig: &BuiltinSignature,
        lhs: ExprId,
        rhs: ExprId,
        result_type: hir::Type,
    ) -> IdRef {
        let rhs = self.lower_expr(fb, body, rhs).expect("expected non-void expression");
        let lhs = self.lower_place(fb, body, lhs);
        let lhs_ptr_ty = fb.pointer_type(lhs.ty, lhs.storage_class);
        let lhs_ptr = IdRef::from(fb.access_chain(lhs_ptr_ty, lhs.base, &lhs.indices));
        let lhs_val = fb.emit_load(lhs.ty, lhs_ptr, None);
        let val = IdRef::from((sig.lower)(
            &mut (),
            fb,
            &[IdRef::from(lhs_val), rhs.id],
            &[lhs.ty, rhs.ty],
            result_type,
        ));
        fb.emit_store(lhs_ptr, val, None);
        val
    }

    fn lower_place(&mut self, fb: &mut hir::FunctionBuilder, body: &TypedBody, place_expr: ExprId) -> Place {
        let expr = &body.exprs[place_expr];
        let ty = self.convert_type(fb.module, &expr.ty);
        match expr.kind {
            ExprKind::Field { expr, index } => {
                let mut place = self.lower_place(fb, body, expr);
                let index = fb.const_i32(index as i32);
                place.indices.push(index.into());
                place.ty = ty;
                place
            }
            ExprKind::LocalVar { var } => Place {
                base: self.local_map.get(&var).expect("invalid local var").clone().into(),
                indices: vec![],
                ty,
                storage_class: spirv::StorageClass::Function,
            },
            ExprKind::GlobalVar { var } => {
                let global = self.def_map.get(&var).expect("invalid global var");
                let HirDef::Variable(id) = global else  { panic!("invalid global var") };
                let global = &fb.module.globals[*id];
                Place {
                    base: (*id).into(),
                    indices: vec![],
                    ty,
                    storage_class: global.storage_class,
                }
            }
            ExprKind::Index { array, index } => {
                let mut place = self.lower_place(fb, body, array);
                let index = self.lower_expr(fb, body, index).expect("expected non-void expression");
                place.indices.push(index.id);
                place.ty = ty;
                place
            }
            ExprKind::ComponentAccess { expr, ref components } => {
                todo!("swizzle assignment")
                /*let mut place = self.lower_place(fb, body, expr);
                let index = fb.const_i32(components as i32);
                place.indices.push(index.into());
                place.ty = ty;
                place*/
            }
            _ => {
                panic!("invalid place expression")
            }
        }
    }

    fn access_chain(&mut self, fb: &mut hir::FunctionBuilder, place: Place) -> hir::IdRef {
        let ptr_ty = fb.pointer_type(place.ty, place.storage_class);
        fb.access_chain(ptr_ty, place.base, &place.indices).into()
    }

    fn lower_expr(&mut self, fb: &mut hir::FunctionBuilder, body: &TypedBody, expr_id: ExprId) -> Option<TypedIdRef> {
        let expr = &body.exprs[expr_id];
        let ty = self.convert_type(fb.module, &expr.ty);
        let id = match expr.kind {
            ExprKind::Binary {
                lhs,
                rhs,
                op,
                signature,
            } => {
                self.lower_bin_expr(fb, body, &signature, lhs, rhs, ty)
            },
            ExprKind::BinaryAssign {
                lhs,
                rhs,
                op,
                signature,
            } => {
                self.lower_bin_assign_expr(fb, body, &signature, lhs, rhs, ty);
                return None;
            },
            ExprKind::Unary { expr, signature, op } => self.lower_unary_expr(fb, body, &signature, expr, ty),
            ExprKind::Assign { lhs, rhs } => {
                self.lower_assign_expr(fb, body, lhs, rhs, ty);
                return None;
            },
            ExprKind::Ternary { .. } => {
                todo!("ternary expression")
            }
            ExprKind::Call { .. } => {todo!("call expression")}
            ExprKind::LocalVar { .. } => {todo!("local var expression")}
            ExprKind::GlobalVar { .. } => {todo!("global var expression")}
            ExprKind::FunctionRef { .. } => {todo!("function ref expression")}
            ExprKind::Index { .. } => {todo!("index expression")}
            ExprKind::Field { .. } => {todo!("field expression")}
            ExprKind::ComponentAccess { .. } => {todo!("component access expression")}
            ExprKind::ImplicitConversion { .. } => {todo!("implicit conversion expression")}
            ExprKind::Constructor { .. } => {todo!("constructor expression")}
            ExprKind::Literal { .. } => {todo!("literal expression")}
            ExprKind::Undef => {
                panic!("invalid expression encountered during lowering")
            }
        };
        Some(TypedIdRef { ty, id })
    }

    fn lower_def(&mut self, hir: &mut hir::Module, def: &Def, def_id: LocalDefId) {
        match def.kind {
            DefKind::Function(ref function) => {}
            DefKind::Global(ref global) => {
                self.lower_global(hir, def, global);
            }
            DefKind::Struct(ref struct_def) => {}
        }
    }
}

fn lower_tast(tyctxt: &mut TypeCtxt, module: Module, diag: &mut Diagnostics) -> hir::Module {
    let mut ctxt = LowerCtxt {
        tyctxt,
        module: &module,
        diag,
        def_map: Default::default(),
        local_map: Default::default(),
    };
    let mut hir = hir::Module::new();
    ctxt.lower_module(&mut hir);
    hir
}
