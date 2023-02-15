//! Lowering to HIR
use crate::{
    builtins::BuiltinSignature,
    diagnostic::Diagnostics,
    hir,
    hir::{FunctionData, GlobalVariableData, IdRef},
    tast::{
        def::{FunctionDef, GlobalDef},
        stmt::Stmt,
        Block, BlockId, Def, ExprId, LocalDefId, LocalVar, LocalVarId, Module, StmtId, TypeCtxt, TypedBody,
    },
};
use ashley::tast::{def::DefKind, expr::ExprKind, stmt::StmtKind, DefId, Qualifier, TypeKind};
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
        // lower package imports first since they may be referred to by other definitions
        for (import_id, package) in self.module.packages.iter_full() {
            for (local_def, def) in package.defs.iter_full() {
                let def_id = DefId {
                    package: Some(import_id),
                    local_def,
                };
                if def.builtin {
                    continue;
                }
                self.lower_def(hir, def, def_id);
            }
        }
        for (def_id, def) in self.module.definitions() {
            if def.builtin {
                continue;
            }
            self.lower_def(hir, def, DefId::from(def_id));
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
            } => self.lower_bin_expr(fb, body, &signature, lhs, rhs, ty),
            ExprKind::BinaryAssign {
                lhs,
                rhs,
                op,
                signature,
            } => {
                self.lower_bin_assign_expr(fb, body, &signature, lhs, rhs, ty);
                return None;
            }
            ExprKind::Unary { expr, signature, op } => self.lower_unary_expr(fb, body, &signature, expr, ty),
            ExprKind::Assign { lhs, rhs } => {
                self.lower_assign_expr(fb, body, lhs, rhs, ty);
                return None;
            }
            ExprKind::Ternary { .. } => {
                todo!("ternary expression")
            }
            ExprKind::Call { .. } => {
                todo!("call expression")
            }
            ExprKind::LocalVar { .. } => {
                todo!("local var expression")
            }
            ExprKind::GlobalVar { .. } => {
                todo!("global var expression")
            }
            ExprKind::FunctionRef { .. } => {
                todo!("function ref expression")
            }
            ExprKind::Index { .. } => {
                todo!("index expression")
            }
            ExprKind::Field { .. } => {
                todo!("field expression")
            }
            ExprKind::ComponentAccess { .. } => {
                todo!("component access expression")
            }
            ExprKind::ImplicitConversion { .. } => {
                todo!("implicit conversion expression")
            }
            ExprKind::Constructor { .. } => {
                todo!("constructor expression")
            }
            ExprKind::Literal { .. } => {
                todo!("literal expression")
            }
            ExprKind::Undef => {
                panic!("invalid expression encountered during lowering")
            }
        };
        Some(TypedIdRef { ty, id })
    }

    fn lower_stmt(&mut self, fb: &mut hir::FunctionBuilder, body: &TypedBody, stmt: StmtId) {
        let stmt = &body.stmts[stmt];
        match stmt.kind {
            StmtKind::Select { .. } => {
                todo!("select statement")
            }
            StmtKind::Local { .. } => {
                todo!("local statement")
            }
            StmtKind::Return { .. } => {
                todo!("return statement")
            }
            StmtKind::ExprStmt { expr } => {
                self.lower_expr(fb, body, expr);
            }
            StmtKind::Block { .. } => {}
            StmtKind::Error => {}
        }
    }

    fn lower_block(&mut self, fb: &mut hir::FunctionBuilder, body: &TypedBody, block: BlockId) {
        let block = &body.blocks[block];
        for stmt in block.stmts.iter() {
            self.lower_stmt(fb, body, *stmt);
        }
    }

    fn lower_function(&mut self, hir: &mut hir::Module, def_id: DefId, def: &Def, function_def: &FunctionDef) {
        // build parameter list
        let params: Vec<_> = function_def
            .parameters
            .iter()
            .map(|param| hir::FunctionParameter {
                name: param.name.clone(),
                ty: self.convert_type(hir, &param.ty),
            })
            .collect();
        let arg_types: Vec<_> = params.iter().map(|param| param.ty).collect();

        let return_type = self.convert_type(
            hir,
            &function_def
                .function_type
                .as_function()
                .expect("expected function type")
                .return_type,
        );

        // create function type
        let func_type = hir.define_type(hir::TypeData::Function(hir::types::FunctionType {
            arg_types: Cow::Owned(arg_types),
            return_type,
        }));

        let ast = function_def.ast.clone().expect("expected function ast");

        // if there's a block, then it's a function definition, otherwise it's just a declaration
        let func_data = if let Some(block) = ast.block() {
            // typecheck the function body
            {
                let typed_body = self.tyctxt.typecheck_body(self.module, def_id.local_def, self.diag);
                if typed_body.has_errors() {
                    // emit a dummy body so that lowering can continue
                    FunctionData::new_declaration(
                        def.name.clone(),
                        func_type,
                        params,
                        function_def.linkage,
                        spirv::FunctionControl::NONE,
                    )
                } else {
                    let (mut func_data, entry_block_id) = hir::FunctionData::new(
                        def.name.clone(),
                        func_type,
                        params,
                        function_def.linkage,
                        spirv::FunctionControl::NONE,
                    );
                    let mut builder = hir::FunctionBuilder::new(hir, &mut func_data, entry_block_id);
                    let entry_block = typed_body.entry_block();
                    self.lower_block(&mut builder, &typed_body, entry_block);

                    if !builder.is_block_terminated() {
                        // TODO check for return type
                        builder.ret()
                    }
                    func_data
                }
            }
        } else {
            // only a declaration
            FunctionData::new_declaration(
                def.name.clone(),
                func_type,
                params,
                function_def.linkage,
                spirv::FunctionControl::NONE,
            )
        };

        self.def_map
            .insert(def_id, HirDef::Function(hir.add_function(func_data.clone())));
    }

    fn lower_def(&mut self, hir: &mut hir::Module, def: &Def, def_id: DefId) {
        match def.kind {
            DefKind::Function(ref function) => {
                self.lower_function(hir, def_id, def, function);
            }
            DefKind::Global(ref global) => {
                self.lower_global(hir, def, global);
            }
            DefKind::Struct(ref struct_def) => {
                todo!("struct lowering")
            }
        }
    }
}

pub fn lower_to_hir(tyctxt: &mut TypeCtxt, module: Module, diag: &mut Diagnostics) -> hir::Module {
    // TODO reuse typechecked bodies?
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
