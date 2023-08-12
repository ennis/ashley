//! Lowering to HIR
use crate::{
    builtins::{BuiltinSignature, Constructor},
    hir,
    hir::{Decoration, FunctionData, IdRef, Interpolation, InterpolationKind, InterpolationSampling, StructLayout},
    session::PackageId,
    syntax::{ast, ast::UnaryOp},
    tast::{
        consteval::ConstantValue,
        def::{DefKind, FunctionDef, GlobalDef},
        expr::{ConversionKind, ExprKind},
        stmt::StmtKind,
        swizzle::ComponentIndices,
        BlockId, Def, DefId, ExprId, LocalVarId, Module, Qualifier, StmtId, TypedBody,
    },
    QueryError, Session,
};
use spirv::StorageClass;
use std::{borrow::Cow, collections::HashMap, ops::Deref};

enum HirDef {
    Variable(hir::GlobalVariable),
    Function(hir::Function),
    Constant(hir::Constant),
}

enum LocalOrParam {
    // Pointer-typed
    Local(hir::Value),
    // Not necessarily pointer-typed
    Param(hir::Value),
}

struct LowerCtxt<'a> {
    sess: &'a Session,
    module: &'a Module,
    def_map: HashMap<DefId, HirDef>,
    local_map: HashMap<LocalVarId, hir::Value>,
}

struct TypedIdRef {
    ty: hir::Type,
    id: hir::IdRef,
}

fn apply_interpolation_decorations(decorations: &mut Vec<Decoration>, interp: &Option<Interpolation>) {
    if let Some(interp) = interp {
        match interp.kind {
            InterpolationKind::Flat => decorations.push(hir::Decoration::Flat),
            InterpolationKind::NoPerspective => decorations.push(hir::Decoration::NoPerspective),
            InterpolationKind::Smooth => {}
        }
        match interp.sampling {
            InterpolationSampling::Center => {}
            InterpolationSampling::Centroid => decorations.push(hir::Decoration::Centroid),
            InterpolationSampling::Sample => decorations.push(hir::Decoration::Sample),
        }
    }
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

impl<'a> LowerCtxt<'a> {
    fn lower_module(&mut self, package_id: PackageId, hir: &mut hir::Module) {
        for (def_id, def) in self.module.definitions() {
            if def.builtin {
                continue;
            }
            self.lower_def(hir, def, DefId::new(package_id, def_id));
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
                stride,
            } => hir::TypeData::Matrix {
                component_type,
                columns,
                rows,
                stride,
            },
            crate::tast::TypeKind::Array {
                ref element_type,
                size,
                stride,
            } => {
                let element_type = self.convert_type(hir, element_type);
                hir::TypeData::Array {
                    element_type,
                    size,
                    stride,
                }
            }
            crate::tast::TypeKind::Struct {
                ref name,
                ref fields,
                def: _,
                ref offsets,
            } => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|f| hir::types::Field {
                        ty: self.convert_type(hir, &f.ty),
                        name: None,
                        interpolation: None, // TODO
                    })
                    .collect();

                let layout = if let Some(offsets) = offsets {
                    Some(StructLayout {
                        offsets: offsets.clone(),
                        layouts: vec![],
                    })
                } else {
                    None
                };

                hir::TypeData::Struct(hir::types::StructType {
                    name: Some(Cow::Owned(name.clone())),
                    fields: fields.into(),
                    layout,
                    block: false,
                })
            }
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
            crate::tast::TypeKind::RuntimeArray { .. } => {
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

    fn lower_global(
        &mut self,
        hir: &mut hir::Module,
        def_id: DefId,
        def: &Def,
        global_def: &GlobalDef,
    ) -> hir::GlobalVariable {
        let storage_class = match global_def.qualifier {
            Some(q) => match q {
                Qualifier::Const => {
                    // lower constant
                    todo!("constant lowering")
                }
                Qualifier::Buffer => spirv::StorageClass::StorageBuffer,
                Qualifier::Uniform => {
                    // close enough, not sure in what ways `Uniform` differs from `UniformConstant`
                    // apart from the fact that the latter is read-only.
                    if global_def.ty.is_opaque() {
                        spirv::StorageClass::UniformConstant
                    } else {
                        spirv::StorageClass::Uniform
                    }
                }
                Qualifier::In => spirv::StorageClass::Input,
                Qualifier::Out => spirv::StorageClass::Output,
                Qualifier::Shared => spirv::StorageClass::Workgroup,
            },
            None => spirv::StorageClass::Private,
        };
        let ty = self.convert_type(hir, &global_def.ty);

        let mut decorations = vec![];
        apply_interpolation_decorations(&mut decorations, &global_def.interpolation);
        if let Some(loc) = global_def.location {
            decorations.push(Decoration::Location(loc))
        }
        if let Some(builtin) = global_def.builtin {
            decorations.push(Decoration::BuiltIn(builtin))
        }

        let id = hir.define_global_variable(hir::GlobalVariableData {
            name: def.name.clone(),
            ty,
            storage_class,
            source_location: def.location,
            linkage: global_def.linkage,
            decorations,
            removed: false,
        });
        self.def_map.insert(def_id, HirDef::Variable(id));
        id
    }

    fn lower_unary_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        op: ast::UnaryOp,
        sig: &BuiltinSignature,
        expr: ExprId,
        result_type: hir::Type,
    ) -> IdRef {
        match op {
            UnaryOp::Not | UnaryOp::Neg => {
                // expression with no side effects
                let expr = self.lower_expr(fb, body, expr).expect("expected non-void expression");
                let val = (sig.lower)(&mut (), fb, &[expr.id], &[expr.ty], result_type);
                val.into()
            }
            UnaryOp::PrefixInc | UnaryOp::PrefixDec | UnaryOp::PostfixInc | UnaryOp::PostfixDec => {
                // prefix increment/decrement expr
                let place = self.lower_place_expr(fb, body, expr);
                let place_ty = place.ty;
                let ptr = self.lower_place(fb, place);
                let val = fb.emit_load(place_ty, ptr, None);
                let result = (sig.lower)(&mut (), fb, &[IdRef::from(val)], &[place_ty], place_ty);
                fb.emit_store(ptr, result.into(), None);
                match op {
                    UnaryOp::PrefixInc | UnaryOp::PrefixDec => {
                        // return value before inc/dev
                        val.into()
                    }
                    UnaryOp::PostfixInc | UnaryOp::PostfixDec => {
                        // return value post-inc/dec
                        result.into()
                    }
                    _ => unreachable!(),
                }
            }
        }
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

    fn lower_place(&mut self, fb: &mut hir::FunctionBuilder, place: Place) -> IdRef {
        let ptr_ty = fb.pointer_type(place.ty, place.storage_class);
        if place.indices.is_empty() {
            place.base
        } else {
            fb.access_chain(ptr_ty, place.base, &place.indices).into()
        }
    }

    fn lower_assign_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        lhs: ExprId,
        rhs: ExprId,
        _result_type: hir::Type,
    ) {
        let rhs = self.lower_expr(fb, body, rhs).expect("expected non-void expression");
        let lhs = self.lower_place_expr(fb, body, lhs);
        let lhs_ptr = self.lower_place(fb, lhs);
        fb.emit_store(lhs_ptr, rhs.id, None)
    }

    fn lower_bin_assign_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        sig: &BuiltinSignature,
        lhs: ExprId,
        rhs: ExprId,
        _result_type: hir::Type,
    ) {
        let rhs = self.lower_expr(fb, body, rhs).expect("expected non-void expression");
        let lhs = self.lower_place_expr(fb, body, lhs);
        let lhs_ty = lhs.ty;
        let lhs_ptr = self.lower_place(fb, lhs);
        let lhs_val = fb.emit_load(lhs_ty, lhs_ptr, None);
        let val = (sig.lower)(&mut (), fb, &[IdRef::from(lhs_val), rhs.id], &[lhs_ty, rhs.ty], lhs_ty);
        fb.emit_store(lhs_ptr, val.into(), None);
    }

    fn lower_place_expr(&mut self, fb: &mut hir::FunctionBuilder, body: &TypedBody, place_expr: ExprId) -> Place {
        let expr = &body.exprs[place_expr];
        let ty = self.convert_type(fb.module, &expr.ty);
        match expr.kind {
            ExprKind::Field { expr, index } => {
                let mut place = self.lower_place_expr(fb, body, expr);
                let index = fb.const_i32(index as i32);
                place.indices.push(index.into());
                place.ty = ty;
                place
            }
            ExprKind::LocalVar { var } => {
                //eprintln!("local map: {:?}", self.local_map);
                Place {
                    base: self
                        .local_map
                        .get(&var)
                        .expect(&format!("invalid local var: {:?}", body.local_vars[var]))
                        .clone()
                        .into(),
                    indices: vec![],
                    ty,
                    storage_class: spirv::StorageClass::Function,
                }
            }
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
            ExprKind::Index { array_or_vector, index } => {
                let mut place = self.lower_place_expr(fb, body, array_or_vector);
                let index = self.lower_expr(fb, body, index).expect("expected non-void expression");
                place.indices.push(index.id);
                place.ty = ty;
                place
            }
            ExprKind::ComponentAccess { expr, ref components } => {
                if components.len() == 1 {
                    // single-component assignment is OK
                    let mut place = self.lower_place_expr(fb, body, expr);
                    let index = fb.const_i32(components[0] as i32);
                    place.indices.push(index.into());
                    place.ty = ty;
                    place
                } else {
                    todo!("swizzle assignment")
                }
            }
            _ => {
                panic!("invalid place expression")
            }
        }
    }

    fn lower_call_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        function_id: DefId,
        args: &[ExprId],
        result_type: hir::Type,
    ) -> Option<hir::IdRef> {
        let func = self
            .sess
            .pkgs
            .def(function_id)
            .as_function()
            .expect("expected function");
        let mut arg_ids = Vec::with_capacity(args.len());
        let mut arg_types = Vec::with_capacity(args.len());
        for &arg in args {
            let expr = self.lower_expr(fb, body, arg).expect("expected non-void expression");
            arg_ids.push(expr.id);
            arg_types.push(expr.ty);
        }

        if let Some(builtin) = func.builtin {
            Some((builtin.lower)(&mut (), fb, &arg_ids, &arg_types, result_type).into())
        } else {
            // FIXME: the function may not be defined yet: we rely on the fact that, in GLSL, item definitions must appear before their use in the source code,
            // and thus the definitions are correctly ordered in the resulting TAST, but this might no longer be the case if we decide to use the TAST for other frontends.
            let Some(HirDef::Function(hir_func)) = self.def_map.get(&function_id) else { panic!("function not lowered yet") };
            Some(
                fb.emit_function_call(result_type, IdRef::from(*hir_func), &arg_ids)
                    .into(),
            )
        }
    }

    fn lower_implicit_conversion_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        expr: ExprId,
        conversion_kind: ConversionKind,
        result_type: hir::Type,
    ) -> hir::IdRef {
        let expr = self.lower_expr(fb, body, expr).expect("expected non-void expression");

        let val = match conversion_kind {
            ConversionKind::IntBitcast => fb.emit_bitcast(result_type, expr.id),
            ConversionKind::SignedIntToFloat => fb.emit_convert_s_to_f(result_type, expr.id),
            ConversionKind::UnsignedIntToFloat => fb.emit_convert_u_to_f(result_type, expr.id),
            ConversionKind::FloatConvert => fb.emit_f_convert(result_type, expr.id),
            ConversionKind::FloatToSignedInt => fb.emit_convert_f_to_s(result_type, expr.id),
            ConversionKind::FloatToUnsignedInt => fb.emit_convert_f_to_u(result_type, expr.id),
            ConversionKind::Layout => {
                todo!("layout conversion")
            }
        };

        val.into()
    }

    fn lower_constructor_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        ctor: &Constructor,
        args: &[ExprId],
        result_type: hir::Type,
    ) -> hir::IdRef {
        // TODO smallvec(16) should be enough
        let mut arg_ids = Vec::with_capacity(args.len());
        for &arg in args {
            let expr = self.lower_expr(fb, body, arg).expect("expected non-void expression");
            arg_ids.push(expr.id);
        }
        (ctor.lower)(fb, &arg_ids, result_type).into()
    }

    fn lower_literal(&mut self, fb: &mut hir::FunctionBuilder, value: &ConstantValue) -> hir::IdRef {
        match *value {
            ConstantValue::Int(v) => fb.const_i32(v).into(),
            ConstantValue::Float(v) => fb.const_f32(v.0).into(),
            ConstantValue::Bool(v) => fb.const_bool(v).into(),
        }
    }

    fn lower_component_access_expr(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        expr: ExprId,
        components: &ComponentIndices,
        result_type: hir::Type,
    ) -> hir::IdRef {
        let expr = self.lower_expr(fb, body, expr).expect("expected non-void expression");
        if components.len() == 1 {
            // extracting one component (`.x`)
            fb.emit_composite_extract(result_type, expr.id, components).into()
        } else {
            // shuffle syntax (`.xxx`)
            fb.emit_vector_shuffle(result_type, expr.id, expr.id, components).into()
        }
    }

    fn lower_expr(&mut self, fb: &mut hir::FunctionBuilder, body: &TypedBody, expr_id: ExprId) -> Option<TypedIdRef> {
        let expr = &body.exprs[expr_id];
        let ty = self.convert_type(fb.module, &expr.ty);
        let id = match expr.kind {
            ExprKind::Binary {
                lhs,
                rhs,
                op: _,
                signature,
            } => self.lower_bin_expr(fb, body, &signature, lhs, rhs, ty),
            ExprKind::BinaryAssign {
                lhs,
                rhs,
                op: _,
                signature,
            } => {
                self.lower_bin_assign_expr(fb, body, &signature, lhs, rhs, ty);
                return None;
            }
            ExprKind::Unary { expr, signature, op } => self.lower_unary_expr(fb, body, op, &signature, expr, ty),
            ExprKind::Assign { lhs, rhs } => {
                self.lower_assign_expr(fb, body, lhs, rhs, ty);
                return None;
            }
            ExprKind::Ternary { .. } => {
                todo!("ternary expression")
            }
            ExprKind::Call { function, ref args } => {
                if let Some(id) = self.lower_call_expr(fb, body, function, args, ty) {
                    id
                } else {
                    return None;
                }
            }
            ExprKind::LocalVar { var } => {
                let local = *self.local_map.get(&var).expect("invalid local var");

                // Note that both locals and function parameters are represented by LocalVarIds.
                // However, variables are always lowered to pointer-typed values, while parameters may not.
                // TODO Should all parameters be pointers? Or should all parameters be moved into local variables at the start of the function?
                if body.params.contains(&var) {
                    // this is a parameter, and the corresponding value is the correct type
                    local.into()
                } else {
                    // local variable, load it
                    fb.emit_load(ty, local.into(), None).into()
                }
            }
            ExprKind::GlobalVar { var } => {
                let Some(HirDef::Variable(global)) = self.def_map.get(&var) else { panic!("invalid global var") };
                fb.emit_load(ty, (*global).into(), None).into()
            }
            ExprKind::Index { .. } => {
                todo!("index expression")
            }
            ExprKind::Field { .. } => {
                // FIXME: we load the whole base expr, but if the expr is a place we could use OpAccessChain instead
                // problem: we don't know if the expr is a place or not
                todo!("field expression")
            }
            ExprKind::ComponentAccess { expr, ref components } => {
                self.lower_component_access_expr(fb, body, expr, components, ty)
            }
            ExprKind::ImplicitConversion { expr, kind } => {
                self.lower_implicit_conversion_expr(fb, body, expr, kind, ty)
            }
            ExprKind::BuiltinConstructor { ctor, ref args } => self.lower_constructor_expr(fb, body, ctor, args, ty),
            ExprKind::Literal { ref value } => self.lower_literal(fb, value),
            ExprKind::Undef => {
                panic!("invalid expression encountered during lowering")
            }
        };
        Some(TypedIdRef { ty, id })
    }

    fn lower_local_var(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        var_id: LocalVarId,
        initializer_id: Option<ExprId>,
    ) {
        let var = &body.local_vars[var_id];
        let ty = self.convert_type(fb.module, &var.ty);
        let ptr_ty = fb.module.pointer_type(ty, StorageClass::Function);
        let ptr = fb.add_local(ptr_ty, var.name.clone());
        self.local_map.insert(var_id, ptr);
        if let Some(init) = initializer_id {
            let initializer = self.lower_expr(fb, body, init).expect("expected non-void expression");
            fb.emit_store(ptr.into(), initializer.id, None);
        }
    }

    fn lower_return_stmt(&mut self, fb: &mut hir::FunctionBuilder, body: &TypedBody, ret: Option<ExprId>) {
        if let Some(ret) = ret {
            let ret = self.lower_expr(fb, body, ret).expect("expected non-void expression");
            fb.ret_value(ret.id);
        } else {
            fb.ret();
        }
    }

    fn lower_select_stmt(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        condition: ExprId,
        true_branch: StmtId,
        false_branch: Option<StmtId>,
    ) {
        let condition = self
            .lower_expr(fb, body, condition)
            .expect("expected non-void expression");
        let true_block = fb.create_block();
        let false_block = fb.create_block();
        let merge_block = fb.create_block();

        fb.emit_selection_merge(merge_block, spirv::SelectionControl::NONE);
        fb.branch_conditional(condition.id, true_block, false_block);
        fb.select_block(true_block);
        self.lower_stmt(fb, body, true_branch);
        fb.branch(merge_block);
        fb.select_block(false_block);
        if let Some(false_branch) = false_branch {
            self.lower_stmt(fb, body, false_branch);
        }
        fb.branch(merge_block);
        fb.select_block(merge_block);
    }

    fn lower_for_loop(
        &mut self,
        fb: &mut hir::FunctionBuilder,
        body: &TypedBody,
        initializer: Option<StmtId>,
        condition: Option<ExprId>,
        loop_expr: Option<ExprId>,
        stmt: StmtId,
    ) {
        let header_block = fb.create_block();
        let condition_block = fb.create_block();
        let loop_body_block = fb.create_block();
        let merge_block = fb.create_block();
        let continue_target = fb.create_block();

        if let Some(init) = initializer {
            self.lower_stmt(fb, body, init);
        }
        fb.branch(header_block);
        fb.select_block(header_block);
        fb.emit_loop_merge(merge_block, continue_target, Default::default());
        fb.branch(condition_block);
        fb.select_block(condition_block);
        if let Some(condition) = condition {
            let cond_val = self
                .lower_expr(fb, body, condition)
                .expect("expected non-void expression");
            fb.branch_conditional(cond_val.id, loop_body_block, merge_block);
        }
        fb.select_block(loop_body_block);
        self.lower_stmt(fb, body, stmt);
        fb.branch(continue_target);
        fb.select_block(continue_target);
        if let Some(loop_expr) = loop_expr {
            self.lower_expr(fb, body, loop_expr);
        }
        fb.branch(header_block);
        fb.select_block(merge_block);
    }

    fn lower_stmt(&mut self, fb: &mut hir::FunctionBuilder, body: &TypedBody, stmt: StmtId) {
        let stmt = &body.stmts[stmt];
        //eprintln!("lowering stmt {:?}", stmt);
        match stmt.kind {
            StmtKind::Select {
                condition,
                false_branch,
                true_branch,
            } => {
                self.lower_select_stmt(fb, body, condition, true_branch, false_branch);
            }
            StmtKind::Local { initializer, var } => {
                //eprintln!("lowering local var {:?}", body.local_vars[var]);
                self.lower_local_var(fb, body, var, initializer);
            }
            StmtKind::Return { value } => {
                self.lower_return_stmt(fb, body, value);
            }
            StmtKind::ExprStmt { expr } => {
                self.lower_expr(fb, body, expr);
            }
            StmtKind::Block { block } => {
                self.lower_block(fb, body, block);
            }
            StmtKind::Error => {
                panic!("invalid statement encountered during lowering")
            }
            StmtKind::ForLoop {
                initializer,
                condition,
                stmt,
                loop_expr,
            } => {
                self.lower_for_loop(fb, body, initializer, condition, loop_expr, stmt);
            }
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

        let return_type = function_def
            .function_type
            .as_function()
            .expect("expected function type")
            .return_type
            .clone();
        let hir_return_type = self.convert_type(hir, &return_type);

        // create function type
        let func_type = hir.define_type(hir::TypeData::Function(hir::types::FunctionType {
            arg_types: Cow::Owned(arg_types),
            return_type: hir_return_type,
        }));

        let ast = function_def.ast.clone().expect("expected function ast");

        // if there's a block, then it's a function definition, otherwise it's just a declaration
        let func_data = if let Some(_) = ast.block() {
            // get the typechecked body
            let typed_body = self
                .sess
                .get_typed_body_cached(def_id)
                .expect("function should have a type-checked body");
            if typed_body.has_errors() {
                // emit a dummy body so that lowering can continue
                FunctionData::new_declaration(
                    def.name.clone(),
                    func_type,
                    params,
                    function_def.linkage,
                    function_def.function_control,
                )
            } else {
                let (mut func_data, entry_block_id) = hir::FunctionData::new(
                    def.name.clone(),
                    func_type,
                    params,
                    function_def.linkage,
                    function_def.function_control,
                );

                // fill local map (LocalVarId -> hir::Value) with the function parameters
                // (reminder: in TAST, function parameters are also identified with LocalVarIds)
                self.local_map.clear();
                for (arg, local_var_id) in func_data.arguments.iter().zip(typed_body.params.iter()) {
                    self.local_map.insert(*local_var_id, *arg);
                }

                let mut builder = hir::FunctionBuilder::new(hir, &mut func_data, entry_block_id);

                let entry_block = typed_body.entry_block.expect("function has no entry block");
                self.lower_block(&mut builder, &typed_body, entry_block);
                if !builder.is_block_terminated() {
                    // Insert OpReturn if the function returns void & the current block is not terminated
                    // (a return statement is not necessary if the function returns void).
                    // If the function is non-void, we assume that this block is unreachable.
                    // FIXME: emit warning if this is not the case
                    if return_type.is_unit() {
                        builder.ret()
                    } else {
                        warn!("no return statement in exiting block");
                        let und = builder.emit_undef(hir_return_type);
                        builder.ret_value(und.into());
                    }
                }
                func_data
            }
        } else {
            // only a declaration
            FunctionData::new_declaration(
                def.name.clone(),
                func_type,
                params,
                function_def.linkage,
                function_def.function_control,
            )
        };

        //self.local_map.clear();

        let hir_func = hir.add_function(func_data.clone());

        // functions that have an execution model attribute are entry points
        if let Some(execution_model) = function_def.execution_model {
            hir.define_entry_point(hir_func, &def.name, execution_model)
                .expect("error adding entry point");
        }

        self.def_map.insert(def_id, HirDef::Function(hir_func));
    }

    fn lower_def(&mut self, hir: &mut hir::Module, def: &Def, def_id: DefId) {
        //trace!("lowering def")
        match def.kind {
            DefKind::Function(ref function) => {
                self.lower_function(hir, def_id, def, function);
            }
            DefKind::Global(ref global) => {
                self.lower_global(hir, def_id, def, global);
            }
            DefKind::Struct(ref struct_def) => {
                self.convert_type(hir, &struct_def.ty);
            }
        }
    }
}

pub fn lower_to_hir(sess: &mut Session, package: PackageId) -> Result<hir::Module, QueryError> {
    // ensure module is created & typechecked
    sess.typecheck_bodies(package)?;
    let dependencies = sess.dependencies(package)?;
    let mut hir = hir::Module::new();

    // lower dependencies
    for dep in dependencies {
        let module = sess.pkgs.module(dep).unwrap();
        let mut ctxt = LowerCtxt {
            sess,
            module,
            def_map: Default::default(),
            local_map: Default::default(),
        };
        ctxt.lower_module(dep, &mut hir);
    }

    // lower main module
    let module = sess.pkgs.module(package).unwrap();
    let mut ctxt = LowerCtxt {
        sess,
        module,
        def_map: Default::default(),
        local_map: Default::default(),
    };
    ctxt.lower_module(package, &mut hir);
    Ok(hir)
}
