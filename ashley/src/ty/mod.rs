//! Type resolution.

pub mod body;
mod diagnostic;
mod interner;
mod lower;
mod primitive;
mod ty;

use ashley_data_structures::Id;
pub use diagnostic::TyDiagnostic;
pub use primitive::PrimitiveTypes;
pub use ty::{FunctionType, ImageSampling, ImageType, ScalarType, StructField, Type, TypeKind};

use crate::{
    db::{DebugWithDb, ModuleId},
    def,
    def::{AstId, ConstExprLoc, DefLoc, FieldLoc, FunctionId, FunctionLoc, GlobalId, Resolver, StructId, StructLoc},
    syntax::{ast, ast::TypeRef, SyntaxNode},
    ty::{interner::Interner, lower::TypeLoweringCtxt},
    CompilerDb,
};

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Type-checking context.
///
/// Holds the interner for types, and pre-interned versions of commonly-used types.
///
/// TODO integrate with CompilerDB?
pub struct TypeCtxt {
    /// Types interner.
    types: Interner,
    /// Pre-interned primitive types.
    pub(crate) prim_tys: primitive::PrimitiveTypes,
    /// Error type (used for error recovery).
    pub(crate) error: Type,
}

impl TypeCtxt {
    pub fn new() -> Self {
        let mut types = Interner::new();
        let error_type = types.intern(TypeKind::Error);
        let builtin_types = primitive::PrimitiveTypes::new(&mut types);
        TypeCtxt {
            types,
            prim_tys: builtin_types,
            error: error_type,
        }
    }

    pub fn ty(&self, kind: impl Into<TypeKind>) -> Type {
        self.types.intern(kind)
    }
}

impl Default for TypeCtxt {
    fn default() -> Self {
        TypeCtxt::new()
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Identifies something with a type.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum TyOwnerId {
    /*Argument {
        function: FunctionId,
        argument: Id<def::FunctionParam>,
    },*/
    ReturnValue(FunctionLoc),
    Field(FieldLoc),
    ArrayElement(Box<TyOwnerId>),
    LocalVariable {
        function: FunctionLoc,
        var: Id<def::body::LocalVar>,
    },
    // TODO Same as `BodyOwnerId`? Instead of having specific variants, store `AstId<ast::Type>`?
}

impl TyOwnerId {
    pub fn module(&self) -> ModuleId {
        match self {
            //TyOwnerId::Argument { function, .. } => function.module,
            TyOwnerId::ReturnValue(function) => function.module,
            TyOwnerId::Field(field) => field.strukt.module,
            TyOwnerId::ArrayElement(owner) => owner.module(),
            TyOwnerId::LocalVariable { function, .. } => function.module,
        }
    }

    /*/// Returns the TypeRef
    pub fn type_ref(&self, compiler: &dyn CompilerDb) -> &item::Type {
        let items = self.module().items(compiler);
        match self {
            TyOwnerId::Argument { function, argument } => &items[function.function].parameters[*argument].ty,
            TyOwnerId::ReturnValue(function) => &items[function.function]
                .return_type
                .as_ref()
                .expect("invalid TyOwnerId: ReturnValue on function without return type"),
            TyOwnerId::Field(field) => &items.structs[field.strukt.strukt].fields[field.field].ty,
            TyOwnerId::ArrayElement(array_ty) => match array_ty.type_ref(compiler) {
                item::Type::Array { element, stride, size } => &element,
                _ => panic!("invalid TyOwnerId: ArrayElement on non-array type"),
            },
            TyOwnerId::LocalVariable { function, var } => {
                let loc = compiler.body_id(BodyLoc { owner: BodyOwnerId::Function(function), kind: Bo })
                &compiler.body(function.into()).local_vars[*var].ty
            },
        }
    }*/
}

/*
fn struct_ty(compiler: &dyn CompilerDb, strukt: StructId, diagnostics: &mut Vec<TyDiagnostic>) {
    let struct_data = compiler.struct_data(strukt);
}*/

// TODO: this could mutate the resolver (although we could just do a clone if necessary)
fn lower_ty(
    compiler: &dyn CompilerDb,
    resolver: &Resolver,
    ty: &def::Type,
    diagnostics: &mut Vec<TyDiagnostic>,
) -> Type {
    let mut ctxt = TypeLoweringCtxt::new(compiler, resolver, diagnostics);
    ctxt.lower_type(ty)
}

pub(crate) fn struct_field_types_query(db: &dyn CompilerDb, struct_id: StructId) {
    let _span = trace_span!("struct_field_types_query", struct_id = ?struct_id.debug_with(db)).entered();

    let struct_loc = db.lookup_struct_id(struct_id);
    let struct_item = db.struct_data(struct_id);
    let resolver = struct_loc.module.resolver(db);

    let mut diagnostics = Vec::new();
    let mut types = Vec::with_capacity(struct_item.fields.len());
    // lower field types
    for (i, field) in struct_item.fields.iter_full() {
        let ty = lower_ty(db, &resolver, &field.ty, &mut diagnostics);
        trace!("field `{}` has type `{}`", field.name, ty);
        types.push(ty);
    }

    db.set_struct_field_types(struct_id, types);
    db.set_struct_field_types_diagnostics(struct_id, diagnostics);
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionSignature {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
}

pub(crate) fn function_signature_query(db: &dyn CompilerDb, function_id: FunctionId) {
    let _span = trace_span!("function_signature_query", function_id = ?function_id.debug_with(db)).entered();
    let func_data = db.function_data(function_id);
    let func_loc = function_id.loc(db);
    let resolver = func_loc.module.resolver(db);

    let mut diagnostics = Vec::new();
    let mut parameter_types = Vec::with_capacity(func_data.parameters.len());
    for (i, param) in func_data.parameters.iter_full() {
        parameter_types.push(lower_ty(db, &resolver, &param.ty, &mut diagnostics));
    }

    let return_type = if let Some(ref return_type) = func_data.return_type {
        lower_ty(db, &resolver, return_type, &mut diagnostics)
    } else {
        db.tyctxt().prim_tys.void.clone()
    };

    db.set_function_signature(
        function_id,
        FunctionSignature {
            parameter_types,
            return_type,
        },
    );
    db.set_function_signature_diagnostics(function_id, diagnostics);
}

pub(crate) fn global_ty_query(db: &dyn CompilerDb, global_id: GlobalId) {
    let _span = trace_span!("global_ty_query", global_id = ?global_id.debug_with(db)).entered();
    let global_data = db.global_data(global_id);
    let resolver = global_id.loc(db).module.resolver(db);
    let mut diags = vec![];

    let ty = if let Some(ref ty) = global_data.ty {
        lower_ty(db, &resolver, ty, &mut diags)
    } else {
        Type::new(db, TypeKind::Error)
    };
    db.set_global_ty(global_id, ty);
    db.set_global_ty_diagnostics(global_id, diags);
}

pub(crate) fn def_ty_with_diagnostics_query(compiler: &dyn CompilerDb, def_id: DefLoc) -> (Type, Vec<TyDiagnostic>) {
    /*let module = def_id.module();
    let module_items = compiler.module_items(module);
    let source_file = compiler.module_source(module);

    let mut diagnostics = vec![];
    match def_id {
        DefId::Struct(struct_id) => {
            let resolver = def_id.resolver(compiler);
            let struct_data = &module_items[struct_id.strukt];

            // lower fields
            for (i_field, field) in struct_data.fields.iter_full() {
                ctxt.lower_type(
                    TyOwnerId::Field(FieldId {
                        strukt: struct_id,
                        field: i_field,
                    }),
                    &field.ty,
                );
            }
        }
        DefId::Global(_) => {
            todo!()
        }
        DefId::Function(_) => {}
    }*/

    // tycheck cannot emit diagnostics with ast spans directly, otherwise it would be invalidated
    // on every reparse (bad)
    //
    // instead use IDs:
    // * struct => StructId
    // * field => FieldId
    // * type within a field => TypeOwnerId::Field(FieldId)
    // * type within a constant expression within an attribute: TypeOwnerId::Expr(BodyId,ExprId), BodyId::ConstExprInAttributeItem(AttributeOwnerId)

    todo!()
}

////////////////////////////////////////////////////////////////////////////////////////////////////
