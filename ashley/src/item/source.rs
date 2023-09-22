use crate::{
    item::{AstId, FieldId, FunctionId, GlobalId, ImportId, InFile, StructId},
    syntax::ast,
    ty::TyOwnerId,
    CompilerDb,
};
use rowan::ast::AstNode;

pub trait HasSource {
    type Source;

    fn source(&self, db: &dyn CompilerDb) -> Self::Source;
}

macro_rules! impl_has_source {
    ($id_ty:ty, $ast_node:ty, $local_id_field:ident) => {
        impl HasSource for $id_ty {
            type Source = InFile<$ast_node>;

            fn source(&self, db: &dyn CompilerDb) -> Self::Source {
                let items = db.module_items(self.module);
                let ast_map = db.module_map(self.module);
                items[self.$local_id_field]
                    .ast
                    .to_node_with_source_file(db, self.module)
            }
        }
    };
}

impl_has_source!(ImportId, ast::ImportDecl, import);
impl_has_source!(StructId, ast::StructDef, strukt);
impl_has_source!(FunctionId, ast::FnDef, function);
impl_has_source!(GlobalId, ast::Global, global);

impl HasSource for TyOwnerId {
    type Source = InFile<ast::Type>;

    fn source(&self, db: &dyn CompilerDb) -> Self::Source {
        let module = self.module();
        let items = db.module_items(module);

        match self {
            TyOwnerId::Argument { function, argument } => items[function.function].parameters[*argument]
                .ast
                .to_node_with_source_file(db, module)
                .map(|arg| arg.ty().expect("no type on parameter")),
            TyOwnerId::ReturnValue(function) => items[function.function]
                .ast
                .to_node_with_source_file(db, module)
                .map(|f| f.return_type().expect("no return type on function")),
            TyOwnerId::Field(field) => field.source(db).map(|f| f.ty().expect("no type on field")),
            TyOwnerId::ArrayElement(_) => {
                todo!()
            }
            TyOwnerId::LocalVariable { .. } => {
                todo!()
            }
        }
    }
}

impl HasSource for FieldId {
    type Source = InFile<ast::StructField>;

    fn source(&self, db: &dyn CompilerDb) -> Self::Source {
        let items = db.module_items(self.strukt.module);
        items[self.strukt.strukt].fields[self.field]
            .ast
            .to_node_with_source_file(db, self.strukt.module)
    }
}
