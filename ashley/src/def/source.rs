use crate::{
    def::{AstId, FieldLoc, FunctionLoc, GlobalLoc, ImportLoc, InFile, StructLoc},
    syntax::ast,
    ty::TyOwnerId,
    CompilerDb,
};
use rowan::ast::AstNode;

/*
pub trait HasSource {
    type Source;

    fn source(&self, db: &dyn CompilerDb) -> Self::Source;
}

macro_rules! impl_has_source {
    ($id_ty:ty, $ast_node:ty, $local_id_field:ident) => {
        impl HasSource for $id_ty {
            type Source = InFile<$ast_node>;

            fn source(&self, db: &dyn CompilerDb) -> Self::Source {
                let map = db.module_item_map(self.module);
                db.deref_ast_ptr_with_source_file(self.module, map[self.$local_id_field].clone())
            }
        }
    };
}

impl_has_source!(StructLoc, ast::StructDef, strukt);
impl_has_source!(FunctionLoc, ast::FnDef, function);
impl_has_source!(GlobalLoc, ast::Global, global);

impl HasSource for ImportLoc {
    type Source = InFile<ast::ImportDecl>;

    fn source(&self, db: &dyn CompilerDb) -> Self::Source {
        let map = db.module_item_map(self.module);
        db.deref_ast_ptr_with_source_file(self.module, map[self.import].clone())
    }
}

impl HasSource for TyOwnerId {
    type Source = InFile<ast::Type>;

    fn source(&self, db: &dyn CompilerDb) -> Self::Source {
        let module = self.module();
        let items = db.module_items(module);

        todo!()
    }
}

impl HasSource for FieldLoc {
    type Source = InFile<ast::StructField>;

    fn source(&self, db: &dyn CompilerDb) -> Self::Source {
        let items = db.module_items(self.strukt.module);
        let map = db.module_item_map(self.strukt.module);
        map.ast_map_for_struct(self.strukt.strukt).node_in_file(
            db,
            self.strukt.module,
            items[self.strukt.strukt].fields[self.field].ast,
        )
    }
}
*/
