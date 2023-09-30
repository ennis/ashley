use crate::{
    db::{DebugWithDb, ModuleId},
    def::{BodyId, FieldLoc, FunctionId, FunctionLoc, GlobalId, GlobalLoc, StructId, StructLoc},
};
use ashley::{def::BodyOwnerId, CompilerDb};
use ashley_db::AsIndex;
use std::fmt::Formatter;

impl DebugWithDb for ModuleId {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {
        let uri = &compiler.module_data(*self).url;
        let raw_id = self.0;
        write!(formatter, "ModuleId(`{uri}` {raw_id:04x})")?;
        Ok(())
    }
}

/*
impl DebugWithDb for StructLoc {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {
        let mod_uri = &compiler.module_data(self.module).url;
        let name = &self.module.items(compiler)[self.strukt].name;
        let module_id = self.module.0;
        let struct_id = self.strukt.index();
        write!(formatter, "StructId(`{name}` {module_id:04x}:{struct_id:04x})")?;
        Ok(())
    }
}

impl DebugWithDb for FunctionLoc {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {
        let mod_uri = &compiler.module_data(self.module).url;
        let name = &self.module.items(compiler)[self.function].name;
        let module_id = self.module.0;
        let function_id = self.function.index();
        write!(formatter, "FunctionId(`{name}` {module_id:04x}:{function_id:04x})")?;
        Ok(())
    }
}

impl DebugWithDb for GlobalLoc {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {
        let mod_uri = &compiler.module_data(self.module).url;
        let name = &self.module.items(compiler)[self.global].name;
        let module_id = self.module.0;
        let global_id = self.global.index();
        write!(formatter, "GlobalId(`{name}` {module_id:04x}:{global_id:04x})")?;
        Ok(())
    }
}

impl DebugWithDb for FieldLoc {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {
        let mod_uri = &compiler.module_data(self.strukt.module).url;
        let module_id = self.strukt.module.0;
        let struct_id = self.strukt.strukt.index();
        let struct_info = &self.strukt.module.items(compiler)[self.strukt.strukt];
        let struct_name = &struct_info.name;
        let field_name = &struct_info.fields[self.field].name;
        let field_index = self.field.index();
        write!(
            formatter,
            "FieldId(`{struct_name}::{field_name}` {module_id:04x}:{struct_id:04x}:{field_index})"
        )?;
        Ok(())
    }
}*/

impl DebugWithDb for StructId {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {
        let item = compiler.struct_data(*self);
        let name = &item.name;
        let raw_id = self.index().as_u32();
        write!(formatter, "StructId(`{name}` {raw_id:04x})")?;
        Ok(())
    }
}

impl DebugWithDb for FunctionId {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {
        let item = compiler.function_data(*self);
        let name = &item.name;
        let raw_id = self.index().as_u32();
        write!(formatter, "FunctionId(`{name}` {raw_id:04x})")?;
        Ok(())
    }
}

impl DebugWithDb for GlobalId {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {
        let item = compiler.global_data(*self);
        let name = &item.name;
        let raw_id = self.index().as_u32();
        write!(formatter, "GlobalId(`{name}` {raw_id:04x})")?;
        Ok(())
    }
}

/*impl DebugWithDb for BodyId {
    fn fmt<'a>(&'a self, formatter: &mut Formatter<'_>, compiler: &'a dyn CompilerDb) -> std::fmt::Result {

        let body_loc = compiler.lookup_body_id(*self);
        match body_loc.owner {
            BodyOwnerId::InTypeConst(_) => {}
            BodyOwnerId::Function(function) => {}
            BodyOwnerId::Variable(global) => {}
        }
    }
}
*/
