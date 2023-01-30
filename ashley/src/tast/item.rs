use crate::tast::decl::FunctionDef;

use crate::tast::decl::StructDef;



enum Item {
    GlobalVariable(GlobalDef),
    Function(FunctionDef),
    StructDef(StructDef)
}
