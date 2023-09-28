pub mod body;
mod const_expr;
mod debug;
mod diagnostic;
pub mod lower;
mod resolver;
mod scope;
mod source;
mod ty;

pub use self::{
    const_expr::ConstExpr,
    diagnostic::ItemDiagnostic,
    scope::{Scope, TypeRes, ValueRes},
    source::HasSource,
    ty::{Type, TypeKind},
};

pub(crate) use self::{
    resolver::Resolver,
    scope::{module_scope_query, scope_for_body_query},
};

use crate::{
    db::DebugWithDb,
    def::lower::ItemLowerCtxt,
    diagnostic::{Span, Spanned},
    syntax::{ast, Lang, SyntaxNode, SyntaxNodePtr, SyntaxToken},
    CompilerDb, ModuleId, SourceFileId,
};
use ashley_data_structures::{Id, IndexVec};
use ashley_db::new_key_type;
use rowan::{
    ast::{AstNode, AstPtr},
    GreenNode,
};
use std::{
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Index,
};

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Wraps something alongside a source file ID.
// (inspired by r-a)
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct InFile<T> {
    pub file: SourceFileId,
    pub data: T,
}

impl<T> InFile<T> {
    pub fn new(file: SourceFileId, data: T) -> InFile<T> {
        InFile { file, data }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> InFile<U> {
        InFile {
            file: self.file,
            data: f(self.data),
        }
    }
}

impl<N: AstNode> InFile<AstPtr<N>> {
    pub fn new_ast_ptr(file: SourceFileId, node: &N) -> InFile<AstPtr<N>> {
        InFile::new(file, AstPtr::new(node))
    }
}

impl Spanned for InFile<SyntaxNode> {
    fn span(&self) -> Span {
        Span::new(self.file, self.data.text_range())
    }
}

impl Spanned for InFile<SyntaxNodePtr> {
    fn span(&self) -> Span {
        Span::new(self.file, self.data.text_range())
    }
}

impl Spanned for InFile<SyntaxToken> {
    fn span(&self) -> Span {
        Span::new(self.file, self.data.text_range())
    }
}

impl<N: AstNode> Spanned for InFile<AstPtr<N>> {
    fn span(&self) -> Span {
        Span::new(self.file, self.data.syntax_node_ptr().text_range())
    }
}

impl<N: AstNode> InFile<N> {
    pub fn span(&self) -> Span {
        Span::new(self.file, self.data.syntax().text_range())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
pub struct AstId<N: AstNode> {
    pub(crate) raw: Id<SyntaxNodePtr>,
    _phantom: PhantomData<fn() -> N>,
}

impl<N: AstNode> fmt::Debug for AstId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AstId").field("raw", &self.raw).finish()
    }
}

impl<N: AstNode> Copy for AstId<N> {}

impl<N: AstNode> Clone for AstId<N> {
    fn clone(&self) -> Self {
        Self {
            raw: self.raw.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<N: AstNode> PartialEq for AstId<N> {
    fn eq(&self, other: &AstId<N>) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode> Eq for AstId<N> {}

impl<N: AstNode> Hash for AstId<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state)
    }
}

impl<N: AstNode> From<AstId<N>> for Id<SyntaxNodePtr> {
    fn from(ptr: AstId<N>) -> Id<SyntaxNodePtr> {
        ptr.raw
    }
}

impl<N: AstNode<Language = Lang>> AstId<N> {
    /*pub fn to_node(&self, compiler: &dyn CompilerDb, owner_module: ModuleId) -> N {
        let module_map = compiler.module_item_map(owner_module);
        module_map.node(*self)
    }

    pub fn to_node_with_source_file(&self, compiler: &dyn CompilerDb, owner_module: ModuleId) -> InFile<N> {
        let source_file_id = compiler.module_source(owner_module);
        InFile::new(source_file_id, self.to_node(compiler, owner_module))
    }*/
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// AST id relative to a definition.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DefAstId<N: AstNode<Language = Lang>> {
    owner: DefLoc,
    ast_id: AstId<N>,
}

impl<N: AstNode<Language = Lang>> DefAstId<N> {
    pub fn new(owner: DefLoc, ast_id: AstId<N>) -> DefAstId<N> {
        DefAstId { owner, ast_id }
    }
}

impl<N: AstNode<Language = Lang>> HasSource for DefAstId<N> {
    type Source = InFile<N>;

    fn source(&self, db: &dyn CompilerDb) -> Self::Source {
        self.owner
            .def_map(db)
            .node_with_source_file(db, self.owner.module(), self.ast_id)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
#[derive(Clone, PartialEq, Eq)]
pub struct AstIdMap {
    syntax: GreenNode,
    map: IndexVec<SyntaxNodePtr, Id<SyntaxNodePtr>>,
}

impl AstIdMap {
    pub fn push<N: AstNode<Language = Lang>>(&mut self, node: &N) -> AstId<N> {
        let raw = self.map.push(SyntaxNodePtr::new(node.syntax()));
        AstId {
            raw,
            _phantom: PhantomData,
        }
    }

    pub fn get_node_ptr<N: AstNode<Language = Lang>>(&self, id: AstId<N>) -> AstPtr<N> {
        self.map
            .get(id.raw)
            .expect("AST node ID not found")
            .clone()
            .cast()
            .expect("invalid AST node type")
    }

    pub fn get_node<N: AstNode<Language = Lang>>(&self, id: AstId<N>) -> N {
        let root = SyntaxNode::new_root(self.syntax.clone());
        self.get_node_ptr(id).to_node(&root)
    }

    pub fn get_raw_node(&self, id: Id<SyntaxNodePtr>) -> SyntaxNodePtr {
        self.map.get(id).expect("AST node ID not found").clone()
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Linkage {
    Internal,
    Extern,
}

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

/// Describes the kind of a global program variable.
/// TODO remove this and use spirv::StorageClass directly?
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Qualifier {
    Buffer,
    Shared,
    Uniform,
    Const,
    In,
    Out,
}

/// display impl for qualifier
impl fmt::Display for Qualifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Qualifier::Buffer => write!(f, "buffer"),
            Qualifier::Uniform => write!(f, "uniform"),
            Qualifier::Shared => write!(f, "shared"),
            Qualifier::Const => write!(f, "const"),
            Qualifier::In => write!(f, "in"),
            Qualifier::Out => write!(f, "out"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Global {
    //pub ast: AstId<ast::Global>,
    pub attrs: Vec<AstId<ast::Attribute>>,
    pub name: String,
    pub visibility: Visibility,
    pub linkage: Linkage,
    pub storage_class: Option<Qualifier>,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub ast: AstId<ast::StructField>,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    //pub ast: AstId<ast::StructDef>,
    pub attrs: Vec<AstId<ast::Attribute>>,
    pub name: String,
    pub visibility: Visibility,
    pub fields: IndexVec<StructField>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionParam {
    pub ast: AstId<ast::FnParam>,
    pub name: String,
    pub ty: Type,
}

/// A function definition or declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    //pub ast: AstId<ast::FnDef>,
    pub attributes: Vec<AstId<ast::Attribute>>,
    pub name: String,
    pub visibility: Visibility,
    pub linkage: Linkage,
    pub parameters: IndexVec<FunctionParam>,
    pub return_type: Option<Type>,
    pub body: Option<AstId<ast::Block>>,
}

/// An `import` directive
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Import {
    //pub ast: AstId<ast::ImportDecl>,
    pub uri: String,
}

/// Represents a module.
///
/// Bodies of items (functions and initializers) are type-checked on-demand.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ModuleItems {
    /// Imported modules.
    pub imports: IndexVec<Import>,
    pub structs: IndexVec<Struct>,
    pub globals: IndexVec<Global>,
    pub functions: IndexVec<Function>,
    /// Collected errors (& warnings)
    pub diagnostics: Vec<ItemDiagnostic>,
}

impl Index<Id<Import>> for ModuleItems {
    type Output = Import;
    fn index(&self, index: Id<Import>) -> &Self::Output {
        &self.imports[index]
    }
}

impl Index<Id<Struct>> for ModuleItems {
    type Output = Struct;
    fn index(&self, index: Id<Struct>) -> &Self::Output {
        &self.structs[index]
    }
}

impl Index<Id<Global>> for ModuleItems {
    type Output = Global;
    fn index(&self, index: Id<Global>) -> &Self::Output {
        &self.globals[index]
    }
}

impl Index<Id<Function>> for ModuleItems {
    type Output = Function;
    fn index(&self, index: Id<Function>) -> &Self::Output {
        &self.functions[index]
    }
}

impl ModuleItems {
    /// Creates a new module.
    pub fn new() -> ModuleItems {
        ModuleItems {
            imports: Default::default(),
            structs: Default::default(),
            globals: Default::default(),
            functions: Default::default(),
            diagnostics: vec![],
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct DefMap {
    ast_ids: IndexVec<SyntaxNodePtr, Id<SyntaxNodePtr>>,
}

impl DefMap {
    fn new() -> DefMap {
        DefMap {
            ast_ids: Default::default(),
        }
    }

    pub fn node_ptr<M: AstNode<Language = Lang>>(&self, id: AstId<M>) -> AstPtr<M> {
        self.ast_ids
            .get(id.raw)
            .expect("AST node ID not found")
            .clone()
            .cast()
            .expect("invalid AST node type")
    }

    pub fn node<M: AstNode<Language = Lang>>(&self, db: &dyn CompilerDb, owner_module: ModuleId, id: AstId<M>) -> M {
        self.node_with_source_file(db, owner_module, id).data
    }

    pub fn node_with_source_file<M: AstNode<Language = Lang>>(
        &self,
        db: &dyn CompilerDb,
        owner_module: ModuleId,
        id: AstId<M>,
    ) -> InFile<M> {
        let (source_file, green_node) = db.module_syntax_tree(owner_module);
        let root = SyntaxNode::new_root(green_node);
        InFile::new(source_file, self.node_ptr(id).to_node(&root))
    }

    pub fn push<M: AstNode<Language = Lang>>(&mut self, node: &M) -> AstId<M> {
        let raw = self.ast_ids.push(SyntaxNodePtr::new(node.syntax()));
        AstId {
            raw,
            _phantom: PhantomData,
        }
    }

    /*pub fn get_node<N: AstNode<Language = Lang>>(&self, id: AstId<N>) -> N {
        let root = SyntaxNode::new_root(self.syntax.clone());
        self.get_node_ptr(id).to_node(&root)
    }

    pub fn get_raw_node(&self, id: Id<SyntaxNodePtr>) -> SyntaxNodePtr {
        self.map.get(id).expect("AST node ID not found").clone()
    }*/
}

#[derive(Clone, PartialEq, Eq)]
pub struct ModuleItemMap {
    imports: IndexVec<AstPtr<ast::ImportDecl>, Id<Import>>,
    structs: IndexVec<(AstPtr<ast::StructDef>, DefMap), Id<Struct>>,
    globals: IndexVec<(AstPtr<ast::Global>, DefMap), Id<Global>>,
    functions: IndexVec<(AstPtr<ast::FnDef>, DefMap), Id<Function>>,
}

impl ModuleItemMap {
    pub fn new() -> ModuleItemMap {
        ModuleItemMap {
            imports: Default::default(),
            structs: Default::default(),
            globals: Default::default(),
            functions: Default::default(),
        }
    }

    pub fn struct_def_map(&self, strukt: Id<Struct>) -> &DefMap {
        &self.structs[strukt].1
    }

    pub fn global_def_map(&self, global: Id<Global>) -> &DefMap {
        &self.globals[global].1
    }

    pub fn function_def_map(&self, function: Id<Function>) -> &DefMap {
        &self.functions[function].1
    }
}

impl Index<Id<Struct>> for ModuleItemMap {
    type Output = AstPtr<ast::StructDef>;

    fn index(&self, index: Id<Struct>) -> &Self::Output {
        &self.structs[index].0
    }
}
impl Index<Id<Import>> for ModuleItemMap {
    type Output = AstPtr<ast::ImportDecl>;

    fn index(&self, index: Id<Import>) -> &Self::Output {
        &self.imports[index]
    }
}
impl Index<Id<Global>> for ModuleItemMap {
    type Output = AstPtr<ast::Global>;

    fn index(&self, index: Id<Global>) -> &Self::Output {
        &self.globals[index].0
    }
}
impl Index<Id<Function>> for ModuleItemMap {
    type Output = AstPtr<ast::FnDef>;

    fn index(&self, index: Id<Function>) -> &Self::Output {
        &self.functions[index].0
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportLoc {
    pub module: ModuleId,
    pub import: Id<Import>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructLoc {
    pub module: ModuleId,
    pub strukt: Id<Struct>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionLoc {
    pub module: ModuleId,
    pub function: Id<Function>,
}

impl FunctionLoc {
    /// Returns the AST node
    pub fn node_from_id<N: AstNode<Language = Lang>>(&self, db: &dyn CompilerDb, id: AstId<N>) -> InFile<N> {
        let def_map = db.module_item_map(self.module);
        def_map
            .function_def_map(self.function)
            .node_with_source_file(db, self.module, id)
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalLoc {
    pub module: ModuleId,
    pub global: Id<Global>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldLoc {
    pub strukt: StructLoc,
    pub field: Id<StructField>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeOwnerId {
    Struct(StructLoc),
    Global(GlobalLoc),
    Field(FieldLoc),
    Function(FunctionLoc),
    ReturnValue(FunctionLoc),
    Argument { function: FunctionLoc, argument: u32 },
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefLoc {
    Struct(StructLoc),
    Global(GlobalLoc),
    Function(FunctionLoc),
}

impl DefLoc {
    pub fn module(&self) -> ModuleId {
        match self {
            DefLoc::Struct(s) => s.module,
            DefLoc::Global(g) => g.module,
            DefLoc::Function(f) => f.module,
        }
    }

    pub fn def_map<'db>(&self, compiler: &'db dyn CompilerDb) -> &'db DefMap {
        let module = self.module();
        let module_item_map = compiler.module_item_map(module);
        match self {
            DefLoc::Struct(s) => module_item_map.struct_def_map(s.strukt),
            DefLoc::Global(g) => module_item_map.global_def_map(g.global),
            DefLoc::Function(f) => module_item_map.function_def_map(f.function),
        }
    }
}

impl From<StructLoc> for DefLoc {
    fn from(value: StructLoc) -> Self {
        DefLoc::Struct(value)
    }
}
impl From<GlobalLoc> for DefLoc {
    fn from(value: GlobalLoc) -> Self {
        DefLoc::Global(value)
    }
}
impl From<FunctionLoc> for DefLoc {
    fn from(value: FunctionLoc) -> Self {
        DefLoc::Function(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BodyOwnerLoc {
    Def(DefLoc),
    FunctionBody(FunctionId),
}

new_key_type!(
    pub struct BodyOwnerId;
);

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum BodyKind {
    Expr(AstId<ast::Expr>),
    Block(AstId<ast::Block>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BodyLoc {
    pub owner: BodyOwnerLoc,
    pub kind: BodyKind,
}

////////////////////////////////////////////////////////////////////////////////////////////////////

new_key_type!(
    pub struct BodyId;
);

new_key_type!(
    pub struct StructId;
);

new_key_type!(
    pub struct FunctionId;
);

new_key_type!(
    pub struct GlobalId;
);

impl FunctionId {
    pub fn loc(&self, db: &dyn CompilerDb) -> FunctionLoc {
        db.lookup_function_id(*self)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

pub(crate) fn module_items_and_item_map_query(compiler: &dyn CompilerDb, module_id: ModuleId) {
    let _span = trace_span!("module_items_and_item_map_query", module_id = ?module_id.debug_with(compiler)).entered();
    let (_source_file_id, syntax) = compiler.module_syntax_tree(module_id);
    let mut ctxt = ItemLowerCtxt::new(compiler, syntax, module_id);
    let (items, module_item_map) = ctxt.lower_module();

    compiler.set_module_items(module_id, items);
    compiler.set_module_item_map(module_id, module_item_map);
    //dump_module_items(compiler, module_id, &items);
}

pub(crate) fn struct_data_query(compiler: &dyn CompilerDb, strukt: StructId) -> Struct {
    let item_struct_id = compiler.lookup_struct_id(strukt);
    compiler.module_items(item_struct_id.module).structs[item_struct_id.strukt].clone()
}

pub(crate) fn function_data_query(compiler: &dyn CompilerDb, function: FunctionId) -> Function {
    let item_function_id = compiler.lookup_function_id(function);
    compiler.module_items(item_function_id.module).functions[item_function_id.function].clone()
}

pub(crate) fn global_data_query(compiler: &dyn CompilerDb, global: GlobalId) -> Global {
    let item_global_id = compiler.lookup_global_id(global);
    compiler.module_items(item_global_id.module).globals[item_global_id.global].clone()
}
