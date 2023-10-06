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
    ty::{Type, TypeKind},
};

pub(crate) use self::{
    resolver::Resolver,
    scope::{module_scope_query, scope_for_body_query},
};

use crate::{
    db::DebugWithDb,
    def::lower::DefLowerCtxt,
    diagnostic::{Span, Spanned},
    syntax::{ast, ast::AstToken, Lang, SyntaxNode, SyntaxNodePtr, SyntaxToken},
    ty::FunctionSignature,
    CompilerDb, ModuleId, SourceFileId,
};
use ashley::ir::FunctionData;
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

/*/// AST id relative to a definition.
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
            .node_in_file(db, self.owner.module(), self.ast_id)
    }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////////
/*#[derive(Clone, PartialEq, Eq)]
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
}*/

pub type RawAstId = Id<SyntaxNodePtr>;

/// Assigns and maps syntax nodes to indices.
///
/// This is useful to refer to syntax nodes in query results without
/// having the result change on every reparse (the indices stay the same
/// as long as the change doesn't affect the syntax tree).
#[derive(Clone, Eq, PartialEq)]
pub struct AstMap {
    // TODO: maybe consider putting the module ID here, or the source file ID?
    ast_ids: IndexVec<SyntaxNodePtr, RawAstId>,
}

impl AstMap {
    fn new() -> AstMap {
        AstMap {
            ast_ids: Default::default(),
        }
    }

    pub fn raw_node_ptr(&self, id: RawAstId) -> SyntaxNodePtr {
        self.ast_ids.get(id).expect("AST node ID not found").clone()
    }

    pub fn node_ptr<M: AstNode<Language = Lang>>(&self, id: AstId<M>) -> AstPtr<M> {
        //trace!("{:?}", id);
        //trace!("{:?}", self.ast_ids);
        self.ast_ids
            .get(id.raw)
            .expect("AST node ID not found")
            .clone()
            .cast()
            .expect("invalid AST node type")
    }

    pub fn node<M: AstNode<Language = Lang>>(&self, db: &dyn CompilerDb, owner_module: ModuleId, id: AstId<M>) -> M {
        self.node_in_file(db, owner_module, id).data
    }

    pub fn node_in_file<M: AstNode<Language = Lang>>(
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
    pub attrs: Vec<AstId<ast::Attribute>>,
    pub name: String,
    pub visibility: Visibility,
    pub linkage: Linkage,
    pub storage_class: Option<Qualifier>,
    // The parser may fail to produce a type due to a syntax error, but still produce a global.
    // (although that's currently impossible in the current grammar)
    pub ty: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub ast: AstId<ast::StructField>,
    pub name: String,
    // TODO: Option<Type> in case of syntax errors
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    pub attrs: Vec<AstId<ast::Attribute>>,
    pub name: String,
    pub visibility: Visibility,
    pub fields: IndexVec<StructField>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionParam {
    pub ast: AstId<ast::FnParam>,
    pub name: String,
    // TODO: Option<Type> in case of syntax errors
    pub ty: Type,
}

/// A function definition or declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
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
pub struct Import {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructIndexEntry {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionIndexEntry {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GlobalIndexEntry {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImportIndexEntry {
    pub ast_id: AstId<ast::ImportDecl>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IndexEntry {
    Import {
        ast_id: AstId<ast::ImportDecl>,
        uri: String,
    },
    Struct {
        ast_id: AstId<ast::StructDef>,
        name: String,
    },
    Function {
        ast_id: AstId<ast::FnDef>,
        name: String,
    },
    Global {
        ast_id: AstId<ast::Global>,
        name: String,
    },
}

// Invalidate all IDs on definition added?
// E.g. add definition => module index invalidated, all StructId,

/// Represents a module.
///
/// Bodies of items (functions and initializers) are type-checked on-demand.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ModuleIndex {
    pub imports: Vec<Import>,
    pub definitions: Vec<IndexEntry>,
    // Collected errors (& warnings)
    pub diagnostics: Vec<ItemDiagnostic>,
}

/*
impl Index<Id<Import>> for ModuleIndex {
    type Output = Import;
    fn index(&self, index: Id<Import>) -> &Self::Output {
        &self.imports[index]
    }
}

impl Index<Id<Struct>> for ModuleIndex {
    type Output = Struct;
    fn index(&self, index: Id<Struct>) -> &Self::Output {
        &self.structs[index]
    }
}

impl Index<Id<Global>> for ModuleIndex {
    type Output = Global;
    fn index(&self, index: Id<Global>) -> &Self::Output {
        &self.globals[index]
    }
}

impl Index<Id<Function>> for ModuleIndex {
    type Output = Function;
    fn index(&self, index: Id<Function>) -> &Self::Output {
        &self.functions[index]
    }
}*/

impl ModuleIndex {
    /// Creates a new module.
    pub fn new() -> ModuleIndex {
        ModuleIndex {
            imports: Default::default(),
            definitions: vec![],
            diagnostics: vec![],
        }
    }
}

/*
#[derive(Clone, PartialEq, Eq)]
pub struct ModuleIndexMap {
    pub(crate) ast_map: AstMap,
}

impl ModuleIndexMap {
    pub fn new() -> ModuleIndexMap {
        ModuleIndexMap { ast_map: AstMap::new() }
    }

    /*pub fn ast_map_for_struct(&self, strukt: Id<Struct>) -> &AstMap {
        &self.structs[strukt].1
    }

    pub fn ast_map_for_global(&self, global: Id<Global>) -> &AstMap {
        &self.globals[global].1
    }

    pub fn ast_map_for_function(&self, function: Id<Function>) -> &AstMap {
        &self.functions[function].1
    }*/
}*/

/*
impl Index<Id<Struct>> for ModuleMap {
    type Output = AstPtr<ast::StructDef>;

    fn index(&self, index: Id<Struct>) -> &Self::Output {
        &self.structs[index].0
    }
}
impl Index<Id<Import>> for ModuleMap {
    type Output = AstPtr<ast::ImportDecl>;

    fn index(&self, index: Id<Import>) -> &Self::Output {
        &self.imports[index]
    }
}
impl Index<Id<Global>> for ModuleMap {
    type Output = AstPtr<ast::Global>;

    fn index(&self, index: Id<Global>) -> &Self::Output {
        &self.globals[index].0
    }
}
impl Index<Id<Function>> for ModuleMap {
    type Output = AstPtr<ast::FnDef>;

    fn index(&self, index: Id<Function>) -> &Self::Output {
        &self.functions[index].0
    }
}*/

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportLoc {
    pub module: ModuleId,
    pub import: AstId<ast::ImportDecl>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructLoc {
    pub module: ModuleId,
    pub strukt: AstId<ast::StructDef>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionLoc {
    pub module: ModuleId,
    pub function: AstId<ast::FnDef>,
}

impl FunctionLoc {
    /*/// Returns the AST node
    pub fn node_from_id<N: AstNode<Language = Lang>>(&self, db: &dyn CompilerDb, id: AstId<N>) -> InFile<N> {
        let def_map = db.module_item_map(self.module);
        def_map
            .ast_map_for_function(self.function)
            .node_in_file(db, self.module, id)
    }*/

    /*/// Returns function data.
    pub fn data<'db>(&self, db: &'db dyn CompilerDb) -> &'db Function {

    }*/
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalLoc {
    pub module: ModuleId,
    pub global: AstId<ast::Global>,
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

    /*pub fn ast_map<'db>(&self, compiler: &'db dyn CompilerDb) -> &'db AstMap {
        let module = self.module().index()
        let module_item_map = compiler.module_item_map(module);
        match self {
            DefLoc::Struct(s) => module_item_map.ast_map_for_struct(s.strukt),
            DefLoc::Global(g) => module_item_map.ast_map_for_global(g.global),
            DefLoc::Function(f) => module_item_map.ast_map_for_function(f.function),
        }
    }*/
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

/// Identifies a parent scope.
#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParentScopeId {
    /// Inside a definition.
    Def(DefId),
    /// Inside a function body.
    FunctionBody(FunctionId),
}

impl ParentScopeId {
    pub fn module(&self, db: &dyn CompilerDb) -> ModuleId {
        match self {
            ParentScopeId::Def(d) => d.module(db),
            ParentScopeId::FunctionBody(function) => function.loc(db).module,
        }
    }

    pub fn ast_map<'db>(&self, db: &'db dyn CompilerDb) -> &'db AstMap {
        match self {
            ParentScopeId::Def(d) => d.ast_map(db),
            ParentScopeId::FunctionBody(function) => &db.function_body_map(*function).ast_map,
        }
    }

    /// Returns the span of an AstId relative to this scope.
    pub fn span(&self, db: &dyn CompilerDb, id: impl Into<RawAstId>) -> Span {
        let file = self.module(db).source_file(db);
        let range = self.ast_map(db).raw_node_ptr(id.into()).text_range();
        Span { file, range }
    }
}

impl From<StructId> for ParentScopeId {
    fn from(value: StructId) -> Self {
        ParentScopeId::Def(value.into())
    }
}
impl From<GlobalId> for ParentScopeId {
    fn from(value: GlobalId) -> Self {
        ParentScopeId::Def(value.into())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstExprLoc {
    pub parent: ParentScopeId,
    pub ast_id: AstId<ast::Expr>,
}

new_key_type!(
    pub struct ConstExprId;
);

impl ConstExprId {
    pub fn parent(&self, db: &dyn CompilerDb) -> ParentScopeId {
        self.loc(db).parent
    }

    pub fn loc(&self, db: &dyn CompilerDb) -> ConstExprLoc {
        db.lookup_const_expr_id(*self)
    }

    pub fn ast_node(&self, db: &dyn CompilerDb) -> InFile<ast::Expr> {
        let loc = db.lookup_const_expr_id(*self);
        loc.parent
            .ast_map(db)
            .node_in_file(db, loc.parent.module(db), loc.ast_id)
    }
}

new_key_type!(
    pub struct StructId;
);

new_key_type!(
    pub struct FunctionId;
);

new_key_type!(
    pub struct GlobalId;
);

// TODO: dedup methods

impl StructId {
    pub fn loc(&self, db: &dyn CompilerDb) -> StructLoc {
        db.lookup_struct_id(*self)
    }

    pub fn data<'db>(&self, db: &'db dyn CompilerDb) -> &'db Struct {
        db.struct_data(*self)
    }

    pub fn field_types<'db>(&self, db: &'db dyn CompilerDb) -> &'db [crate::ty::Type] {
        db.struct_field_types(*self)
    }

    pub fn ast_node<'db>(&self, db: &'db dyn CompilerDb) -> InFile<ast::StructDef> {
        let loc = self.loc(db);
        db.module_index_map(loc.module).node_in_file(db, loc.module, loc.strukt)
    }

    pub fn child_ast_node<N: AstNode<Language = Lang>>(
        &self,
        db: &dyn CompilerDb,
        local_ast_id: AstId<N>,
    ) -> InFile<N> {
        db.struct_data_ast_map(*self)
            .node_in_file(db, self.loc(db).module, local_ast_id)
    }

    pub fn ast_map<'db>(&self, db: &'db dyn CompilerDb) -> &'db AstMap {
        db.struct_data_ast_map(*self)
    }
}

impl GlobalId {
    pub fn loc(&self, db: &dyn CompilerDb) -> GlobalLoc {
        db.lookup_global_id(*self)
    }

    pub fn data<'db>(&self, db: &'db dyn CompilerDb) -> &'db Global {
        db.global_data(*self)
    }

    pub fn ast_node<'db>(&self, db: &'db dyn CompilerDb) -> InFile<ast::Global> {
        let loc = self.loc(db);
        db.module_index_map(loc.module).node_in_file(db, loc.module, loc.global)
    }

    pub fn child_ast_node<N: AstNode<Language = Lang>>(
        &self,
        db: &dyn CompilerDb,
        local_ast_id: AstId<N>,
    ) -> InFile<N> {
        db.global_data_ast_map(*self)
            .node_in_file(db, self.loc(db).module, local_ast_id)
    }

    pub fn ast_map<'db>(&self, db: &'db dyn CompilerDb) -> &'db AstMap {
        db.global_data_ast_map(*self)
    }

    pub fn ty(self, db: &dyn CompilerDb) -> crate::ty::Type {
        db.global_ty(self).clone()
    }
}

impl FunctionId {
    pub fn loc(&self, db: &dyn CompilerDb) -> FunctionLoc {
        db.lookup_function_id(*self)
    }

    pub fn data<'db>(&self, db: &'db dyn CompilerDb) -> &'db Function {
        db.function_data(*self)
    }

    pub fn signature<'db>(&self, db: &'db dyn CompilerDb) -> &'db FunctionSignature {
        db.function_signature(*self)
    }

    pub fn ast_node(&self, db: &dyn CompilerDb) -> InFile<ast::FnDef> {
        let loc = self.loc(db);
        db.module_index_map(loc.module)
            .node_in_file(db, loc.module, loc.function)
    }

    pub fn child_ast_node<N: AstNode<Language = Lang>>(
        &self,
        db: &dyn CompilerDb,
        local_ast_id: AstId<N>,
    ) -> InFile<N> {
        db.function_data_ast_map(*self)
            .node_in_file(db, self.loc(db).module, local_ast_id)
    }

    pub fn ast_map<'db>(&self, db: &'db dyn CompilerDb) -> &'db AstMap {
        db.function_data_ast_map(*self)
    }
}

/// ID of a definition (struct, function, or global).
#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefId {
    Struct(StructId),
    Function(FunctionId),
    Global(GlobalId),
}

impl DefId {
    /// Returns the corresponding `DefLoc`.
    pub fn loc(&self, db: &dyn CompilerDb) -> DefLoc {
        match self {
            DefId::Struct(struct_id) => struct_id.loc(db).into(),
            DefId::Function(function_id) => function_id.loc(db).into(),
            DefId::Global(global_id) => global_id.loc(db).into(),
        }
    }

    /// Returns the AST map for AstIds relative to this definition.
    pub fn ast_map<'db>(&self, db: &'db dyn CompilerDb) -> &'db AstMap {
        match self {
            DefId::Struct(id) => id.ast_map(db),
            DefId::Function(id) => id.ast_map(db),
            DefId::Global(id) => id.ast_map(db),
        }
    }

    /*pub fn ast_node(&self, db: &dyn CompilerDb) -> InFile<ast::FnDef> {
        match self {
            DefId::Struct(id) => id.ast_node(db),
            DefId::Function(id) => id.ast_node(db),
            DefId::Global(id) => id.ast_node(db),
        }
    }*/

    pub fn child_ast_node<N: AstNode<Language = Lang>>(
        &self,
        db: &dyn CompilerDb,
        local_ast_id: AstId<N>,
    ) -> InFile<N> {
        match self {
            DefId::Struct(id) => id.child_ast_node(db, local_ast_id),
            DefId::Function(id) => id.child_ast_node(db, local_ast_id),
            DefId::Global(id) => id.child_ast_node(db, local_ast_id),
        }
    }

    pub fn module(&self, db: &dyn CompilerDb) -> ModuleId {
        self.loc(db).module()
    }
}

impl From<StructId> for DefId {
    fn from(value: StructId) -> Self {
        DefId::Struct(value)
    }
}
impl From<GlobalId> for DefId {
    fn from(value: GlobalId) -> Self {
        DefId::Global(value)
    }
}
impl From<FunctionId> for DefId {
    fn from(value: FunctionId) -> Self {
        DefId::Function(value)
    }
}
/*
/// Represents something with its own AstMap (either a Def or a Body).
#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstMapOwnerId {
    Def(DefId),
    FunctionBody(FunctionId),
}*/

////////////////////////////////////////////////////////////////////////////////////////////////////

fn lower_module_item(_index: &mut ModuleIndex, map: &mut AstMap, item: &ast::Item) -> Option<IndexEntry> {
    // TODO: this is the place to check for name clashes
    match item {
        ast::Item::Global(global_var) => {
            let name = global_var.name()?;
            let ast_id = map.push(global_var);
            Some(IndexEntry::Global {
                ast_id,
                name: name.text(),
            })
        }
        ast::Item::FnDef(fn_def) => {
            let name = fn_def.name()?;
            let ast_id = map.push(fn_def);
            Some(IndexEntry::Function {
                ast_id,
                name: name.text(),
            })
        }
        ast::Item::StructDef(struct_def) => {
            let name = struct_def.name()?;
            let ast_id = map.push(struct_def);
            Some(IndexEntry::Struct {
                ast_id,
                name: name.text(),
            })
        }
        ast::Item::ImportDecl(import) => {
            let ast_id = map.push(import);
            let uri = import.uri()?.string()?.text().to_string();
            Some(IndexEntry::Import { ast_id, uri })
        }
    }
}

pub(crate) fn module_index_query(db: &dyn CompilerDb, module: ModuleId) {
    let (_source_file_id, syntax) = module.syntax(db);
    let module_ast = ast::Module::cast(SyntaxNode::new_root(syntax)).expect("invalid module syntax");
    let mut module_index = ModuleIndex::new();
    let mut module_map = AstMap::new();
    for item in module_ast.items() {
        if let Some(entry) = lower_module_item(&mut module_index, &mut module_map, &item) {
            module_index.definitions.push(entry);
        }
    }
    db.set_module_index(module, module_index);
    db.set_module_index_map(module, module_map);
}

pub(crate) fn struct_data_query(db: &dyn CompilerDb, strukt: StructId) {
    let mut ctxt = DefLowerCtxt::new(db, strukt.into());
    let (struct_data, map) = ctxt.lower_struct(&strukt.ast_node(db).data);
    db.set_struct_data(strukt, struct_data);
    db.set_struct_data_ast_map(strukt, map);
}

pub(crate) fn function_data_query(db: &dyn CompilerDb, function: FunctionId) {
    let mut ctxt = DefLowerCtxt::new(db, function.into());
    let (function_data, map) = ctxt.lower_function(&function.ast_node(db).data);
    db.set_function_data(function, function_data);
    db.set_function_data_ast_map(function, map);
}

pub(crate) fn global_data_query(db: &dyn CompilerDb, global: GlobalId) {
    let mut ctxt = DefLowerCtxt::new(db, global.into());
    let (global_data, map) = ctxt.lower_global_variable(&global.ast_node(db).data);
    db.set_global_data(global, global_data);
    db.set_global_data_ast_map(global, map);
}
