pub mod body;
mod const_expr;
mod debug;
mod diagnostic;
mod lower;
mod resolver;
mod scope;
mod source;
mod ty;

pub use self::{
    const_expr::ConstExpr,
    scope::{Scope, TypeRes, ValueRes},
    source::HasSource,
    ty::Type,
};
pub(crate) use self::{
    resolver::Resolver,
    scope::{scope_for_body_query, scope_for_definition_query},
};

use crate::{
    db::DebugWithDb,
    diagnostic::{Span, Spanned},
    item::{diagnostic::ItemDiagnostic, lower::ItemLowerCtxt},
    syntax::{ast, Lang, SyntaxNode, SyntaxNodePtr, SyntaxToken, SyntaxTree},
    ty::TyOwnerId,
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
    fmt::Write,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::Index,
    sync::Arc,
};

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
    pub fn to_node(&self, compiler: &dyn CompilerDb, owner_module: ModuleId) -> N {
        let module_map = compiler.module_map(owner_module);
        module_map.get_node(*self)
    }

    pub fn to_node_with_source_file(&self, compiler: &dyn CompilerDb, owner_module: ModuleId) -> InFile<N> {
        let source_file_id = compiler.module_source(owner_module);
        InFile::new(source_file_id, self.to_node(compiler, owner_module))
    }
}

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

/*/// Uniquely identifies a definition.
///
/// This stores the ModuleId and the index of the definition in the module, so
/// it should be stable as long as no definition is added, removed, or reordered.
///
/// Note: if we want to make it stable across definition reorders, we could use the name of the
/// definition as a key, but I'm not sure it's worth the trouble.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DefId {
    pub module: ModuleId,
    pub index: LocalDefId,
}*/

////////////////////////////////////////////////////////////////////////////////////////////////////

/// An `import` directive
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Import {
    pub ast: AstId<ast::ImportDecl>,
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

// Stability of `Def` objects?
// What are the uses of Defs?
// - lower them to SPIR-V
// - resolve their type
// - resolve bodies
//
// Stability?
// - don't want to re-resolve their types on reparses that do not affect the them
//      Q: which reparses do not affect the them?
//      A: only those that don't add a new definition; otherwise resolution may change
//          examples: reordering structs, adding comments
//          reordering defs should not trigger a recheck
//
//      -> this means that tycheck shouldn't depend on the AST directly
//      -> AST IDs instead? No => it will be unstable anyway
//      -> staged approach:
//          -> extract information from definitions into a structure that is independent of the AST
//              -> this *will* be recreated on every AST change, but if the result is the same, it won't dirty anything that depends on it
//          -> use this structure for type-checking: it doesn't depend directly on the AST, so won't recalc if nothing changes

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

/*/// Stable across reparses.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct MemberId {
    /// Owner struct.
    pub strukt: StructId,
    /// Member index.
    pub index: u32,
}*/

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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FunctionParam {
    pub ast: AstId<ast::FnParam>,
    pub name: String,
    pub ty: Type,
}

/// A function definition or declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub ast: AstId<ast::FnDef>,
    pub attributes: Vec<AstId<ast::Attribute>>,
    pub name: String,
    pub visibility: Visibility,
    pub has_body: bool,
    pub linkage: Linkage,
    pub parameters: IndexVec<FunctionParam>,
    pub return_type: Option<Type>,
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
    pub ast: AstId<ast::Global>,
    pub attrs: Vec<AstId<ast::Attribute>>,
    pub name: String,
    pub visibility: Visibility,
    pub linkage: Linkage,
    pub storage_class: Option<Qualifier>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub ast: AstId<ast::StructField>,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Struct {
    pub ast: AstId<ast::StructDef>,
    pub attrs: Vec<AstId<ast::Attribute>>,
    pub name: String,
    pub visibility: Visibility,
    pub fields: IndexVec<StructField>,
}

////////////////////////////////////////////////////////////////////////////////////////////////////
#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportId {
    pub module: ModuleId,
    pub import: Id<Import>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructId {
    pub module: ModuleId,
    pub strukt: Id<Struct>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionId {
    pub module: ModuleId,
    pub function: Id<Function>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct GlobalId {
    pub module: ModuleId,
    pub global: Id<Global>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldId {
    pub strukt: StructId,
    pub field: Id<StructField>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeOwnerId {
    Struct(StructId),
    Global(GlobalId),
    Field(FieldId),
    Function(FunctionId),
    ReturnValue(FunctionId),
    Argument { function: FunctionId, argument: u32 },
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum DefId {
    Struct(StructId),
    Global(GlobalId),
    Function(FunctionId),
}

impl DefId {
    pub fn module(&self) -> ModuleId {
        match self {
            DefId::Struct(s) => s.module,
            DefId::Global(g) => g.module,
            DefId::Function(f) => f.module,
        }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BodyOwnerId {
    /// Constant expression in a type.
    InTypeConst(TyOwnerId),
    /// Function body.
    Function(FunctionId),
    /// Global variable initializer.
    Variable(GlobalId),
    // FIXME: instead of specific variants, have a generic "AstId<Expr|Block>", plus
    // a "ContainerId", which defines the scope for body check.
    // We don't actually care about whether the Id is the "Const Array Size" or the "Stride Specifier", only
    // that is it an expression, and that it is resolved relative to the scope of some other definition.
}

impl BodyOwnerId {
    pub fn module(&self) -> ModuleId {
        match self {
            BodyOwnerId::InTypeConst(ty) => ty.module(),
            BodyOwnerId::Function(function) => function.module,
            BodyOwnerId::Variable(global) => global.module,
        }
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum BodyKind {
    Expr(AstId<ast::Expr>),
    Block(AstId<ast::Block>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BodyLoc {
    pub owner: BodyOwnerId,
    pub kind: BodyKind,
}

new_key_type!(
    pub struct BodyId;
);

////////////////////////////////////////////////////////////////////////////////////////////////////

fn dump_module_items(compiler: &dyn CompilerDb, module: ModuleId, items: &ModuleItems) {
    let mut o = String::new();

    writeln!(o, "=== imports ===");
    for (i, _s) in items.structs.iter_full() {
        let id = StructId { module, strukt: i };
        writeln!(o, "{:?}", id.debug_with(compiler));
    }
    writeln!(o, "=== structs ===");
    for (i, _s) in items.structs.iter_full() {
        let id = StructId { module, strukt: i };
        writeln!(o, "{:?}", id.debug_with(compiler));
    }
    writeln!(o, "=== functions ===");
    for (i, _f) in items.functions.iter_full() {
        let id = FunctionId { module, function: i };
        writeln!(o, "{:?}", id.debug_with(compiler));
    }
    writeln!(o, "=== globals ===");
    for (i, _g) in items.globals.iter_full() {
        let id = GlobalId { module, global: i };
        writeln!(o, "{:?}", id.debug_with(compiler));
    }

    trace!("{}", o);
}

pub(crate) fn module_items_and_ast_map_query(compiler: &dyn CompilerDb, module_id: ModuleId) {
    let _span = trace_span!("module_items_and_ast_map_query", module_id = ?module_id.debug_with(compiler)).entered();
    let (_source_file_id, syntax) = compiler.module_syntax_tree(module_id);
    let mut ctxt = ItemLowerCtxt::new(compiler, syntax);
    let (items, ast_id_map) = ctxt.lower_module();

    compiler.set_module_items(module_id, items);
    compiler.set_module_map(module_id, ast_id_map);
    //dump_module_items(compiler, module_id, &items);
}
