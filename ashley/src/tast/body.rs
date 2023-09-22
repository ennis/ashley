#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalVar {
    pub name: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<StmtId>,
}

impl Block {
    pub fn new() -> Block {
        Block { stmts: Vec::new() }
    }
}

pub type ExprPtr = AstPtr<ast::Expr>;
pub type ExprSource = InFile<ExprPtr>;

pub type StmtPtr = AstPtr<ast::Stmt>;
pub type StmtSource = InFile<StmtPtr>;

pub type LocalVarPtr = AstPtr<ast::LocalVariable>;
pub type LocalVarSource = InFile<LocalVarPtr>;

pub type BlockPtr = AstPtr<ast::Block>;
pub type BlockSource = InFile<BlockPtr>;

#[derive(Clone, PartialEq, Eq)]
pub struct TypedBody {
    pub stmts: IndexVec<Stmt>,
    pub exprs: IndexVec<Expr>,
    pub local_vars: IndexVec<LocalVar>,
    pub blocks: IndexVec<Block>,
    pub params: Vec<LocalVarId>,
    // TODO also warnings
    pub errors: Vec<TyDiagnostic>,
    pub entry_block: Option<BlockId>,

    pub expr_map: HashMap<ExprSource, ExprId>,
    pub expr_map_back: IndexVecMap<ExprId, ExprSource>,

    pub stmt_map: HashMap<StmtSource, StmtId>,
    pub stmt_map_back: IndexVecMap<StmtId, StmtSource>,

    pub local_var_map: HashMap<LocalVarSource, LocalVarId>,
    pub local_var_map_back: IndexVecMap<LocalVarId, LocalVarSource>,
}

impl TypedBody {
    pub fn new() -> TypedBody {
        TypedBody {
            stmts: IndexVec::new(),
            exprs: IndexVec::new(),
            local_vars: IndexVec::new(),
            blocks: IndexVec::new(),
            params: vec![],
            errors: vec![],
            entry_block: None,
            expr_map: Default::default(),
            expr_map_back: Default::default(),
            stmt_map: Default::default(),
            stmt_map_back: Default::default(),
            local_var_map: Default::default(),
            local_var_map_back: Default::default(),
        }
    }

    pub fn root_expr(&self) -> ExprId {
        ExprId::from_index(self.exprs.len() - 1)
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

pub(crate) struct TypeCheckBodyCtxt<'a> {
    compiler: &'a dyn CompilerDb,
    /// Source file containing the AST of the body.
    ///
    /// TODO: we kinda assume that the AST of a def body is all contained in the same file.
    /// This may not be true anymore if we decide to use macros?
    source_file: SourceFileId,
    scopes: Vec<Scope>,
    typed_body: &'a mut TypedBody,
    errors: Vec<TyDiagnostic>,
}

impl<'a> TypeCheckBodyCtxt<'a> {
    pub(crate) fn convert_type(&mut self, ty: ast::Type) -> Type {
        let mut lower_ctxt = TypeLoweringCtxt::new(self.compiler, &self.scopes, &mut self.errors);
        lower_ctxt.lower_type(ty, self.source_file)
    }

    pub(crate) fn in_file_syntax_ptr<N: AstNode>(&self, node: &N) -> InFile<AstPtr<N>> {
        InFile::new(self.source_file, AstPtr::new(node))
    }

    pub(crate) fn span<N: AstNode>(&self, node: &N) -> Span {
        Span::new(self.source_file, node.syntax().text_range())
    }
}

// TODO: some defs do not have bodies, return None for them

/// Builds a scope with all visible global-scope definitions visible to the body of the specified definition.
fn build_scope_for_def_body(compiler: &dyn CompilerDb, def_id: DefId) -> Scope {
    let mut scope = Scope::new();

    // bring definitions from imported packages in the scope
    // TODO: import only externally visible definitions,
    // TODO: import only the items specified in the import clause
    // for now there's no syntax for that (we bring the whole package in scope), but this might change at some point

    let module = compiler.parent_module(def_id);
    let imported_packages = compiler.module_imports(module);

    for &imp_pkg in imported_packages {
        let imp_defs = compiler.module_definitions(imp_pkg);
        for &imp_def_id in imp_defs {
            // retrieve the name of the definition, and add a new entry to the scope under that name
            // NOTE: at some point we may gain the ability to import definitions under different names,
            // and this will need to be updated.
            let imp_def = compiler.definition(imp_def_id);
            scope.add_def(imp_def_id, imp_def);
        }
    }

    // Reconstruct the set of visible declarations before the item body. These are the declarations
    // appearing before the item body. `module_definitions` returns them in declaration order,
    // so add all definitions until we encounter `def_id`.
    let mod_defs = compiler.module_definitions(module);
    let mut i = 0;
    while i < mod_defs.len() && mod_defs[i] != def_id {
        let prev_def = compiler.definition(mod_defs[i]);
        scope.add_def(mod_defs[i], prev_def);
        i += 1;
    }
    scope
}

/// Typechecks the body of a definition.
pub(crate) fn typecheck_body(compiler: &dyn CompilerDb, def: DefId) -> TypedBody {
    let scope = build_scope_for_def_body(compiler, def);

    let def_info = compiler.definition(def);
    // TODO: the idea here is that def bodies always come from ASTs,
    // and thus always have an associated source file, and a span.
    // This may not be the best way, or place, to check that.
    // (We need the source file when emitting diagnostics)
    let source_file = def_info.span.expect("typecheck_body called on definition without a span; definitions with bodies should always be associated to a source file").file;

    let mut typed_body = TypedBody::new();
    let mut ctxt = TypeCheckBodyCtxt {
        compiler,
        scopes: vec![scope],
        source_file,
        typed_body: &mut typed_body,
        errors: vec![],
    };

    match def_info.kind {
        DefKind::Function(ref func) => {
            if let Some(InFile { data: ref ast_ptr, .. }) = func.ast {
                let func_ast = ctxt.compiler.ast_node_for_def(def, ast_ptr);
                if let Some(ref body) = func_ast.block() {
                    // create local vars for function parameters
                    let mut param_scope = Scope::new();
                    for param in func.parameters.iter() {
                        let id = ctxt.typed_body.local_vars.push(LocalVar {
                            name: param.name.clone(),
                            ty: param.ty.clone(),
                        });
                        param_scope.add(param.name.clone(), Res::Local(id));
                        ctxt.typed_body.params.push(id);
                    }
                    ctxt.scopes.push(param_scope);
                    // typecheck main block
                    let entry_block = ctxt.typecheck_block(body);
                    ctxt.typed_body.entry_block = Some(entry_block);
                }
            }
        }
        DefKind::Global(ref global) => {
            if let Some(InFile { data: ref ast_ptr, .. }) = global.ast {
                let global_ast = ctxt.compiler.ast_node_for_def(def, ast_ptr);
                if let Some(ref initializer) = global_ast.initializer() {
                    if let Some(ref expr) = initializer.expr() {
                        ctxt.typecheck_expr(expr);
                    }
                }
            }
        }
        DefKind::Struct(_) => {}
    };

    typed_body.errors = ctxt.errors;
    typed_body
}
