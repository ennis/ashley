mod const_eval;
mod lower;

use crate::{
    builtins::BuiltinSignature,
    def,
    def::{FunctionLoc, GlobalLoc},
    ty,
    ty::{body::lower::TyBodyLowerCtxt, TyOwnerId, Type},
    CompilerDb, ConstantValue,
};
use ashley::def::AstId;
use ashley_data_structures::{Id, IndexVec};
use smallvec::SmallVec;
use std::sync::Arc;

use crate::{
    builtins::{BuiltinOperationPtr, Constructor},
    def::{ConstExprId, FunctionId, GlobalId},
    syntax::ast,
    ty::{body::const_eval::ConstEvalCtxt, TyDiagnostic},
};

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Stmt {
    pub ast_id: AstId<ast::Stmt>,
    pub kind: StmtKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StmtKind {
    // TODO empty statement?
    Select {
        condition: Id<Expr>,
        true_branch: Id<Stmt>,
        false_branch: Option<Id<Stmt>>,
    },
    ForLoop {
        initializer: Option<Id<Stmt>>,
        condition: Option<Id<Expr>>,
        loop_expr: Option<Id<Expr>>,
        stmt: Id<Stmt>,
    },
    WhileLoop {
        condition: Id<Expr>,
        stmt: Id<Stmt>,
    },
    Local {
        var: Id<LocalVar>,
        initializer: Option<Id<Expr>>,
    },
    Return {
        value: Option<Id<Expr>>,
    },
    ExprStmt {
        expr: Id<Expr>,
    },
    Block {
        block: Id<Block>,
    },
    Discard,
    Break,
    Continue,
    Error,
}

pub type ExprAstId = AstId<ast::Expr>;

/// Represents an expression with its inferred type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    /// Inferred type of the expression.
    pub ty: Type,
    /// Original expression in the `def::Body`.
    pub ast_id: AstId<ast::Expr>,
    /// Expression kind.
    pub kind: ExprKind,
}

impl Expr {
    pub(crate) fn new(kind: ExprKind, ast_id: AstId<ast::Expr>, ty: Type) -> Self {
        Expr { kind, ast_id, ty }
    }
}

/// This is because both local variables and functions parameters (in the AST) are represented as local variables in
/// typed bodies.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalVarAstId {
    /// **NOTE**: this is relative to AST map of the `Function` definition, not the corresponding `Body`.
    FnParam(AstId<ast::FnParam>),
    /// **NOTE**: contrary to the above, this is relative to AST map of the `Body` of the function.
    LocalVariable(AstId<ast::LocalVariable>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalVar {
    pub name: String,
    pub ast_id: LocalVarAstId,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Id<Stmt>>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConversionKind {
    /// int32 -> uint32 bit casts (no-op)
    IntBitcast,
    /// Integer to float or double
    SignedIntToFloat,
    /// Unsigned integer to float or double
    UnsignedIntToFloat,
    /// Float to double
    FloatConvert,
    FloatToSignedInt,
    FloatToUnsignedInt,
    /// Different layouts (e.g. different array stride or matrix stride or order)
    Layout,
    /// Automatic dereference of places (lvalues)
    Deref,
}

pub(crate) type ComponentIndices = SmallVec<[i32; 4]>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Binary {
        op: ast::BinaryOp,
        /// Signature of the binary operation.
        // TODO replace this with a DefId to the operator function
        signature: BuiltinSignature,
        lhs: Id<Expr>,
        rhs: Id<Expr>,
    },
    BinaryAssign {
        op: ast::BinaryOp,
        /// Signature of the operation that is used to compute the value to assign.
        signature: BuiltinSignature,
        place: Id<Expr>,
        lhs: Id<Expr>,
        rhs: Id<Expr>,
    },
    Unary {
        op: ast::UnaryOp,
        /// Signature of the unary operation.
        signature: BuiltinSignature,
        expr: Id<Expr>,
    },
    Assign {
        lhs: Id<Expr>,
        rhs: Id<Expr>,
    },
    Ternary {
        condition: Id<Expr>,
        true_expr: Id<Expr>,
        false_expr: Id<Expr>,
    },
    Call {
        function: FunctionId,
        args: Vec<Id<Expr>>,
    },
    BuiltinCall {
        signature: BuiltinSignature,
        args: Vec<Id<Expr>>,
    },
    LocalVar {
        var: Id<LocalVar>,
    },
    GlobalVar {
        var: GlobalId,
    },
    //FunctionRef {},
    Index {
        array_or_vector: Id<Expr>,
        index: Id<Expr>,
    },
    Field {
        expr: Id<Expr>,
        //def: Arc<StructDef>,
        index: usize,
    },
    /// Vector component access & shuffle (e.g. `pos.y` or `offset.yx`).
    ComponentAccess {
        expr: Id<Expr>,
        components: ComponentIndices,
    },
    ImplicitConversion {
        expr: Id<Expr>,
        kind: ConversionKind,
    },
    BuiltinConstructor {
        ctor: &'static Constructor,
        args: Vec<Id<Expr>>,
    },
    Literal {
        value: ConstantValue,
    },
    Undef,
}

/// Type-checked body.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub stmts: IndexVec<Stmt>,
    pub exprs: IndexVec<Expr>,
    pub local_vars: IndexVec<LocalVar>,
    pub blocks: IndexVec<Block>,
    pub entry_block: Option<Id<Block>>,
    pub params: Vec<Id<LocalVar>>,
    pub diagnostics: Vec<TyDiagnostic>,
}

impl Body {
    fn with_capacity(body: &crate::def::body::Body) -> Body {
        Body {
            stmts: IndexVec::with_capacity(body.statements.len()),
            exprs: IndexVec::with_capacity(body.expressions.len()),
            local_vars: IndexVec::with_capacity(body.local_vars.len()),
            blocks: IndexVec::with_capacity(body.blocks.len()),
            entry_block: None,
            params: vec![],
            diagnostics: vec![],
        }
    }

    pub fn root_expr(&self) -> Id<Expr> {
        // TODO do the same as def::body
        Id::from_index(self.exprs.len() - 1)
    }
}

pub(crate) fn ty_body_query(compiler: &dyn CompilerDb, body: ConstExprId) {
    let _span = trace_span!("ty_body_query", ?body).entered();

    todo!()

    //let module = body.module();
    //let module_items = compiler.module_items(module);

    // Issues: queries would like to return a reference to a TypeRef (via TyOwner),
    // or to a body, but they return Arc<>, so they can't borrow the compiler DB

    /*match body {
        BodyOwnerId::ConstArraySize(ty_owner) => {}
        BodyOwnerId::ConstArrayStrideSpecifier(ty_owner) => {
            // could be in the module tree, or in another body
            match ty_owner {
                TyOwnerId::Argument { function, argument } => {}
                TyOwnerId::ReturnValue(_) => {}
                TyOwnerId::Field(_) => {}
                TyOwnerId::ArrayElement(_) => {}
                TyOwnerId::LocalVariable { .. } => {}
            }
        }
        BodyOwnerId::FunctionBody(function_id) => {}
        BodyOwnerId::VariableInitializer(_) => {}
    }*/
}

pub(crate) fn ty_function_body_query(compiler: &dyn CompilerDb, function: FunctionId) -> ty::body::Body {
    let _span = trace_span!("ty_function_body_query", ?function).entered();
    lower::lower_function_body(compiler, function)
}

pub(crate) fn ty_const_expr_body_query(compiler: &dyn CompilerDb, const_expr: ConstExprId) -> ty::body::Body {
    let _span = trace_span!("ty_const_expr_body_query", ?const_expr).entered();
    lower::lower_const_expr_body(compiler, const_expr)
}

pub(crate) fn const_eval_query(
    compiler: &dyn CompilerDb,
    const_expr: ConstExprId,
) -> Result<ConstantValue, TyDiagnostic> {
    let _span = trace_span!("const_eval_query", ?const_expr).entered();
    let body = compiler.ty_const_expr_body(const_expr);
    const_eval::eval_const_expr_body(compiler, const_expr, body)
}
