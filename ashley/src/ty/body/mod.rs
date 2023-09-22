mod const_eval;
mod diagnostic;
mod lower;

use crate::{
    builtins::BuiltinSignature,
    item::{BodyOwnerId, FunctionId, GlobalId},
    ty::{body::lower::TyBodyLowerCtxt, TyOwnerId, Type},
    CompilerDb, ConstantValue,
};
use ashley_data_structures::{Id, IndexVec};
use smallvec::SmallVec;
use std::sync::Arc;

use crate::{builtins::Constructor, item::BodyId, syntax::ast};
pub use diagnostic::{ConstEvalDiagnostic, TyBodyDiagnostic};

////////////////////////////////////////////////////////////////////////////////////////////////////

/*pub type Id<Expr = Id<Expr>;
pub type StmtId = Id<Stmt>;
pub type BlockId = Id<Block>;
pub type LocalVarId = Id<LocalVar>;*/

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
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
    Error,
}

/// Represents an expression with its inferred type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub ty: Type,
    pub kind: ExprKind,
}

impl Expr {
    pub(crate) fn new(kind: ExprKind, ty: Type) -> Self {
        Expr { kind, ty }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalVar {
    pub name: String,
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

pub struct Body {
    pub stmts: IndexVec<Stmt>,
    pub exprs: IndexVec<Expr>,
    pub local_var: IndexVec<LocalVar>,
    pub blocks: IndexVec<Block>,
    pub params: Vec<Id<LocalVar>>,
    pub diagnostics: Vec<TyBodyDiagnostic>,
}

pub(crate) fn ty_body_query(compiler: &dyn CompilerDb, body: BodyId) {
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

pub(crate) fn const_eval_query(compiler: &dyn CompilerDb, body: BodyId) -> ConstantValue {
    let _span = trace_span!("const_eval_query", ?body).entered();
    //
    //let body = compiler.body(body);
    //let ty_body = compiler.ty_body(body);
    todo!("const_eval_query")
}
