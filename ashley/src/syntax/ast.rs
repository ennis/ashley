use std::num::{ParseFloatError, ParseIntError};
use crate::{
    syntax::{ArithOp, BinaryOp, CmpOp, LogicOp, SyntaxKind, SyntaxNode, SyntaxToken},
    T,
};

pub trait AstNode {
    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

pub trait AstToken {
    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;
    fn syntax(&self) -> &SyntaxToken;
    fn text(&self) -> &str;
}

macro_rules! impl_ast_token {
    ($name:ident <$syntax_kind:ident>) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            syntax: SyntaxToken,
        }

        impl AstToken for $name {
            fn cast(syntax: SyntaxToken) -> Option<Self> {
                match syntax.kind() {
                    SyntaxKind::$syntax_kind => Some($name { syntax }),
                    _ => None,
                }
            }

            fn syntax(&self) -> &SyntaxToken {
                &self.syntax
            }

            fn text(&self) -> &str {
                self.syntax.text()
            }
        }
    };
}

macro_rules! impl_ast_node {
    (@getter node $method:ident: $ast:ty ) => {
        pub fn $method(&self) -> Option<$ast> {
            self.syntax.children().find_map(<$ast>::cast)
        }
    };
    (@getter nodes $method:ident: $ast:ty ) => {
        pub fn $method(&self) -> impl Iterator<Item=$ast> {
            self.syntax.children().filter_map(<$ast>::cast)
        }
    };
    (@getter token $method:ident: $ast:ty ) => {
        pub fn $method(&self) -> Option<$ast> {
             self.syntax.children_with_tokens().filter_map(|it| it.into_token()).find_map(<$ast>::cast)
        }
    };
    ($name:ident <$syntax_kind:ident> [$( $kind:ident $method:ident: $ast:ty ),*] ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            syntax: SyntaxNode,
        }

        impl AstNode for $name {
            fn cast(syntax: SyntaxNode) -> Option<Self> {
                match syntax.kind() {
                    SyntaxKind::$syntax_kind => Some($name { syntax }),
                    _ => None,
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.syntax
            }
        }

        impl $name {
            $(
                impl_ast_node!(@getter $kind $method: $ast);
            )*
        }
    };

    /*($name:ident <$syntax_kind:ident>) => {
        impl_ast_node!($name <$syntax_kind> CHILD [] CHILDREN [] TOKEN []);
    };

    ($name:ident <$syntax_kind:ident> CHILD [ $($child_method:ident: $child_ast:ty),* ]) => {
        impl_ast_node!($name <$syntax_kind> CHILD [$($child_method:$child_ast),*] CHILDREN [] TOKEN []);
    };

    ($name:ident <$syntax_kind:ident> TOKEN [ $($token_method:ident: $token_ast:ty),* ]) => {
        impl_ast_node!($name <$syntax_kind> CHILD [] CHILDREN [] TOKEN [$($token_method:$token_ast),*]);
    };*/
}

macro_rules! impl_ast_variant_node {
    ($name:ident, [ $($kind:ident => $variant:ident),* ]) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant ($variant)),*
        }

        impl AstNode for $name {
            fn cast(syntax: SyntaxNode) -> Option<Self> {
                match syntax.kind() {
                    $(SyntaxKind::$kind => Some(Self::$variant($variant::cast(syntax).unwrap())),)*
                    _ => None,
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$variant(x) => &x.syntax(),)*
                }
            }
        }
    };
}

impl_ast_token!(Ident<IDENT>);
impl_ast_token!(AstString<STRING>);
impl_ast_token!(IntNumber<INT_NUMBER>);
impl_ast_token!(FloatNumber<FLOAT_NUMBER>);
impl_ast_token!(Else<ELSE_KW>);
impl_ast_token!(Eq<EQ>);

impl_ast_node!(Module      <MODULE>       [nodes items: Item]);
impl_ast_node!(ImportDecl  <IMPORT_DECL>  [token package_name: Ident]);
impl_ast_node!(ImportParamList <IMPORT_PARAM_LIST>  []);
impl_ast_node!(ImportAlias <IMPORT_ALIAS> [token alias: Ident]);
impl_ast_node!(TypeRef     <TYPE_REF>     [token ident: Ident]);
impl_ast_node!(TupleType   <TUPLE_TYPE>   [nodes fields: Type]);
impl_ast_node!(ArrayType   <ARRAY_TYPE>   [node  element_type: Type, node length: Expr]);
impl_ast_node!(ClosureType <CLOSURE_TYPE> [node param_list: ClosureParamList, node ret_type: RetType]);
impl_ast_node!(StructDef   <STRUCT_DEF>   [nodes fields: StructField]);
impl_ast_node!(StructField <STRUCT_FIELD> [token ident: Ident, node ty: Type]);
impl_ast_node!(Block       <BLOCK>        [nodes stmts: Stmt]);
impl_ast_node!(FnParam     <FN_PARAM>     [token ident: Ident, node ty: Type]);
impl_ast_node!(ParamList   <PARAM_LIST>   [nodes parameters: FnParam]);
impl_ast_node!(ClosureParamList   <CLOSURE_PARAM_LIST>   [nodes parameters: Type]);

impl_ast_node!(FnDef<FN_DEF>
               [node extern_: Extern,
                node ret_type: RetType,
                node param_list: ParamList,
                node block: Block,
                token name: Ident]);

impl_ast_node!(FnDecl<FN_DECL>
               [node extern_: Extern,
                node ret_type: RetType,
                node param_list: ParamList,
                node block: Block,
                token name: Ident]);

impl_ast_node!(ArgList       <ARG_LIST>    [nodes arguments: Expr]);
impl_ast_node!(RetType       <RET_TYPE>    [node ty: Type]);
impl_ast_node!(ExprStmt      <EXPR_STMT>   [node expr: Expr]);
impl_ast_node!(ReturnStmt    <RETURN_STMT> [node expr: Expr]);
impl_ast_node!(BreakStmt     <BREAK_STMT> []);
impl_ast_node!(ContinueStmt  <CONTINUE_STMT> []);
impl_ast_node!(DiscardStmt   <DISCARD_STMT> []);
impl_ast_node!(IfStmt        <IF_STMT>     [node condition: Expr, node block: Block, node else_branch: ElseBranch]);
impl_ast_node!(WhileStmt     <WHILE_STMT>  [node condition: Expr, node block: Block]);
impl_ast_node!(ElseBranch    <ELSE_BRANCH> [token else_: Else, node block: Block]);
impl_ast_node!(BinExpr       <BIN_EXPR>    []);
impl_ast_node!(IndexExpr     <INDEX_EXPR>  []);
impl_ast_node!(ParenExpr     <PAREN_EXPR>  [node expr: Expr]);
impl_ast_node!(CallExpr      <CALL_EXPR>   [node func: Expr, node arg_list: ArgList]);
impl_ast_node!(PrefixExpr    <PREFIX_EXPR> []);
impl_ast_node!(FieldExpr     <FIELD_EXPR>  []);
impl_ast_node!(LitExpr       <LIT_EXPR>    []);
impl_ast_node!(PathExpr      <PATH_EXPR>   [token ident: Ident]);
impl_ast_node!(TupleExpr     <TUPLE_EXPR>  [nodes fields: Expr]);
impl_ast_node!(ArrayExpr     <ARRAY_EXPR>  [nodes elements: Expr]);
impl_ast_node!(Initializer   <INITIALIZER> [token eq_: Eq, node expr: Expr]);
impl_ast_node!(Qualifier     <QUALIFIER>   []);
impl_ast_node!(Extern        <EXTERN>      []);
impl_ast_node!(Global        <GLOBAL>      [token name: Ident, node extern_: Extern, node qualifier: Qualifier, node ty: Type, node initializer: Initializer ]);
impl_ast_node!(LocalVariable <LOCAL_VARIABLE> [token name: Ident, node ty: Type, node initializer: Initializer ]);

impl_ast_variant_node!(Type, [ TYPE_REF => TypeRef, TUPLE_TYPE => TupleType, ARRAY_TYPE => ArrayType, CLOSURE_TYPE => ClosureType ]);
impl_ast_variant_node!(Item, [ FN_DEF => FnDef, FN_DECL => FnDecl, GLOBAL => Global, STRUCT_DEF => StructDef, IMPORT_DECL => ImportDecl ]);
impl_ast_variant_node!(Stmt, [
    EXPR_STMT => ExprStmt,
    RETURN_STMT => ReturnStmt,
    WHILE_STMT => WhileStmt,
    BREAK_STMT => BreakStmt,
    CONTINUE_STMT => ContinueStmt,
    DISCARD_STMT => DiscardStmt,
    LOCAL_VARIABLE => LocalVariable,
    IF_STMT => IfStmt
]);
impl_ast_variant_node!(Expr, [
    BIN_EXPR => BinExpr,
    PREFIX_EXPR => PrefixExpr,
    CALL_EXPR => CallExpr,
    INDEX_EXPR => IndexExpr,
    PAREN_EXPR => ParenExpr,
    LIT_EXPR => LitExpr,
    PATH_EXPR => PathExpr,
    TUPLE_EXPR => TupleExpr,
    ARRAY_EXPR => ArrayExpr,
    FIELD_EXPR => FieldExpr
]);

//--------------------------------------------------------------------------------------------------
impl Ident {
    pub fn text(&self) -> &str {
        self.syntax.text()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Radix {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl Radix {
    pub fn prefix_len(&self) -> usize {
        match self {
            Radix::Binary => 2,
            Radix::Octal => 2,
            Radix::Decimal => 0,
            Radix::Hexadecimal => 2,
        }
    }
}

impl IntNumber {
    pub fn radix(&self) -> Radix {
        match self.text().get(..2).unwrap_or_default() {
            "0b" => Radix::Binary,
            "0o" => Radix::Octal,
            "0x" => Radix::Hexadecimal,
            _ => Radix::Decimal,
        }
    }

    pub fn split_into_parts(&self) -> (&str, &str, &str) {
        let radix = self.radix();
        let (prefix, mut text) = self.text().split_at(radix.prefix_len());

        let is_suffix_start: fn(&(usize, char)) -> bool = match radix {
            Radix::Hexadecimal => |(_, c)| matches!(c, 'g'..='z' | 'G'..='Z'),
            _ => |(_, c)| c.is_ascii_alphabetic(),
        };

        let mut suffix = "";
        if let Some((suffix_start, _)) = text.char_indices().find(is_suffix_start) {
            let (text2, suffix2) = text.split_at(suffix_start);
            text = text2;
            suffix = suffix2;
        }

        (prefix, text, suffix)
    }

    pub fn value(&self) -> Result<i64, ParseIntError> {
        let (_, text, _) = self.split_into_parts();
        i64::from_str_radix(&text.replace('_', ""), self.radix() as u32)
    }

    pub fn suffix(&self) -> Option<&str> {
        let (_, _, suffix) = self.split_into_parts();
        if suffix.is_empty() {
            None
        } else {
            Some(suffix)
        }
    }

    pub fn float_value(&self) -> Option<f64> {
        let (_, text, _) = self.split_into_parts();
        text.replace('_', "").parse::<f64>().ok()
    }
}

impl FloatNumber {
    pub fn value(&self) -> Result<f64, ParseFloatError> {
        // TODO hex floats
        self.text().parse::<f64>()
    }
}

//--------------------------------------------------------------------------------------------------
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    String(AstString),
    IntNumber(IntNumber),
    FloatNumber(FloatNumber),
    Bool(bool),
}

impl LitExpr {
    pub fn token(&self) -> SyntaxToken {
        self.syntax()
            .children_with_tokens()
            .find(|e| !e.kind().is_trivia())
            .and_then(|e| e.into_token())
            .unwrap()
    }

    pub fn kind(&self) -> LiteralKind {
        let token = self.token();

        if let Some(t) = IntNumber::cast(token.clone()) {
            return LiteralKind::IntNumber(t);
        }
        if let Some(t) = FloatNumber::cast(token.clone()) {
            return LiteralKind::FloatNumber(t);
        }
        if let Some(t) = AstString::cast(token.clone()) {
            return LiteralKind::String(t);
        }

        match token.kind() {
            T![true] => LiteralKind::Bool(true),
            T![false] => LiteralKind::Bool(false),
            _ => unreachable!(),
        }
    }
}

//--------------------------------------------------------------------------------------------------

impl BinExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(1)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(2)
    }

    pub fn op_details(&self) -> Option<(SyntaxToken, BinaryOp)> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|c| {
                #[rustfmt::skip] let bin_op = match c.kind() {
                T![||] => BinaryOp::LogicOp(LogicOp::Or),
                T![&&] => BinaryOp::LogicOp(LogicOp::And),

                T![==] => BinaryOp::CmpOp(CmpOp::Eq),
                T![!=] => BinaryOp::CmpOp(CmpOp::Ne),
                T![<=] => BinaryOp::CmpOp(CmpOp::Le),
                T![>=] => BinaryOp::CmpOp(CmpOp::Ge),
                T![<]  => BinaryOp::CmpOp(CmpOp::Lt),
                T![>]  => BinaryOp::CmpOp(CmpOp::Gt),

                T![+]  => BinaryOp::ArithOp(ArithOp::Add),
                T![*]  => BinaryOp::ArithOp(ArithOp::Mul),
                T![-]  => BinaryOp::ArithOp(ArithOp::Sub),
                T![/]  => BinaryOp::ArithOp(ArithOp::Div),
                T![%]  => BinaryOp::ArithOp(ArithOp::Rem),
                T![<<] => BinaryOp::ArithOp(ArithOp::Shl),
                T![>>] => BinaryOp::ArithOp(ArithOp::Shr),
                T![^]  => BinaryOp::ArithOp(ArithOp::BitXor),
                T![|]  => BinaryOp::ArithOp(ArithOp::BitOr),
                T![&]  => BinaryOp::ArithOp(ArithOp::BitAnd),

                T![=]   => BinaryOp::Assignment(None),
                T![+=]  => BinaryOp::Assignment(Some(ArithOp::Add)),
                T![*=]  => BinaryOp::Assignment(Some(ArithOp::Mul)),
                T![-=]  => BinaryOp::Assignment(Some(ArithOp::Sub)),
                T![/=]  => BinaryOp::Assignment(Some(ArithOp::Div)),
                T![%=]  => BinaryOp::Assignment(Some(ArithOp::Rem)),
                T![<<=] => BinaryOp::Assignment(Some(ArithOp::Shl)),
                T![>>=] => BinaryOp::Assignment(Some(ArithOp::Shr)),
                T![^=]  => BinaryOp::Assignment(Some(ArithOp::BitXor)),
                T![|=]  => BinaryOp::Assignment(Some(ArithOp::BitOr)),
                T![&=]  => BinaryOp::Assignment(Some(ArithOp::BitAnd)),

                _ => return None,
            };
                Some((c, bin_op))
            })
    }
}

impl IndexExpr {
    pub fn array(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(1)
    }

    pub fn index(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(2)
    }
}

/// Describes the kind of a global program variable.
#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum QualifierKind {
    Uniform,
    Const,
    In,
    Out,
}

fn first_token(node: &SyntaxNode) -> SyntaxToken {
    node
        .children_with_tokens()
        .find(|e| !e.kind().is_trivia())
        .and_then(|e| e.into_token())
        .unwrap()
}

impl Qualifier {
    pub fn token(&self) -> SyntaxToken {
        first_token(self.syntax())
    }

    pub fn global_kind(&self) -> Option<QualifierKind> {
        match self.token().kind() {
            SyntaxKind::UNIFORM_KW => Some(QualifierKind::Uniform),
            SyntaxKind::IN_KW => Some(QualifierKind::In),
            SyntaxKind::OUT_KW => Some(QualifierKind::Out),
            SyntaxKind::CONST_KW => Some(QualifierKind::Const),
            _ => None,
        }
    }
}

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::{
        diagnostic::SourceFileProvider,
        syntax,
        syntax::ast::{AstNode, Item, Module},
    };
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

    fn parse_module(text: &str) -> Option<Module> {
        let mut sources = SourceFileProvider::new();
        let src_id = sources.register_source("<input>", text);
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        Module::cast(dbg!(syntax::parse(text, src_id, sources, writer)))
    }

    #[test]
    fn basic_ast() {
        let m = parse_module(
            r#"
fn main() {
}
        "#,
        )
        .unwrap();

        let item = m.items().next().unwrap();
        match item {
            Item::FnDef(d) => {
                assert_eq!(d.name().unwrap().text(), "main");
            }
            _ => {}
        }
    }
}
