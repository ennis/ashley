use crate::{
    def,
    syntax::{Lang, SyntaxKind, SyntaxNode, SyntaxToken},
    T,
};
use std::num::{ParseFloatError, ParseIntError};

pub use rowan::ast::{AstNode, AstPtr};

//--------------------------------------------------------------------------------------------------
// Operators

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// `!`
    Not,
    /// `-`
    Neg,
    // TODO: complement op
    /// `++`
    PrefixInc,
    /// `--`
    PrefixDec,
    /// `++`
    PostfixInc,
    /// `--`
    PostfixDec,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    LogicOp(LogicOp),
    ArithOp(ArithOp),
    CmpOp(CmpOp),
    Assignment(Option<ArithOp>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArithOp {
    Add,
    Mul,
    Sub,
    Div,
    Rem,
    Shl,
    Shr,
    BitXor,
    BitOr,
    BitAnd,
}

//--------------------------------------------------------------------------------------------------
// AST nodes

/*pub trait AstNode {
    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}*/

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
            type Language = Lang;

            fn can_cast(kind: SyntaxKind) -> bool
            {
                match kind {
                    SyntaxKind::$syntax_kind => true,
                    _ => false,
                }
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some($name { syntax })
                } else {
                    None
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
}

macro_rules! impl_ast_variant_node {
    ($name:ident, [ $($kind:ident => $variant:ident),* ]) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant ($variant)),*
        }

        impl AstNode for $name {
            type Language = Lang;

            fn can_cast(kind: SyntaxKind) -> bool {
                match kind {
                    $(SyntaxKind::$kind => true,)*
                    _ => false,
                }
            }

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

// TODO: there should be a way in impl_ast_node to say that a child node or token is "guaranteed", i.e. that it will always be there in the AST, even if there are syntax errors.
// This could avoid redundant Option checks in lowering code when, by construction, the parser always produces a child node of the expected kind.
// TODO Alternatively, remove useless nodes with only one child?

impl_ast_variant_node!(TypeQualifier, [
    STRIDE_QUALIFIER => StrideQualifier,
    ROW_MAJOR_QUALIFIER => RowMajorQualifier,
    COLUMN_MAJOR_QUALIFIER => ColumnMajorQualifier
]);

impl_ast_variant_node!(AttrItem, [
    ATTR_IDENT => AttrIdent,
    ATTR_NESTED => AttrNested,
    ATTR_LITERAL => AttrLiteral,
    ATTR_KEY_VALUE => AttrKeyValue
]);
impl_ast_node!(Attribute   <ATTRIBUTE>      [node name: Name, node args: AttrArgs]);
impl_ast_node!(AttrIdent   <ATTR_IDENT>     [token ident: Ident]);
impl_ast_node!(AttrLiteral <ATTR_LITERAL>   [node expr: LitExpr]);
impl_ast_node!(AttrKeyValue <ATTR_KEY_VALUE> [token key: Ident, node value: Expr]);
impl_ast_node!(AttrNested  <ATTR_NESTED>  [token ident: Ident, node args: AttrArgs]);
impl_ast_node!(AttrArgs    <ATTR_ARGS>  [nodes args: AttrItem]);
impl_ast_node!(Module      <MODULE>       [nodes items: Item]);
impl_ast_node!(ImportUri   <IMPORT_URI>   [token string: AstString]);
impl_ast_node!(ImportDecl  <IMPORT_DECL>  [node uri: ImportUri]);
impl_ast_node!(ImportParamList <IMPORT_PARAM_LIST>  []);
impl_ast_node!(ImportAlias <IMPORT_ALIAS> [node name: Name]);
impl_ast_node!(TypeRef     <TYPE_REF>     [node name: Name, nodes qualifiers: TypeQualifier]);
impl_ast_node!(TupleType   <TUPLE_TYPE>   [nodes fields: Type]);
impl_ast_node!(StrideQualifier   <STRIDE_QUALIFIER>   [node stride: Expr]);
impl_ast_node!(RowMajorQualifier   <ROW_MAJOR_QUALIFIER>   []);
impl_ast_node!(ColumnMajorQualifier   <COLUMN_MAJOR_QUALIFIER>   []);
impl_ast_node!(ArrayType   <ARRAY_TYPE>   [node element_type: Type, node length: Expr, nodes qualifiers: TypeQualifier]);
impl_ast_node!(ClosureType <CLOSURE_TYPE> [node param_list: ClosureParamList, node return_type: Type]);
impl_ast_node!(StructDef   <STRUCT_DEF>   [nodes attrs: Attribute, node visibility: Visibility, node name: Name, nodes fields: StructField]);
impl_ast_node!(StructField <STRUCT_FIELD> [nodes attrs: Attribute, node name: Name, node ty: Type]);
impl_ast_node!(Block       <BLOCK>        [nodes stmts: Stmt]);
impl_ast_node!(FnParam     <FN_PARAM>     [node name: Name, node ty: Type]);
impl_ast_node!(ParamList   <PARAM_LIST>   [nodes parameters: FnParam]);
impl_ast_node!(ClosureParamList   <CLOSURE_PARAM_LIST>   [nodes parameters: Type]);

impl_ast_node!(FnDef<FN_DEF>
               [nodes attrs: Attribute,
                node visibility: Visibility,
                node extern_: Linkage,
                node return_type: Type,
                node param_list: ParamList,
                node block: Block,
                node name: Name]);
impl_ast_node!(Name          <NAME>        [token ident: Ident]);
impl_ast_node!(Condition     <CONDITION>   [node expr: Expr]);
impl_ast_node!(ArgList       <ARG_LIST>    [nodes arguments: Expr]);
impl_ast_node!(ExprStmt      <EXPR_STMT>   [node expr: Expr]);
impl_ast_node!(ReturnStmt    <RETURN_STMT> [node expr: Expr]);
impl_ast_node!(BlockStmt     <BLOCK_STMT>  [node block: Block]);
impl_ast_node!(BreakStmt     <BREAK_STMT>  []);
impl_ast_node!(ContinueStmt  <CONTINUE_STMT> []);
impl_ast_node!(DiscardStmt   <DISCARD_STMT> []);
impl_ast_node!(IfStmt        <IF_STMT>     [node condition: Condition, node stmt: Stmt, node else_branch: ElseBranch]);
impl_ast_node!(WhileStmt     <WHILE_STMT>  [node condition: Condition, node stmt: Stmt]);
impl_ast_node!(LoopExpr      <LOOP_EXPR>   [node expr: Expr]); // actually an expression statement
impl_ast_node!(ForStmt       <FOR_STMT>    [node initializer: ForInit, node condition: Condition, node loop_expr: LoopExpr, node body: Stmt]); // FIXME: this can be confusing: FOR_INIT, CONDITION, and LOOP_EXPR are always present, but the expression nodes inside them are optional.
impl_ast_node!(ForInit       <FOR_INIT>    [node stmt: Stmt]); // STMT is optional
impl_ast_node!(ElseBranch    <ELSE_BRANCH> [token else_: Else, node stmt: Stmt]);
impl_ast_node!(BinExpr       <BIN_EXPR>    []);
impl_ast_node!(TernaryExpr   <TERNARY_EXPR> []);
impl_ast_node!(IndexExpr     <INDEX_EXPR>  []); // FIXME the name can be confusing: this is the whole "indexing expression", i.e. `array[index]` and not just `index`
impl_ast_node!(ParenExpr     <PAREN_EXPR>  [node expr: Expr]);
impl_ast_node!(CallExpr      <CALL_EXPR>   [node callee: Expr, node arg_list: ArgList]);
impl_ast_node!(PrefixExpr    <PREFIX_EXPR> []);
impl_ast_node!(PostfixExpr   <POSTFIX_EXPR> []);
impl_ast_node!(FieldExpr     <FIELD_EXPR>  [node expr: Expr, node field: Name]);
impl_ast_node!(LitExpr       <LIT_EXPR>    []);
impl_ast_node!(PathExpr      <PATH_EXPR>   [node name: Name]);
impl_ast_node!(TupleExpr     <TUPLE_EXPR>  [nodes fields: Expr]);
impl_ast_node!(ArrayExpr     <ARRAY_EXPR>  [nodes elements: Expr]);
impl_ast_node!(ConstructorExpr <CONSTRUCTOR>  [node ty: Type, node arg_list: ArgList]);
impl_ast_node!(Initializer   <INITIALIZER> [token eq_: Eq, node expr: Expr]);
impl_ast_node!(Qualifier     <QUALIFIER>   []);
impl_ast_node!(Visibility    <VISIBILITY>  []);
impl_ast_node!(Linkage        <LINKAGE>    []);
impl_ast_node!(Global        <GLOBAL>      [nodes attrs: Attribute, node name: Name, node visibility: Visibility, node extern_: Linkage, node qualifier: Qualifier, node ty: Type, node initializer: Initializer ]);
impl_ast_node!(LocalVariable <LOCAL_VARIABLE> [node name: Name, node ty: Type, node initializer: Initializer ]);

impl_ast_variant_node!(Type, [ TYPE_REF => TypeRef, TUPLE_TYPE => TupleType, ARRAY_TYPE => ArrayType, CLOSURE_TYPE => ClosureType ]);
impl_ast_variant_node!(Item, [ FN_DEF => FnDef, GLOBAL => Global, STRUCT_DEF => StructDef, IMPORT_DECL => ImportDecl ]);
impl_ast_variant_node!(Stmt, [
    EXPR_STMT => ExprStmt,
    RETURN_STMT => ReturnStmt,
    BLOCK_STMT => BlockStmt,
    WHILE_STMT => WhileStmt,
    BREAK_STMT => BreakStmt,
    CONTINUE_STMT => ContinueStmt,
    DISCARD_STMT => DiscardStmt,
    LOCAL_VARIABLE => LocalVariable,
    IF_STMT => IfStmt,
    FOR_STMT => ForStmt
]);
/*impl_ast_variant_node!(ForInitStmt, [
    EXPR_STMT => ExprStmt,
    LOCAL_VARIABLE => LocalVariable
]);*/
impl_ast_variant_node!(Expr, [
    BIN_EXPR => BinExpr,
    PREFIX_EXPR => PrefixExpr,
    POSTFIX_EXPR => PostfixExpr,
    CALL_EXPR => CallExpr,
    INDEX_EXPR => IndexExpr,
    PAREN_EXPR => ParenExpr,
    LIT_EXPR => LitExpr,
    PATH_EXPR => PathExpr,
    TUPLE_EXPR => TupleExpr,
    ARRAY_EXPR => ArrayExpr,
    FIELD_EXPR => FieldExpr,
    TERNARY_EXPR => TernaryExpr,
    CONSTRUCTOR => ConstructorExpr
]);

//--------------------------------------------------------------------------------------------------
impl Ident {
    pub fn text(&self) -> &str {
        self.syntax.text()
    }
}

impl Name {
    pub fn text(&self) -> String {
        // IDENT is never None because if parse_name fails, no NAME node is created.
        self.ident().unwrap().text().to_string()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum Radix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
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

    pub fn value_i32(&self) -> Result<i32, ParseIntError> {
        let (_, text, _) = self.split_into_parts();
        i32::from_str_radix(&text.replace('_', ""), self.radix() as u32)
    }

    pub fn value_i64(&self) -> Result<i64, ParseIntError> {
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
    pub fn value_f32(&self) -> Result<f32, ParseFloatError> {
        // TODO hex floats
        self.text().parse::<f32>()
    }

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

impl PrefixExpr {
    pub fn op_details(&self) -> Option<(SyntaxToken, UnaryOp)> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|c| {
                let op = match c.kind() {
                    T![-] => UnaryOp::Neg,
                    T![!] => UnaryOp::Not,
                    T![++] => UnaryOp::PrefixInc,
                    T![--] => UnaryOp::PrefixDec,
                    _ => return None,
                };
                Some((c, op))
            })
    }

    pub fn expr(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(0)
    }
}

impl PostfixExpr {
    pub fn op_details(&self) -> Option<(SyntaxToken, UnaryOp)> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|c| {
                let op = match c.kind() {
                    T![++] => UnaryOp::PostfixInc,
                    T![--] => UnaryOp::PostfixDec,
                    _ => return None,
                };
                Some((c, op))
            })
    }

    pub fn expr(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(0)
    }
}

impl TernaryExpr {
    pub fn condition(&self) -> Option<Condition> {
        self.syntax.children().filter_map(Condition::cast).nth(0)
    }

    pub fn true_alt(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(0)
    }

    pub fn false_alt(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(1)
    }
}

impl BinExpr {
    pub fn lhs(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(0)
    }

    pub fn rhs(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(1)
    }

    pub fn op_details(&self) -> Option<(SyntaxToken, BinaryOp)> {
        self.syntax()
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find_map(|c| {
                let bin_op = match c.kind() {
                    T![||] => BinaryOp::LogicOp(LogicOp::Or),
                    T![&&] => BinaryOp::LogicOp(LogicOp::And),

                    T![==] => BinaryOp::CmpOp(CmpOp::Eq),
                    T![!=] => BinaryOp::CmpOp(CmpOp::Ne),
                    T![<=] => BinaryOp::CmpOp(CmpOp::Le),
                    T![>=] => BinaryOp::CmpOp(CmpOp::Ge),
                    T![<] => BinaryOp::CmpOp(CmpOp::Lt),
                    T![>] => BinaryOp::CmpOp(CmpOp::Gt),

                    T![+] => BinaryOp::ArithOp(ArithOp::Add),
                    T![*] => BinaryOp::ArithOp(ArithOp::Mul),
                    T![-] => BinaryOp::ArithOp(ArithOp::Sub),
                    T![/] => BinaryOp::ArithOp(ArithOp::Div),
                    T![%] => BinaryOp::ArithOp(ArithOp::Rem),
                    T![<<] => BinaryOp::ArithOp(ArithOp::Shl),
                    T![>>] => BinaryOp::ArithOp(ArithOp::Shr),
                    T![^] => BinaryOp::ArithOp(ArithOp::BitXor),
                    T![|] => BinaryOp::ArithOp(ArithOp::BitOr),
                    T![&] => BinaryOp::ArithOp(ArithOp::BitAnd),

                    T![=] => BinaryOp::Assignment(None),
                    T![+=] => BinaryOp::Assignment(Some(ArithOp::Add)),
                    T![*=] => BinaryOp::Assignment(Some(ArithOp::Mul)),
                    T![-=] => BinaryOp::Assignment(Some(ArithOp::Sub)),
                    T![/=] => BinaryOp::Assignment(Some(ArithOp::Div)),
                    T![%=] => BinaryOp::Assignment(Some(ArithOp::Rem)),
                    T![<<=] => BinaryOp::Assignment(Some(ArithOp::Shl)),
                    T![>>=] => BinaryOp::Assignment(Some(ArithOp::Shr)),
                    T![^=] => BinaryOp::Assignment(Some(ArithOp::BitXor)),
                    T![|=] => BinaryOp::Assignment(Some(ArithOp::BitOr)),
                    T![&=] => BinaryOp::Assignment(Some(ArithOp::BitAnd)),

                    _ => return None,
                };
                Some((c, bin_op))
            })
    }
}

impl IndexExpr {
    pub fn array(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(0)
    }

    pub fn index(&self) -> Option<Expr> {
        self.syntax.children().filter_map(Expr::cast).nth(1)
    }
}

fn first_token(node: &SyntaxNode) -> SyntaxToken {
    node.children_with_tokens()
        .find(|e| !e.kind().is_trivia())
        .and_then(|e| e.into_token())
        .unwrap()
}

impl Qualifier {
    pub fn token(&self) -> SyntaxToken {
        first_token(self.syntax())
    }

    pub fn qualifier(&self) -> Option<def::Qualifier> {
        match self.token().kind() {
            SyntaxKind::BUFFER_KW => Some(def::Qualifier::Buffer),
            SyntaxKind::SHARED_KW => Some(def::Qualifier::Shared),
            SyntaxKind::UNIFORM_KW => Some(def::Qualifier::Uniform),
            SyntaxKind::IN_KW => Some(def::Qualifier::In),
            SyntaxKind::OUT_KW => Some(def::Qualifier::Out),
            SyntaxKind::CONST_KW => Some(def::Qualifier::Const),
            _ => None,
        }
    }
}

impl Linkage {
    pub fn token(&self) -> SyntaxToken {
        first_token(self.syntax())
    }

    pub fn is_extern(&self) -> bool {
        match self.token().kind() {
            SyntaxKind::EXTERN_KW => true,
            _ => false,
        }
    }
}

impl Visibility {
    pub fn token(&self) -> SyntaxToken {
        first_token(self.syntax())
    }

    pub fn visibility(&self) -> Option<def::Visibility> {
        match self.token().kind() {
            SyntaxKind::PUBLIC_KW => Some(def::Visibility::Public),
            _ => None,
        }
    }
}

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::{
        db::{ModuleName, Session},
        syntax::ast::{Item, Module},
    };
    use codespan_reporting::{
        term,
        term::termcolor::{ColorChoice, StandardStream},
    };

    fn parse_module(text: &str) -> Module {
        let mut session = Session::new();
        let package = session.create_source_package(ModuleName::new("<input>"), "<input>", text);
        session.get_ast(package).module
    }

    #[test]
    fn basic_ast() {
        let m = parse_module(
            r#"
fn main() {
}
        "#,
        );

        let item = m.items().next().unwrap();
        match item {
            Item::FnDef(d) => {
                assert_eq!(d.name().unwrap().text(), "main");
            }
            _ => {}
        }
    }
}
