use logos::Logos;

/// The kind of syntax node.
///
/// We also define a `logos` lexer for the token nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[cfg_attr(any(test, feature = "serde"), derive(serde::Serialize, serde::Deserialize))]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    //
    // Tokens
    //
    #[token("(")]
    L_PAREN = 0,
    #[token(")")]
    R_PAREN,
    #[token("{")]
    L_CURLY,
    #[token("}")]
    R_CURLY,
    #[token(",")]
    COMMA,
    #[token("[")]
    L_BRACK,
    #[token("]")]
    R_BRACK,
    #[token(".")]
    DOT,
    #[token("+")]
    PLUS,
    #[token("-")]
    MINUS,
    #[token("!")]
    BANG,
    #[token("~")]
    TILDE,
    #[token("*")]
    STAR,
    #[token("/")]
    SLASH,
    #[token("%")]
    PERCENT,
    #[token("<<")]
    SHL,
    #[token(">>")]
    SHR,
    #[token("<")]
    L_ANGLE,
    #[token(">")]
    R_ANGLE,
    #[token("<=")]
    LTEQ,
    #[token(">=")]
    GTEQ,
    #[token("==")]
    EQ2,
    #[token("!=")]
    NEQ,
    #[token("&")]
    AMP,
    #[token("^")]
    CARET,
    #[token("|")]
    PIPE,
    #[token("&&")]
    AMP2,
    #[token("^^")]
    CARET2,
    #[token("||")]
    PIPE2,
    #[token("?")]
    QUESTION,
    #[token(":")]
    COLON,
    #[token(";")]
    SEMICOLON,
    #[token("=")]
    EQ,
    #[token("*=")]
    STAREQ,
    #[token("/=")]
    SLASHEQ,
    #[token("%=")]
    PERCENTEQ,
    #[token("+=")]
    PLUSEQ,
    #[token("-=")]
    MINUSEQ,
    #[token("<<=")]
    SHLEQ,
    #[token(">>=")]
    SHREQ,
    #[token("&=")]
    AMPEQ,
    #[token("^=")]
    CARETEQ,
    #[token("|=")]
    PIPEEQ,
    #[token("->")]
    THIN_ARROW,

    #[token("fn")]
    FN_KW,
    #[token("const")]
    CONST_KW,
    #[token("import")]
    IMPORT_KW,
    #[token("extern")]
    EXTERN_KW,
    #[token("if")]
    IF_KW,
    #[token("else")]
    ELSE_KW,
    #[token("return")]
    RETURN_KW,
    #[token("let")]
    LET_KW,
    #[token("var")]
    VAR_KW,
    #[token("break")]
    BREAK_KW,
    #[token("continue")]
    CONTINUE_KW,
    #[token("while")]
    WHILE_KW,
    #[token("for")]
    FOR_KW,
    #[token("true")]
    TRUE_KW,
    #[token("false")]
    FALSE_KW,
    #[token("discard")]
    DISCARD_KW,
    #[token("in")]
    IN_KW,
    #[token("out")]
    OUT_KW,
    #[token("uniform")]
    UNIFORM_KW,
    #[token("struct")]
    STRUCT_KW,
    #[token("as")]
    AS_KW,

    #[regex(r#"[a-zA-Z_][a-zA-Z0-9_]*"#)]
    IDENT,

    #[regex(r"0b[0-1_]*[0-1][0-1_]*")]
    #[regex(r"0o[0-7_]*[0-7][0-7_]*")]
    #[regex(r"[0-9][0-9_]*")]
    #[regex(r"0[xX][0-9A-Fa-f_]*[0-9A-Fa-f][0-9A-Fa-f_]*")]
    INT_NUMBER,
    #[regex(r#""([^\\"]*)""#)]
    STRING,
    #[regex("[0-9][0-9_]*[.]")]
    #[regex("[0-9][0-9_]*(?:[eE][+-]?[0-9_]*[0-9][0-9_]*)")]
    #[regex("[0-9][0-9_]*[.][0-9][0-9_]*(?:[eE][+-]?[0-9_]*[0-9][0-9_]*)?")]
    //#[regex("[+-]?[0-9][0-9_]*([.][0-9][0-9_]*)?(?:[eE][+-]?[0-9_]*[0-9][0-9_]*)?(f32|f64)")]
    FLOAT_NUMBER,

    #[regex("//.*")]
    LINE_COMMENT,
    #[regex(r"/\*([^*]|\*[^/])+\*/")]
    BLOCK_COMMENT,

    #[error]
    LEXER_ERROR,

    //
    // Nodes
    //
    IMPORT_DECL,
    IMPORT_PARAM_LIST,
    IMPORT_ALIAS,
    QUALIFIER,
    EXTERN,
    FN_DEF,
    FN_DECL,
    STRUCT_DEF,
    STRUCT_FIELD,
    GLOBAL,
    INITIALIZER,
    LOCAL_VARIABLE,
    PARAM_LIST,
    ARG_LIST,
    FN_PARAM,
    BLOCK,
    LIT_EXPR,
    CONST_EXPR,
    BIN_EXPR,
    PREFIX_EXPR,
    INDEX_EXPR,
    INDEX,
    PAREN_EXPR,
    TUPLE_EXPR,
    ARRAY_EXPR,
    PATH_EXPR,
    FIELD_EXPR,
    CALL_EXPR,
    TYPE_REF,
    TUPLE_TYPE,
    ARRAY_TYPE,
    CLOSURE_TYPE,
    CLOSURE_PARAM_LIST,
    RET_TYPE,
    STMT_LIST,
    EXPR_STMT,
    DISCARD_STMT,
    BREAK_STMT,
    CONTINUE_STMT,
    WHILE_STMT,
    FOR_STMT,
    IF_STMT,
    CONDITION,
    ELSE_BRANCH,
    RETURN_STMT,
    #[regex("[ \t\r\n]*")]
    WHITESPACE, // whitespaces is explicit
    ERROR, // as well as errors
    MODULE,
    ROOT,
}

impl SyntaxKind {
    pub fn is_literal(self) -> bool {
        matches!(
            self,
            SyntaxKind::INT_NUMBER | SyntaxKind::FLOAT_NUMBER | SyntaxKind::STRING
        )
    }

    pub fn is_trivia(&self) -> bool {
        matches!(
            self,
            SyntaxKind::WHITESPACE | SyntaxKind::LINE_COMMENT | SyntaxKind::BLOCK_COMMENT
        )
    }
}

#[macro_export]
macro_rules ! T {
    [;] => { $ crate :: syntax :: SyntaxKind :: SEMICOLON } ;
    [,] => { $ crate :: syntax :: SyntaxKind :: COMMA } ;
    ['('] => { $ crate :: syntax :: SyntaxKind :: L_PAREN } ;
    [')'] => { $ crate :: syntax :: SyntaxKind :: R_PAREN } ;
    ['{'] => { $ crate :: syntax :: SyntaxKind :: L_CURLY } ;
    ['}'] => { $ crate :: syntax :: SyntaxKind :: R_CURLY } ;
    ['['] => { $ crate :: syntax :: SyntaxKind :: L_BRACK } ;
    [']'] => { $ crate :: syntax :: SyntaxKind :: R_BRACK } ;
    [<] => { $ crate :: syntax :: SyntaxKind :: L_ANGLE } ;
    [>] => { $ crate :: syntax :: SyntaxKind :: R_ANGLE } ;
    [@] => { $ crate :: syntax :: SyntaxKind :: AT } ;
    [#] => { $ crate :: syntax :: SyntaxKind :: POUND } ;
    [~] => { $ crate :: syntax :: SyntaxKind :: TILDE } ;
    [?] => { $ crate :: syntax :: SyntaxKind :: QUESTION } ;
    //[$] => { $ crate :: syntax :: SyntaxKind :: DOLLAR } ;
    [&] => { $ crate :: syntax :: SyntaxKind :: AMP } ;
    [|] => { $ crate :: syntax :: SyntaxKind :: PIPE } ;
    [+] => { $ crate :: syntax :: SyntaxKind :: PLUS } ;
    [*] => { $ crate :: syntax :: SyntaxKind :: STAR } ;
    [/] => { $ crate :: syntax :: SyntaxKind :: SLASH } ;
    [^] => { $ crate :: syntax :: SyntaxKind :: CARET } ;
    [%] => { $ crate :: syntax :: SyntaxKind :: PERCENT } ;
    [_] => { $ crate :: syntax :: SyntaxKind :: UNDERSCORE } ;
    [.] => { $ crate :: syntax :: SyntaxKind :: DOT } ;
    //[..] => { $ crate :: syntax :: SyntaxKind :: DOT2 } ;
    //[...] => { $ crate :: syntax :: SyntaxKind :: DOT3 } ;
    //[..=] => { $ crate :: syntax :: SyntaxKind :: DOT2EQ } ;
    [:] => { $ crate :: syntax :: SyntaxKind :: COLON } ;
    //[::] => { $ crate :: syntax :: SyntaxKind :: COLON2 } ;
    [=] => { $ crate :: syntax :: SyntaxKind :: EQ } ;
    [==] => { $ crate :: syntax :: SyntaxKind :: EQ2 } ;
    //[=>] => { $ crate :: syntax :: SyntaxKind :: FAT_ARROW } ;
    [!] => { $ crate :: syntax :: SyntaxKind :: BANG } ;
    [!=] => { $ crate :: syntax :: SyntaxKind :: NEQ } ;
    [-] => { $ crate :: syntax :: SyntaxKind :: MINUS } ;
    [->] => { $ crate :: syntax :: SyntaxKind :: THIN_ARROW } ;
    [<=] => { $ crate :: syntax :: SyntaxKind :: LTEQ } ;
    [>=] => { $ crate :: syntax :: SyntaxKind :: GTEQ } ;
    [+=] => { $ crate :: syntax :: SyntaxKind :: PLUSEQ } ;
    [-=] => { $ crate :: syntax :: SyntaxKind :: MINUSEQ } ;
    [|=] => { $ crate :: syntax :: SyntaxKind :: PIPEEQ } ;
    [&=] => { $ crate :: syntax :: SyntaxKind :: AMPEQ } ;
    [^=] => { $ crate :: syntax :: SyntaxKind :: CARETEQ } ;
    [/=] => { $ crate :: syntax :: SyntaxKind :: SLASHEQ } ;
    [*=] => { $ crate :: syntax :: SyntaxKind :: STAREQ } ;
    [%=] => { $ crate :: syntax :: SyntaxKind :: PERCENTEQ } ;
    [&&] => { $ crate :: syntax :: SyntaxKind :: AMP2 } ;
    [||] => { $ crate :: syntax :: SyntaxKind :: PIPE2 } ;
    [<<] => { $ crate :: syntax :: SyntaxKind :: SHL } ;
    [>>] => { $ crate :: syntax :: SyntaxKind :: SHR } ;
    [<<=] => { $ crate :: syntax :: SyntaxKind :: SHLEQ } ;
    [>>=] => { $ crate :: syntax :: SyntaxKind :: SHREQ } ;
    [as] => { $ crate :: syntax :: SyntaxKind :: AS_KW } ;
    //[async] => { $ crate :: syntax :: SyntaxKind :: ASYNC_KW } ;
    //[await] => { $ crate :: syntax :: SyntaxKind :: AWAIT_KW } ;
    //[box] => { $ crate :: syntax :: SyntaxKind :: BOX_KW } ;
    [break] => { $ crate :: syntax :: SyntaxKind :: BREAK_KW } ;
    [const] => { $ crate :: syntax :: SyntaxKind :: CONST_KW } ;
    [continue] => { $ crate :: syntax :: SyntaxKind :: CONTINUE_KW } ;
    //[crate] => { $ crate :: syntax :: SyntaxKind :: CRATE_KW } ;
    //[dyn] => { $ crate :: syntax :: SyntaxKind :: DYN_KW } ;
    [else] => { $ crate :: syntax :: SyntaxKind :: ELSE_KW } ;
    //[enum] => { $ crate :: syntax :: SyntaxKind :: ENUM_KW } ;
    //[extern] => { $ crate :: syntax :: SyntaxKind :: EXTERN_KW } ;
    [false] => { $ crate :: syntax :: SyntaxKind :: FALSE_KW } ;
    [fn] => { $ crate :: syntax :: SyntaxKind :: FN_KW } ;
    [for] => { $ crate :: syntax :: SyntaxKind :: FOR_KW } ;
    [if] => { $ crate :: syntax :: SyntaxKind :: IF_KW } ;
    //[impl] => { $ crate :: syntax :: SyntaxKind :: IMPL_KW } ;
    //[in] => { $ crate :: syntax :: SyntaxKind :: IN_KW } ;
    [let] => { $ crate :: syntax :: SyntaxKind :: LET_KW } ;
    [loop] => { $ crate :: syntax :: SyntaxKind :: LOOP_KW } ;
    //[macro] => { $ crate :: syntax :: SyntaxKind :: MACRO_KW } ;
    //[match] => { $ crate :: syntax :: SyntaxKind :: MATCH_KW } ;
    //[mod] => { $ crate :: syntax :: SyntaxKind :: MOD_KW } ;
    //[move] => { $ crate :: syntax :: SyntaxKind :: MOVE_KW } ;
    //[mut] => { $ crate :: syntax :: SyntaxKind :: MUT_KW } ;
    //[pub] => { $ crate :: syntax :: SyntaxKind :: PUB_KW } ;
    //[ref] => { $ crate :: syntax :: SyntaxKind :: REF_KW } ;
    [return] => { $ crate :: syntax :: SyntaxKind :: RETURN_KW } ;
    //[self] => { $ crate :: syntax :: SyntaxKind :: SELF_KW } ;
    //[Self] => { $ crate :: syntax :: SyntaxKind :: SELF_TYPE_KW } ;
    //[static] => { $ crate :: syntax :: SyntaxKind :: STATIC_KW } ;
    [struct] => { $ crate :: syntax :: SyntaxKind :: STRUCT_KW } ;
    //[super] => { $ crate :: syntax :: SyntaxKind :: SUPER_KW } ;
    //[trait] => { $ crate :: syntax :: SyntaxKind :: TRAIT_KW } ;
    [true] => { $ crate :: syntax :: SyntaxKind :: TRUE_KW } ;
    //[try] => { $ crate :: syntax :: SyntaxKind :: TRY_KW } ;
    //[type] => { $ crate :: syntax :: SyntaxKind :: TYPE_KW } ;
    //[unsafe] => { $ crate :: syntax :: SyntaxKind :: UNSAFE_KW } ;
    //[use] => { $ crate :: syntax :: SyntaxKind :: USE_KW } ;
    //[where] => { $ crate :: syntax :: SyntaxKind :: WHERE_KW } ;
    [while] => { $ crate :: syntax :: SyntaxKind :: WHILE_KW } ;
    //[yield] => { $ crate :: syntax :: SyntaxKind :: YIELD_KW } ;
    //[auto] => { $ crate :: syntax :: SyntaxKind :: AUTO_KW } ;
    //[default] => { $ crate :: syntax :: SyntaxKind :: DEFAULT_KW } ;
    //[existential] => { $ crate :: syntax :: SyntaxKind :: EXISTENTIAL_KW } ;
    //[union] => { $ crate :: syntax :: SyntaxKind :: UNION_KW } ;
    //[raw] => { $ crate :: syntax :: SyntaxKind :: RAW_KW } ;
    //[macro_rules] => { $ crate :: syntax :: SyntaxKind :: MACRO_RULES_KW } ;
    //[lifetime_ident] => { $ crate :: syntax :: SyntaxKind :: LIFETIME_IDENT } ;
    //[ident] => { $ crate :: syntax :: SyntaxKind :: IDENT } ;
    //[shebang] => { $ crate :: syntax :: SyntaxKind :: SHEBANG } ;
}

pub use T;

/// Some boilerplate is needed, as rowan settled on using its own
/// `struct SyntaxKind(u16)` internally, instead of accepting the
/// user's `enum SyntaxKind` as a type parameter.
///
/// First, to easily pass the enum variants into rowan via `.into()`:
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

pub(crate) type Lexer<'source> = logos::SpannedIter<'source, SyntaxKind>;

impl SyntaxKind {
    /// Creates a lexer that produces (SyntaxKind, span) pairs from an input text.
    pub(crate) fn create_lexer(text: &str) -> Lexer {
        logos::Lexer::new(text).spanned()
    }
}

#[cfg(test)]
mod tests {
    use super::SyntaxKind;
    use std::ops::Range;

    #[cfg(test)]
    fn to_tokens(text: &str) -> Vec<(SyntaxKind, Range<usize>)> {
        let lexer = SyntaxKind::create_lexer(text);
        let tokens: Vec<(SyntaxKind, Range<usize>)> = lexer.collect();
        tokens
    }

    #[test]
    fn test_lexer_basic() {
        insta::assert_yaml_snapshot!(to_tokens(r#"100 0.1234 ident _ident _0 "string" "#));
    }
}
