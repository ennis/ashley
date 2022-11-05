use logos::Logos;

/// The kind of syntax node.
///
/// We also define a `logos` lexer for the token nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)]
#[cfg_attr(
    any(test, feature = "serde"),
    derive(serde::Serialize, serde::Deserialize)
)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub(crate) enum SyntaxKind {
    //
    // Tokens
    //
    #[token("(")]
    L_PAREN = 0,
    #[token(")")]
    R_PAREN,
    #[token("{")]
    L_BRACE,
    #[token("}")]
    R_BRACE,
    #[token(",")]
    COMMA,
    #[token("[")]
    L_BRACKET,
    #[token("]")]
    R_BRACKET,
    #[token(".")]
    DOT,
    #[token("++")]
    INC,
    #[token("--")]
    DEC,
    #[token("+")]
    PLUS,
    #[token("-")]
    DASH,
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
    L_SHIFT,
    #[token(">>")]
    R_SHIFT,
    #[token("<")]
    L_ANGLE,
    #[token(">")]
    R_ANGLE,
    #[token("<=")]
    L_EQUAL,
    #[token(">=")]
    R_EQUAL,
    #[token("==")]
    D_EQUAL,
    #[token("!=")]
    BANG_EQUAL,
    #[token("&")]
    AMPERSAND,
    #[token("^")]
    CARET,
    #[token("|")]
    BAR,
    #[token("&&")]
    AND,
    #[token("^^")]
    XOR,
    #[token("||")]
    OR,
    #[token("?")]
    QUESTION,
    #[token(":")]
    COLON,
    #[token(";")]
    SEMICOLON,
    #[token("=")]
    EQUAL,
    #[token("*=")]
    STAR_EQUAL,
    #[token("/=")]
    SLASH_EQUAL,
    #[token("%=")]
    PERCENT_EQUAL,
    #[token("+=")]
    PLUS_EQUAL,
    #[token("-=")]
    DASH_EQUAL,
    #[token("<<=")]
    L_SHIFT_EQUAL,
    #[token(">>=")]
    R_SHIFT_EQUAL,
    #[token("&=")]
    AMPERSAND_EQUAL,
    #[token("^=")]
    CARET_EQUAL,
    #[token("|=")]
    BAR_EQUAL,

    #[token("fn")]
    FN_KW,
    #[token("const")]
    CONST_KW,
    #[token("if")]
    IF_KW,
    #[token("else")]
    ELSE_KW,
    #[regex(r#"[a-zA-Z_][a-zA-Z0-9_]*"#)]
    IDENT,

    #[token("true")]
    #[token("false")]
    BOOL_LIT,
    #[regex(r"0b[0-1_]*[0-1][0-1_]*")]
    #[regex(r"0o[0-7_]*[0-7][0-7_]*")]
    #[regex(r"[0-9][0-9_]*")]
    #[regex(r"0[xX][0-9A-Fa-f_]*[0-9A-Fa-f][0-9A-Fa-f_]*")]
    INT_LIT,
    #[regex(r#""([^\\"]*)""#)]
    STRING,
    #[regex("[0-9][0-9_]*[.]")]
    #[regex("[0-9][0-9_]*(?:[eE][+-]?[0-9_]*[0-9][0-9_]*)")]
    #[regex("[0-9][0-9_]*[.][0-9][0-9_]*(?:[eE][+-]?[0-9_]*[0-9][0-9_]*)?")]
    //#[regex("[+-]?[0-9][0-9_]*([.][0-9][0-9_]*)?(?:[eE][+-]?[0-9_]*[0-9][0-9_]*)?(f32|f64)")]
    FLOAT_LIT,

    #[regex("//.*")]
    LINE_COMMENT,
    #[regex(r"/\*([^*]|\*[^/])+\*/")]
    BLOCK_COMMENT,

    #[error]
    LEXER_ERROR,

    //
    // Nodes
    //
    FUNCTION,
    ARG_LIST,
    FN_ARG,
    BLOCK,
    TYPE_REF,
    #[regex("[ \t\r\n]*")]
    WHITESPACE, // whitespaces is explicit
    ERROR, // as well as errors
    ROOT,
}

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
    use crate::syntax_kind::SyntaxKind;
    use std::ops::Range;

    #[cfg(test)]
    fn to_tokens(text: &str) -> Vec<(SyntaxKind, Range<usize>)> {
        let mut lexer = SyntaxKind::create_lexer(text);
        let tokens: Vec<(SyntaxKind, Range<usize>)> = lexer.collect();
        tokens
    }

    #[test]
    fn test_lexer_basic() {
        insta::assert_yaml_snapshot!(to_tokens(r#"100 0.1234 ident _ident _0 "string" "#));
    }
}
