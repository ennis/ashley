mod ast;
mod operators;
mod session;
mod syntax_kind;

use crate::diagnostic::Diagnostics;
use crate::T;
pub use ast::{AstNode, FnDef, Item, Module};
pub use operators::{ArithOp, BinaryOp, CmpOp, LogicOp, UnaryOp};
use rowan::{GreenNode, GreenNodeBuilder};
pub(crate) use session::FileId;
pub(crate) use session::SourceFiles;
pub use session::{Session, SourceId};
use std::ops::Range;
use syntax_kind::Lexer;
pub(crate) use syntax_kind::SyntaxKind;
use syntax_kind::SyntaxKind::*;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    pub file_id: FileId,
    pub start: usize,
    pub end: usize,
}

impl Default for Span {
    fn default() -> Self {
        Span {
            file_id: usize::MAX,
            start: 0,
            end: 0
        }
    }
}

//--------------------------------------------------------------------------------------------------

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
pub type SyntaxToken = rowan::SyntaxToken<Lang>;

/// Now, let's write a parser.
/// Note that `parse` does not return a `Result`:
/// by design, syntax tree can be built even for
/// completely invalid source code.
pub(crate) fn parse(text: &str, file_id: FileId, diag: Diagnostics) -> SyntaxNode {
    let mut lex: Lexer = SyntaxKind::create_lexer(text);
    let b = GreenNodeBuilder::new();
    let current = lex.next();
    let mut parser = Parser {
        file_id,
        text,
        current,
        lex,
        b,
        diag,
    };
    parser.parse()
}

struct Parser<'a, 'b> {
    text: &'a str,
    file_id: FileId,
    /// Current token & span
    current: Option<(SyntaxKind, Range<usize>)>,
    /// lexer for the input text
    lex: Lexer<'a>,
    /// the in-progress tree.
    b: GreenNodeBuilder<'static>,
    /// The list of syntax errors we've accumulated
    /// so far. They are reported all at once when parsing is complete.
    diag: Diagnostics<'b>,
}

impl<'a, 'b> Parser<'a, 'b> {
    fn parse(mut self) -> SyntaxNode {
        self.start_node(MODULE);
        while self.current().is_some() {
            self.parse_items();
        }
        self.finish_node();
        let green_node = self.b.finish();
        SyntaxNode::new_root(green_node)
    }

    //
    // --- Utilities ---
    //

    fn bump(&mut self) {
        let (kind, span) = self.current.clone().unwrap();
        self.b.token(kind.into(), &self.text[span]);
        self.current = self.lex.next();
    }

    /// Peek at the first unprocessed token
    fn current(&self) -> Option<SyntaxKind> {
        self.current.clone().map(|(kind, _)| kind)
    }

    fn skip_ws(&mut self) {
        while matches!(
            self.current(),
            Some(WHITESPACE | BLOCK_COMMENT | LINE_COMMENT)
        ) {
            self.bump()
        }
    }

    /// Returns the `Span` of the current token.
    fn span(&self) -> Option<Span> {
        if let Some((_, span)) = self.current.clone() {
            Some(Span {
                file_id: self.file_id,
                start: span.start,
                end: span.end,
            })
        } else {
            None
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.b.start_node(kind.into());
    }

    fn finish_node(&mut self) {
        self.b.finish_node();
    }

    fn expect_ident(&mut self, ident_kind: &str) -> bool {
        if self.current() != Some(IDENT) {
            let span = self.span();
            self.diag
                .error(format!("expected {ident_kind}"))
                .primary_label(span, "expected IDENT")
                .emit();
            false
        } else {
            self.bump();
            true
        }
    }

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.current() != Some(kind) {
            let span = self.span();
            let msg = format!("expected {kind:?}");
            self.diag.error(msg.clone()).primary_label(span, msg).emit();
            false
        } else {
            self.bump();
            true
        }
    }

    fn expect_any(&mut self, kinds: &[SyntaxKind]) -> bool {
        if let Some(kind) = self.current() {
            if kinds.contains(&kind) {
                self.bump();
                return true;
            }
        }

        let span = self.span();
        let msg = format!("expected one of {kinds:?}");
        self.diag.error(msg.clone()).primary_label(span, msg).emit();
        false
    }

    //
    // --- Nodes ---
    //

    fn parse_items(&mut self) {
        loop {
            self.skip_ws();
            match self.current() {
                Some(FN_KW) => {
                    self.parse_fn();
                }
                Some(IN_KW | OUT_KW | CONST_KW | UNIFORM_KW) => {
                    self.parse_global_variable();
                }
                None => break,
                _ => {
                    // unexpected token
                    let span = self.span();
                    self.diag
                        .error("unexpected token")
                        .primary_label(span, "")
                        .emit();
                    self.bump();
                }
            }
        }
    }

    fn parse_fn(&mut self) {
        self.start_node(FN_DEF);
        self.expect(FN_KW);
        self.skip_ws();
        self.expect_ident("function name");
        self.skip_ws();
        self.parse_fn_arg_list();
        self.skip_ws();
        if self.current() == Some(THIN_ARROW) {
            self.start_node(RET_TYPE);
            self.bump();
            self.skip_ws();
            self.parse_type();
            self.finish_node();
            self.skip_ws();
        }
        self.parse_block();
        self.finish_node();
    }

    fn parse_fn_arg_list(&mut self) {
        self.start_node(PARAM_LIST);
        if !self.expect(L_PAREN) {
            self.finish_node();
            return;
        }

        self.skip_ws();

        loop {
            match self.current() {
                Some(R_PAREN) => {
                    self.bump();
                    break;
                }
                None => {
                    let cur_span = self.span();
                    self.diag
                        .error("unexpected end of file")
                        .primary_label(cur_span, "")
                        .emit();
                    break;
                }
                _ => {}
            }
            self.parse_fn_arg();
            self.skip_ws();
            if self.current() == Some(COMMA) {
                self.bump();
                self.skip_ws();
                continue;
            }
            self.expect(R_PAREN);
            self.skip_ws();
            break;
        }

        self.finish_node();
    }

    fn parse_block(&mut self) {
        // TODO
        self.start_node(BLOCK);
        self.expect(L_CURLY);
        self.skip_ws();
        self.parse_stmt_list();
        self.skip_ws();
        self.expect(R_CURLY);
        self.finish_node();
    }

    fn parse_fn_arg(&mut self) {
        self.start_node(FN_ARG);
        // <ident> : <type>
        self.expect_ident("argument name");
        self.skip_ws();
        self.expect(COLON);
        self.skip_ws();
        self.parse_type();
        self.finish_node();
    }

    fn parse_type(&mut self) {
        // for now, only ident
        match self.current() {
            Some(T!['(']) => {
                self.parse_tuple_type();
            }
            Some(IDENT) => {
                self.start_node(TYPE_REF);
                self.bump();
                self.finish_node();
            }
            None => {
                let cur_span = self.span();
                self.diag
                    .error("unexpected end of file")
                    .primary_label(cur_span, "")
                    .emit();
            }
            _ => {
                let cur_span = self.span();
                self.diag
                    .error("syntax error (TODO parse_type)")
                    .primary_label(cur_span, "")
                    .emit();
            }
        }
    }

    fn parse_separated_list(&mut self, sep: SyntaxKind, end: SyntaxKind, allow_trailing: bool, tuple_rule: bool, mut parse_item: impl FnMut(&mut Self)) {
        let mut trailing_sep = false;
        let mut last_sep_span = None;
        let mut item_count = 0;
        loop {
            self.skip_ws();
            match self.current() {
                Some(x) if x == end => {
                    if trailing_sep && !allow_trailing {
                        self.diag
                            .error("trailing separator not allowed here")
                            .primary_label(last_sep_span, "")
                            .emit()
                    }
                    break;
                }
                None => break,
                _ => {}
            }
            parse_item(self);
            item_count += 1;
            self.skip_ws();
            match self.current() {
                Some(x) if x == end => {
                    if item_count == 1 && tuple_rule {
                        // should end with a comma
                        self.diag
                            .error("single-element tuple types should have a trailing `,`")
                            .primary_label(last_sep_span, "")
                            .emit()
                    }
                    break;
                }
                Some(x) if x == sep => {
                    last_sep_span = self.span();
                    self.bump();
                    trailing_sep = true;
                }
                _ => {
                    let span = self.span();
                    self.diag
                        .error("syntax error (TODO)")
                        .primary_label(span, "")
                        .emit();
                    trailing_sep = false;
                }
            }
        }
    }

    fn parse_tuple_type(&mut self) {
        self.start_node(TUPLE_TYPE);
        self.expect(T!['(']);
        self.parse_separated_list(T![,], T![')'], true, true, Self::parse_type);
        self.expect(T![')']);
        self.finish_node();
    }

    /// Parses an operand to a binary expression.
    fn parse_atom(&mut self) {
        match self.current() {
            Some(INT_NUMBER) => {
                self.start_node(LIT_EXPR);
                self.bump();
                self.finish_node();
            }
            Some(FLOAT_NUMBER) => {
                self.start_node(LIT_EXPR);
                self.bump();
                self.finish_node();
            }
            Some(IDENT) => {
                self.start_node(PATH_EXPR);
                self.bump();
                self.finish_node();
            }
            Some(TRUE_KW) => {
                self.start_node(LIT_EXPR);
                self.bump();
                self.finish_node();
            }
            Some(FALSE_KW) => {
                self.start_node(LIT_EXPR);
                self.bump();
                self.finish_node();
            }
            Some(T!['(']) => {
                let cp = self.b.checkpoint();
                self.bump();
                self.skip_ws();
                self.parse_expr();
                self.skip_ws();
                match self.current() {
                    Some(T![')']) => {
                        self.b.start_node_at(cp, PAREN_EXPR.into());
                        self.bump();
                        self.finish_node();
                    }
                    Some(T![,]) => {
                        // tuple
                        self.b.start_node_at(cp, TUPLE_EXPR.into());
                        self.bump();
                        self.skip_ws();
                        self.parse_separated_list(T![,], T![')'], true, false, Self::parse_expr);
                        self.expect(T![')']);
                        self.finish_node();
                    }
                    _ => {
                        let span = self.span();
                        self.diag
                            .error("syntax error")
                            .primary_label(span, "")
                            .emit();
                    }
                }
            }
            _ => {
                let span = self.span();
                self.diag
                    .error("syntax error")
                    .primary_label(span, "")
                    .emit();
                self.bump();
            }
        }
    }

    fn parse_stmt_list(&mut self) {
        loop {
            match self.current() {
                Some(R_CURLY) => break,
                None => break,
                _ => self.parse_stmt(),
            }
            self.skip_ws();
        }
    }

    fn parse_stmt(&mut self) {
        match self.current() {
            Some(RETURN_KW) => self.parse_return(),
            Some(IF_KW) => self.parse_if_stmt(),
            Some(BREAK_KW) => self.parse_break_stmt(),
            Some(CONTINUE_KW) => self.parse_continue_stmt(),
            Some(DISCARD_KW) => self.parse_discard_stmt(),
            Some(WHILE_KW) => self.parse_while_stmt(),
            Some(LET_KW | VAR_KW) => self.parse_local_variable_stmt(),
            _ => {
                self.parse_expr_stmt();
            }
        }
    }

    fn parse_local_variable_stmt(&mut self) {
        self.start_node(LOCAL_VARIABLE);
        self.expect_any(&[VAR_KW, LET_KW]);
        self.skip_ws();
        self.expect_ident("variable name");
        self.skip_ws();
        if self.current() == Some(T![:]) {
            self.bump();
            self.skip_ws();
            self.parse_type();
            self.skip_ws();
        }
        if self.current() == Some(T![=]) {
            self.start_node(INITIALIZER);
            self.bump();
            self.skip_ws();
            self.parse_expr();
            self.finish_node(); // INITIALIZER
            self.skip_ws();
        }
        self.expect(SEMICOLON);
        self.finish_node(); // LOCAL_VARIABLE
    }

    fn parse_break_stmt(&mut self) {
        self.start_node(BREAK_STMT);
        self.expect(BREAK_KW);
        self.skip_ws();
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_continue_stmt(&mut self) {
        self.start_node(CONTINUE_STMT);
        self.expect(CONTINUE_KW);
        self.skip_ws();
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_discard_stmt(&mut self) {
        self.start_node(DISCARD_STMT);
        self.expect(DISCARD_KW);
        self.skip_ws();
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_while_stmt(&mut self) {
        self.start_node(WHILE_STMT);
        self.expect(WHILE_KW);
        self.skip_ws();
        self.start_node(CONDITION);
        self.parse_expr();
        self.finish_node(); // CONDITION
        self.skip_ws();
        self.parse_block();
        self.finish_node(); // WHILE_STMT
    }

    fn parse_if_stmt(&mut self) {
        self.start_node(IF_STMT);
        self.expect(IF_KW);
        self.skip_ws();
        self.start_node(CONDITION);
        self.parse_expr();
        self.skip_ws();
        self.finish_node();
        self.parse_block();
        self.skip_ws();
        if self.current() == Some(ELSE_KW) {
            self.start_node(ELSE_BRANCH);
            self.bump();
            self.skip_ws();
            if self.current() == Some(IF_KW) {
                // else if
                self.parse_if_stmt();
            } else {
                self.parse_block();
            }
            self.finish_node(); // ELSE_BRANCH
        }
        self.finish_node(); // IF_STMT
    }

    /*fn parse_for_stmt(&mut self) {

    }*/

    fn parse_expr_stmt(&mut self) {
        self.start_node(EXPR_STMT);
        self.parse_expr();
        self.skip_ws();
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_return(&mut self) {
        self.start_node(RETURN_STMT);
        self.expect(RETURN_KW);
        self.skip_ws();
        self.parse_expr();
        self.skip_ws();
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_call_args(&mut self) {
        self.start_node(ARG_LIST);
        self.expect(T!['(']);

        loop {
            self.skip_ws();
            match self.current() {
                Some(T![')']) => {
                    self.bump();
                    break;
                }
                None => break,
                _ => {}
            }
            self.parse_expr();
            self.skip_ws();
            match self.current() {
                Some(T![')']) => {
                    self.bump();
                    break;
                }
                Some(T![,]) => {
                    // continue arg list
                    self.bump();
                }
                _ => {
                    let span = self.span();
                    self.diag
                        .error("syntax error (TODO)")
                        .primary_label(span, "")
                        .emit();
                }
            }
        }

        self.finish_node();
    }

    fn parse_global_variable(&mut self) {
        self.start_node(GLOBAL);
        self.expect_any(&[IN_KW, OUT_KW, CONST_KW, UNIFORM_KW]);
        self.skip_ws();
        self.expect_ident("variable name");
        self.skip_ws();
        self.expect(COLON);
        self.skip_ws();
        self.parse_type();
        self.skip_ws();
        if self.current() == Some(EQ) {
            self.start_node(INITIALIZER);
            self.bump();
            self.skip_ws();
            self.parse_expr();
            self.finish_node(); // INITIALIZER
        }
        self.expect(SEMICOLON);
        self.finish_node(); // GLOBAL
    }

    fn parse_expr(&mut self) {
        self.parse_expr_bp(0)
    }

    // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn parse_expr_bp(&mut self, min_bp: u8) {
        let cp = self.b.checkpoint();
        match self.current() {
            Some(op @ T![+] | op @ T![-] | op @ T![!]) => {
                self.start_node(PREFIX_EXPR);
                self.bump();
                self.skip_ws();
                let r_bp = prefix_binding_power(op);
                self.parse_expr_bp(r_bp);
                self.finish_node();
            }
            _ => {
                self.parse_atom();
            }
        }

        self.skip_ws();

        loop {
            let op = match self.current() {
                Some(
                    op @ T![+]
                    | op @ T![-]
                    | op @ T![*]
                    | op @ T![/]
                    | op @ T![=]
                    | op @ T![+=]
                    | op @ T![-=]
                    | op @ T![*=]
                    | op @ T![/=]
                    | op @ T![%=]
                    | op @ T![&=]
                    | op @ T![|=]
                    | op @ T![^=]
                    | op @ T![<<=]
                    | op @ T![>>=]
                    | op @ T![||]
                    | op @ T![&&]
                    | op @ T![==]
                    | op @ T![!=]
                    | op @ T![<=]
                    | op @ T![>=]
                    | op @ T![<]
                    | op @ T![>]
                    | op @ T![|]
                    | op @ T![^]
                    | op @ T![&]
                    | op @ T![<<]
                    | op @ T![>>]
                    | op @ T![%],
                ) => op,
                Some(T!['[']) => {
                    // the array index binds closer to everything else, no need to check binding power
                    self.b.start_node_at(cp, INDEX_EXPR.into());
                    self.bump();
                    self.skip_ws();
                    self.parse_expr();
                    self.expect(T![']']);
                    self.finish_node();
                    continue;
                }
                Some(T!['(']) => {
                    self.b.start_node_at(cp, CALL_EXPR.into());
                    self.parse_call_args();
                    self.finish_node();
                    continue;
                }
                Some(T![.]) => {
                    self.b.start_node_at(cp, FIELD_EXPR.into());
                    self.bump();
                    self.skip_ws();
                    self.expect_ident("field identifier");
                    self.finish_node();
                    continue;
                }
                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }

            self.b.start_node_at(cp, BIN_EXPR.into());

            self.bump();
            self.skip_ws();
            self.parse_expr_bp(r_bp);

            self.b.finish_node();
        }
    }
}

fn prefix_binding_power(op: SyntaxKind) -> u8 {
    match op {
        T![+] | T![-] | T![!] => 22,
        _ => panic!("bad op: {:?}", op),
    }
}

fn infix_binding_power(op: SyntaxKind) -> (u8, u8) {
    match op {
        T![=]
        | T![+=]
        | T![-=]
        | T![*=]
        | T![/=]
        | T![%=]
        | T![&=]
        | T![|=]
        | T![^=]
        | T![<<=]
        | T![>>=] => (2, 1),
        T![||] => (4, 5),
        T![&&] => (6, 7),
        T![==] | T![!=] | T![<=] | T![>=] | T![<] | T![>] => (8, 9),
        T![|] => (10, 11),
        T![^] => (12, 13),
        T![&] => (14, 15),
        T![<<] | T![>>] => (16, 17),
        T![+] | T![-] => (18, 19),
        T![*] | T![/] | T![%] => (20, 21),
        _ => panic!("bad op: {op:?}"),
    }
}

// 1. Create a session object, specifying the debug output
// 2. Register source code by calling session.register_source(file_id, contents) -> SourceId
// 3. Given a SourceId, get the syntax tree with session.syntax_tree(sourceId)
// 4.
// 2. Call session.parse(file id, file contents), returns a green node (and puts it in cache)
// 3. Call session.hir(file id

#[cfg(test)]
mod tests {
    use crate::syntax::{parse, Diagnostics, Lang, Session, SyntaxNode};
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
    use rowan::GreenNode;

    fn parse_source_text(text: &str) -> SyntaxNode {
        let mut sess = Session::new();
        let src_id = sess.register_source("<input>", text);
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        sess.parse(src_id, &mut writer)
    }

    fn expr(expr: &str) -> SyntaxNode {
        let src = format!("fn main() {{ {expr}; }}");
        parse_source_text(&src)
    }

    #[test]
    fn whitespace_tests() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
fn main(){}
    fn main2    (    )    {     }
fn main3(    arg:f32    ) { }
fn main4(  arg   :   f32) {}
"#
        ));
    }

    #[test]
    fn basic_parser_tests() {
        insta::assert_debug_snapshot!(
            "empty function",
            parse_source_text(
                r#"
fn main() {
}

fn test() -> i32 {
}

// tuple types
fn test2() -> (i32,i32) {
}
"#
            )
        );

        insta::assert_debug_snapshot!(
            "empty functions with arguments",
            parse_source_text(
                r#"
fn zero_arg() {}
fn one_arg(a : i32) {}
fn two_args(a : f32, b: f32) {}
"#
            )
        );
    }

    #[test]
    fn expr_parser_tests() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
fn expr_test() {
    0;
    a;
    0 + 1;
    a + 1;
    1 + 2 * 3;
    1 * 2 + 3 * 4;
    1 + 2 * 3 + 4;
    1 + a[i];
    a[i+1];
    a[i][j];
    -a[j];
    1 + -2 + 3;
    -x(1);
    -x[1];
    1 + 2 * 3 % 4 - 5 / 6;
    1 + 2 * 3;
    1 << 2 + 3;
    1 & 2 >> 3;
    1 ^ 2 & 3;
    1 | 2 ^ 3;
    1 == 2 | 3;
    1 && 2 == 3;
    1 || 2 && 2;
    ---!1 - --2 * 9;
    1 * (2 + 3) * 4;
    1 + 2 * (3 + 4);
    f(1 + 2 * (3 + 4))(4)[0];
    return 0;
}
"#
        ));

        insta::assert_debug_snapshot!(expr(
            "f(1, 10.0, f(10), test, 1 + 2, tests()[4], tests()[f(0)])"
        ));

        insta::assert_debug_snapshot!(expr(
            "f(1, 10.0, f(10), (test, 1 + 2), (test,), tests()[4], tests()[f(0)])"
        ));
    }

    #[test]
    fn control_flow_statements() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
fn main() {
    if one {
        ok();
    }

    if one {
        ok();
    } else {
        not_ok();
    }

    if one {
    } else if two {
    } else if three {
    } else {
    }

    if one {
    } else if two {
    } else if three {
    }

    while true {
    }

    while (true) {
    }
}
"#
        ));
    }

    #[test]
    fn variables() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
// global variables
in v_position: vec3;
out f_position: vec3 = vec3(0.0);
uniform u_model_matrix: mat4;

fn main() {
    let x = 0;
    var x = 0;
    let y: f32 = 0;
    let z;
    let w: f32;

    if one {
        let x;
        var x;
    }

    if one {
        let x;
        var x;
    } else {
        let x;
        var x;
    }

    while true {
        let x;
        var x;
    }
}
"#
        ));
    }
}
