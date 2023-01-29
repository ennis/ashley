pub mod ast;
mod operators;
mod syntax_kind;

pub use self::operators::{ArithOp, BinaryOp, CmpOp, LogicOp, UnaryOp};

pub(crate) use self::syntax_kind::SyntaxKind;
use self::syntax_kind::{Lexer, SyntaxKind::*};
use crate::{
    diagnostic::{AsSourceLocation, Diagnostics, SourceFileProvider, SourceId, SourceLocation},
    T,
};
use codespan_reporting::{term, term::termcolor::WriteColor};
use rowan::{Checkpoint, GreenNodeBuilder, TextRange, TextSize};
use std::ops::Range;

//--------------------------------------------------------------------------------------------------

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

impl AsSourceLocation for SyntaxToken {
    fn source_location(&self) -> SourceLocation {
        SourceLocation::new(None, self.text_range())
    }
}

impl AsSourceLocation for SyntaxNode {
    fn source_location(&self) -> SourceLocation {
        SourceLocation::new(None, self.text_range())
    }
}

pub fn parse(
    text: &str,
    file: SourceId,
    source_provider: SourceFileProvider,
    diag_writer: impl WriteColor + 'static,
) -> SyntaxNode {
    let diag = Diagnostics::new(source_provider, file, diag_writer, term::Config::default());
    parse_inner(text, file, &diag)
}

pub(crate) fn parse_inner(text: &str, source_id: SourceId, diag: &Diagnostics) -> SyntaxNode {
    let mut lex: Lexer = SyntaxKind::create_lexer(text);
    let b = GreenNodeBuilder::new();
    let current = lex.next();
    let mut parser = Parser {
        source_id,
        text,
        current,
        lex,
        b,
        diag,
    };
    parser.parse()
}

struct Parser<'a> {
    text: &'a str,
    source_id: SourceId,
    /// Current token & span
    current: Option<(SyntaxKind, Range<usize>)>,
    /// lexer for the input text
    lex: Lexer<'a>,
    /// the in-progress tree.
    b: GreenNodeBuilder<'static>,
    /// The list of syntax errors we've accumulated
    /// so far. They are reported all at once when parsing is complete.
    diag: &'a Diagnostics,
}

impl<'a> Parser<'a> {
    fn parse(mut self) -> SyntaxNode {
        // Don't skip whitespace for the root module
        self.b.start_node(MODULE.into());
        while self.current().is_some() {
            self.parse_items();
        }
        self.skip_ws2();
        self.b.finish_node();
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

    fn checkpoint(&mut self) -> Checkpoint {
        self.skip_ws2();
        self.b.checkpoint()
    }

    /// Peek at the first unprocessed token
    fn current(&mut self) -> Option<SyntaxKind> {
        self.skip_ws2();
        self.current.clone().map(|(kind, _)| kind)
    }

    fn skip_ws2(&mut self) {
        while matches!(self.current.clone().map(|(kind, _)| kind), Some(WHITESPACE | BLOCK_COMMENT | LINE_COMMENT)) {
            self.bump()
        }
    }

    /// Returns the `Span` of the current token.
    fn span(&self) -> Option<SourceLocation> {
        if let Some((_, span)) = self.current.clone() {
            Some(SourceLocation {
                file: Some(self.source_id),
                range: TextRange::new(TextSize::from(span.start as u32), TextSize::from(span.end as u32)),
            })
        } else {
            None
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.skip_ws2();
        self.b.start_node(kind.into());
    }

    fn finish_node(&mut self) {
        self.b.finish_node();
    }

    //--------------------------------------------------------------------
    fn expect_ident(&mut self, ident_kind: &str) -> bool {
        if self.current() != Some(IDENT) {
            let span = self.span();
            self.diag
                .error(format!("expected {ident_kind}"))
                .primary_label_opt(span, "expected IDENT")
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
            self.diag.error(msg.clone()).primary_label_opt(span, msg).emit();
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
        self.diag.error(msg.clone()).primary_label_opt(span, msg).emit();
        false
    }


    //
    // --- Nodes ---
    //

    fn parse_function_or_variable(&mut self) {
        let cp = self.checkpoint();
        // parse extern
        if self.current() == Some(EXTERN_KW) {
            self.start_node(EXTERN);
            self.bump();
            self.finish_node(); // EXTERN
        }
        match self.current() {
            Some(FN_KW) => {
                self.parse_fn(cp);
            }
            Some(IN_KW | OUT_KW | CONST_KW | UNIFORM_KW) => {
                self.parse_global_variable(cp);
            }
            _ => {
                let span = self.span();
                self.diag.error("unexpected token").primary_label_opt(span, "").emit();
            }
        }
    }

    fn parse_items(&mut self) {
        loop {
            match self.current() {
                Some(IMPORT_KW) => {
                    self.parse_import_declaration();
                }
                Some(STRUCT_KW) => {
                    self.parse_struct_def();
                }
                Some(IN_KW | OUT_KW | CONST_KW | UNIFORM_KW | EXTERN_KW | FN_KW) => {
                    self.parse_function_or_variable();
                }
                None => break,
                _ => {
                    // unexpected token
                    let span = self.span();
                    self.diag.error("unexpected token").primary_label_opt(span, "").emit();
                    self.bump();
                }
            }
        }
    }

    fn parse_package_parameter(&mut self) {
        match self.current() {
            Some(INT_NUMBER | STRING) => {
                self.start_node(LIT_EXPR);
                self.bump();
                self.finish_node();
            }
            _ => {
                let span = self.span();
                self.diag
                    .error("expected package parameter")
                    .primary_label_opt(span, "")
                    .emit();
            }
        }
    }

    fn parse_import_declaration(&mut self) {
        self.start_node(IMPORT_DECL);
        self.expect(IMPORT_KW);
        self.expect_ident("package name");
        if let Some(T!['(']) = self.current() {
            self.start_node(IMPORT_PARAM_LIST);
            self.bump();
            self.parse_separated_list(T![,], T![')'], true, false, Self::parse_package_parameter);
            self.expect(T![')']);
            self.finish_node(); // IMPORT_PARAM_LIST
        }

        if let Some(T![as]) = self.current() {
            self.start_node(IMPORT_ALIAS);
            self.bump();
            self.expect_ident("package import alias");
            self.finish_node(); // IMPORT_ALIAS
        }
        self.expect(T![;]);
        self.finish_node(); // IMPORT_DECL
    }

    fn parse_struct_def(&mut self) {
        self.start_node(STRUCT_DEF);
        self.expect(STRUCT_KW);
        self.expect_ident("struct name");
        self.expect(T!['{']);
        self.parse_separated_list(T![,], T!['}'], true, false, Self::parse_struct_field);
        self.expect(T!['}']);
        self.finish_node(); // STRUCT_DEF
    }

    fn parse_struct_field(&mut self) {
        self.start_node(STRUCT_FIELD);
        self.expect_ident("field name");
        self.expect(T![:]);
        self.parse_type();
        self.finish_node(); // STRUCT_FIELD
    }

    // parse function after "extern"
    fn parse_fn(&mut self, cp: Checkpoint) {
        //self.start_node(FN_DEF);
        if self.current() == Some(EXTERN_KW) {
            self.start_node(EXTERN);
            self.bump();
            self.finish_node(); // EXTERN
        }
        self.expect(FN_KW);
        self.expect_ident("function name");
        self.parse_fn_param_list();
        if self.current() == Some(THIN_ARROW) {
            self.start_node(RET_TYPE);
            self.bump();
            self.parse_type();
            self.finish_node();
        }

        match self.current() {
            Some(T!['{']) => {
                self.b.start_node_at(cp, FN_DEF.into());
                self.parse_block();
            }
            Some(T![;]) => {
                self.b.start_node_at(cp, FN_DECL.into());
                self.bump();
            }
            _ => {
                // TODO better recovery
                let span = self.span();
                self.diag.error("unexpected token").primary_label_opt(span, "").emit();
                self.b.start_node_at(cp, FN_DEF.into());
                self.bump();
            }
        }
        self.finish_node();
    }

    fn parse_fn_param_list(&mut self) {
        self.start_node(PARAM_LIST);
        if !self.expect(L_PAREN) {
            self.finish_node();
            return;
        }


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
                        .primary_label_opt(cur_span, "")
                        .emit();
                    break;
                }
                _ => {}
            }
            self.parse_fn_parameter();
            if self.current() == Some(COMMA) {
                self.bump();
                continue;
            }
            self.expect(R_PAREN);
            break;
        }

        self.finish_node();
    }

    fn parse_block(&mut self) {
        // TODO
        self.start_node(BLOCK);
        self.expect(L_CURLY);
        self.parse_stmt_list();
        self.expect(R_CURLY);
        self.finish_node();
    }

    fn parse_fn_parameter(&mut self) {
        self.start_node(FN_PARAM);
        // <ident> : <type>
        self.expect_ident("argument name");
        self.expect(COLON);
        self.parse_type();
        self.finish_node();
    }

    fn parse_type_parameter(&mut self) {
        match self.current() {
            Some(INT_NUMBER | FLOAT_NUMBER | STRING) => {
                self.start_node(LIT_EXPR);
                self.bump();
                self.finish_node();
            }
            Some(T!['{']) => {
                self.start_node(CONST_EXPR);
                self.bump();
                self.parse_expr();
                self.expect(T!['}']);
                self.finish_node();
            }
            _ => self.parse_type(),
        }
    }

    fn parse_type(&mut self) {
        // for now, only ident
        match self.current() {
            Some(T!['(']) => {
                self.parse_tuple_type();
            }
            Some(T!['[']) => {
                self.parse_array_type();
            }
            Some(FN_KW) => {
                self.parse_closure_type();
            }
            Some(IDENT) => {
                self.start_node(TYPE_REF);
                self.bump();
                /*// optional type parameter list
                if self.current() == Some(T![<]) {
                    self.bump();
                    self.parse_separated_list(T![,], T![>], true, false, Self::parse_type_parameter);
                    self.expect(T![>]);
                }*/
                self.finish_node();
            }
            None => {
                let cur_span = self.span();
                self.diag
                    .error("unexpected end of file")
                    .primary_label_opt(cur_span, "")
                    .emit();
            }
            _ => {
                let cur_span = self.span();
                self.diag
                    .error("syntax error (TODO parse_type)")
                    .primary_label_opt(cur_span, "")
                    .emit();
                // FIXME: forward progress bump
                self.bump();
            }
        }
    }

    fn parse_separated_list(
        &mut self,
        sep: SyntaxKind,
        end: SyntaxKind,
        allow_trailing: bool,
        tuple_rule: bool,
        mut parse_item: impl FnMut(&mut Self),
    ) {
        let mut trailing_sep = false;
        let mut last_sep_span = None;
        let mut item_count = 0;
        loop {
            match self.current() {
                Some(x) if x == end => {
                    if trailing_sep && !allow_trailing {
                        self.diag
                            .error("trailing separator not allowed here")
                            .primary_label_opt(last_sep_span, "")
                            .emit()
                    }
                    break;
                }
                None => break,
                _ => {}
            }
            parse_item(self);
            item_count += 1;
            match self.current() {
                Some(x) if x == end => {
                    if item_count == 1 && tuple_rule {
                        // should end with a comma
                        self.diag
                            .error("single-element tuple types should have a trailing `,`")
                            .primary_label_opt(last_sep_span, "")
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
                        .primary_label_opt(span, "")
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

    /// Parses a closure type like `fn(vec2, mat3) -> float`
    fn parse_closure_type(&mut self) {
        self.start_node(CLOSURE_TYPE);
        self.expect(FN_KW);
        self.start_node(CLOSURE_PARAM_LIST);
        self.expect(T!['(']);
        self.parse_separated_list(T![,], T![')'], true, false, Self::parse_type);
        self.expect(T![')']);
        self.finish_node(); // CLOSURE_PARAM_LIST
        if self.current() == Some(T![->]) {
            self.start_node(RET_TYPE);
            self.bump();
            self.parse_type();
            self.finish_node();
        }
        self.finish_node();
    }

    /// Parses an array type like `[f32; 16]`.
    fn parse_array_type(&mut self) {
        self.start_node(ARRAY_TYPE);
        self.expect(T!['[']);
        self.parse_type();
        if self.current() == Some(T![;]) {
            self.bump();
            self.parse_expr();
        }
        self.expect(T![']']);
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
                let cp = self.checkpoint();
                self.bump();
                self.parse_expr();
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
                        self.parse_separated_list(T![,], T![')'], true, false, Self::parse_expr);
                        self.expect(T![')']);
                        self.finish_node();
                    }
                    _ => {
                        let span = self.span();
                        self.diag.error("syntax error").primary_label_opt(span, "").emit();
                    }
                }
            }
            Some(T!['[']) => {
                self.start_node(ARRAY_EXPR);
                self.parse_separated_list(T![,], T![']'], true, false, Self::parse_expr);
                self.expect(T![']']);
                self.finish_node();
            }
            _ => {
                let span = self.span();
                self.diag.error("syntax error").primary_label_opt(span, "").emit();
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
        self.expect_ident("variable name");
        if self.current() == Some(T![:]) {
            self.bump();
            self.parse_type();
        }
        if self.current() == Some(T![=]) {
            self.start_node(INITIALIZER);
            self.bump();
            self.parse_expr();
            self.finish_node(); // INITIALIZER
        }
        self.expect(SEMICOLON);
        self.finish_node(); // LOCAL_VARIABLE
    }

    fn parse_break_stmt(&mut self) {
        self.start_node(BREAK_STMT);
        self.expect(BREAK_KW);
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_continue_stmt(&mut self) {
        self.start_node(CONTINUE_STMT);
        self.expect(CONTINUE_KW);
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_discard_stmt(&mut self) {
        self.start_node(DISCARD_STMT);
        self.expect(DISCARD_KW);
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_while_stmt(&mut self) {
        self.start_node(WHILE_STMT);
        self.expect(WHILE_KW);
        self.start_node(CONDITION);
        self.parse_expr();
        self.finish_node(); // CONDITION
        self.parse_block();
        self.finish_node(); // WHILE_STMT
    }

    fn parse_if_stmt(&mut self) {
        self.start_node(IF_STMT);
        self.expect(IF_KW);
        self.start_node(CONDITION);
        self.parse_expr();
        self.finish_node();
        self.parse_block();
        if self.current() == Some(ELSE_KW) {
            self.start_node(ELSE_BRANCH);
            self.bump();
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
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_return(&mut self) {
        self.start_node(RETURN_STMT);
        self.expect(RETURN_KW);
        self.parse_expr();
        self.expect(SEMICOLON);
        self.finish_node();
    }

    fn parse_call_args(&mut self) {
        self.start_node(ARG_LIST);
        self.expect(T!['(']);

        loop {
            match self.current() {
                Some(T![')']) => {
                    self.bump();
                    break;
                }
                None => break,
                _ => {}
            }
            self.parse_expr();
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
                        .primary_label_opt(span, "")
                        .emit();
                }
            }
        }

        self.finish_node();
    }

    fn parse_global_variable(&mut self, start: Checkpoint) {
        self.b.start_node_at(start, GLOBAL.into());
        if self.current() == Some(EXTERN_KW) {
            self.start_node(EXTERN);
            self.bump();
            self.finish_node(); // EXTERN
        }
        self.start_node(QUALIFIER);
        self.expect_any(&[IN_KW, OUT_KW, CONST_KW, UNIFORM_KW]);
        self.finish_node(); // QUALIFIER
        self.expect_ident("variable name");
        self.expect(COLON);
        self.parse_type();
        if self.current() == Some(EQ) {
            self.start_node(INITIALIZER);
            self.bump();
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
        let cp = self.checkpoint();
        match self.current() {
            Some(op @ T![+] | op @ T![-] | op @ T![!]) => {
                self.start_node(PREFIX_EXPR);
                self.bump();
                let r_bp = prefix_binding_power(op);
                self.parse_expr_bp(r_bp);
                self.finish_node();
            }
            _ => {
                self.parse_atom();
            }
        }

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
            self.parse_expr_bp(r_bp);

            self.finish_node();
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
        T![=] | T![+=] | T![-=] | T![*=] | T![/=] | T![%=] | T![&=] | T![|=] | T![^=] | T![<<=] | T![>>=] => (2, 1),
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
    use crate::{
        diagnostic::SourceFileProvider,
        syntax,
        syntax::{parse_inner, Diagnostics, Lang, SyntaxNode},
    };
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
    use rowan::GreenNode;

    fn parse_source_text(text: &str) -> SyntaxNode {
        let mut sources = SourceFileProvider::new();
        let src_id = sources.register_source("<input>", text);
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        syntax::parse(text, src_id, sources, writer)
    }

    fn expr(expr: &str) -> SyntaxNode {
        let src = format!("fn main() {{ {expr}; }}");
        parse_source_text(&src)
    }

    #[test]
    fn empty_test() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"    "#
        ));
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

// array types
fn test3() -> [f32; 4] {
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

        insta::assert_debug_snapshot!(expr("f(1, 10.0, f(10), test, 1 + 2, tests()[4], tests()[f(0)])"));

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

    #[test]
    fn structs() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
struct EmptyStruct {}
struct OneField {
    a: f32
}
struct OneFieldTrailing {
    a: f32,
}
struct TwoFields {
    a: f32,
    b: f32
}
struct TwoFieldsTrailing {
    a: f32,
    b: f32,
}
"#
        ));
    }

    #[test]
    fn imports() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
import package;
import package2 as p;
import package3("test") as p1;
import package3  (   "test"   )  as p1  ;
import package4  ;
"#
        ));
    }

    #[test]
    fn function_decl() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
extern fn main ( ) ;
       fn main ( ) ;
extern fn main ( ) { }
"#
        ));
    }
}
