pub mod ast;
mod operators;
mod syntax_kind;

pub use self::operators::{ArithOp, BinaryOp, CmpOp, LogicOp, UnaryOp};
use std::collections::{HashSet, VecDeque};

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

const BUILTIN_TYPE_NAMES: &[&str] = &[
    "atomic_uint",
    "int",
    "void",
    "bool",
    "float",
    "double",
    "vec2",
    "vec3",
    "vec4",
    "ivec2",
    "ivec3",
    "ivec4",
    "bvec2",
    "bvec3",
    "bvec4",
    "uint",
    "uvec2",
    "uvec3",
    "uvec4",
    "dvec2",
    "dvec3",
    "dvec4",
    "mat2",
    "mat3",
    "mat4",
    "mat2x2",
    "mat2x3",
    "mat2x4",
    "mat3x2",
    "mat3x3",
    "mat3x4",
    "mat4x2",
    "mat4x3",
    "mat4x4",
    "dmat2",
    "dmat3",
    "dmat4",
    "dmat2x2",
    "dmat2x3",
    "dmat2x4",
    "dmat3x2",
    "dmat3x3",
    "dmat3x4",
    "dmat4x2",
    "dmat4x3",
    "dmat4x4",
    "sampler1D",
    "sampler1DShadow",
    "sampler1DArray",
    "sampler1DArrayShadow",
    "isampler1D",
    "isampler1DArray",
    "usampler1D",
    "usampler1DArray",
    "sampler2D",
    "sampler2DShadow",
    "sampler2DArray",
    "sampler2DArrayShadow",
    "isampler2D",
    "isampler2DArray",
    "usampler2D",
    "usampler2DArray",
    "sampler2DRect",
    "sampler2DRectShadow",
    "isampler2DRect",
    "usampler2DRect",
    "sampler2DMS",
    "isampler2DMS",
    "usampler2DMS",
    "sampler2DMSArray",
    "isampler2DMSArray",
    "usampler2DMSArray",
    "sampler3D",
    "isampler3D",
    "usampler3D",
    "samplerCube",
    "samplerCubeShadow",
    "isamplerCube",
    "usamplerCube",
    "samplerCubeArray",
    "samplerCubeArrayShadow",
    "isamplerCubeArray",
    "usamplerCubeArray",
    "samplerBuffer",
    "isamplerBuffer",
    "usamplerBuffer",
    "image1D",
    "iimage1D",
    "uimage1D",
    "image1DArray",
    "iimage1DArray",
    "uimage1DArray",
    "image2D",
    "iimage2D",
    "uimage2D",
    "image2DArray",
    "iimage2DArray",
    "uimage2DArray",
    "image2DRect",
    "iimage2DRect",
    "uimage2DRect",
    "image2DMS",
    "iimage2DMS",
    "uimage2DMS",
    "image2DMSArray",
    "iimage2DMSArray",
    "uimage2DMSArray",
    "image3D",
    "iimage3D",
    "uimage3D",
    "imageCube",
    "iimageCube",
    "uimageCube",
    "imageCubeArray",
    "iimageCubeArray",
    "uimageCubeArray",
    "imageBuffer",
    "iimageBuffer",
    "uimageBuffer",
    "texture1D",
    "texture1DArray",
    "itexture1D",
    "itexture1DArray",
    "utexture1D",
    "utexture1DArray",
    "texture2D",
    "texture2DArray",
    "itexture2D",
    "itexture2DArray",
    "utexture2D",
    "utexture2DArray",
    "texture2DRect",
    "itexture2DRect",
    "utexture2DRect",
    "texture2DMS",
    "itexture2DMS",
    "utexture2DMS",
    "texture2DMSArray",
    "itexture2DMSArray",
    "utexture2DMSArray",
    "texture3D",
    "itexture3D",
    "utexture3D",
    "textureCube",
    "itextureCube",
    "utextureCube",
    "textureCubeArray",
    "itextureCubeArray",
    "utextureCubeArray",
    "textureBuffer",
    "itextureBuffer",
    "utextureBuffer",
    "sampler",
    "samplerShadow",
    "subpassInput",
    "isubpassInput",
    "usubpassInput",
    "subpassInputMS",
    "isubpassInputMS",
    "usubpassInputMS",
];

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
    let mut lookahead = VecDeque::default();
    if let Some(next) = lex.next() {
        lookahead.push_back(next);
    }
    let mut types = HashSet::new();
    for ty in BUILTIN_TYPE_NAMES {
        types.insert(ty.to_string());
    }
    let mut parser = Parser {
        source_id,
        text,
        lookahead,
        lex,
        b,
        diag,
        types,
        consumed_tokens: 0,
    };
    parser.parse()
}

struct Parser<'a> {
    text: &'a str,
    source_id: SourceId,
    // Current token & span
    //current: Option<(SyntaxKind, Range<usize>)>,
    /// lexer for the input text
    lex: Lexer<'a>,
    consumed_tokens: usize,
    lookahead: VecDeque<(SyntaxKind, Range<usize>)>,
    /// the in-progress tree.
    b: GreenNodeBuilder<'static>,
    /// The list of syntax errors we've accumulated
    /// so far. They are reported all at once when parsing is complete.
    diag: &'a Diagnostics,
    types: HashSet<String>,
}

fn is_trivia(kind: SyntaxKind) -> bool {
    matches!(kind, WHITESPACE | BLOCK_COMMENT | LINE_COMMENT)
}

struct Progress(usize);

impl Default for Progress {
    fn default() -> Self {
        Progress(usize::MAX)
    }
}

impl<'a> Parser<'a> {
    fn parse(mut self) -> SyntaxNode {
        // Don't skip whitespace for the root module
        self.b.start_node(MODULE.into());
        self.parse_items();
        self.eat_ws();
        self.b.finish_node();
        let green_node = self.b.finish();
        SyntaxNode::new_root(green_node)
    }

    //
    // --- Utilities ---
    //

    /// Skips whitespace and eats the next token
    fn eat(&mut self) {
        self.eat_ws();
        if let Some((kind, span)) = self.next_raw() {
            self.b.token(kind.into(), &self.text[span]);
            self.lookahead.pop_front();
            self.consumed_tokens += 1;
        }
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.b.checkpoint()
    }

    fn next_ensure_progress(&mut self, progress: &mut Progress) -> Option<SyntaxKind> {
        // if the lexer hasn't advanced, consider the parser stuck
        if self.consumed_tokens == progress.0 {
            let tk = self.lookahead(0);
            eprintln!("parser may be stuck: current token = {tk:?}");
            self.eat();
        }

        self.eat_ws();
        progress.0 = self.consumed_tokens;
        self.next_raw().map(|(kind, _)| kind)
    }

    /// Skips whitespace and returns the next unprocessed token.
    fn next(&mut self) -> Option<SyntaxKind> {
        self.eat_ws();
        self.next_raw().map(|(kind, _)| kind)
    }

    fn next_is_type(&mut self) -> bool {
        let text = self.text();
        eprintln!("next_is_type {text}");
        self.types.contains(text)
    }

    fn lookahead(&mut self, n: usize) -> Option<SyntaxKind> {
        let mut i = 0; // tokens including trivia
        let mut tk = n; // non-trivia tokens remaining
        loop {
            // feed the lookahead buffer if it is empty
            if i >= self.lookahead.len() {
                let next = self.lex.next();
                if let Some(next) = next {
                    self.lookahead.push_back(next);
                } else {
                    break None;
                }
            }

            // skip trivia
            let kind = self.lookahead[i].0;
            if is_trivia(kind) {
                i += 1;
            } else {
                if tk == 0 {
                    break Some(kind);
                }
                tk -= 1;
                i += 1;
            }
        }
    }

    fn next_raw(&mut self) -> Option<(SyntaxKind, Range<usize>)> {
        if self.lookahead.is_empty() {
            let next = self.lex.next();
            if let Some(next) = next {
                self.lookahead.push_back(next);
            } else {
                return None;
            }
        }
        Some(self.lookahead[0].clone())
    }

    fn eat_ws(&mut self) {
        loop {
            if let Some((kind, span)) = self.next_raw() {
                if is_trivia(kind) {
                    self.b.token(kind.into(), &self.text[span]);
                    self.lookahead.pop_front();
                    self.consumed_tokens += 1;
                    continue;
                }
            }
            break;
        }
    }

    /// Returns the `Span` of the current token.
    fn span(&self) -> Option<SourceLocation> {
        if let Some((_, span)) = self.lookahead.front().clone() {
            Some(SourceLocation {
                file: Some(self.source_id),
                range: TextRange::new(TextSize::from(span.start as u32), TextSize::from(span.end as u32)),
            })
        } else {
            None
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        self.eat_ws();
        self.b.start_node(kind.into());
    }

    fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        self.eat_ws();
        self.b.start_node_at(checkpoint, kind.into());
    }

    fn finish_node(&mut self) {
        self.b.finish_node();
    }

    fn text(&self) -> &str {
        &self.text[self.lookahead.front().clone().unwrap().clone().1]
    }

    fn register_type_name(&mut self, name: &str) {
        self.types.insert(name.to_string());
    }

    //--------------------------------------------------------------------
    fn expect_ident(&mut self, ident_kind: &str) -> bool {
        if self.next() != Some(IDENT) {
            let span = self.span();
            self.diag
                .error(format!("expected {ident_kind}"))
                .primary_label_opt(span, "expected IDENT")
                .emit();
            false
        } else {
            self.eat();
            true
        }
    }

    fn expect_new_type_name(&mut self, ident_kind: &str) -> bool {
        if self.next() != Some(IDENT) {
            let span = self.span();
            self.diag
                .error(format!("expected {ident_kind}"))
                .primary_label_opt(span, "expected IDENT")
                .emit();
            false
        } else {
            let text = self.text();
            self.types.insert(text.to_string());
            self.eat();
            true
        }
    }

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.next() != Some(kind) {
            let span = self.span();
            let msg = format!("expected {kind:?}");
            self.diag.error(msg.clone()).primary_label_opt(span, msg).emit();
            false
        } else {
            self.eat();
            true
        }
    }

    fn expect_any(&mut self, kinds: &[SyntaxKind]) -> bool {
        if let Some(kind) = self.next() {
            if kinds.contains(&kind) {
                self.eat();
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
        eprintln!("parse_function_or_variable");
        let cp = self.checkpoint();

        // parse extern
        if self.next() == Some(EXTERN_KW) {
            self.start_node(EXTERN);
            self.eat();
            self.finish_node(); // EXTERN
        }

        // qualifiers
        let mut has_qualifiers = false;
        match self.next() {
            Some(IN_KW | OUT_KW | CONST_KW | UNIFORM_KW) => {
                self.start_node(QUALIFIER);
                self.eat();
                self.finish_node(); // QUALIFIER
                has_qualifiers = true;
            }
            _ => {}
        }

        // parse type (return type or variable type)
        self.parse_type();

        // name
        self.expect_ident("function or variable name");

        match self.next() {
            Some(T!['(']) => {
                // function
                self.parse_fn_param_list();

                match self.next() {
                    Some(T!['{']) => {
                        self.b.start_node_at(cp, FN_DEF.into());
                        self.parse_block();
                    }
                    Some(T![;]) => {
                        self.b.start_node_at(cp, FN_DECL.into());
                        self.eat();
                    }
                    _ => {
                        // TODO better recovery
                        let span = self.span();
                        self.diag.error("unexpected token").primary_label_opt(span, "").emit();
                        self.b.start_node_at(cp, FN_DEF.into());
                        self.eat();
                    }
                }
                self.finish_node(); // FN_DEF or FN_DECL
            }
            _ => {
                // variable declaration
                self.start_node_at(cp, GLOBAL);
                if self.next() == Some(EQ) {
                    self.start_node(INITIALIZER);
                    self.eat();
                    self.parse_expr();
                    self.finish_node(); // INITIALIZER
                }
                self.expect(SEMICOLON);
                self.finish_node(); // GLOBAL
            }
        }
    }

    fn parse_items(&mut self) {
        let mut progress = Progress::default();
        loop {
            match self.next_ensure_progress(&mut progress) {
                Some(IMPORT_KW) => {
                    self.parse_import_declaration();
                }
                Some(STRUCT_KW) => {
                    self.parse_struct_def();
                }
                None => break,
                _ => {
                    self.parse_function_or_variable();
                }
            }
        }
    }

    fn parse_package_parameter(&mut self) {
        match self.next() {
            Some(INT_NUMBER | STRING) => {
                self.start_node(LIT_EXPR);
                self.eat();
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
        if let Some(T!['(']) = self.next() {
            self.start_node(IMPORT_PARAM_LIST);
            self.eat();
            self.parse_separated_list(T![,], T![')'], true, false, Self::parse_package_parameter);
            self.expect(T![')']);
            self.finish_node(); // IMPORT_PARAM_LIST
        }

        if let Some(T![as]) = self.next() {
            self.start_node(IMPORT_ALIAS);
            self.eat();
            self.expect_ident("package import alias");
            self.finish_node(); // IMPORT_ALIAS
        }
        self.expect(T![;]);
        self.finish_node(); // IMPORT_DECL
    }

    fn parse_struct_def(&mut self) {
        eprintln!("parse_struct_def");
        self.start_node(STRUCT_DEF);
        self.expect(STRUCT_KW);
        self.expect_new_type_name("struct name");
        self.expect(T!['{']);
        let mut progress = Progress::default();
        loop {
            match self.next_ensure_progress(&mut progress) {
                Some(T!['}']) => break,
                _ => {
                    self.parse_struct_field();
                }
            }
        }
        self.expect(T!['}']);
        self.finish_node(); // STRUCT_DEF
    }

    fn parse_struct_field(&mut self) {
        self.start_node(STRUCT_FIELD);
        self.parse_type();
        self.expect_ident("field name");
        self.expect(T![;]);
        self.finish_node(); // STRUCT_FIELD
    }

    /*// parse function after "extern"
    fn parse_fn(&mut self, cp: Checkpoint) {
        //self.start_node(FN_DEF);
        if self.next() == Some(EXTERN_KW) {
            self.start_node(EXTERN);
            self.eat();
            self.finish_node(); // EXTERN
        }
        self.expect(FN_KW);
        self.expect_ident("function name");
        self.parse_fn_param_list();
        if self.next() == Some(THIN_ARROW) {
            self.start_node(RET_TYPE);
            self.eat();
            self.parse_type();
            self.finish_node();
        }

        match self.next() {
            Some(T!['{']) => {
                self.b.start_node_at(cp, FN_DEF.into());
                self.parse_block();
            }
            Some(T![;]) => {
                self.b.start_node_at(cp, FN_DECL.into());
                self.eat();
            }
            _ => {
                // TODO better recovery
                let span = self.span();
                self.diag.error("unexpected token").primary_label_opt(span, "").emit();
                self.b.start_node_at(cp, FN_DEF.into());
                self.eat();
            }
        }
        self.finish_node();
    }*/

    fn parse_fn_param_list(&mut self) {
        self.start_node(PARAM_LIST);
        if !self.expect(L_PAREN) {
            self.finish_node();
            return;
        }

        let mut progress = Progress::default();
        loop {
            match self.next_ensure_progress(&mut progress) {
                Some(R_PAREN) => {
                    self.eat();
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
            if self.next() == Some(COMMA) {
                self.eat();
                continue;
            }
            self.expect(R_PAREN);
            break;
        }

        self.finish_node();
    }

    fn parse_block(&mut self) {
        eprintln!("parse_block");
        self.start_node(BLOCK);
        self.expect(L_CURLY);
        self.parse_stmt_list();
        self.expect(R_CURLY);
        self.finish_node();
    }

    fn parse_fn_parameter(&mut self) {
        self.start_node(FN_PARAM);
        self.parse_type();
        self.expect_ident("argument name");
        self.finish_node();
    }

    /*fn parse_type_parameter(&mut self) {
        match self.next() {
            Some(INT_NUMBER | FLOAT_NUMBER | STRING) => {
                self.start_node(LIT_EXPR);
                self.eat();
                self.finish_node();
            }
            Some(T!['{']) => {
                self.start_node(CONST_EXPR);
                self.eat();
                self.parse_expr();
                self.expect(T!['}']);
                self.finish_node();
            }
            _ => self.parse_type(),
        }
    }*/

    fn parse_type(&mut self) {
        // for now, only ident
        match self.next() {
            //Some(T!['(']) => {
            //    self.parse_tuple_type();
            //}
            /*Some(T!['[']) => {
                self.parse_array_type();
            }*/
            /*Some(FN_KW) => {
                self.parse_closure_type();
            }*/
            Some(IDENT) => {
                // if lookahead is `[` -> start array_type
                let cp = self.checkpoint();
                self.start_node(TYPE_REF);
                self.eat();
                self.finish_node();
                while self.lookahead(0) == Some(T!['[']) {
                    self.start_node_at(cp, ARRAY_TYPE);
                    self.eat();
                    if self.next() != Some(T![']']) {
                        self.parse_expr();
                    }
                    self.expect(T![']']);
                    self.finish_node(); // ARRAY_TYPE
                }
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
                self.eat();
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
        let mut progress = Progress::default();
        loop {
            match self.next_ensure_progress(&mut progress) {
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
            match self.next() {
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
                    self.eat();
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

    /*/// Parses a closure type like `fn(vec2, mat3) -> float`
    fn parse_closure_type(&mut self) {
        self.start_node(CLOSURE_TYPE);
        self.expect(FN_KW);
        self.start_node(CLOSURE_PARAM_LIST);
        self.expect(T!['(']);
        self.parse_separated_list(T![,], T![')'], true, false, Self::parse_type);
        self.expect(T![')']);
        self.finish_node(); // CLOSURE_PARAM_LIST
        if self.next() == Some(T![->]) {
            self.start_node(RET_TYPE);
            self.eat();
            self.parse_type();
            self.finish_node();
        }
        self.finish_node();
    }*/

    // parse_type
    // if ident:
    //    start cp
    //    parse non-array type
    //    if next == '['
    //

    /// Parses an array type like `float[16][4]`.
    fn parse_array_type(&mut self) {
        self.start_node(ARRAY_TYPE);
        self.parse_type();
        self.expect(T!['[']);
        if self.next() == Some(T![;]) {
            self.eat();
            self.parse_expr();
        }
        self.expect(T![']']);
        self.finish_node();
    }

    /// Parses an operand to a binary expression.
    fn parse_atom(&mut self) {
        match self.next() {
            Some(INT_NUMBER) => {
                self.start_node(LIT_EXPR);
                self.eat();
                self.finish_node();
            }
            Some(FLOAT_NUMBER) => {
                self.start_node(LIT_EXPR);
                self.eat();
                self.finish_node();
            }
            Some(IDENT) => {
                self.start_node(PATH_EXPR);
                self.eat();
                self.finish_node();
            }
            Some(TRUE_KW) => {
                self.start_node(LIT_EXPR);
                self.eat();
                self.finish_node();
            }
            Some(FALSE_KW) => {
                self.start_node(LIT_EXPR);
                self.eat();
                self.finish_node();
            }
            Some(T!['(']) => {
                self.start_node(PAREN_EXPR);
                self.eat();
                self.parse_expr();
                self.expect(T![')']);
                self.finish_node();
            }
            /*Some(T!['[']) => {
                self.start_node(ARRAY_EXPR);
                self.parse_separated_list(T![,], T![']'], true, false, Self::parse_expr);
                self.expect(T![']']);
                self.finish_node();
            }*/
            _ => {
                let span = self.span();
                self.diag.error("syntax error").primary_label_opt(span, "").emit();
                self.eat();
            }
        }
    }

    fn parse_stmt_list(&mut self) {
        eprintln!("parse_stmt_list");
        let mut progress = Progress::default();
        loop {
            match self.next_ensure_progress(&mut progress) {
                Some(R_CURLY) => break,
                None => break,
                _ => self.parse_stmt(),
            }
        }
    }

    fn parse_stmt(&mut self) {
        eprintln!("parse_stmt");
        match self.next() {
            Some(RETURN_KW) => self.parse_return(),
            Some(IF_KW) => self.parse_if_stmt(),
            Some(BREAK_KW) => self.parse_break_stmt(),
            Some(CONTINUE_KW) => self.parse_continue_stmt(),
            Some(DISCARD_KW) => self.parse_discard_stmt(),
            Some(WHILE_KW) => self.parse_while_stmt(),
            Some(IDENT) => {
                if self.next_is_type() {
                    self.parse_local_variable_stmt()
                } else {
                    self.parse_expr_stmt()
                }
            }
            Some(_) => {
                self.parse_expr_stmt()
            }
            _ => {
                let loc = self.span();
                self.diag.error("syntax error").primary_label_opt(loc, "").emit()
            }
        }
    }

    fn parse_local_variable_stmt(&mut self) {
        eprintln!("parse_local_variable_stmt");
        self.start_node(LOCAL_VARIABLE);
        self.parse_type();
        self.expect_ident("variable name");
        if self.next() == Some(T![=]) {
            self.start_node(INITIALIZER);
            self.eat();
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
        if self.next() == Some(ELSE_KW) {
            self.start_node(ELSE_BRANCH);
            self.eat();
            if self.next() == Some(IF_KW) {
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
        eprintln!("parse_expr_stmt");
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

        let mut progress = Progress::default();
        loop {
            match self.next_ensure_progress(&mut progress) {
                Some(T![')']) => {
                    self.eat();
                    break;
                }
                None => break,
                _ => {}
            }
            self.parse_expr();
            match self.next() {
                Some(T![')']) => {
                    self.eat();
                    break;
                }
                Some(T![,]) => {
                    // continue arg list
                    self.eat();
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
        if self.next() == Some(EXTERN_KW) {
            self.start_node(EXTERN);
            self.eat();
            self.finish_node(); // EXTERN
        }
        self.start_node(QUALIFIER);
        self.expect_any(&[IN_KW, OUT_KW, CONST_KW, UNIFORM_KW]);
        self.finish_node(); // QUALIFIER
        self.expect_ident("variable name");
        self.expect(COLON);
        self.parse_type();
        if self.next() == Some(EQ) {
            self.start_node(INITIALIZER);
            self.eat();
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
        eprintln!("parse_expr_bp");
        let cp = self.checkpoint();
        match self.next() {
            Some(op @ T![+] | op @ T![-] | op @ T![!]) => {
                self.start_node(PREFIX_EXPR);
                self.eat();
                let r_bp = prefix_binding_power(op);
                self.parse_expr_bp(r_bp);
                self.finish_node();
            }
            _ => {
                self.parse_atom();
            }
        }

        let mut progress = Progress::default();
        loop {
            let op = match self.next_ensure_progress(&mut progress) {
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
                    self.eat();
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
                    self.eat();
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

            self.eat();
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
        let src = format!("void main() {{ {expr}; }}");
        parse_source_text(&src)
    }

    #[test]
    fn empty_test() {
        insta::assert_debug_snapshot!(parse_source_text(r#"    "#));
    }

    #[test]
    fn whitespace_tests() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
void main(){}
void main2    (    )    {     }
void main3(    float arg    ) { }
void main4(  float      arg) {}
"#
        ));
    }

    #[test]
    fn basic_parser_tests() {
        insta::assert_debug_snapshot!(
            "empty function",
            parse_source_text(
                r#"
void main() {
}

int test() {
}

// array types
float[4] test3() {
}
"#
            )
        );

        insta::assert_debug_snapshot!(
            "empty functions with arguments",
            parse_source_text(
                r#"
void zero_arg() {}
void one_arg(int a) {}
void two_args(float a, float b) {}
"#
            )
        );
    }

    #[test]
    fn expr_parser_tests() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
void expr_test() {
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

        insta::assert_debug_snapshot!(expr("f(1, 10.0, f(10), tests()[4], tests()[f(0)])"));
    }

    #[test]
    fn control_flow_statements() {
        insta::assert_debug_snapshot!(parse_source_text(
            r#"
void main() {
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
    float a;
}
struct TwoFields {
    float a;
    float b;
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
