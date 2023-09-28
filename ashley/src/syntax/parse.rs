use crate::{
    db::CompilerDb,
    diagnostic::Span,
    syntax::{
        diagnostics::SyntaxDiagnostic,
        syntax_kind::{Lexer, SyntaxKind::*},
        SyntaxKind,
    },
    SourceFileId, T,
};
use rowan::{Checkpoint, GreenNode, GreenNodeBuilder, TextLen, TextRange, TextSize};
use std::{
    collections::{HashSet, VecDeque},
    ops::Range,
};

/// List of built-in type names that should be recognized as such by the parser.
///
/// NOTE: contrary to the GLSL spec, these are not considered reserved keywords.
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

pub(crate) fn parse_raw(
    compiler: &dyn CompilerDb,
    text: &str,
    source_file: SourceFileId,
) -> (GreenNode, Vec<SyntaxDiagnostic>) {
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
    let parser = Parser {
        compiler,
        source_file,
        text,
        lookahead,
        lex,
        b,
        types,
        consumed_tokens: 0,
        diagnostics: vec![],
    };
    parser.parse()
}

struct Parser<'a> {
    compiler: &'a dyn CompilerDb,
    text: &'a str,
    source_file: SourceFileId,
    // Current token & span
    //current: Option<(SyntaxKind, Range<usize>)>,
    /// lexer for the input text
    lex: Lexer<'a>,
    consumed_tokens: usize,
    lookahead: VecDeque<(SyntaxKind, Range<usize>)>,
    /// the in-progress tree.
    b: GreenNodeBuilder<'static>,
    types: HashSet<String>,
    diagnostics: Vec<SyntaxDiagnostic>,
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
    fn parse(mut self) -> (GreenNode, Vec<SyntaxDiagnostic>) {
        // Don't skip whitespace for the root module
        self.b.start_node(MODULE.into());
        self.parse_items();
        self.eat_ws();
        self.b.finish_node();
        (self.b.finish(), self.diagnostics)
    }

    // TODO parser recovery situations:
    // - argument lists

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

    fn checkpoint(&self) -> Checkpoint {
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
        //eprintln!("next_is_type {text}");
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
    fn span(&self) -> Span {
        let range = if let Some((_, span)) = self.lookahead.front().clone() {
            TextRange::new(TextSize::from(span.start as u32), TextSize::from(span.end as u32))
        } else {
            // EOF span
            TextRange::empty(self.text.text_len())
        };
        Span {
            file: self.source_file,
            range,
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

    /*fn register_type_name(&mut self, name: &str) {
        self.types.insert(name.to_string());
    }*/

    //--------------------------------------------------------------------
    fn expect_ident(&mut self, ident_kind: &str) -> bool {
        if self.next() != Some(IDENT) {
            self.syntax_error(format!("expected {ident_kind}"));
            false
        } else {
            self.eat();
            true
        }
    }

    fn expect_new_type_name(&mut self, ident_kind: &str) -> bool {
        if self.next() != Some(IDENT) {
            self.syntax_error(format!("expected {ident_kind}"));
            false
        } else {
            let text = self.text();
            self.types.insert(text.to_string());
            self.start_node(NAME);
            self.eat();
            self.finish_node();
            true
        }
    }

    fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.next() != Some(kind) {
            self.syntax_error(format!("expected {kind:?}"));
            false
        } else {
            self.eat();
            true
        }
    }

    /// Expects a token. If the token is not there, skip until the token is found.
    ///
    /// TODO stop recovery when encountering a closing terminator
    fn expect_recover(&mut self, kind: SyntaxKind) -> bool {
        if self.next() != Some(kind) {
            self.syntax_error_at(self.span(), format!("expected {kind:?}"));
            while let Some(tk) = self.next() {
                self.eat();
                if tk == kind {
                    break;
                }
            }
            false
        } else {
            self.eat();
            true
        }
    }

    fn syntax_error(&mut self, message: impl Into<String>) {
        self.diagnostics
            .push(SyntaxDiagnostic::syntax_error(self.span(), message.into()));
    }

    fn syntax_error_at(&mut self, span: Span, message: impl Into<String>) {
        self.diagnostics
            .push(SyntaxDiagnostic::syntax_error(span, message.into()));
    }

    /*fn expect_any(&mut self, kinds: &[SyntaxKind]) -> bool {
        if let Some(kind) = self.next() {
            if kinds.contains(&kind) {
                self.eat();
                return true;
            }
        }

        let span = self.span();
        let msg = format!("expected one of {kinds:?}");
        self.compiler.diag_error(msg.clone()).primary_label_opt(span, msg).emit();
        false
    }*/

    //
    // --- Nodes ---
    //

    fn parse_name(&mut self, name_kind: &str) {
        if self.next() == Some(IDENT) {
            self.start_node(NAME);
            self.eat();
            self.finish_node();
        } else {
            self.syntax_error(format!("expected {name_kind}"));
        }
    }

    fn parse_function_or_variable(&mut self, start: Option<Checkpoint>) {
        //eprintln!("parse_function_or_variable");
        let start = start.unwrap_or(self.checkpoint());

        // parse visibility
        if self.next() == Some(PUBLIC_KW) {
            self.start_node(VISIBILITY);
            self.eat();
            self.finish_node(); // VISIBILTITY
        }

        // parse extern
        if self.next() == Some(EXTERN_KW) {
            self.start_node(LINKAGE);
            self.eat();
            self.finish_node(); // LINKAGE
        }

        // qualifiers
        match self.next() {
            Some(IN_KW | OUT_KW | CONST_KW | UNIFORM_KW | BUFFER_KW | SHARED_KW) => {
                self.start_node(QUALIFIER);
                self.eat();
                self.finish_node(); // QUALIFIER
            }
            _ => {}
        }

        // parse type (return type or variable type)
        self.parse_type();

        // name
        self.parse_name("function or variable name");

        match self.next() {
            Some(T!['(']) => {
                // function
                self.start_node_at(start, FN_DEF);
                self.parse_fn_param_list();

                match self.next() {
                    Some(T!['{']) => {
                        self.parse_block();
                    }
                    Some(T![;]) => {
                        self.eat();
                    }
                    _ => {
                        // TODO better recovery
                        self.syntax_error("unexpected token");
                        self.eat();
                    }
                }
                self.finish_node(); // FN_DEF
            }
            _ => {
                // variable declaration
                self.start_node_at(start, GLOBAL);
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

    fn parse_attribute_item(&mut self) {
        //let start = self.checkpoint();
        match self.next() {
            Some(INT_NUMBER | STRING | FLOAT_NUMBER) => {
                self.start_node(ATTR_LITERAL);
                self.parse_lit_expr();
                self.finish_node(); // ATTR_LITERAL
            }
            Some(IDENT) => {
                match self.lookahead(1) {
                    Some(T!['(']) => {
                        // sub-attribute (without '@')
                        self.start_node(ATTR_NESTED);
                        self.eat(); // ident
                        self.start_node(ATTR_ARGS);
                        self.eat(); // '('
                        self.parse_separated_list(T![,], T![')'], false, false, Self::parse_attribute_item);
                        self.expect(T![')']);
                        self.finish_node(); // ATTR_ARGS
                        self.finish_node(); // ATTR_NESTED
                    }
                    Some(T![=]) => {
                        // key-value pair
                        self.start_node(ATTR_KEY_VALUE);
                        self.eat();
                        self.eat();
                        self.parse_atom();
                        self.finish_node(); // ATTR_KEY_VALUE
                    }
                    _ => {
                        // assume this is an ident
                        self.start_node(ATTR_IDENT);
                        self.eat();
                        self.finish_node();
                    }
                }
            }
            _ => {
                // assume expression?
                self.syntax_error("expected literal or ident");
            }
        }
    }

    fn parse_attribute(&mut self) {
        // @attribute
        // @attribute(ident)
        // @attribute(key=value, key2=value2)
        // @attribute(<literal>)
        // @attribute(sub_attribute(...))
        self.start_node(ATTRIBUTE);
        self.expect(T![@]);
        self.parse_name("attribute name");
        if self.next() == Some(T!['(']) {
            self.start_node(ATTR_ARGS);
            self.eat();
            self.parse_separated_list(T![,], T![')'], false, false, Self::parse_attribute_item);
            self.expect(T![')']);
            self.finish_node(); // ATTR_ARGS
        }
        self.finish_node();
    }

    /// Parse a top-level item.
    fn parse_items(&mut self) {
        let mut progress = Progress::default();
        let mut decl_start: Option<Checkpoint> = None;
        loop {
            match self.next_ensure_progress(&mut progress) {
                Some(IMPORT_KW) => {
                    self.parse_import_declaration(decl_start);
                    decl_start = None;
                }
                Some(PUBLIC_KW) => {
                    match self.lookahead(1) {
                        Some(STRUCT_KW) => {
                            self.parse_struct_def(decl_start);
                        }
                        _ => {
                            self.parse_function_or_variable(decl_start);
                        }
                    }
                    decl_start = None;
                }
                Some(STRUCT_KW) => {
                    self.parse_struct_def(decl_start);
                }
                Some(AT) => {
                    // attribute syntax like `@location(0)`
                    decl_start.get_or_insert(self.checkpoint());
                    self.parse_attributes();
                }
                None => break,
                _ => {
                    self.parse_function_or_variable(decl_start);
                    decl_start = None;
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
                self.syntax_error("expected package parameter");
            }
        }
    }

    fn parse_import_declaration(&mut self, start: Option<Checkpoint>) {
        if let Some(start) = start {
            self.start_node_at(start, IMPORT_DECL);
        } else {
            self.start_node(IMPORT_DECL);
        }
        self.expect(IMPORT_KW);
        self.start_node(IMPORT_URI);
        self.expect(STRING);
        self.finish_node();
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
            self.parse_name("package import alias");
            self.finish_node(); // IMPORT_ALIAS
        }
        self.expect(T![;]);
        self.finish_node(); // IMPORT_DECL
    }

    fn parse_struct_def(&mut self, start: Option<Checkpoint>) {
        //eprintln!("parse_struct_def");

        let start = start.unwrap_or(self.checkpoint());

        // parse visibility
        if self.next() == Some(PUBLIC_KW) {
            self.start_node(VISIBILITY);
            self.eat();
            self.finish_node(); // VISIBILITY
        }

        self.start_node_at(start, STRUCT_DEF);
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

    fn parse_attributes(&mut self) {
        while self.next() == Some(T![@]) {
            self.parse_attribute();
        }
    }

    fn parse_struct_field(&mut self) {
        self.start_node(STRUCT_FIELD);
        self.parse_attributes();
        self.parse_type();
        self.parse_name("field name");
        self.expect(T![;]);
        self.finish_node(); // STRUCT_FIELD
    }

    fn parse_fn_param_list(&mut self) {
        self.start_node(PARAM_LIST);
        if !self.expect(L_PAREN) {
            self.finish_node();
            return;
        }

        // TODO: use parse_separated_list?
        let mut progress = Progress::default();
        loop {
            match self.next_ensure_progress(&mut progress) {
                Some(R_PAREN) => {
                    self.eat();
                    break;
                }
                None => {
                    self.syntax_error("unexpected EOF");
                    break;
                }
                _ => {}
            }
            self.parse_fn_parameter();
            if self.next() == Some(COMMA) {
                self.eat();
                continue;
            }
            self.expect_recover(R_PAREN);
            break;
        }

        self.finish_node();
    }

    fn parse_block(&mut self) {
        //eprintln!("parse_block");
        self.start_node(BLOCK);
        self.expect(L_CURLY);
        self.parse_stmt_list();
        self.expect(R_CURLY);
        self.finish_node();
    }

    fn parse_fn_parameter(&mut self) {
        self.start_node(FN_PARAM);
        self.parse_attributes();
        self.parse_type();
        self.parse_name("argument name");
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
                self.parse_name("type name");
                match self.next() {
                    Some(ROW_MAJOR_KW | COLUMN_MAJOR_KW) => {
                        // row_major or column_major qualifier on matrix type
                        self.eat()
                    }
                    _ => {}
                }
                self.finish_node();
                while self.next() == Some(T!['[']) {
                    self.start_node_at(cp, ARRAY_TYPE);
                    self.eat();
                    if self.next() != Some(T![']']) {
                        self.parse_expr();
                    }
                    self.expect(T![']']);
                    if self.next() == Some(STRIDE_KW) {
                        // stride(N) qualifier
                        self.start_node(STRIDE_QUALIFIER);
                        self.eat();
                        self.expect(T!['(']);
                        self.parse_expr();
                        self.expect(T![')']);
                        self.finish_node();
                    }
                    self.finish_node(); // ARRAY_TYPE
                }
            }
            None => {
                self.syntax_error("unexpected EOF");
            }
            _ => {
                self.syntax_error("unexpected EOF");
                // forward progress bump
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
        let mut last_sep_span = self.span();
        let mut item_count = 0;
        let mut progress = Progress::default();
        loop {
            match self.next_ensure_progress(&mut progress) {
                Some(x) if x == end => {
                    if trailing_sep && !allow_trailing {
                        self.syntax_error_at(last_sep_span, "trailing separator not allowed here");
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
                        self.syntax_error("single-element tuple types should have a trailing `,`");
                    }
                    break;
                }
                Some(x) if x == sep => {
                    last_sep_span = self.span();
                    self.eat();
                    trailing_sep = true;
                }
                _ => {
                    // TODO better error message
                    self.syntax_error("syntax error (TODO)");
                    trailing_sep = false;
                }
            }
        }
    }

    /*fn parse_tuple_type(&mut self) {
        self.start_node(TUPLE_TYPE);
        self.expect(T!['(']);
        self.parse_separated_list(T![,], T![')'], true, true, Self::parse_type);
        self.expect(T![')']);
        self.finish_node();
    }*/

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

    /*/// Parses an array type like `float[16][4]`.
    fn parse_array_type(&mut self) {
        self.start_node(ARRAY_TYPE);
        self.parse_type();
        self.expect(T!['[']);
        if self.next() == Some(T![']']) {
            self.parse_expr();
        }
        self.expect(T![']']);
        self.finish_node();
    }*/

    fn parse_constructor(&mut self) {
        self.start_node(CONSTRUCTOR);
        self.parse_type();
        self.parse_call_args();
        self.finish_node();
    }

    fn parse_lit_expr(&mut self) {
        match self.next() {
            Some(INT_NUMBER | FLOAT_NUMBER | STRING | TRUE_KW | FALSE_KW) => {
                self.start_node(LIT_EXPR);
                self.eat();
                self.finish_node();
            }
            _ => {
                self.syntax_error("expected boolean, integer, float or string literal");
                self.eat();
            }
        }
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
            Some(STRING) => {
                self.start_node(LIT_EXPR);
                self.eat();
                self.finish_node();
            }
            Some(IDENT) => {
                if self.next_is_type() {
                    // type constructor
                    self.parse_constructor();
                } else {
                    self.start_node(PATH_EXPR);
                    self.parse_name("path");
                    self.finish_node();
                }
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
                self.syntax_error("syntax error");
                self.eat();
            }
        }
    }

    fn parse_stmt_list(&mut self) {
        //eprintln!("parse_stmt_list");
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
        //eprintln!("parse_stmt");
        match self.next() {
            Some(T!['{']) => {
                self.start_node(BLOCK_STMT);
                self.parse_block();
                self.finish_node(); // BLOCK_STMT
            }
            Some(RETURN_KW) => self.parse_return(),
            Some(IF_KW) => self.parse_if_stmt(),
            Some(BREAK_KW) => self.parse_break_stmt(),
            Some(CONTINUE_KW) => self.parse_continue_stmt(),
            Some(DISCARD_KW) => self.parse_discard_stmt(),
            Some(WHILE_KW) => self.parse_while_stmt(),
            Some(FOR_KW) => self.parse_for_stmt(),
            Some(IDENT) => {
                if self.next_is_type() {
                    // FIXME: this doesn't work with constructors, lol
                    // E.g.:
                    //
                    //       float[3](2.5, 7.0, 1.5);
                    //
                    // first token is a type name, so the rest of the statement will be parsed as a definition, but it's really just a no-op expression statement.
                    // how the fuck are you supposed to parse this shit?
                    //
                    // Note that we could just ignore this particular case, since those expressions are no-ops anyway.
                    self.parse_local_variable_stmt()
                } else {
                    self.parse_expr_stmt()
                }
            }
            Some(_) => self.parse_expr_stmt(),
            _ => self.syntax_error("syntax error"),
        }
    }

    // TODO: declaration lists
    fn parse_local_variable_stmt(&mut self) {
        //eprintln!("parse_local_variable_stmt");
        self.start_node(LOCAL_VARIABLE);
        self.parse_type();
        self.parse_name("variable name");
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
        self.expect(T!['(']);
        self.start_node(CONDITION);
        self.parse_expr();
        self.finish_node(); // finish CONDITION
        self.expect(T![')']);
        self.parse_stmt();
        self.finish_node(); // finish WHILE_STMT
    }

    fn parse_for_stmt(&mut self) {
        self.start_node(FOR_STMT);
        self.expect(FOR_KW);
        self.expect(T!['(']);
        self.start_node(FOR_INIT);
        if self.next() != Some(T![;]) {
            if self.next_is_type() {
                self.parse_local_variable_stmt();
            } else {
                self.parse_expr_stmt();
            }
        }
        self.finish_node(); // finish FOR_INIT
        self.start_node(CONDITION);
        if self.next() != Some(T![;]) {
            self.parse_expr();
        }
        self.expect_recover(T![;]);
        self.finish_node(); // finish CONDITION
        self.start_node(LOOP_EXPR);
        if self.next() != Some(T![')']) {
            self.parse_expr();
        }
        self.finish_node(); // finish LOOP_EXPR
        self.expect_recover(T![')']);
        self.parse_stmt();
        self.finish_node(); // finish FOR_STMT
    }

    fn parse_if_stmt(&mut self) {
        self.start_node(IF_STMT);
        self.expect(IF_KW);
        self.expect(T!['(']);
        self.start_node(CONDITION);
        self.parse_expr();
        self.finish_node();
        self.expect(T![')']);
        self.parse_stmt();
        if self.next() == Some(ELSE_KW) {
            self.start_node(ELSE_BRANCH);
            self.eat();
            self.parse_stmt();
            self.finish_node(); // ELSE_BRANCH
        }
        self.finish_node(); // IF_STMT
    }

    fn parse_expr_stmt(&mut self) {
        //eprintln!("parse_expr_stmt");
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
                _ => self.syntax_error("syntax error (TODO)"),
            }
        }

        self.finish_node();
    }

    fn parse_expr(&mut self) {
        self.parse_expr_bp(0)
    }

    // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn parse_expr_bp(&mut self, min_bp: u8) {
        //eprintln!("parse_expr_bp");
        let cp = self.checkpoint();
        match self.next() {
            Some(op @ T![+] | op @ T![-] | op @ T![!] | op @ T![++] | op @ T![--]) => {
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
                Some(op @ T![++] | op @ T![--]) => {
                    // postfix
                    let l_bp = postfix_binding_power(op);
                    if l_bp < min_bp {
                        break;
                    }
                    self.start_node_at(cp, POSTFIX_EXPR);
                    self.eat();
                    self.finish_node();
                    continue;
                }
                Some(T!['[']) => {
                    // the array index binds closer to everything else, no need to check binding power
                    self.start_node_at(cp, INDEX_EXPR);
                    self.eat();
                    self.parse_expr();
                    self.expect(T![']']);
                    self.finish_node();
                    continue;
                }
                Some(T!['(']) => {
                    self.start_node_at(cp, CALL_EXPR);
                    self.parse_call_args();
                    self.finish_node();
                    continue;
                }
                Some(T![.]) => {
                    self.start_node_at(cp, FIELD_EXPR);
                    self.eat();
                    self.parse_name("field identifier");
                    self.finish_node();
                    continue;
                }
                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }
            if op == T![?] {
                // ternary
                self.start_node_at(cp, TERNARY_EXPR);
                self.start_node_at(cp, CONDITION);
                self.finish_node();
                self.eat();
                let _true_alt = self.parse_expr_bp(0);
                self.expect(T![:]);
                let _false_alt = self.parse_expr_bp(r_bp);
                self.finish_node(); // TERNARY_EXPR
                continue;
            } else {
                self.start_node_at(cp, BIN_EXPR.into());
                self.eat();
                self.parse_expr_bp(r_bp);
                self.finish_node(); // BIN_EXPR
            }
        }
    }
}

fn prefix_binding_power(op: SyntaxKind) -> u8 {
    match op {
        T![+] | T![-] | T![!] => 22,
        T![++] | T![--] => 22,
        _ => panic!("bad op: {:?}", op),
    }
}

fn infix_binding_power(op: SyntaxKind) -> (u8, u8) {
    match op {
        // TODO comma operator
        T![=] | T![+=] | T![-=] | T![*=] | T![/=] | T![%=] | T![&=] | T![|=] | T![^=] | T![<<=] | T![>>=] => (2, 1),
        T![?] => (3, 2),
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

fn postfix_binding_power(op: SyntaxKind) -> u8 {
    match op {
        T![++] => 23,
        T![--] => 23,
        _ => panic!("bad op: {op:?}"),
    }
}
