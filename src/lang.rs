use crate::syntax_kind::SyntaxKind::*;
use crate::syntax_kind::{Lexer, SyntaxKind};
use codespan_reporting::diagnostic::{Diagnostic, Label, LabelStyle, Severity};
use codespan_reporting::files::{Files, SimpleFiles};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::WriteColor;
use rowan::{GreenNode, GreenNodeBuilder};
use std::collections::HashMap;
use std::ops::Range;
use std::sync::{Arc, Mutex};

type FileId = usize;
type SourceFiles = SimpleFiles<String, Arc<str>>;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    pub file_id: FileId,
    pub start: usize,
    pub end: usize,
}

struct Diagnostics<'a> {
    writer: &'a mut dyn WriteColor,
    config: term::Config,
    files: SourceFiles,
    bug_count: usize,
    error_count: usize,
    warning_count: usize,
}

impl<'a> Diagnostics<'a> {
    fn new(
        files: SourceFiles,
        writer: &'a mut dyn WriteColor,
        config: term::Config,
    ) -> Diagnostics {
        Diagnostics {
            writer,
            config,
            files,
            bug_count: 0,
            error_count: 0,
            warning_count: 0,
        }
    }

    pub fn bug<'b>(&'b mut self, message: impl Into<String>) -> DiagnosticBuilder<'b, 'a> {
        DiagnosticBuilder {
            sink: self,
            diag: Diagnostic::new(Severity::Bug).with_message(message.into()),
        }
    }

    pub fn error<'b>(&'b mut self, message: impl Into<String>) -> DiagnosticBuilder<'b, 'a> {
        DiagnosticBuilder {
            sink: self,
            diag: Diagnostic::new(Severity::Error).with_message(message.into()),
        }
    }

    pub fn error_count(&self) -> usize {
        self.error_count
    }

    pub fn warning_count(&self) -> usize {
        self.warning_count
    }

    pub fn bug_count(&self) -> usize {
        self.bug_count
    }
}

struct DiagnosticBuilder<'a, 'b> {
    sink: &'a mut Diagnostics<'b>,
    diag: Diagnostic<FileId>,
}

impl<'a, 'b> DiagnosticBuilder<'a, 'b> {
    fn label(
        mut self,
        span: impl Into<Option<Span>>,
        style: LabelStyle,
        message: impl Into<String>,
    ) -> DiagnosticBuilder<'a, 'b> {
        if let Some(span) = span.into() {
            self.diag.labels.push(Label {
                style,
                file_id: span.file_id,
                range: span.start..span.end,
                message: message.into(),
            });
        } else {
            self.diag.notes.push(message.into());
        }
        self
    }

    pub fn primary_label(
        self,
        span: impl Into<Option<Span>>,
        message: impl Into<String>,
    ) -> DiagnosticBuilder<'a, 'b> {
        self.label(span, LabelStyle::Primary, message)
    }

    pub fn secondary_label(
        self,
        span: impl Into<Option<Span>>,
        message: impl Into<String>,
    ) -> DiagnosticBuilder<'a, 'b> {
        self.label(span, LabelStyle::Secondary, message)
    }

    pub fn note(mut self, message: impl Into<String>) -> DiagnosticBuilder<'a, 'b> {
        self.diag.notes.push(message.into());
        self
    }

    pub fn emit(mut self) {
        match self.diag.severity {
            Severity::Bug => {
                self.sink.bug_count += 1;
            }
            Severity::Error => {
                self.sink.error_count += 1;
            }
            Severity::Warning => {
                self.sink.warning_count += 1;
            }
            _ => {}
        }
        term::emit(
            self.sink.writer,
            &self.sink.config,
            &self.sink.files,
            &self.diag,
        )
        .expect("diagnostic output failed")
    }
}

//--------------------------------------------------------------------------------------------------

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
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

/// The parse results are stored as a "green tree".
/// We'll discuss working with the results later
pub struct Parse {
    green_node: GreenNode,
}

/// Now, let's write a parser.
/// Note that `parse` does not return a `Result`:
/// by design, syntax tree can be built even for
/// completely invalid source code.
fn parse(text: &str, file_id: FileId, diag: Diagnostics) -> Parse {
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
    fn parse(mut self) -> Parse {
        self.start_node(ROOT);
        while self.current().is_some() {
            self.parse_items();
        }
        self.finish_node();
        Parse {
            green_node: self.b.finish(),
        }
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
        while self.current() == Some(WHITESPACE) {
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
        self.start_node(FUNCTION);
        self.expect(FN_KW);
        self.skip_ws();
        self.expect_ident("function name");
        self.skip_ws();
        self.parse_fn_arg_list();
        self.skip_ws();
        self.parse_block();
        self.finish_node();
    }

    fn parse_fn_arg_list(&mut self) {
        self.start_node(ARG_LIST);
        if !self.expect(L_PAREN) {
            self.finish_node();
            return;
        }

        self.skip_ws();

        loop {
            match self.current() {
                Some(R_PAREN) => {
                    self.bump();
                    break
                },
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
            self.expect(COMMA);
            self.skip_ws();
        }

        self.finish_node();
    }

    fn parse_block(&mut self) {
        // TODO
        self.start_node(BLOCK);
        self.expect(L_BRACE);
        self.skip_ws();
        self.expect(R_BRACE);
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
        self.start_node(TYPE_REF);
        self.expect_ident("type name");
        self.finish_node();
    }

    /*fn try_parse_kw_fn(&mut self) -> bool {
        if self.lex.peek() == Some(Token::Fn) {
            self.lex.
        }
    }*/
}

pub struct Session {
    files: SimpleFiles<String, Arc<str>>,
    parse_results: HashMap<String, Parse>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SourceId(usize);

impl Session {
    pub fn new() -> Session {
        Session {
            files: SimpleFiles::new(),
            parse_results: Default::default(),
        }
    }

    pub fn register_source(&mut self, name: &str, text: impl Into<Arc<str>>) -> SourceId {
        let file_id = self.files.add(name.to_string(), text.into());
        SourceId(file_id)
    }

    pub fn parse(&mut self, source_id: SourceId, diag_writer: &mut dyn WriteColor) -> Parse {
        let source = self.files.source(source_id.0).expect("invalid source ID");
        let diag = Diagnostics::new(self.files.clone(), diag_writer, term::Config::default());
        parse(source, source_id.0, diag)
    }
}

// to parse:
// ROOT
// ITEM
//    FUNCTION
//

// 1. Create a session object, specifying the debug output
// 2. Register source code by calling session.register_source(file_id, contents) -> SourceId
// 3. Given a SourceId, get the syntax tree with session.syntax_tree(sourceId)
// 4.
// 2. Call session.parse(file id, file contents), returns a green node (and puts it in cache)
// 3. Call session.hir(file id

#[cfg(test)]
mod tests {
    use crate::lang::{parse, Diagnostics, Parse, Session, Lang};
    use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
    use rowan::{GreenNode, SyntaxNode};

    fn parse_source_text(text: &str) -> Parse {
        let mut sess = Session::new();
        let src_id = sess.register_source("<input>", text);
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        sess.parse(src_id, &mut writer)
    }

    /*fn dump_syntax(green_node: GreenNode) {
        fn dump_syntax_rec(node: &GreenNode, indent: usize, lines: &mut Vec<usize>, is_last: bool) {
            let mut pad = vec![' '; indent];
            for &p in lines.iter() {
                pad[p] = '│';
            }

            let mut msg: String = pad.into_iter().collect();
            msg += if is_last { "└" } else { "├" };

            let kind = node.kind();
            let span = node.children
            write!(&mut msg, "")
            if let Some(id) = node.id {
                msg += &format!("({:?})", id);
            }
            if let Some(ref content) = node.debug_node.content {
                msg += "  `";
                msg += content;
                msg += "`";
            }
            println!("{}", msg);

            if !is_last {
                lines.push(indent);
            }

            for (i, n) in node.children.iter().enumerate() {
                if i == node.children.len() - 1 {
                    dump_widget_tree_rec(n, indent + 2, lines, true);
                } else {
                    dump_widget_tree_rec(n, indent + 2, lines, false);
                }
            }

            if !is_last {
                lines.pop();
            }
        }
    }*/

    fn parse_source_text_to_syntax(text: &str) -> SyntaxNode<Lang> {
        SyntaxNode::new_root(parse_source_text(text).green_node)
    }

    #[test]
    fn test_parser_basic() {
        insta::assert_debug_snapshot!(parse_source_text_to_syntax(
            r#"
fn main() {
}
        "#
        ));
    }
}
