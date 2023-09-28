use crate::{db::CompilerDb, SourceFileId};
use codespan_reporting::files::{Error, Files};
use rowan::{TextRange, TextSize};
use std::ops::Range;

////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'a> Files<'a> for dyn CompilerDb {
    type FileId = SourceFileId;
    type Name = String;
    type Source = &'a str;

    fn name(&'a self, id: SourceFileId) -> Result<String, Error> {
        let src = self.source_file(id);
        Ok(src.url.clone())
    }

    fn source(&'a self, id: SourceFileId) -> Result<&'a str, Error> {
        let src = self.source_file(id);
        Ok(&src.contents)
    }

    fn line_index(&'a self, id: SourceFileId, byte_index: usize) -> Result<usize, Error> {
        let src = self.source_file(id);
        let text_pos = TextSize::try_from(byte_index).expect("byte index exceeds maximum supported value");
        Ok(src.line_index(text_pos))
    }

    fn line_range(&'a self, id: SourceFileId, line_index: usize) -> Result<Range<usize>, Error> {
        let src = self.source_file(id);
        src.line_range(line_index).ok_or(Error::LineTooLarge {
            given: line_index,
            max: src.line_count(),
        })
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Describes a source location: source file and byte range within the source file.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Span {
    /// Source file.
    pub file: SourceFileId,
    /// Byte range.
    pub range: TextRange,
}

impl Span {
    pub const fn new(file: SourceFileId, range: TextRange) -> Span {
        Span { file, range }
    }
}

/// Types that can provide span information for diagnostic.
pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl<S: Spanned> Spanned for &S {
    fn span(&self) -> Span {
        (*self).span()
    }
}

/*
/// Types that can provide span information for diagnostic.
pub trait DiagnosticSpan {
    /// Returns the source file of the span.
    ///
    /// This can be `None`, in which case the diagnostic should use the "current" source file as a reference
    /// (which is the one that was registered with the last call to  `CompilerDb::push_diagnostic_source_file`).
    fn file(&self) -> Option<SourceFileId>;

    /// Byte range of the span in the source file.
    fn range(&self) -> TextRange;
}

impl DiagnosticSpan for Span {
    fn file(&self) -> Option<SourceFileId> {
        Some(self.file)
    }

    fn range(&self) -> TextRange {
        self.range
    }
}

impl<T: DiagnosticSpan> DiagnosticSpan for &T {
    fn file(&self) -> Option<SourceFileId> {
        (*self).file()
    }

    fn range(&self) -> TextRange {
        (*self).range()
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////

/// Represents a finalized diagnostic.
pub type Diagnostic = CsDiagnostic<SourceFileId>;

/// Diagnostic handlers
pub trait DiagnosticSink {
    fn emit(&self, diagnostic: &Diagnostic, compiler: &dyn CompilerDb);
}

/// The default diag sink that writes to stderr.
pub struct TermDiagnosticSink {
    config: term::Config,
    color_stderr: RefCell<termcolor::StandardStream>,
}

impl TermDiagnosticSink {
    pub fn new(config: term::Config) -> TermDiagnosticSink {
        TermDiagnosticSink {
            config,
            color_stderr: RefCell::new(termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto)),
        }
    }
}

impl DiagnosticSink for TermDiagnosticSink {
    fn emit(&self, diagnostic: &Diagnostic, compiler: &dyn CompilerDb) {
        let mut out = self.color_stderr.borrow_mut();
        term::emit(&mut *out, &self.config, compiler, diagnostic).expect("diagnostic output failed")
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Holds all diagnostic sinks to which emitted diagnostic should be sent, and keeps track of
/// the number of diagnostics emitted of each kind.
pub struct Diagnostics {
    sinks: Vec<Box<dyn DiagnosticSink + Send + 'static>>,
    bug_count: Cell<usize>,
    error_count: Cell<usize>,
    warning_count: Cell<usize>,
}

impl Diagnostics {
    pub fn new() -> Diagnostics {
        Diagnostics {
            sinks: vec![],
            bug_count: Cell::new(0),
            error_count: Cell::new(0),
            warning_count: Cell::new(0),
        }
    }

    /// Removes all diagnostic sinks.
    pub fn clear_sinks(&mut self) {
        self.sinks.clear()
    }

    /// Adds a diagnostic sink.
    pub fn add_sink(&mut self, sink: impl DiagnosticSink + Send + 'static) {
        self.sinks.push(Box::new(sink))
    }

    /// Returns the number of emitted error diagnostics.
    pub fn error_count(&self) -> usize {
        self.error_count.get()
    }

    /// Returns the number of emitted warning diagnostics.
    pub fn warning_count(&self) -> usize {
        self.warning_count.get()
    }

    /// Returns the number of emitted bug diagnostics.
    pub fn bug_count(&self) -> usize {
        self.bug_count.get()
    }
}

/// Used to build and emit diagnostic messages.
#[must_use]
pub struct DiagnosticBuilder<'a> {
    pub(crate) compiler: &'a dyn CompilerDb,
    pub(crate) diag: Diagnostic,
}

impl<'a> DiagnosticBuilder<'a> {
    fn label<S: DiagnosticSpan>(
        mut self,
        span: S,
        style: LabelStyle,
        message: impl Into<String>,
    ) -> DiagnosticBuilder<'a> {
        let range = span.range();
        let source_file = span.file().or_else(|| todo!());

        if let Some(source_file) = source_file {
            self.diag.labels.push(Label {
                style,
                file_id: source_file,
                range: range.start().into()..range.end().into(),
                message: message.into(),
            });
        } else {
            // Downgrade to a note if we can't figure out which file the span refers to
            self.diag.notes.push(message.into())
        }
        self
    }

    /// Sets the primary span of the diagnostic.
    pub fn location<S: DiagnosticSpan>(self, span: S) -> DiagnosticBuilder<'a> {
        self.label(span, LabelStyle::Primary, "")
    }

    /// Sets the primary label of the diagnostic.
    pub fn primary_label<S: DiagnosticSpan>(self, span: S, message: impl Into<String>) -> DiagnosticBuilder<'a> {
        self.label(span, LabelStyle::Primary, message)
    }

    /// Sets the primary label of the diagnostic, only if `loc` is not `None`.
    pub fn primary_label_opt<S: DiagnosticSpan>(
        self,
        span: Option<S>,
        message: impl Into<String>,
    ) -> DiagnosticBuilder<'a> {
        if let Some(span) = span {
            self.label(span, LabelStyle::Primary, message)
        } else {
            self
        }
    }

    pub fn secondary_label<S: DiagnosticSpan>(self, span: S, message: impl Into<String>) -> DiagnosticBuilder<'a> {
        self.label(span, LabelStyle::Secondary, message)
    }

    pub fn note(mut self, message: impl Into<String>) -> DiagnosticBuilder<'a> {
        self.diag.notes.push(message.into());
        self
    }

    pub fn emit(self) {
        /*let d = self.compiler.diag();
        match self.diag.severity {
            Severity::Bug => {
                d.bug_count.set(d.bug_count.get() + 1);
            }
            Severity::Error => {
                d.error_count.set(d.error_count.get() + 1);
            }
            Severity::Warning => {
                d.warning_count.set(d.warning_count.get() + 1);
            }
            _ => {}
        }

        for sink in d.sinks.iter() {
            sink.emit(&self.diag, self.compiler)
        }*/
    }
}
*/

////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Severity {
    Bug,
    Error,
    Warning,
}

pub struct Label {
    pub message: String,
    pub secondary: bool,
    pub span: Span,
}

#[must_use]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub code: Option<String>,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(severity: Severity, message: impl Into<String>) -> Diagnostic {
        Diagnostic {
            severity,
            message: message.into(),
            code: None,
            labels: vec![],
            notes: vec![],
        }
    }

    pub fn bug(message: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Bug, message)
    }

    pub fn warn(message: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Warning, message)
    }

    pub fn error(message: impl Into<String>) -> Diagnostic {
        Diagnostic::new(Severity::Error, message)
    }

    pub fn label<S: Spanned>(mut self, span: S, message: impl Into<String>) -> Diagnostic {
        self.labels.push(Label {
            message: message.into(),
            secondary: false,
            span: span.span(),
        });
        self
    }

    /// Sets the primary span of the diagnostic.
    pub fn span<S: Spanned>(self, span: S) -> Diagnostic {
        self.label(span, "")
    }

    pub fn note(mut self, message: impl Into<String>) -> Diagnostic {
        self.notes.push(message.into());
        self
    }
}

/*
#[macro_export]
macro_rules! diag_span_bug {
    ($span:expr, $($arg:tt)*) => {
        $crate::diagnostic::Diagnostic2::bug(format!($($arg)*)).span(&$span)
    };
}

#[macro_export]
macro_rules! diag_span_error {
    ($span:expr, $($arg:tt)*) => {
        $crate::diagnostic::Diagnostic2::error(format!($($arg)*)).span(&$span)
    };
}

#[macro_export]
macro_rules! diag_span_warn {
    ($span:expr, $($arg:tt)*) => {
        $crate::diagnostic::Diagnostic2::warn(format!($($arg)*)).span(&$span)
    };
}
*/
