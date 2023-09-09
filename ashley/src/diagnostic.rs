use crate::{
    session::{CompilerDb, SourceFileId},
    termcolor,
};
use codespan_reporting::{
    diagnostic::{Diagnostic as CsDiagnostic, Label, LabelStyle, Severity},
    files::{Error, Files},
    term,
};
use rowan::TextRange;
use std::{
    cell::{Cell, RefCell},
    ops::Range,
};

/// Represents a finalized diagnostic.
pub type Diagnostic = CsDiagnostic<SourceFileId>;

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
        Ok(src.line_index(byte_index))
    }

    fn line_range(&'a self, id: SourceFileId, line_index: usize) -> Result<Range<usize>, Error> {
        let src = self.source_file(id);
        src.line_range(line_index).ok_or(Error::LineTooLarge {
            given: line_index,
            max: src.line_count(),
        })
    }
}

/// Describes a source location.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SourceLocation {
    /// Module ID, or `None` to use the current source.
    pub file: Option<SourceFileId>,
    /// Byte range start
    pub range: TextRange,
}

impl SourceLocation {
    pub fn new(file: Option<SourceFileId>, range: TextRange) -> SourceLocation {
        SourceLocation { file, range }
    }
}

/// FIXME: replace by `From<X> for SourceLocation`?
pub trait AsSourceLocation {
    fn source_location(&self) -> SourceLocation;
}

impl AsSourceLocation for SourceLocation {
    fn source_location(&self) -> SourceLocation {
        *self
    }
}

impl<T> AsSourceLocation for &T
where
    T: AsSourceLocation,
{
    fn source_location(&self) -> SourceLocation {
        (*self).source_location()
    }
}

/*impl Default for SourceLocation {
    fn default() -> Self {
        SourceLocation {
            file: None,
            start: 0,
            end: 0,
        }
    }
}*/

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

struct DiagnosticsInner {
    sinks: Vec<Box<dyn DiagnosticSink + Send + 'static>>,
    bug_count: Cell<usize>,
    error_count: Cell<usize>,
    warning_count: Cell<usize>,
}

pub struct Diagnostics {
    // TODO remove this, it doesn't serve any purpose now
    inner: DiagnosticsInner,
}

impl Diagnostics {
    pub fn new() -> Diagnostics {
        Diagnostics {
            inner: DiagnosticsInner {
                sinks: vec![],
                bug_count: Cell::new(0),
                error_count: Cell::new(0),
                warning_count: Cell::new(0),
            },
        }
    }

    /// Removes all diagnostic sinks.
    pub fn clear_sinks(&mut self) {
        self.inner.sinks.clear()
    }

    /// Adds a diagnostic sink.
    pub fn add_sink(&mut self, sink: impl DiagnosticSink + Send + 'static) {
        self.inner.sinks.push(Box::new(sink))
    }

    /// Returns the number of emitted error diagnostics.
    pub fn error_count(&self) -> usize {
        self.inner.error_count.get()
    }

    /// Returns the number of emitted warning diagnostics.
    pub fn warning_count(&self) -> usize {
        self.inner.warning_count.get()
    }

    /// Returns the number of emitted bug diagnostics.
    pub fn bug_count(&self) -> usize {
        self.inner.bug_count.get()
    }
}

/// Used to build and emit diagnostic messages.
#[must_use]
pub struct DiagnosticBuilder<'a> {
    pub(crate) compiler: &'a dyn CompilerDb,
    pub(crate) diag: Diagnostic,
}

impl<'a> DiagnosticBuilder<'a> {
    fn label<L: AsSourceLocation>(
        mut self,
        loc: L,
        style: LabelStyle,
        message: impl Into<String>,
    ) -> DiagnosticBuilder<'a> {
        let span = loc.source_location();
        let file_id = span.file.or_else(|| self.compiler.diagnostic_source_file());
        if let Some(file_id) = file_id {
            self.diag.labels.push(Label {
                style,
                file_id,
                range: span.range.start().into()..span.range.end().into(),
                message: message.into(),
            });
        } else {
            // Downgrade to a note if we can't figure out which file it is
            self.diag.notes.push(message.into())
        }
        self
    }

    /// Sets the primary label of the diagnostic.

    pub fn location<L: AsSourceLocation>(self, loc: L) -> DiagnosticBuilder<'a> {
        self.label(loc, LabelStyle::Primary, "")
    }

    /// Sets the primary label of the diagnostic.
    pub fn primary_label<L: AsSourceLocation>(self, loc: L, message: impl Into<String>) -> DiagnosticBuilder<'a> {
        self.label(loc, LabelStyle::Primary, message)
    }

    /// Sets the primary label of the diagnostic, only if `loc` is not `None`.
    pub fn primary_label_opt<L: AsSourceLocation>(
        self,
        loc: Option<L>,
        message: impl Into<String>,
    ) -> DiagnosticBuilder<'a> {
        if let Some(loc) = loc {
            self.label(loc, LabelStyle::Primary, message)
        } else {
            self
        }
    }

    pub fn secondary_label<L: AsSourceLocation>(self, loc: L, message: impl Into<String>) -> DiagnosticBuilder<'a> {
        self.label(loc, LabelStyle::Secondary, message)
    }

    pub fn note(mut self, message: impl Into<String>) -> DiagnosticBuilder<'a> {
        self.diag.notes.push(message.into());
        self
    }

    pub fn emit(self) {
        let d = self.compiler.diag();
        match self.diag.severity {
            Severity::Bug => {
                d.inner.bug_count.set(d.inner.bug_count.get() + 1);
            }
            Severity::Error => {
                d.inner.error_count.set(d.inner.error_count.get() + 1);
            }
            Severity::Warning => {
                d.inner.warning_count.set(d.inner.warning_count.get() + 1);
            }
            _ => {}
        }

        for sink in d.inner.sinks.iter() {
            sink.emit(&self.diag, self.compiler)
        }
    }
}
