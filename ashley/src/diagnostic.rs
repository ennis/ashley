use codespan_reporting::{
    diagnostic::{Diagnostic as CsDiagnostic, Label, LabelStyle, Severity},
    files::{line_starts, Error, Files},
    term,
    term::termcolor::WriteColor,
};
use rowan::TextRange;
use std::{
    collections::HashMap,
    ops::Range,
    sync::{Arc, Mutex, RwLock},
};

/// Uniquely identifies a file registered to a `SourceFileProvider`
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct SourceId(usize);

/// A source file with a line map.
#[derive(Debug)]
struct SourceFile {
    text: Arc<str>,
    name: String,
    /// The starting byte indices in the source code.
    line_starts: Vec<usize>,
}

impl SourceFile {
    /// Return the starting byte index of the line with the specified line index.
    /// Convenience method that already generates errors if necessary.
    fn line_start(&self, line_index: usize) -> Result<usize, Error> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.line_starts.len()) {
            Ordering::Less => Ok(self
                .line_starts
                .get(line_index)
                .cloned()
                .expect("failed despite previous check")),
            Ordering::Equal => Ok(self.text.as_ref().len()),
            Ordering::Greater => Err(Error::LineTooLarge {
                given: line_index,
                max: self.line_starts.len() - 1,
            }),
        }
    }

    fn line_index(&self, byte_index: usize) -> usize {
        self.line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1)
    }

    fn line_range(&self, line_index: usize) -> Result<Range<usize>, Error> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + 1)?;
        Ok(line_start..next_line_start)
    }
}

#[derive(Debug)]
struct SourceFileProviderInner {
    files: Vec<SourceFile>,
    files_by_path: HashMap<String, usize>,
}

/// Provides access to source files by ID and by path, for use with diagnostics.
#[derive(Clone, Debug)]
pub struct SourceFileProvider(Arc<RwLock<SourceFileProviderInner>>);

impl SourceFileProvider {
    pub fn new() -> SourceFileProvider {
        SourceFileProvider(Arc::new(RwLock::new(SourceFileProviderInner {
            files: vec![],
            files_by_path: Default::default(),
        })))
    }

    /// Registers a source file with the specified name and contents.
    ///
    /// Returns the corresponding source ID.
    ///
    /// # Notes
    ///
    /// This method always adds a new entry with a new `SourceId`, even if a file with the same name
    /// has already been registered.
    pub fn register_source(&self, name: impl Into<String>, text: impl Into<Arc<str>>) -> SourceId {
        let mut inner = self.0.write().unwrap();
        let name = name.into();
        let text = text.into();
        let source_id = SourceId(inner.files.len());
        let line_starts = line_starts(&text).collect();
        inner.files.push(SourceFile {
            text,
            name,
            line_starts,
        });
        source_id
    }
}

impl<'a> Files<'a> for SourceFileProvider {
    type FileId = SourceId;
    type Name = String;
    type Source = Arc<str>;

    fn name(&'a self, id: SourceId) -> Result<String, Error> {
        let inner = self.0.read().unwrap();
        inner
            .files
            .get(id.0)
            .map(|source| source.name.clone())
            .ok_or(Error::FileMissing)
    }

    fn source(&'a self, id: SourceId) -> Result<Arc<str>, Error> {
        let inner = self.0.read().unwrap();
        inner
            .files
            .get(id.0)
            .map(|source| source.text.clone())
            .ok_or(Error::FileMissing)
    }

    fn line_index(&'a self, id: SourceId, byte_index: usize) -> Result<usize, Error> {
        let inner = self.0.read().unwrap();
        Ok(inner.files.get(id.0).ok_or(Error::FileMissing)?.line_index(byte_index))
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Result<Range<usize>, Error> {
        let inner = self.0.read().unwrap();
        inner.files.get(id.0).ok_or(Error::FileMissing)?.line_range(line_index)
    }
}

/// Describes a source location.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SourceLocation {
    /// Source file ID, as returned by `SourceFileProvider`, or `None` to use the default source.
    pub file: Option<SourceId>,
    /// Byte range start
    pub range: TextRange,
}

impl SourceLocation {
    pub fn new(file: Option<SourceId>, range: TextRange) -> SourceLocation {
        SourceLocation { file, range }
    }
}

pub trait AsSourceLocation {
    fn source_location(&self) -> SourceLocation;
}

impl AsSourceLocation for SourceLocation {
    fn source_location(&self) -> SourceLocation {
        *self
    }
}

impl<T> AsSourceLocation for &T where T: AsSourceLocation {
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

struct DiagnosticsInner {
    writer: Box<dyn WriteColor>,
    //current_source:
    bug_count: usize,
    error_count: usize,
    warning_count: usize,
}

pub struct Diagnostics {
    config: term::Config,
    files: SourceFileProvider,
    default_source: SourceId,
    inner: parking_lot::Mutex<DiagnosticsInner>,
}

impl Diagnostics {
    pub(crate) fn new(
        files: SourceFileProvider,
        default_source: SourceId,
        writer: impl WriteColor + 'static,
        config: term::Config,
    ) -> Diagnostics {
        Diagnostics {
            files,
            config,
            default_source,
            inner: parking_lot::Mutex::new(DiagnosticsInner {
                writer: Box::new(writer),
                bug_count: 0,
                error_count: 0,
                warning_count: 0,
            }),
        }
    }

    /// Creates a new diagnostic with `Bug` severity.
    ///
    /// Emit the diagnostic by calling `DiagnosticBuilder::emit`.
    ///
    /// # Example
    /// ```
    ///# fn main() {
    ///diag.bug("Internal compiler error").emit();
    ///# }
    /// ```
    ///
    ///
    pub fn bug(&self, message: impl Into<String>) -> DiagnosticBuilder {
        DiagnosticBuilder {
            sink: self,
            diag: CsDiagnostic::new(Severity::Bug).with_message(message.into()),
        }
    }

    /// Creates a new diagnostic with `Warning` severity.
    ///
    /// Emit the diagnostic by calling `DiagnosticBuilder::emit`.
    pub fn warn(&self, message: impl Into<String>) -> DiagnosticBuilder {
        DiagnosticBuilder {
            sink: self,
            diag: CsDiagnostic::new(Severity::Warning).with_message(message.into()),
        }
    }

    /// Creates a new diagnostic with `Error` severity.
    ///
    /// Emit the diagnostic by calling `DiagnosticBuilder::emit`.
    pub fn error(&self, message: impl Into<String>) -> DiagnosticBuilder {
        DiagnosticBuilder {
            sink: self,
            diag: CsDiagnostic::new(Severity::Error).with_message(message.into()),
        }
    }

    /// Returns the number of emitted error diagnostics.
    pub fn error_count(&self) -> usize {
        self.inner.lock().error_count
    }

    /// Returns the number of emitted warning diagnostics.
    pub fn warning_count(&self) -> usize {
        self.inner.lock().warning_count
    }

    /// Returns the number of emitted bug diagnostics.
    pub fn bug_count(&self) -> usize {
        self.inner.lock().bug_count
    }
}

/// Used to build and emit diagnostic messages.
pub struct DiagnosticBuilder<'a> {
    sink: &'a Diagnostics,
    diag: CsDiagnostic<SourceId>,
}

impl<'a> DiagnosticBuilder<'a> {
    fn label<L: AsSourceLocation>(
        mut self,
        loc: L,
        style: LabelStyle,
        message: impl Into<String>,
    ) -> DiagnosticBuilder<'a> {
        let span = loc.source_location();
        let file_id = span.file.unwrap_or(self.sink.default_source);
        self.diag.labels.push(Label {
            style,
            file_id,
            range: span.range.start().into()..span.range.end().into(),
            message: message.into(),
        });
        self
    }

    /// Sets the primary label of the diagnostic.
    pub fn primary_label<L: AsSourceLocation>(self, loc: L, message: impl Into<String>) -> DiagnosticBuilder<'a> {
        self.label(loc, LabelStyle::Primary, message)
    }

    /// Sets the primary label of the diagnostic, only if `loc` is not `None`.
    pub fn primary_label_opt(mut self, loc: Option<SourceLocation>, message: impl Into<String>) -> DiagnosticBuilder<'a> {
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

    pub fn emit(mut self) {
        let mut inner = self.sink.inner.lock();
        match self.diag.severity {
            Severity::Bug => {
                inner.bug_count += 1;
            }
            Severity::Error => {
                inner.error_count += 1;
            }
            Severity::Warning => {
                inner.warning_count += 1;
            }
            _ => {}
        }
        term::emit(&mut inner.writer, &self.sink.config, &self.sink.files, &self.diag)
            .expect("diagnostic output failed")
    }
}

/// Diagnostic object in the process of being built.
///
/// It must be reported to a diagnostic sink (see `Diagnostics::emit`).
pub struct Diagnostic {
    diag: CsDiagnostic<SourceId>,
}
