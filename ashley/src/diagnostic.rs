use crate::syntax::{FileId, SourceFiles, Span};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label, LabelStyle, Severity},
    term,
    term::termcolor::WriteColor,
};

pub(crate) struct Diagnostics<'a> {
    writer: &'a mut dyn WriteColor,
    config: term::Config,
    files: SourceFiles,
    bug_count: usize,
    error_count: usize,
    warning_count: usize,
}

impl<'a> Diagnostics<'a> {
    pub(crate) fn new(files: SourceFiles, writer: &'a mut dyn WriteColor, config: term::Config) -> Diagnostics {
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

pub(crate) struct DiagnosticBuilder<'a, 'b> {
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

    pub fn primary_label(self, span: impl Into<Option<Span>>, message: impl Into<String>) -> DiagnosticBuilder<'a, 'b> {
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
        term::emit(self.sink.writer, &self.sink.config, &self.sink.files, &self.diag).expect("diagnostic output failed")
    }
}
