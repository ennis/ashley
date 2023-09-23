use crate::{
    diagnostic::{Diagnostic, Span},
    CompilerDb,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum SyntaxDiagnostic {
    // TODO more precise diagnostics
    Expected { span: Span, message: String },
    SyntaxError { span: Span, message: String },
}

impl SyntaxDiagnostic {
    pub fn expected(span: Span, message: impl Into<String>) -> SyntaxDiagnostic {
        SyntaxDiagnostic::Expected {
            span,
            message: message.into(),
        }
    }

    pub fn syntax_error(span: Span, message: impl Into<String>) -> SyntaxDiagnostic {
        SyntaxDiagnostic::SyntaxError {
            span,
            message: message.into(),
        }
    }
}

impl SyntaxDiagnostic {
    pub fn render(&self, compiler: &dyn CompilerDb) -> Diagnostic {
        match self {
            SyntaxDiagnostic::Expected { span, message } => Diagnostic::error(message).span(span),
            SyntaxDiagnostic::SyntaxError { span, message } => Diagnostic::error(message).span(span),
        }
    }
}
