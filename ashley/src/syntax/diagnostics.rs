use crate::diagnostic::Span;

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
