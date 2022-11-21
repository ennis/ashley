use crate::{
    diagnostic::Diagnostics,
    syntax,
    syntax::{
        ast::{AstNode, Module},
        SyntaxNode,
    },
};
use codespan_reporting::{
    files::{Files, SimpleFiles},
    term,
    term::termcolor::WriteColor,
};
use std::{collections::HashMap, sync::Arc};

pub(crate) type FileId = usize;
pub(crate) type SourceFiles = SimpleFiles<String, Arc<str>>;

/// Identifies a source file in a compilation session.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct SourceId(usize);

/// Represents a syntax session.
#[derive(Debug)]
pub struct Session {
    files: SourceFiles,
    parse_results: HashMap<String, SyntaxNode>,
}

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

    pub fn parse(&mut self, source_id: SourceId, diag_writer: &mut dyn WriteColor) -> SyntaxNode {
        let source = self.files.source(source_id.0).expect("invalid source ID");
        let diag = Diagnostics::new(self.files.clone(), diag_writer, term::Config::default());
        syntax::parse(source, source_id.0, diag)
    }

    /// Returns the AST for the specified source.
    pub fn ast(&mut self, source_id: SourceId, diag_writer: &mut dyn WriteColor) -> Option<Module> {
        let syntax_node = self.parse(source_id, diag_writer);
        Module::cast(syntax_node)
    }
}
