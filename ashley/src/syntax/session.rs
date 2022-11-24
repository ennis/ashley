use crate::{
    diagnostic::{Diagnostics, SourceFileProvider, SourceId},
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
