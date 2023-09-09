use codespan_reporting::files::{line_starts, Error};
use std::ops::Range;

#[derive(Clone, Debug)]
pub struct SourceFile {
    /// Canonical URL of the source file.
    pub url: String,
    /// Contents of the source file.
    pub contents: String,
    /// Line map: byte offset of the start of each line.
    pub line_starts: Vec<usize>,
}

// FIXME Default impl is only there for use in InputTable
impl Default for SourceFile {
    fn default() -> Self {
        SourceFile {
            url: "".to_string(),
            contents: "".to_string(),
            line_starts: vec![],
        }
    }
}

impl PartialEq for SourceFile {
    fn eq(&self, other: &Self) -> bool {
        // We don't need to compare `line_starts` since it's derived from the contents.
        self.contents == other.contents && self.url == other.url
    }
}

impl Eq for SourceFile {}

impl SourceFile {
    /// Creates a new `SourceFile` with the given canonical URL, and file contents.
    ///
    /// Also computes the line start offsets of the contents.
    pub fn new(canonical_url: impl Into<String>, contents: impl Into<String>) -> SourceFile {
        let contents = contents.into();
        let line_starts = line_starts(&contents).collect();
        SourceFile {
            url: canonical_url.into(),
            contents,
            line_starts,
        }
    }

    /// Updates the contents of the source file.
    pub fn update(&mut self, new_contents: impl Into<String>) {
        self.contents = new_contents.into();
        self.line_starts = line_starts(&self.contents).collect();
    }

    /// Returns the number of lines.
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Returns the starting byte index of the given line.
    ///
    /// In addition, if `line_index == self.num_lines()`, the function returns returns the length of the string in bytes.
    /// Otherwise, returns `None` if there's no such line.
    pub fn line_start(&self, line_index: usize) -> Option<usize> {
        if line_index < self.line_starts.len() {
            Some(self.line_starts[line_index])
        } else if line_index == self.line_starts.len() {
            Some(self.contents.len())
        } else {
            None
        }
    }

    /// Returns the line corresponding to the given byte index.
    pub fn line_index(&self, byte_index: usize) -> usize {
        self.line_starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1)
    }

    /// Returns the byte range for the given line.
    ///
    /// Returns `None` if there's no such line.
    pub fn line_range(&self, line_index: usize) -> Option<Range<usize>> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + 1)?;
        Some(line_start..next_line_start)
    }
}
