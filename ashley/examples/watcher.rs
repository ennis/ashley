use anyhow::Result;
use ashley::{
    diagnostic::{Diagnostic, Severity},
    syntax::SyntaxDiagnostic,
    termcolor, Compiler, CompilerDb, ModuleId, SourceFileId,
};
use codespan_reporting::{diagnostic::Label, files::Files, term};
use notify::{Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use rowan::TextSize;
use std::{
    fs,
    fs::File,
    io,
    io::Read,
    ops::Range,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};
use tracing::{error, trace};
use tracing_subscriber::{layer::SubscriberExt, Layer};

const TESTBENCH_PATH: &str = "data/shaders/testbench.glsl";

////////////////////////////////////////////////////////////////////////////////////////////////////

struct CompilerDbFiles<'a>(&'a dyn CompilerDb);

impl<'a> Files<'a> for CompilerDbFiles<'a> {
    type FileId = SourceFileId;
    type Name = String;
    type Source = &'a str;

    fn name(&'a self, id: SourceFileId) -> Result<String, codespan_reporting::files::Error> {
        let src = self.0.source_file(id);
        Ok(src.url.clone())
    }

    fn source(&'a self, id: SourceFileId) -> Result<&'a str, codespan_reporting::files::Error> {
        let src = self.0.source_file(id);
        Ok(&src.contents)
    }

    fn line_index(&'a self, id: SourceFileId, byte_index: usize) -> Result<usize, codespan_reporting::files::Error> {
        let src = self.0.source_file(id);
        let text_pos = TextSize::try_from(byte_index).expect("byte index exceeds maximum supported value");
        Ok(src.line_index(text_pos))
    }

    fn line_range(
        &'a self,
        id: SourceFileId,
        line_index: usize,
    ) -> Result<Range<usize>, codespan_reporting::files::Error> {
        let src = self.0.source_file(id);
        src.line_range(line_index)
            .ok_or(codespan_reporting::files::Error::LineTooLarge {
                given: line_index,
                max: src.line_count(),
            })
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Session {
    compiler: Mutex<Compiler>,
    source_file: SourceFileId,
    module: ModuleId,
}

fn emit_diagnostic(db: &dyn CompilerDb, diag: Diagnostic) {
    let sev = match diag.severity {
        Severity::Bug => codespan_reporting::diagnostic::Severity::Bug,
        Severity::Error => codespan_reporting::diagnostic::Severity::Error,
        Severity::Warning => codespan_reporting::diagnostic::Severity::Warning,
    };
    let mut cs_diag = codespan_reporting::diagnostic::Diagnostic::new(sev);
    cs_diag.message = diag.message;
    for label in diag.labels {
        let range: Range<usize> = label.span.range.start().into()..label.span.range.end().into();
        let style = if label.secondary {
            codespan_reporting::diagnostic::LabelStyle::Secondary
        } else {
            codespan_reporting::diagnostic::LabelStyle::Primary
        };
        cs_diag
            .labels
            .push(Label::new(style, label.span.file, range).with_message(label.message));
    }

    let mut color_stderr = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);
    term::emit(
        &mut color_stderr,
        &codespan_reporting::term::Config::default(),
        &CompilerDbFiles(db),
        &cs_diag,
    )
    .unwrap()
}

fn recompile(db: &dyn CompilerDb, module: ModuleId, source_file: SourceFileId) {
    db.module_items(module);

    // display syntax errors
    for diag in db.syntax_diagnostics(source_file) {
        emit_diagnostic(db, diag.render(db));
    }

    // dump struct field types
    let module_scope = db.module_scope(module);
    for struct_id in module_scope.structs() {
        let field_types = db.struct_field_types(struct_id);

        /*for diag in diags {
            emit_diagnostic(db, diag.render(db));
        }*/

        let sd = db.struct_data(struct_id);
        let name = &sd.name;
        let visibility = sd.visibility;
        trace!("=== struct `{name}` ({visibility:?}) ===");
        let field_names = sd.fields.iter().map(|f| &f.name);
        for (ty, name) in field_types.iter().zip(field_names) {
            trace!("    {name}: {ty}")
        }
    }

    for function_id in module_scope.functions() {
        //let body = db.function_body(function_id);
        //let body_map = db.function_body_map(function_id);
        let ty_body = db.ty_function_body(function_id);
        let func_data = db.function_data(function_id);
        trace!("=== Function `{}` ===", func_data.name);
        trace!("{ty_body:#?}")
    }
}

impl Session {
    pub fn new() -> Session {
        let mut compiler = Compiler::new();
        let db: &mut dyn CompilerDb = &mut compiler;
        let contents = fs::read_to_string(TESTBENCH_PATH).expect("failed to read file");
        let (module, source_file) = db.create_module_from_source_file(TESTBENCH_PATH, &contents);
        db.module_items(module);

        Session {
            compiler: Mutex::new(compiler),
            source_file,
            module,
        }
    }

    pub fn update(&self) {
        let mut compiler = self.compiler.lock().unwrap();
        let db: &mut dyn CompilerDb = &mut *compiler;
        let contents = fs::read_to_string(TESTBENCH_PATH).expect("failed to read file");
        db.update_source_file_contents(self.source_file, &contents);
        recompile(db, self.module, self.source_file);
    }
}

fn main() -> Result<()> {
    let subscriber = tracing_subscriber::Registry::default().with(
        tracing_tree::HierarchicalLayer::new(4)
            .with_ansi(true)
            .with_bracketed_fields(true)
            .with_writer(io::stderr)
            .with_timer(())
            .with_filter(tracing_subscriber::EnvFilter::from_default_env()),
    );
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let session = Arc::new(Session::new());

    let mut watcher = notify::PollWatcher::new(
        move |res| match res {
            Ok(event) => {
                trace!("file changed: {event:?}");
                session.update()
            }
            Err(e) => error!("watch error: {}", e),
        },
        Config::default().with_poll_interval(Duration::from_millis(500)),
    )?;

    watcher.watch(TESTBENCH_PATH.as_ref(), RecursiveMode::NonRecursive)?;

    thread::park();
    Ok(())
}
