use anyhow::{anyhow, bail};
use ashley::{
    diagnostic::{Severity, Span},
    Compiler, CompilerDb, LineCharacterRange,
};
use std::{env, fmt::Write, io, io::Stderr};
use tokio::{
    fs::{canonicalize, File},
    sync::{MappedMutexGuard, Mutex, MutexGuard},
};
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};
use tracing::{error, info, trace};
use tracing_subscriber::{fmt::MakeWriter, layer::SubscriberExt, Layer};

////////////////////////////////////////////////////////////////////////////////////////////////////

const LANGUAGE_ID: &str = "glsl2";

// shorthands
const INFO: MessageType = MessageType::INFO;
const WARN: MessageType = MessageType::WARNING;
const ERROR: MessageType = MessageType::ERROR;

/*
/// Returns the canonical module URI.
///
/// FIXME: canonicalization should be part of the compiler, but then it cannot be async, and the compiler would have to "know about" file systems, etc.
/// It should be part of the compiler because URIs in `import` directives must be resolved there. This could be handled by a user-provided resolver,
/// but there's _no way_ that this resolver can be async without considerable damage to the compiler query API.
///
/// Solution: put canonicalization in the compiler. Put compiler stuff in `spawn_blocking`.
///
/// Alternative solution: do not canonicalize. After all, the compiler should not "generate" URIs from path itself. The only thing it does is resolving relative URIs
/// from the base URI specified by the client when creating the source files. As long as those are consistent this shouldn't be a problem.
///
/// The only problem would be when importing the file both via a `package` URI and via an absolute `file` URI. This will resolve to two different files.
///
async fn canonicalize_uri(uri: &Url) -> anyhow::Result<Url> {
    // if no scheme is provided, assume it's "file"
    if uri.scheme() == "file" || uri.scheme() == "" {
        let file_path = uri.to_file_path().map_err(|_| anyhow!("invalid file path"))?;
        let canonical_file_path = canonicalize(&file_path).await?;
        Ok(Url::from_file_path(canonical_file_path).map_err(|_| anyhow!("non-UTF8 file path"))?)
    } else {
        Ok(uri.clone())
    }
}*/

/// Reads the source file at the specified URL.
async fn read_source_file(url: &Url) -> anyhow::Result<String> {
    if url.scheme() == "file" || url.scheme() == "" {
        // canonicalize file path
        let file_path = url.to_file_path().map_err(|_| anyhow!("invalid file path"))?;
        Ok(tokio::fs::read_to_string(&file_path).await?)
    } else {
        // unsupported scheme
        bail!("unsupported URL scheme: `{}`", url);
    }
}

macro_rules! client_info {
    ($client:expr, $($arg:tt)*) => {
        $client.log_message(MessageType::INFO, format!($($arg)*)).await;
    };
}

macro_rules! client_error {
    ($client:expr, $($arg:tt)*) => {
        $client.log_message(MessageType::ERROR, format!($($arg)*)).await;
    };
}
macro_rules! client_warn {
    ($client:expr, $($arg:tt)*) => {
        $client.log_message(MessageType::WARNING, format!($($arg)*)).await;
    };
}

fn span_to_lsp_range(compiler: &dyn CompilerDb, span: Span) -> Range {
    let range: LineCharacterRange = compiler.span_to_line_character_range(span);
    let start_line: u32 = range.start.line.try_into().expect("line index overflow");
    let end_line: u32 = range.end.line.try_into().expect("line index overflow");
    let start_offset: u32 = range
        .start
        .character_offset
        .try_into()
        .expect("character offset overflow");
    let end_offset: u32 = range
        .end
        .character_offset
        .try_into()
        .expect("character offset overflow");
    Range {
        start: Position::new(start_line, start_offset),
        end: Position::new(end_line, end_offset),
    }
}

fn diagnostic_to_lsp(db: &dyn CompilerDb, diag: ashley::diagnostic::Diagnostic) -> Diagnostic {
    let range = if let Some(span) = diag.get_span() {
        span_to_lsp_range(db, span)
    } else {
        Default::default()
    };

    let severity = match diag.severity {
        Severity::Bug => DiagnosticSeverity::ERROR,
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
    };

    let related_information = if !diag.labels.is_empty() {
        let mut infos = vec![];
        for label in diag.labels {
            let uri = &db.source_file(label.span.file).url.clone();
            let range = span_to_lsp_range(db, label.span);
            infos.push(DiagnosticRelatedInformation {
                location: Location {
                    uri: Url::parse(uri).unwrap(),
                    range,
                },
                message: label.message.to_string(),
            })
        }
        Some(infos)
    } else {
        None
    };

    let mut message = diag.message;
    for note in diag.notes {
        write!(message, "\nnote: {note}").unwrap();
    }

    Diagnostic {
        range,
        severity: Some(severity),
        code: None,
        code_description: None,
        source: Some("ashley-lsp".to_string()),
        message,
        related_information,
        tags: None,
        data: None,
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

struct Backend {
    client: Client,
    compiler: Mutex<ashley::Compiler>,
}

impl Backend {
    /*async fn compiler(&self) -> MappedMutexGuard<dyn CompilerDb> {
        MutexGuard::map(
            self.compiler.lock().await,
            |compiler: &mut Compiler| -> &mut dyn CompilerDb { compiler },
        )
    }*/
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let initialize_result = InitializeResult {
            capabilities: ServerCapabilities {
                position_encoding: None,
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                selection_range_provider: None,
                hover_provider: None,
                completion_provider: None,
                signature_help_provider: None,
                definition_provider: None,
                type_definition_provider: None,
                implementation_provider: None,
                references_provider: None,
                document_highlight_provider: None,
                document_symbol_provider: Some(OneOf::Right(DocumentSymbolOptions { label: None, work_done_progress_options: Default::default() })),
                workspace_symbol_provider: None,
                code_action_provider: None,
                code_lens_provider: None,
                document_formatting_provider: None,
                document_range_formatting_provider: None,
                document_on_type_formatting_provider: None,
                rename_provider: None,
                document_link_provider: None,
                color_provider: None,
                folding_range_provider: None,
                declaration_provider: None,
                execute_command_provider: None,
                workspace: None,
                call_hierarchy_provider: None,
                semantic_tokens_provider: /*Some(SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                    SemanticTokensRegistrationOptions {
                        text_document_registration_options: TextDocumentRegistrationOptions {
                            document_selector: Some(vec![DocumentFilter {
                                language: Some("glsl2".to_string()),
                                scheme: Some("file".to_string()),
                                pattern: None,
                            }]),
                        },
                        semantic_tokens_options: SemanticTokensOptions {
                            work_done_progress_options: Default::default(),
                            legend: Default::default(),
                            range: Some(true),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                        static_registration_options: Default::default(),
                    },
                ))*/ None,
                moniker_provider: None,
                linked_editing_range_provider: None,
                inline_value_provider: None,
                inlay_hint_provider: None,
                diagnostic_provider: None,
                experimental: None,
            },
            server_info: None,
        };
        Ok(initialize_result)
    }

    async fn initialized(&self, _: InitializedParams) {
        client_info!(self.client, "server initialized!");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // did_open
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let lang_id = params.text_document.language_id;

        client_info!(self.client, "file opened! uri={uri} lang={lang_id}");

        if lang_id == LANGUAGE_ID {
            match read_source_file(&uri).await {
                Ok(contents) => {
                    let mut compiler = self.compiler.lock().await;
                    let compiler: &mut dyn CompilerDb = &mut *compiler;
                    let (module, source_file) = compiler.create_module_from_source_file(uri.as_str(), &contents);
                    client_info!(
                        self.client,
                        "successfully created module `{uri}` (module ID {module:?}, source file ID {source_file:?})"
                    );
                }
                Err(err) => {
                    client_error!(self.client, "could not open source file `{uri}` : {err}");
                }
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // did_change
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut compiler = self.compiler.lock().await;
        let module = compiler.module_id(params.text_document.uri.to_string());
        let Some(source_file) = compiler.module_data(module).source else {
            client_info!(
                self.client,
                "BUG: module `{}` does not have a source file",
                params.text_document.uri
            );
            return;
        };

        let compiler: &mut dyn CompilerDb = &mut *compiler;

        compiler.update_source_file_contents(source_file, &params.content_changes[0].text);

        let all_diagnostics: Vec<_> = compiler
            .module_diagnostics(module)
            .into_iter()
            .map(|d| diagnostic_to_lsp(compiler, d))
            .collect();

        self.client
            .publish_diagnostics(
                params.text_document.uri,
                all_diagnostics,
                Some(params.text_document.version),
            )
            .await;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // DOCUMENT SYMBOLS
    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        trace!("document_symbol");
        let mut compiler = self.compiler.lock().await;
        let db: &dyn CompilerDb = &*compiler;
        let module = db.module_id(params.text_document.uri.to_string());

        let scope = db.module_scope(module);
        let mut document_symbols = vec![];

        for (name, strukt) in scope.structs() {
            let ast = strukt.ast_node(db);
            let range = span_to_lsp_range(db, ast.span());
            document_symbols.push(DocumentSymbol {
                name: name.to_string(),
                detail: None,
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                range,
                selection_range: range,
                children: None,
            });
        }

        for (name, function) in scope.functions() {
            let ast = function.ast_node(db);
            let range = span_to_lsp_range(db, ast.span());
            //trace!("document_symbol: function {name} range {range:?}");
            document_symbols.push(DocumentSymbol {
                name: name.to_string(),
                detail: None,
                kind: SymbolKind::FUNCTION,
                tags: None,
                deprecated: None,
                range,
                selection_range: range,
                children: None,
            });
        }

        for (name, global) in scope.globals() {
            let ast = global.ast_node(db);
            let range = span_to_lsp_range(db, ast.span());
            //trace!("document_symbol: global {name} range {range:?}");
            document_symbols.push(DocumentSymbol {
                name: name.to_string(),
                detail: None,
                kind: SymbolKind::VARIABLE,
                tags: None,
                deprecated: None,
                range,
                selection_range: range,
                children: None,
            });
        }

        Ok(Some(DocumentSymbolResponse::Nested(document_symbols)))
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

struct MakeWriterStderr;

impl MakeWriter<'_> for MakeWriterStderr {
    type Writer = Stderr;

    fn make_writer(&self) -> Self::Writer {
        io::stderr()
    }
}

#[tokio::main]
async fn main() {
    //tracing::subscriber::set_global_default(tracing_subscriber::registry().with(tracing_tracy::TracyLayer::new()))
    //    .expect("set up the subscriber");

    let subscriber = tracing_subscriber::Registry::default().with(
        tracing_tree::HierarchicalLayer::new(4)
            .with_bracketed_fields(true)
            .with_writer(io::stderr)
            .with_timer(())
            .with_filter(tracing_subscriber::EnvFilter::from_default_env()),
    );
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    info!("Ashley LSP server");

    let (service, socket) = LspService::new(|client| Backend {
        client,
        compiler: Mutex::new(ashley::Compiler::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
