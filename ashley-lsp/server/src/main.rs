use anyhow::{anyhow, bail};
use ashley::{tast::DefKind, Compiler, CompilerDb, LineCharacterRange};
use std::io;
use tokio::{
    fs::{canonicalize, File},
    sync::{MappedMutexGuard, Mutex, MutexGuard},
};
use tower_lsp::{jsonrpc::Result, lsp_types::*, Client, LanguageServer, LspService, Server};

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
        let module = compiler.module_id(params.text_document.uri.as_str());
        let Some(source_file) = compiler.module_data(module).source else {
            client_info!(self.client,"BUG: module `{}` does not have a source file", params.text_document.uri);
            return
        };
        compiler.update_source_file(source_file, params.content_changes[0].text.clone());
        /*self.client
        .log_message(
            INFO,
            format!("text document changed! uri={uri}, version={version}, `{text}`"),
        )
        .await;*/
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    // DOCUMENT SYMBOLS
    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        let mut compiler = self.compiler.lock().await;
        let compiler: &dyn CompilerDb = &*compiler;
        let module = compiler.module_id(params.text_document.uri.as_str());
        let definitions = compiler.module_definitions(module);

        let mut document_symbols = vec![];
        for &def in definitions.iter() {
            let def_info = compiler.definition(def);
            let Some(span) = def_info.span else { continue };

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
            let range = Range {
                start: Position::new(start_line, start_offset),
                end: Position::new(end_line, end_offset),
            };
            let name = def_info.name.clone();

            let kind = match def_info.kind {
                DefKind::Function(_) => SymbolKind::FUNCTION,
                DefKind::Global(_) => SymbolKind::VARIABLE,
                DefKind::Struct(_) => SymbolKind::STRUCT,
            };

            document_symbols.push(DocumentSymbol {
                name,
                detail: None,
                kind,
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

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        compiler: Mutex::new(ashley::Compiler::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
