// Copyright 2024 Jeremy Wall
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! UCG Language Server Protocol (LSP) server.
//!
//! Provides diagnostics, hover, go-to-definition, and completions for `.ucg` files.
//! Uses the synchronous `lsp-server` crate — no async runtime required.

mod analysis;
mod workspace;

use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use std::rc::Rc;

use lsp_server::{
    Connection, Message, Notification as LspNotification, Request as LspRequest, RequestId,
    Response,
};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification,
    PublishDiagnostics,
};
use lsp_types::request::{Completion, GotoDefinition, HoverRequest, Request};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionList, CompletionOptions, CompletionResponse,
    GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability, Location, MarkupContent,
    MarkupKind, OneOf, Position as LspPosition, PublishDiagnosticsParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

use self::analysis::{ucg_pos_to_range, AnalysisResult};
use self::workspace::WorkspaceIndex;

struct ServerState {
    /// In-memory analysis results for open documents (overrides workspace index).
    documents: HashMap<Url, AnalysisResult>,
    workspace: WorkspaceIndex,
}

impl ServerState {
    fn new(workspace: WorkspaceIndex) -> Self {
        ServerState {
            documents: HashMap::new(),
            workspace,
        }
    }

    fn update_document(&mut self, uri: Url, content: &str) -> &AnalysisResult {
        let working_dir = uri_to_path(&uri).and_then(|p| p.parent().map(|d| d.to_path_buf()));
        let result = analysis::analyze(content, working_dir.as_deref());
        // Also keep the workspace index up to date.
        if let Some(path) = uri_to_path(&uri) {
            self.workspace.update_from_content(&path, content);
        }
        self.documents.insert(uri.clone(), result);
        self.documents.get(&uri).unwrap()
    }
}

/// Start the UCG language server, communicating over stdio.
pub fn run_server() -> Result<(), Box<dyn Error + Send + Sync>> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), "\"".to_string(), "/".to_string()]),
            ..Default::default()
        }),
        ..Default::default()
    })?;

    let (id, params_value) = connection.initialize_start()?;

    // Extract workspace root from InitializeParams.
    let root = extract_workspace_root(&params_value);

    let initialize_result = serde_json::json!({
        "capabilities": server_capabilities,
        "serverInfo": {
            "name": "ucg-lsp",
            "version": env!("CARGO_PKG_VERSION"),
        }
    });
    connection.initialize_finish(id, initialize_result)?;

    // Build the workspace index before entering the message loop.
    let mut workspace = WorkspaceIndex::new(root);
    workspace.index_all();

    let mut state = ServerState::new(workspace);
    main_loop(&connection, &mut state)?;
    io_threads.join()?;
    Ok(())
}

/// Extract the workspace root directory from `InitializeParams`.
/// Falls back to the current working directory if none is provided.
fn extract_workspace_root(params_value: &serde_json::Value) -> PathBuf {
    // Try root_uri first, then root_path.
    if let Some(uri_str) = params_value
        .get("rootUri")
        .and_then(|v| v.as_str())
        .filter(|s| !s.is_empty())
    {
        if let Ok(url) = Url::parse(uri_str) {
            if let Ok(path) = url.to_file_path() {
                return path;
            }
        }
    }
    if let Some(path_str) = params_value.get("rootPath").and_then(|v| v.as_str()) {
        return PathBuf::from(path_str);
    }
    std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
}

fn main_loop(
    conn: &Connection,
    state: &mut ServerState,
) -> Result<(), Box<dyn Error + Send + Sync>> {
    for msg in &conn.receiver {
        match msg {
            Message::Request(req) => {
                if conn.handle_shutdown(&req)? {
                    break;
                }
                handle_request(conn, state, req)?;
            }
            Message::Notification(notif) => {
                handle_notification(conn, state, notif)?;
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

fn handle_notification(
    conn: &Connection,
    state: &mut ServerState,
    notif: LspNotification,
) -> Result<(), Box<dyn Error + Send + Sync>> {
    if notif.method == DidOpenTextDocument::METHOD {
        let params: lsp_types::DidOpenTextDocumentParams = serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        let result = state.update_document(uri.clone(), &content);
        publish_diagnostics(conn, uri, result)?;
    } else if notif.method == DidChangeTextDocument::METHOD {
        let params: lsp_types::DidChangeTextDocumentParams = serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            let result = state.update_document(uri.clone(), &change.text);
            publish_diagnostics(conn, uri, result)?;
        }
    } else if notif.method == DidCloseTextDocument::METHOD {
        let params: lsp_types::DidCloseTextDocumentParams = serde_json::from_value(notif.params)?;
        let uri = params.text_document.uri;
        state.documents.remove(&uri);
        let clear = PublishDiagnosticsParams {
            uri,
            diagnostics: Vec::new(),
            version: None,
        };
        conn.sender
            .send(Message::Notification(LspNotification::new(
                PublishDiagnostics::METHOD.to_string(),
                clear,
            )))?;
    }
    Ok(())
}

fn handle_request(
    conn: &Connection,
    state: &mut ServerState,
    req: LspRequest,
) -> Result<(), Box<dyn Error + Send + Sync>> {
    if req.method == HoverRequest::METHOD {
        let (id, params): (RequestId, lsp_types::HoverParams) =
            req.extract(HoverRequest::METHOD)?;
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = &params.text_document_position_params.position;
        let hover = state
            .documents
            .get(uri)
            .and_then(|doc| find_hover(doc, pos.line, pos.character));
        conn.sender
            .send(Message::Response(Response::new_ok(id, hover)))?;
    } else if req.method == GotoDefinition::METHOD {
        let (id, params): (RequestId, lsp_types::GotoDefinitionParams) =
            req.extract(GotoDefinition::METHOD)?;
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = &params.text_document_position_params.position;
        let location = state
            .documents
            .get(uri)
            .and_then(|doc| find_definition(doc, uri, pos.line, pos.character, &state.workspace));
        let response: Option<GotoDefinitionResponse> = location.map(GotoDefinitionResponse::Scalar);
        conn.sender
            .send(Message::Response(Response::new_ok(id, response)))?;
    } else if req.method == Completion::METHOD {
        let (id, params): (RequestId, lsp_types::CompletionParams) =
            req.extract(Completion::METHOD)?;
        let uri = &params.text_document_position.text_document.uri;
        let pos = &params.text_document_position.position;
        let completions = state
            .documents
            .get(uri)
            .map(|doc| collect_completions(doc, pos.line, pos.character, &state.workspace))
            .unwrap_or_default();
        let response = CompletionResponse::List(CompletionList {
            is_incomplete: false,
            items: completions,
        });
        conn.sender
            .send(Message::Response(Response::new_ok(id, response)))?;
    }
    Ok(())
}

fn publish_diagnostics(
    conn: &Connection,
    uri: Url,
    result: &AnalysisResult,
) -> Result<(), Box<dyn Error + Send + Sync>> {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics: result.diagnostics.clone(),
        version: None,
    };
    conn.sender
        .send(Message::Notification(LspNotification::new(
            PublishDiagnostics::METHOD.to_string(),
            params,
        )))?;
    Ok(())
}

/// Find the token at a (line, character) position (0-based LSP coords).
/// Matches BAREWORD tokens whose text span covers the cursor.
fn token_at(doc: &AnalysisResult, line: u32, character: u32) -> Option<Rc<str>> {
    let target_line = (line + 1) as usize;
    let target_col = (character + 1) as usize;
    doc.tokens
        .iter()
        .find(|tok| {
            tok.pos.line == target_line
                && tok.pos.column <= target_col
                && target_col < tok.pos.column + tok.fragment.len()
        })
        .map(|tok| tok.fragment.clone())
}

/// Find the token prefix being typed at (line, character): the longest BAREWORD
/// ending at or before the cursor on the same line.
fn token_prefix_at(doc: &AnalysisResult, line: u32, character: u32) -> String {
    let target_line = (line + 1) as usize;
    let target_col = (character + 1) as usize;
    // Find the last token on this line that starts at or before the cursor.
    doc.tokens
        .iter()
        .filter(|tok| tok.pos.line == target_line && tok.pos.column <= target_col)
        .last()
        .map(|tok| {
            // Trim the fragment to only the characters up to the cursor.
            let chars_before = target_col.saturating_sub(tok.pos.column);
            tok.fragment.chars().take(chars_before).collect::<String>()
        })
        .unwrap_or_default()
}

/// Check if the cursor is inside a QUOTED string token.
fn cursor_in_string(doc: &AnalysisResult, line: u32, character: u32) -> Option<String> {
    use crate::ast::TokenType;
    let target_line = (line + 1) as usize;
    let target_col = (character + 1) as usize;
    doc.tokens
        .iter()
        .find(|tok| {
            tok.typ == TokenType::QUOTED
                && tok.pos.line == target_line
                && tok.pos.column <= target_col
                && target_col < tok.pos.column + tok.fragment.len() + 2 // account for quotes
        })
        .map(|tok| tok.fragment.to_string())
}

fn find_hover(doc: &AnalysisResult, line: u32, character: u32) -> Option<Hover> {
    let name = token_at(doc, line, character)?;
    let (shape, def_pos) = doc.symbol_table.get(&name)?;

    let shape_str = format_shape(shape);
    let contents = format!(
        "**type**: `{}`\n\n**binding**: `{}`\n\n*defined at line {}, col {}*",
        shape_str, name, def_pos.line, def_pos.column
    );

    let target_line = (line + 1) as usize;
    let target_col = (character + 1) as usize;
    let tok_range = doc
        .tokens
        .iter()
        .find(|tok| {
            tok.pos.line == target_line
                && tok.pos.column <= target_col
                && target_col < tok.pos.column + tok.fragment.len()
        })
        .map(|tok| ucg_pos_to_range(&tok.pos));

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: contents,
        }),
        range: tok_range,
    })
}

fn find_definition(
    doc: &AnalysisResult,
    current_uri: &Url,
    line: u32,
    character: u32,
    _workspace: &WorkspaceIndex,
) -> Option<Location> {
    let name = token_at(doc, line, character)?;

    // If this binding is an import, jump to the imported file.
    if let Some(import_path) = doc.import_map.get(&name) {
        // For stdlib imports the path is relative ("std/lists.ucg"); for user
        // files it is absolute (resolved in analyze()).
        if import_path.is_absolute() {
            if let Ok(uri) = Url::from_file_path(import_path) {
                return Some(Location {
                    uri,
                    range: zero_range(),
                });
            }
        }
        // Stdlib: we can't navigate into an embedded file, so fall through to
        // the definition position in the current file.
    }

    // Otherwise jump to the definition site in the current file.
    let (_, def_pos) = doc.symbol_table.get(&name)?;

    // Cross-file: if def_pos carries a file path, use that; otherwise use
    // the current document's URI.
    let uri = def_pos
        .file
        .as_ref()
        .and_then(|p| Url::from_file_path(p).ok())
        .unwrap_or_else(|| current_uri.clone());

    Some(Location {
        uri,
        range: ucg_pos_to_range(def_pos),
    })
}

fn collect_completions(
    doc: &AnalysisResult,
    line: u32,
    character: u32,
    workspace: &WorkspaceIndex,
) -> Vec<CompletionItem> {
    let mut items: Vec<CompletionItem> = Vec::new();

    // If the cursor is inside a string literal, offer stdlib import paths.
    let in_string = cursor_in_string(doc, line, character);
    if in_string.is_some() {
        let prefix = in_string.unwrap_or_default();
        for key in &workspace.stdlib_keys {
            if key.starts_with(&prefix) {
                items.push(CompletionItem {
                    label: key.clone(),
                    kind: Some(CompletionItemKind::MODULE),
                    detail: Some("stdlib module".to_string()),
                    ..Default::default()
                });
            }
        }
        return items;
    }

    let prefix = token_prefix_at(doc, line, character);

    // Local bindings in this document.
    for (name, (shape, _)) in &doc.symbol_table {
        if name.starts_with(prefix.as_str()) {
            items.push(CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some(format_shape(shape)),
                ..Default::default()
            });
        }
    }

    // Symbols exported from imported files.
    for (import_name, import_path) in &doc.import_map {
        let imported_doc = if import_path.is_absolute() {
            workspace.get(import_path)
        } else {
            None
        };
        if let Some(imported) = imported_doc {
            for (field_name, (shape, _)) in &imported.symbol_table {
                let full_name = format!("{}.{}", import_name, field_name);
                if full_name.starts_with(prefix.as_str()) {
                    items.push(CompletionItem {
                        label: full_name,
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(format_shape(shape)),
                        ..Default::default()
                    });
                }
            }
        }
    }

    items
}

fn format_shape(shape: &crate::ast::Shape) -> String {
    use crate::ast::Shape;
    match shape {
        Shape::Boolean(_) => "Bool".to_string(),
        Shape::Int(_) => "Int".to_string(),
        Shape::Float(_) => "Float".to_string(),
        Shape::Str(_) => "Str".to_string(),
        Shape::Tuple(_) => "Tuple".to_string(),
        Shape::List(_) => "List".to_string(),
        Shape::Func(_) => "Func".to_string(),
        Shape::Module(_) => "Module".to_string(),
        Shape::Hole(pi) => format!("?({})", pi.val),
        Shape::Narrowed(_) => "Narrowed".to_string(),
        Shape::Import(_) => "Import".to_string(),
        Shape::TypeErr(_, msg) => format!("TypeError({})", msg),
    }
}

/// Convert a file-system path to an LSP `Url`, returning the original on failure.
fn _path_to_uri(path: &std::path::Path) -> Option<Url> {
    Url::from_file_path(path).ok()
}

/// Convert an LSP `Url` to a filesystem `PathBuf`.
fn uri_to_path(uri: &Url) -> Option<PathBuf> {
    uri.to_file_path().ok()
}

/// An LSP `Range` pointing at the very start of a file (line 0, char 0).
fn zero_range() -> Range {
    let start = LspPosition {
        line: 0,
        character: 0,
    };
    Range { start, end: start }
}
