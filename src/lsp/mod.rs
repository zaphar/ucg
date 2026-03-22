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
use lsp_types::request::{
    Completion, GotoDefinition, HoverRequest, Request, SemanticTokensFullRequest,
    WorkspaceSymbolRequest,
};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionList, CompletionOptions, CompletionResponse,
    GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability, Location, MarkupContent,
    MarkupKind, OneOf, Position as LspPosition, PublishDiagnosticsParams, Range, SemanticToken,
    SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, SymbolInformation, SymbolKind, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url, WorkspaceSymbolOptions,
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
        // Update workspace first so its resolved-imports cache is current.
        if let Some(path) = uri_to_path(&uri) {
            self.workspace.update_from_content(&path, content);
        }
        // Analyze with the workspace's resolved cache so import shapes are populated.
        let result =
            analysis::analyze(content, working_dir.as_deref(), self.workspace.resolved_files());
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
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: vec![
                        SemanticTokenType::KEYWORD,
                        SemanticTokenType::VARIABLE,
                        SemanticTokenType::STRING,
                        SemanticTokenType::NUMBER,
                        SemanticTokenType::COMMENT,
                        SemanticTokenType::OPERATOR,
                        SemanticTokenType::NAMESPACE,
                    ],
                    token_modifiers: vec![SemanticTokenModifier::DECLARATION],
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: None,
                work_done_progress_options: Default::default(),
            },
        )),
        workspace_symbol_provider: Some(OneOf::Right(WorkspaceSymbolOptions {
            work_done_progress_options: Default::default(),
            resolve_provider: None,
        })),
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
        // Re-sync the workspace cache from disk now that the editor's in-memory
        // version is gone.
        if let Some(path) = uri_to_path(&uri) {
            state.workspace.update_from_disk(&path);
        }
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
            .and_then(|doc| find_hover(doc, &state.workspace, pos.line, pos.character));
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
    } else if req.method == WorkspaceSymbolRequest::METHOD {
        let (id, params): (RequestId, lsp_types::WorkspaceSymbolParams) =
            req.extract(WorkspaceSymbolRequest::METHOD)?;
        let symbols = collect_workspace_symbols(&state.workspace, &params.query);
        conn.sender
            .send(Message::Response(Response::new_ok(id, symbols)))?;
    } else if req.method == SemanticTokensFullRequest::METHOD {
        let (id, params): (RequestId, lsp_types::SemanticTokensParams) =
            req.extract(SemanticTokensFullRequest::METHOD)?;
        let uri = &params.text_document.uri;
        let data = state
            .documents
            .get(uri)
            .map(|doc| encode_semantic_tokens(doc))
            .unwrap_or_default();
        let response = SemanticTokens {
            result_id: None,
            data,
        };
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

/// Prepend an optional doc comment (in markdown) above `type_info`.
fn prepend_doc(doc_str: Option<String>, type_info: String) -> String {
    match doc_str {
        Some(d) if !d.is_empty() => format!("{}\n\n{}", d, type_info),
        _ => type_info,
    }
}

fn find_hover(
    doc: &AnalysisResult,
    workspace: &WorkspaceIndex,
    line: u32,
    character: u32,
) -> Option<Hover> {
    // 0. Dot-expression field hover (e.g. `lists.len`, `a.b.c`).
    if let Some(hover) = find_hover_dot_expr(doc, workspace, line, character) {
        return Some(hover);
    }

    let target_line = (line + 1) as usize;
    let target_col = (character + 1) as usize;

    // Find the exact token under the cursor.
    let tok = doc.tokens.iter().find(|tok| {
        tok.pos.line == target_line
            && tok.pos.column <= target_col
            && target_col < tok.pos.column + tok.fragment.len()
    })?;
    let tok_range = ucg_pos_to_range(&tok.pos);
    let name = tok.fragment.clone();

    // 1. Check the position-keyed map first — covers function/module args,
    //    inner let bindings, and tuple field names without name-collision risk.
    if let Some(shape) = doc.token_types.get(&(tok.pos.line, tok.pos.column)) {
        let def_pos = shape.pos();
        let label = doc
            .path_map
            .get(&(tok.pos.line, tok.pos.column))
            .map(|p| p.as_ref())
            .unwrap_or_else(|| name.as_ref());
        let label_kind = if doc.path_map.contains_key(&(tok.pos.line, tok.pos.column)) {
            "path"
        } else {
            "binding"
        };
        let doc_str = analysis::doc_comment_for_binding(&doc.comment_map, def_pos.line);
        let type_info = format!(
            "**type**: {}\n\n**{}**: `{}`\n\n*defined at line {}, col {}*",
            hover_type_display(shape),
            label_kind,
            label,
            def_pos.line,
            def_pos.column
        );
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: prepend_doc(doc_str, type_info),
            }),
            range: Some(tok_range),
        });
    }

    // 2. Fall back to the top-level symbol table (name-keyed).
    let (shape, def_pos) = doc.symbol_table.get(&name)?;
    // For import bindings use the imported file's file-level doc comment;
    // for regular bindings use the comment immediately above the definition.
    let doc_str = if let crate::ast::Shape::Import(_) = shape {
        doc.import_map
            .get(&name)
            .and_then(|p| workspace.get(p))
            .and_then(|imported| imported.file_doc.clone())
    } else {
        analysis::doc_comment_for_binding(&doc.comment_map, def_pos.line)
    };
    let type_info = format!(
        "**type**: {}\n\n**binding**: `{}`\n\n*defined at line {}, col {}*",
        hover_type_display(shape),
        name,
        def_pos.line,
        def_pos.column
    );
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: prepend_doc(doc_str, type_info),
        }),
        range: Some(tok_range),
    })
}

/// Returns the definition position of a named field within a shape.
/// For `Tuple` and resolved `Import`, returns the `PositionedItem::pos` of the field key.
/// For `Module`, descends into its return type.
fn lookup_field_definition(shape: &crate::ast::Shape, field: &str) -> Option<crate::ast::Position> {
    use crate::ast::{ImportShape, Shape};
    match shape {
        Shape::Tuple(pi) => pi
            .val
            .iter()
            .find(|(n, _)| n.val.as_ref() == field)
            .map(|(n, _)| n.pos.clone()),
        Shape::Import(ImportShape::Resolved(_, fields)) => fields
            .iter()
            .find(|(n, _)| n.val.as_ref() == field)
            .map(|(n, _)| n.pos.clone()),
        Shape::Module(mdef) => lookup_field_definition(mdef.ret(), field),
        _ => None,
    }
}

/// Walk backwards from the cursor token through `.`-separated BAREWORDs and
/// return `(path, root_token_pos)` where `path` is the full dot chain from root
/// to cursor (e.g. `["a","b","c"]` for cursor on `c` in `a.b.c`) and
/// `root_token_pos` is the `(line, col)` key of the root token in the token stream.
/// Returns `None` if the cursor isn't part of a dot expression (path length < 2).
fn collect_dot_path(
    doc: &AnalysisResult,
    line: u32,
    character: u32,
) -> Option<(Vec<Rc<str>>, (usize, usize))> {
    use crate::ast::TokenType;
    let target_line = (line + 1) as usize;
    let target_col = (character + 1) as usize;
    let idx = doc.tokens.iter().position(|tok| {
        tok.pos.line == target_line
            && tok.pos.column <= target_col
            && target_col < tok.pos.column + tok.fragment.len()
    })?;
    if doc.tokens[idx].typ != TokenType::BAREWORD {
        return None;
    }
    let mut path = vec![doc.tokens[idx].fragment.clone()];
    let mut i = idx;
    while i >= 2 {
        let dot = &doc.tokens[i - 1];
        if dot.typ != TokenType::PUNCT || dot.fragment.as_ref() != "." {
            break;
        }
        let prev = &doc.tokens[i - 2];
        if prev.typ != TokenType::BAREWORD {
            break;
        }
        path.push(prev.fragment.clone());
        i -= 2;
    }
    path.reverse();
    if path.len() < 2 {
        return None;
    }
    // `i` now points at the root token's index.
    let root_tok = &doc.tokens[i];
    Some((path, (root_tok.pos.line, root_tok.pos.column)))
}

/// Given a shape, look up a named field and return its shape.
/// Handles `Tuple`, resolved `Import`, and `Module` (by descending into the return type).
fn lookup_field_in_shape(shape: &crate::ast::Shape, field: &str) -> Option<crate::ast::Shape> {
    use crate::ast::{ImportShape, Shape};
    match shape {
        Shape::Tuple(pi) => pi
            .val
            .iter()
            .find(|(name_pi, _)| name_pi.val.as_ref() == field)
            .map(|(_, s)| s.clone()),
        Shape::Import(ImportShape::Resolved(_, fields)) => fields
            .iter()
            .find(|(name_pi, _)| name_pi.val.as_ref() == field)
            .map(|(_, s)| s.clone()),
        // A module's fields are its return type (what you get after calling it).
        Shape::Module(mdef) => lookup_field_in_shape(mdef.ret(), field),
        _ => None,
    }
}

/// Build a hover response for a dot-expression field access (e.g. cursor on
/// `len` in `lists.len`, or on `c` in `a.b.c`).
fn find_hover_dot_expr(
    doc: &AnalysisResult,
    workspace: &WorkspaceIndex,
    line: u32,
    character: u32,
) -> Option<Hover> {
    let (path, root_pos) = collect_dot_path(doc, line, character)?;

    // Resolve root shape: prefer position-keyed map (inner scopes) over symbol table.
    let root_name = &path[0];
    let root_shape = doc
        .token_types
        .get(&root_pos)
        .cloned()
        .or_else(|| doc.symbol_table.get(root_name).map(|(s, _)| s.clone()))?;

    // Walk the shape through each field segment.
    let mut current = root_shape;
    for field in &path[1..] {
        current = lookup_field_in_shape(&current, field)?;
    }

    // Look up the doc comment for the field.  When the root is an import binding,
    // the field definition lives in the imported file — look there.  Otherwise
    // check the current document.
    let doc_str = doc
        .import_map
        .get(root_name)
        .and_then(|p| workspace.get(p))
        .and_then(|imported| {
            analysis::doc_comment_for_binding(&imported.comment_map, current.pos().line)
        })
        .or_else(|| {
            analysis::doc_comment_for_binding(&doc.comment_map, current.pos().line)
        });

    // Find the cursor token for the hover range.
    let target_line = (line + 1) as usize;
    let target_col = (character + 1) as usize;
    let tok = doc.tokens.iter().find(|tok| {
        tok.pos.line == target_line
            && tok.pos.column <= target_col
            && target_col < tok.pos.column + tok.fragment.len()
    })?;

    let full_path = path.iter().map(|s| s.as_ref()).collect::<Vec<_>>().join(".");
    let type_info = format!(
        "**type**: {}\n\n**path**: `{}`",
        hover_type_display(&current),
        full_path,
    );
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: prepend_doc(doc_str, type_info),
        }),
        range: Some(ucg_pos_to_range(&tok.pos)),
    })
}

fn find_definition(
    doc: &AnalysisResult,
    current_uri: &Url,
    line: u32,
    character: u32,
    workspace: &WorkspaceIndex,
) -> Option<Location> {
    // Dot-expression go-to-definition: handles arbitrary-depth chains like
    // `import.field`, `local_tuple.field`, and `import.tuple_binding.nested_field`.
    if let Some((path, root_pos)) = collect_dot_path(doc, line, character) {
        let root_name = &path[0];
        let fields = &path[1..]; // always non-empty (collect_dot_path requires len >= 2)

        // If the root is an absolute import, navigate within the imported file.
        if let Some(import_path) = doc.import_map.get(root_name).filter(|p| p.is_absolute()) {
            if let Ok(import_uri) = Url::from_file_path(import_path) {
                if let Some(imported_doc) = workspace.get(import_path) {
                    let first_field = &fields[0];
                    if let Some((first_shape, first_pos)) =
                        imported_doc.symbol_table.get(first_field)
                    {
                        if fields.len() == 1 {
                            return Some(Location {
                                uri: import_uri,
                                range: ucg_pos_to_range(first_pos),
                            });
                        }
                        // Multi-level: walk through intermediate fields then resolve last.
                        let mut current = first_shape.clone();
                        let mut ok = true;
                        for f in &fields[1..fields.len() - 1] {
                            if let Some(next) = lookup_field_in_shape(&current, f) {
                                current = next;
                            } else {
                                ok = false;
                                break;
                            }
                        }
                        if ok {
                            if let Some(pos) =
                                lookup_field_definition(&current, &fields[fields.len() - 1])
                            {
                                return Some(Location {
                                    uri: import_uri,
                                    range: ucg_pos_to_range(&pos),
                                });
                            }
                        }
                    }
                }
            }
        } else {
            // Local: resolve root shape from token_types (inner scope) or symbol_table.
            let root_shape = doc
                .token_types
                .get(&root_pos)
                .cloned()
                .or_else(|| doc.symbol_table.get(root_name).map(|(s, _)| s.clone()));
            if let Some(mut current) = root_shape {
                let mut ok = true;
                for f in &fields[..fields.len() - 1] {
                    if let Some(next) = lookup_field_in_shape(&current, f) {
                        current = next;
                    } else {
                        ok = false;
                        break;
                    }
                }
                if ok {
                    if let Some(pos) =
                        lookup_field_definition(&current, &fields[fields.len() - 1])
                    {
                        return Some(Location {
                            uri: current_uri.clone(),
                            range: ucg_pos_to_range(&pos),
                        });
                    }
                }
            }
        }
    }

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

fn collect_workspace_symbols(workspace: &WorkspaceIndex, query: &str) -> Vec<SymbolInformation> {
    let mut results = Vec::new();
    for (path, doc) in workspace.iter_files() {
        let uri = match Url::from_file_path(path) {
            Ok(u) => u,
            Err(_) => continue,
        };
        for (name, (shape, pos)) in &doc.symbol_table {
            if query.is_empty() || name.contains(query) {
                #[allow(deprecated)]
                results.push(SymbolInformation {
                    name: name.to_string(),
                    kind: shape_to_symbol_kind(shape),
                    location: Location {
                        uri: uri.clone(),
                        range: ucg_pos_to_range(pos),
                    },
                    container_name: None,
                    deprecated: None,
                    tags: None,
                });
            }
        }
    }
    results
}

fn shape_to_symbol_kind(shape: &crate::ast::Shape) -> SymbolKind {
    use crate::ast::Shape;
    match shape {
        Shape::Func(_) => SymbolKind::FUNCTION,
        Shape::Module(_) => SymbolKind::MODULE,
        Shape::Tuple(_) => SymbolKind::STRUCT,
        _ => SymbolKind::VARIABLE,
    }
}

fn encode_semantic_tokens(doc: &AnalysisResult) -> Vec<SemanticToken> {
    use crate::ast::TokenType;
    const KEYWORDS: &[&str] = &[
        "let", "import", "func", "module", "select", "cast", "map", "filter", "reduce", "format",
        "include", "assert", "out", "convert", "range", "fail", "debug", "is", "in", "true",
        "false", "NULL",
    ];
    const OPERATORS: &[&str] = &[
        "+", "-", "*", "/", "==", "!=", ">", "<", ">=", "<=", "%", "!",
    ];

    let definition_positions: std::collections::HashSet<(usize, usize)> = doc
        .symbol_table
        .values()
        .map(|(_, pos)| (pos.line, pos.column))
        .collect();

    let mut data: Vec<SemanticToken> = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_col = 0u32;

    for tok in &doc.tokens {
        let (token_type, token_modifiers_bitset): (u32, u32) = match tok.typ {
            TokenType::WS | TokenType::END => continue,
            TokenType::COMMENT => (4, 0),
            TokenType::QUOTED | TokenType::PIPEQUOTE => (2, 0),
            TokenType::DIGIT => (3, 0),
            TokenType::BOOLEAN | TokenType::EMPTY => (0, 0),
            TokenType::PUNCT => {
                if OPERATORS.contains(&tok.fragment.as_ref()) {
                    (5, 0)
                } else {
                    continue;
                }
            }
            TokenType::BAREWORD => {
                let text: &str = tok.fragment.as_ref();
                if KEYWORDS.contains(&text) {
                    (0, 0)
                } else if doc.import_map.contains_key(text) {
                    (6, 0)
                } else {
                    let is_decl = definition_positions.contains(&(tok.pos.line, tok.pos.column));
                    (1, if is_decl { 1 } else { 0 })
                }
            }
        };

        let line = tok.pos.line.saturating_sub(1) as u32;
        let col = tok.pos.column.saturating_sub(1) as u32;
        let delta_line = line - prev_line;
        let delta_start = if delta_line == 0 { col - prev_col } else { col };

        data.push(SemanticToken {
            delta_line,
            delta_start,
            length: tok.fragment.len() as u32,
            token_type,
            token_modifiers_bitset,
        });

        prev_line = line;
        prev_col = col;
    }
    data
}

fn format_narrowing_shape(ns: &crate::ast::NarrowingShape) -> String {
    use crate::ast::NarrowingShape;
    match ns {
        NarrowingShape::Any => "any".to_string(),
        NarrowingShape::Narrowed(types) => {
            let mut seen = std::collections::HashSet::new();
            let inner: Vec<String> = types
                .iter()
                .map(format_shape)
                .filter(|s| seen.insert(s.clone()))
                .collect();
            if inner.is_empty() {
                "any".to_string()
            } else {
                inner.join(" | ")
            }
        }
    }
}

fn format_shape(shape: &crate::ast::Shape) -> String {
    use crate::ast::Shape;
    match shape {
        Shape::Boolean(_) => "Bool".to_string(),
        Shape::Int(_) => "Int".to_string(),
        Shape::Float(_) => "Float".to_string(),
        Shape::Str(_) => "Str".to_string(),
        Shape::Tuple(pi) => {
            let fields: Vec<String> = pi
                .val
                .iter()
                .map(|(name, shape)| format!("{}: {}", name.val, format_shape(shape)))
                .collect();
            format!("{{{}}}", fields.join(", "))
        }
        Shape::List(ns) => format!("[{}]", format_narrowing_shape(&ns.types)),
        Shape::Narrowed(ns) => format_narrowing_shape(&ns.types),
        Shape::Func(fdef) => {
            let args: Vec<String> = fdef
                .arg_order
                .iter()
                .map(|name| {
                    let ty = fdef
                        .args()
                        .get(name)
                        .map(format_shape)
                        .unwrap_or_else(|| "?".to_string());
                    format!("{}: {}", name, ty)
                })
                .collect();
            format!("func({}) => {}", args.join(", "), format_shape(fdef.ret()))
        }
        Shape::Module(mdef) => format!("Module => {}", format_shape(mdef.ret())),
        Shape::Hole(_) => "any".to_string(),
        Shape::Import(crate::ast::ImportShape::Resolved(_, fields)) => {
            let parts: Vec<String> = fields
                .iter()
                .map(|(name, shape)| format!("{}: {}", name.val, format_shape(shape)))
                .collect();
            format!("import {{{}}}", parts.join(", "))
        }
        Shape::Import(crate::ast::ImportShape::Unresolved(pi)) => {
            format!("import(\"{}\")", pi.val)
        }
        Shape::TypeErr(_, msg) => format!("TypeError({})", msg),
    }
}

/// Indented pretty-printer for shapes (used in hover).
/// `indent` is the current nesting level (0 = top-level).
fn format_shape_pretty(shape: &crate::ast::Shape, indent: usize) -> String {
    use crate::ast::Shape;
    let pad = "  ".repeat(indent);
    let inner = "  ".repeat(indent + 1);
    match shape {
        Shape::Tuple(pi) => {
            if pi.val.is_empty() {
                return "{}".to_string();
            }
            let fields: Vec<String> = pi
                .val
                .iter()
                .map(|(name, s)| {
                    format!("{}{}: {}", inner, name.val, format_shape_pretty(s, indent + 1))
                })
                .collect();
            format!("{{\n{}\n{}}}", fields.join(",\n"), pad)
        }
        Shape::Func(fdef) => {
            let args: Vec<String> = fdef
                .arg_order
                .iter()
                .map(|name| {
                    let ty = fdef
                        .args()
                        .get(name)
                        .map(|s| format_shape_pretty(s, indent))
                        .unwrap_or_else(|| "?".to_string());
                    format!("{}: {}", name, ty)
                })
                .collect();
            format!(
                "func({}) => {}",
                args.join(", "),
                format_shape_pretty(fdef.ret(), indent)
            )
        }
        Shape::Module(mdef) => {
            format!("Module => {}", format_shape_pretty(mdef.ret(), indent))
        }
        Shape::Import(crate::ast::ImportShape::Resolved(_, fields)) => {
            if fields.is_empty() {
                return "import {}".to_string();
            }
            let parts: Vec<String> = fields
                .iter()
                .map(|(name, s)| {
                    format!("{}{}: {}", inner, name.val, format_shape_pretty(s, indent + 1))
                })
                .collect();
            format!("import {{\n{}\n{}}}", parts.join(",\n"), pad)
        }
        // For scalar/simple shapes fall back to compact formatter.
        other => format_shape(other),
    }
}

/// Renders a shape for use in a hover response.
/// Returns an inline code span for simple (single-line) types,
/// or a fenced code block for multi-line types.
fn hover_type_display(shape: &crate::ast::Shape) -> String {
    let s = format_shape_pretty(shape, 0);
    if s.contains('\n') {
        format!("```\n{}\n```", s)
    } else {
        format!("`{}`", s)
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

#[cfg(test)]
mod test {
    use std::path::PathBuf;
    use std::rc::Rc;

    use lsp_types::{HoverContents, SymbolKind};

    use super::*;

    /// Test helper: analyze without a resolved-imports cache.
    fn analyze(content: &str, working_dir: Option<&std::path::Path>) -> analysis::AnalysisResult {
        analysis::analyze(content, working_dir, &std::collections::HashMap::new())
    }

    fn empty_workspace() -> WorkspaceIndex {
        WorkspaceIndex::new(PathBuf::from("/"))
    }

    /// Test helper: find_hover with an empty workspace.
    fn find_hover(doc: &analysis::AnalysisResult, line: u32, character: u32) -> Option<Hover> {
        super::find_hover(doc, &empty_workspace(), line, character)
    }

    fn fake_uri() -> Url {
        Url::parse("file:///tmp/test.ucg").unwrap()
    }

    // --- token_at ---

    #[test]
    fn test_token_at_found() {
        // "let x = 1;"  x is at 0-based (line=0, char=4)
        let doc = analyze("let x = 1;", None);
        let name = token_at(&doc, 0, 4);
        assert_eq!(name.as_deref(), Some("x"));
    }

    #[test]
    fn test_token_at_end_of_token() {
        // Cursor at the last char of the token still matches
        let doc = analyze("let foo = 1;", None);
        // "foo" starts at col 5 (1-based) = char 4 (0-based), len=3, covers chars 4-6
        assert_eq!(token_at(&doc, 0, 6).as_deref(), Some("foo"));
    }

    #[test]
    fn test_token_at_not_found() {
        let doc = analyze("let x = 1;", None);
        // Cursor past end of line
        assert!(token_at(&doc, 0, 100).is_none());
    }

    #[test]
    fn test_token_at_wrong_line() {
        let doc = analyze("let x = 1;", None);
        assert!(token_at(&doc, 5, 4).is_none());
    }

    // --- token_prefix_at ---

    #[test]
    fn test_token_prefix_at_mid_token() {
        // "let foo = 1;" — cursor mid-way through "foo" (char=5, 0-based = after 'fo')
        let doc = analyze("let foo = 1;", None);
        // "foo" starts at 1-based col 5 = 0-based char 4
        // cursor at char 5 (0-based) → target_col=6 (1-based)
        // chars_before = 6 - 5 = 1  → "f"
        let prefix = token_prefix_at(&doc, 0, 5);
        assert_eq!(prefix, "f");
    }

    #[test]
    fn test_token_prefix_at_before_any_token() {
        let doc = analyze("let x = 1;", None);
        // Cursor at very start before any token
        let prefix = token_prefix_at(&doc, 5, 0);
        assert_eq!(prefix, "");
    }

    // --- cursor_in_string ---

    #[test]
    fn test_cursor_in_string_inside() {
        // r#"let x = "hello";"# — cursor inside the string at char 10 (0-based)
        // "hello" starts at col 10 (1-based), fragment = "hello"
        // target_col = 11, tok.col=10, 10<=11 && 11<10+5+2=17 → inside
        let doc = analyze(r#"let x = "hello";"#, None);
        let result = cursor_in_string(&doc, 0, 10);
        assert!(result.is_some(), "cursor inside string should return Some");
    }

    #[test]
    fn test_cursor_in_string_outside() {
        let doc = analyze(r#"let x = "hello";"#, None);
        // Cursor on "let" token, not in a string
        let result = cursor_in_string(&doc, 0, 1);
        assert!(result.is_none());
    }

    // --- collect_dot_path ---

    #[test]
    fn test_collect_dot_path_success() {
        // "let l = import "f"; let y = l.x;"
        //  col (1-based): l=5, "f"=16..18, l=29, .=30, x=31
        // LSP 0-based cursor on x: (line=0, char=30)
        let doc = analyze(r#"let l = import "f"; let y = l.x;"#, None);
        let result = collect_dot_path(&doc, 0, 30);
        assert!(result.is_some(), "should detect dot path");
        let (path, _) = result.unwrap();
        assert_eq!(path.len(), 2);
        assert_eq!(path[0].as_ref(), "l");
        assert_eq!(path[1].as_ref(), "x");
    }

    #[test]
    fn test_collect_dot_path_not_on_dot() {
        // Cursor is on a plain bareword with no preceding dot
        let doc = analyze("let x = 1;", None);
        assert!(collect_dot_path(&doc, 0, 4).is_none());
    }

    #[test]
    fn test_collect_dot_path_too_few_tokens() {
        // Single-token document — can't form a dot path
        let doc = analyze("1;", None);
        assert!(collect_dot_path(&doc, 0, 0).is_none());
    }

    // --- find_hover ---

    #[test]
    fn test_find_hover_symbol_table_lookup() {
        // Hovering on a top-level binding name returns its type
        let doc = analyze("let x = 1;", None);
        // x is at 0-based (line=0, char=4)
        let hover = find_hover(&doc, 0, 4);
        assert!(hover.is_some(), "should return hover for known binding");
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("Int"), "hover should mention type Int");
            assert!(mc.value.contains('x'), "hover should mention binding name");
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_find_hover_tuple_shows_fields() {
        let doc = analyze("let t = {x = 1, y = \"hi\"};", None);
        // hover on `t` at (0, 4)
        let hover = find_hover(&doc, 0, 4);
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("x: Int"), "got: {}", mc.value);
            assert!(mc.value.contains("y: Str"), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_find_hover_list_shows_element_type() {
        let doc = analyze("let l = [1, 2, 3];", None);
        // hover on `l` at (0, 4)
        let hover = find_hover(&doc, 0, 4);
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("[Int]"), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_find_hover_tuple_field_shows_path() {
        // Hovering on a field name inside a tuple literal should show the dotted path.
        // "let t = {x = 1, y = \"hi\"};"
        //          ^ x is at 0-based (line=0, char=9)
        let doc = analyze("let t = {x = 1, y = \"hi\"};", None);
        // Find x's position from path_map
        let x_entry = doc.path_map.iter().find(|(_, p)| p.as_ref() == "t.x");
        assert!(x_entry.is_some(), "t.x should be in path_map");
        let ((line, col), _) = x_entry.unwrap();
        // Convert from 1-based UCG coords to 0-based LSP coords
        let hover = find_hover(&doc, (line - 1) as u32, (col - 1) as u32);
        assert!(hover.is_some(), "should return hover for tuple field");
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("path"), "should use 'path' label, got: {}", mc.value);
            assert!(mc.value.contains("t.x"), "should show dotted path t.x, got: {}", mc.value);
            assert!(mc.value.contains("Int"), "should show field type, got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_find_hover_nested_tuple_field_shows_full_path() {
        let doc = analyze("let t = {outer = {inner = 1}};", None);
        let inner_entry = doc.path_map.iter().find(|(_, p)| p.as_ref() == "t.outer.inner");
        assert!(inner_entry.is_some(), "t.outer.inner should be in path_map");
        let ((line, col), _) = inner_entry.unwrap();
        let hover = find_hover(&doc, (line - 1) as u32, (col - 1) as u32);
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("t.outer.inner"), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_find_hover_token_types_lookup() {
        // Hovering on a function arg (in token_types) returns the inferred type
        // "let f = func(x) => x + 1;" — hover on body x at (0, 19)
        let doc = analyze("let f = func(x) => x + 1;", None);
        let hover = find_hover(&doc, 0, 19);
        assert!(hover.is_some(), "should return hover for scoped arg");
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("Int"), "hover should show inferred Int type");
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_find_hover_unknown_position() {
        let doc = analyze("let x = 1;", None);
        // Cursor past end of content — no token there
        assert!(find_hover(&doc, 99, 99).is_none());
    }

    // --- find_definition ---

    #[test]
    fn test_find_definition_local_binding() {
        // Hovering on a use of `x` should jump to its definition site
        // "let x = 1; let y = x;" — x use at (0, 19)
        let doc = analyze("let x = 1; let y = x;", None);
        let ws = empty_workspace();
        let def = find_definition(&doc, &fake_uri(), 0, 19, &ws);
        assert!(def.is_some(), "should find definition for local binding");
        let loc = def.unwrap();
        assert_eq!(loc.uri, fake_uri());
        // x is defined at line 1 col 5 (1-based) → LSP line 0 char 4
        assert_eq!(loc.range.start.line, 0);
        assert_eq!(loc.range.start.character, 4);
    }

    #[test]
    fn test_find_definition_relative_import_no_jump() {
        // Relative import paths (e.g. stdlib) don't produce a file jump
        let doc = analyze(r#"let lists = import "std/lists.ucg";"#, None);
        let ws = empty_workspace();
        // Hover on "lists" at (0, 4)
        let def = find_definition(&doc, &fake_uri(), 0, 4, &ws);
        // Relative path → falls through to symbol_table lookup, returns current file
        assert!(def.is_some());
        assert_eq!(def.unwrap().uri, fake_uri());
    }

    #[test]
    fn test_find_definition_absolute_import_jumps() {
        // An absolute import path produces a jump to the imported file
        let doc = analyze(r#"let mylib = import "/tmp/mylib.ucg";"#, None);
        let ws = empty_workspace();
        let def = find_definition(&doc, &fake_uri(), 0, 4, &ws);
        assert!(def.is_some());
        let loc = def.unwrap();
        assert_eq!(loc.uri, Url::from_file_path("/tmp/mylib.ucg").unwrap());
    }

    #[test]
    fn test_find_definition_unknown_position_returns_none() {
        // Cursor on whitespace (between tokens) should return None
        let doc = analyze("let x = 1;", None);
        let ws = empty_workspace();
        // position 3 is the space between "let" and "x"
        let def = find_definition(&doc, &fake_uri(), 0, 3, &ws);
        assert!(def.is_none(), "whitespace position should return None");
    }

    #[test]
    fn test_find_definition_cross_file_dot_context_via_workspace() {
        // Cursor on the field name in `lib.foo` where `lib` is an absolute import
        // that the workspace has indexed. Should jump to `foo`'s definition in the
        // imported file.
        let imported_content = "let foo = 42;";
        let import_path = PathBuf::from("/tmp/mylib.ucg");

        let mut ws = WorkspaceIndex::new(PathBuf::from("/tmp"));
        ws.update_from_content(&import_path, imported_content);

        // The doc imports /tmp/mylib.ucg as `lib` then accesses `lib.foo`.
        let doc = analyze(
            r#"let lib = import "/tmp/mylib.ucg"; let y = lib.foo;"#,
            None,
        );

        // "lib.foo" — `foo` starts at char 47 (0-based) in the source above.
        // l=0, "let lib = import \"/tmp/mylib.ucg\"; let y = lib.foo;"
        //  0123456789...
        // "let lib = import \"/tmp/mylib.ucg\"; let y = lib." is 47 chars, foo at 47.
        let def = find_definition(&doc, &fake_uri(), 0, 47, &ws);
        assert!(def.is_some(), "should find cross-file dot definition");
        let loc = def.unwrap();
        assert_eq!(
            loc.uri,
            Url::from_file_path(&import_path).unwrap(),
            "should jump to the imported file"
        );
        // `foo` is defined at line 1 col 5 (1-based) → LSP (0, 4)
        assert_eq!(loc.range.start.line, 0);
        assert_eq!(loc.range.start.character, 4);
    }

    #[test]
    fn test_find_definition_dot_context_field_not_in_imported_doc() {
        // Cursor on a field that doesn't exist in the indexed imported doc.
        // find_definition should fall through and return None (not in symbol_table either).
        let import_path = PathBuf::from("/tmp/mylib.ucg");
        let mut ws = WorkspaceIndex::new(PathBuf::from("/tmp"));
        ws.update_from_content(&import_path, "let bar = 1;");

        let doc = analyze(
            r#"let lib = import "/tmp/mylib.ucg"; let y = lib.missing;"#,
            None,
        );
        // "missing" starts at char 47
        let def = find_definition(&doc, &fake_uri(), 0, 47, &ws);
        // `missing` is not in the imported doc's symbol_table, and not in the current
        // file's symbol_table either → None
        assert!(def.is_none());
    }

    #[test]
    fn test_find_definition_local_tuple_field() {
        // Cursor on `x` in `foo.x` where `foo` is a local tuple — should jump to
        // the `x` field key inside the tuple literal.
        // "let foo = {x = 1}; let y = foo.x;"
        //  0         1         2         3
        //  0123456789012345678901234567890123
        // `x` in `{x = 1}` is at char 11 (0-based); `x` in `foo.x` is at char 31.
        let doc = analyze("let foo = {x = 1}; let y = foo.x;", None);
        let ws = empty_workspace();
        let def = find_definition(&doc, &fake_uri(), 0, 31, &ws);
        assert!(def.is_some(), "should find definition for tuple field");
        let loc = def.unwrap();
        assert_eq!(loc.uri, fake_uri(), "should stay in the same file");
        // `x` in `{x = 1}` is at 0-based char 11 → LSP char 11
        assert_eq!(loc.range.start.line, 0);
        assert_eq!(loc.range.start.character, 11);
    }

    #[test]
    fn test_find_definition_multi_level_dot_cross_file() {
        // Cursor on `x` in `lib.data.x` where `lib` is an absolute import and
        // `data` is a tuple binding in the imported file.
        // Imported: "let data = {x = 1};"
        //            x in {x = 1} is at char 12 (0-based)
        // Current:  "let lib = import \"/tmp/lib.ucg\"; let r = lib.data.x;"
        //            x at char 50 (0-based)
        let import_path = PathBuf::from("/tmp/lib.ucg");
        let mut ws = WorkspaceIndex::new(PathBuf::from("/tmp"));
        ws.update_from_content(&import_path, "let data = {x = 1};");

        let doc = analyze(
            r#"let lib = import "/tmp/lib.ucg"; let r = lib.data.x;"#,
            None,
        );
        let def = find_definition(&doc, &fake_uri(), 0, 50, &ws);
        assert!(def.is_some(), "should find multi-level cross-file definition");
        let loc = def.unwrap();
        assert_eq!(
            loc.uri,
            Url::from_file_path(&import_path).unwrap(),
            "should jump to imported file"
        );
        // `x` in `{x = 1}` in the imported file is at line 0, char 12 (0-based LSP)
        assert_eq!(loc.range.start.line, 0);
        assert_eq!(loc.range.start.character, 12);
    }

    // --- encode_semantic_tokens ---

    #[test]
    fn test_encode_semantic_tokens_basic() {
        // "let x = 1;" should produce: let(keyword), x(variable+decl), 1(number)
        let doc = analyze("let x = 1;", None);
        let tokens = encode_semantic_tokens(&doc);
        assert_eq!(tokens.len(), 3, "expected 3 semantic tokens, got {:?}", tokens);
        // let — keyword (type 0), no modifier
        assert_eq!(tokens[0].token_type, 0);
        assert_eq!(tokens[0].token_modifiers_bitset, 0);
        assert_eq!(tokens[0].length, 3);
        // x — variable (type 1), declaration modifier (bit 0 = 1)
        assert_eq!(tokens[1].token_type, 1);
        assert_eq!(tokens[1].token_modifiers_bitset, 1);
        assert_eq!(tokens[1].length, 1);
        // 1 — number (type 3)
        assert_eq!(tokens[2].token_type, 3);
        assert_eq!(tokens[2].token_modifiers_bitset, 0);
    }

    #[test]
    fn test_encode_semantic_tokens_delta_encoding_same_line() {
        // Two tokens on the same line: second delta_start is relative to first
        let doc = analyze("let x = 1;", None);
        let tokens = encode_semantic_tokens(&doc);
        // let at col 0, x at col 4: delta_start for x = 4 - 0 = 4
        assert_eq!(tokens[0].delta_line, 0);
        assert_eq!(tokens[0].delta_start, 0); // first token: absolute col 0
        assert_eq!(tokens[1].delta_line, 0);
        assert_eq!(tokens[1].delta_start, 4); // col 4 - col 0 = 4
        assert_eq!(tokens[2].delta_line, 0);
        assert_eq!(tokens[2].delta_start, 4); // col 8 - col 4 = 4
    }

    #[test]
    fn test_encode_semantic_tokens_delta_encoding_new_line() {
        // Token on a new line: delta_line > 0, delta_start is absolute col
        let doc = analyze("let x = 1;\nlet y = 2;", None);
        let tokens = encode_semantic_tokens(&doc);
        // First line: let(0,0), x(0,4), 1(0,8)
        // Second line: let(1, *), y(1, *), 2(1, *)
        // The 4th token (let on line 2) should have delta_line=1
        assert!(tokens.len() >= 4);
        assert_eq!(tokens[3].delta_line, 1);
        assert_eq!(tokens[3].delta_start, 0); // "let" starts at col 0 on new line
    }

    #[test]
    fn test_encode_semantic_tokens_string() {
        let doc = analyze(r#"let x = "hello";"#, None);
        let tokens = encode_semantic_tokens(&doc);
        // Should have: let(kw), x(var+decl), "hello"(string)
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[2].token_type, 2); // string
    }

    #[test]
    fn test_encode_semantic_tokens_operator() {
        let doc = analyze("let x = 1 + 2;", None);
        let tokens = encode_semantic_tokens(&doc);
        // let, x, 1, +, 2
        assert_eq!(tokens.len(), 5);
        // "+" should be operator (type 5)
        let op = tokens.iter().find(|t| t.token_type == 5);
        assert!(op.is_some(), "should have an operator token");
    }

    #[test]
    fn test_encode_semantic_tokens_import_namespace() {
        let doc = analyze(r#"let lists = import "std/lists.ucg";"#, None);
        let tokens = encode_semantic_tokens(&doc);
        // "lists" after the binding is established should be namespace (type 6)
        // But in this binding it's a declaration — let's check the string token type
        // The "std/lists.ucg" string should be type 2
        let string_tok = tokens.iter().find(|t| t.token_type == 2);
        assert!(string_tok.is_some(), "should have string token for path");
    }

    // --- format_shape ---

    #[test]
    fn test_format_shape_primitives() {
        use crate::ast::{Position, Shape};
        let pos = Position::new(1, 1, 0);
        assert_eq!(format_shape(&Shape::Boolean(pos.clone())), "Bool");
        assert_eq!(format_shape(&Shape::Int(pos.clone())), "Int");
        assert_eq!(format_shape(&Shape::Float(pos.clone())), "Float");
        assert_eq!(format_shape(&Shape::Str(pos.clone())), "Str");
    }

    #[test]
    fn test_format_shape_list_uniform() {
        let doc = analyze("let l = [1, 2, 3];", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("l")).unwrap();
        assert_eq!(format_shape(shape), "[Int]");
    }

    #[test]
    fn test_format_shape_list_of_tuples() {
        let doc = analyze("let l = [{x = 1, y = 2}, {x = 3, y = 4}];", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("l")).unwrap();
        assert_eq!(format_shape(shape), "[{x: Int, y: Int}]");
    }

    #[test]
    fn test_format_shape_list_mixed() {
        let doc = analyze("let l = [1, \"hi\"];", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("l")).unwrap();
        assert_eq!(format_shape(shape), "[Int | Str]");
    }

    #[test]
    fn test_format_shape_tuple_shows_fields() {
        let doc = analyze("let t = {x = 1, y = \"hi\"};", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("t")).unwrap();
        let s = format_shape(shape);
        assert_eq!(s, "{x: Int, y: Str}");
    }

    #[test]
    fn test_format_shape_nested_tuple() {
        let doc = analyze("let t = {inner = {a = 1.0}};", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("t")).unwrap();
        let s = format_shape(shape);
        assert_eq!(s, "{inner: {a: Float}}");
    }

    #[test]
    fn test_format_shape_narrowed_shows_union() {
        // select expressions produce Shape::Narrowed across their branches
        let doc = analyze(
            r#"let x = select ("a", 0) => {a = 1, b = "hi"};"#,
            None,
        );
        let (shape, _) = doc.symbol_table.get(&Rc::from("x")).unwrap();
        let s = format_shape(shape);
        // Should show a union of the branch types, not the raw word "Narrowed"
        assert_ne!(s, "Narrowed", "should not show raw 'Narrowed', got: {}", s);
    }

    #[test]
    fn test_format_shape_func_shows_signature() {
        let doc = analyze("let f = func(x, y) => x + y;", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("f")).unwrap();
        let s = format_shape(shape);
        // Should show arg names, inferred types, and return type
        assert!(s.starts_with("func("), "got: {}", s);
        assert!(s.contains("x:"), "got: {}", s);
        assert!(s.contains("y:"), "got: {}", s);
        assert!(s.contains("=>"), "got: {}", s);
    }

    #[test]
    fn test_format_shape_func_typed_args() {
        let doc = analyze("let f = func(x) => x + 1;", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("f")).unwrap();
        let s = format_shape(shape);
        assert_eq!(s, "func(x: Int) => Int");
    }

    #[test]
    fn test_format_shape_module_shows_return_type() {
        // Derive a real module shape via analyze to avoid constructing private fields.
        let doc = analyze("let m = module {} => (1) {};", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("m")).unwrap();
        assert_eq!(format_shape(shape), "Module => Int");
    }

    #[test]
    fn test_format_shape_module_no_out_expr() {
        // Without an explicit out_expr, the module returns a tuple of all let bindings.
        let doc = analyze("let m = module {} => { let x = 1; };", None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("m")).unwrap();
        let s = format_shape(shape);
        assert_eq!(s, "Module => {x: Int}", "got: {}", s);
    }

    #[test]
    fn test_format_shape_resolved_import_shows_fields() {
        let import_path = PathBuf::from("/tmp/mylib.ucg");
        let mut resolved = std::collections::HashMap::new();
        let imported =
            analysis::analyze("let foo = 1; let bar = \"hi\";", None, &std::collections::HashMap::new());
        resolved.insert(import_path.clone(), imported);
        let doc = analysis::analyze(r#"let lib = import "/tmp/mylib.ucg";"#, None, &resolved);
        let (shape, _) = doc.symbol_table.get(&Rc::from("lib")).unwrap();
        let s = format_shape(shape);
        // Should show the exported fields, not just "import(...)"
        assert!(s.contains("foo"), "got: {}", s);
        assert!(s.contains("bar"), "got: {}", s);
    }

    #[test]
    fn test_format_shape_unresolved_import_shows_path() {
        let doc = analyze(r#"let lib = import "/tmp/mylib.ucg";"#, None);
        let (shape, _) = doc.symbol_table.get(&Rc::from("lib")).unwrap();
        let s = format_shape(shape);
        assert!(s.contains("/tmp/mylib.ucg"), "got: {}", s);
    }

    #[test]
    fn test_find_hover_resolved_import_shows_fields() {
        let import_path = PathBuf::from("/tmp/mylib.ucg");
        let mut resolved = std::collections::HashMap::new();
        let imported =
            analysis::analyze("let foo = 1; let bar = \"hi\";", None, &std::collections::HashMap::new());
        resolved.insert(import_path.clone(), imported);
        let doc = analysis::analyze(r#"let lib = import "/tmp/mylib.ucg";"#, None, &resolved);
        // hover on `lib` at (0, 4)
        let hover = find_hover(&doc, 0, 4);
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("foo"), "got: {}", mc.value);
            assert!(mc.value.contains("bar"), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    // --- find_hover_dot_expr ---

    #[test]
    fn test_find_hover_dot_expr_one_level() {
        // `lib.foo` — cursor on `foo` → should show Int
        let import_path = PathBuf::from("/tmp/mylib.ucg");
        let mut resolved = std::collections::HashMap::new();
        let imported =
            analysis::analyze("let foo = 1;", None, &std::collections::HashMap::new());
        resolved.insert(import_path.clone(), imported);
        let src = r#"let lib = import "/tmp/mylib.ucg"; let x = lib.foo;"#;
        // `foo` starts at col 47 (0-based)
        let doc = analysis::analyze(src, None, &resolved);
        let hover = find_hover(&doc, 0, 47);
        assert!(hover.is_some(), "expected hover on dot field");
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("Int"), "got: {}", mc.value);
            assert!(mc.value.contains("lib.foo"), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_find_hover_dot_expr_two_levels() {
        // `t.inner.y` — cursor on `y` → should show Str
        let src = "let t = {inner = {y = \"hi\"}};  let z = t.inner.y;";
        // `y` is at the end: "t.inner.y" starts at col 40 for `t`, `.inner` at 41, `.y` at 47
        // Let's compute: "let z = t.inner.y;" starts at col 31
        // t=31+8=39 (0-based), '.inner' is 39+1=40, 'y'=40+5+1=46 for `.y`
        // Actually let's just search
        let doc = analysis::analyze(src, None, &std::collections::HashMap::new());
        // Find the position of the last `y` token
        let y_col = src.rfind('y').unwrap() as u32;
        let hover = find_hover(&doc, 0, y_col);
        assert!(hover.is_some(), "expected hover on nested dot field");
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("Str"), "got: {}", mc.value);
            assert!(mc.value.contains("t.inner.y"), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_find_hover_dot_expr_missing_field_falls_through() {
        // `t.missing` — cursor on `missing` → dot hover returns None, falls through to symbol_table
        // Since `missing` is not a key there either, hover should be None.
        let src = "let t = {x = 1}; let z = t.missing;";
        let doc = analysis::analyze(src, None, &std::collections::HashMap::new());
        let missing_col = src.find("missing").unwrap() as u32;
        let hover = find_hover(&doc, 0, missing_col);
        // `missing` is not in the tuple shape → dot hover fails
        // `missing` is not in symbol_table either → overall None
        assert!(hover.is_none());
    }

    // --- doc comments in hover ---

    #[test]
    fn test_hover_shows_binding_doc_comment() {
        // Comment immediately above binding should appear in hover.
        let src = "// Returns the answer.\nlet x = 42;";
        let doc = analyze(src, None);
        let hover = find_hover(&doc, 1, 4); // line 1 (0-based), col 4 = `x`
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("Returns the answer."), "got: {}", mc.value);
            // Doc comment should appear before the type line.
            let doc_pos = mc.value.find("Returns the answer.").unwrap();
            let type_pos = mc.value.find("**type**").unwrap();
            assert!(doc_pos < type_pos, "doc comment should precede type info");
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_hover_no_doc_comment_when_blank_line_between() {
        // A blank line between comment and binding disqualifies the comment.
        let src = "// Unrelated comment.\n\nlet x = 42;";
        let doc = analyze(src, None);
        let hover = find_hover(&doc, 2, 4); // `x` on line 2 (0-based)
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(!mc.value.contains("Unrelated comment."), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_hover_import_shows_file_doc() {
        // Imported file has a file-level doc (2 blank lines before first binding).
        let import_path = PathBuf::from("/tmp/mylib.ucg");
        let mut resolved = std::collections::HashMap::new();
        let imported = analysis::analyze(
            "// Utilities for working with lists.\n// Use freely.\n\n\nlet foo = 1;",
            None,
            &std::collections::HashMap::new(),
        );
        resolved.insert(import_path.clone(), imported);
        let mut ws = WorkspaceIndex::new(PathBuf::from("/tmp"));
        ws.update_from_content(
            &import_path,
            "// Utilities for working with lists.\n// Use freely.\n\n\nlet foo = 1;",
        );
        let src = r#"let lib = import "/tmp/mylib.ucg";"#;
        let doc = analysis::analyze(src, None, &resolved);
        let hover = super::find_hover(&doc, &ws, 0, 4); // hover on `lib`
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("Utilities for working with lists."), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_hover_import_no_file_doc_when_only_one_blank_line() {
        // Only 1 blank line before first binding: not a file-level doc.
        let import_path = PathBuf::from("/tmp/mylib.ucg");
        let mut ws = WorkspaceIndex::new(PathBuf::from("/tmp"));
        ws.update_from_content(
            &import_path,
            "// Not a file doc.\n\nlet foo = 1;",
        );
        let src = r#"let lib = import "/tmp/mylib.ucg";"#;
        let doc = analysis::analyze(src, None, ws.resolved_files());
        let hover = super::find_hover(&doc, &ws, 0, 4);
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(!mc.value.contains("Not a file doc."), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    #[test]
    fn test_hover_dot_expr_shows_field_doc_comment() {
        // `lib.foo` — `foo` has a doc comment in the imported file.
        let import_path = PathBuf::from("/tmp/mylib.ucg");
        let mut ws = WorkspaceIndex::new(PathBuf::from("/tmp"));
        ws.update_from_content(&import_path, "// The answer.\nlet foo = 42;");
        let src = r#"let lib = import "/tmp/mylib.ucg"; let x = lib.foo;"#;
        let doc = analysis::analyze(src, None, ws.resolved_files());
        let foo_col = src.rfind("foo").unwrap() as u32;
        let hover = super::find_hover(&doc, &ws, 0, foo_col);
        assert!(hover.is_some());
        if let Some(Hover { contents: HoverContents::Markup(mc), .. }) = hover {
            assert!(mc.value.contains("The answer."), "got: {}", mc.value);
        } else {
            panic!("expected Markup hover");
        }
    }

    // --- shape_to_symbol_kind ---

    #[test]
    fn test_shape_to_symbol_kind() {
        // Derive real shapes via analyze to avoid constructing private struct fields.
        let func_doc = analyze("let f = func(x) => x + 1;", None);
        let (func_shape, _) = func_doc.symbol_table.get(&Rc::from("f")).unwrap();
        assert_eq!(shape_to_symbol_kind(func_shape), SymbolKind::FUNCTION);

        let mod_doc = analyze("let m = module {} => (1) {};", None);
        let (mod_shape, _) = mod_doc.symbol_table.get(&Rc::from("m")).unwrap();
        assert_eq!(shape_to_symbol_kind(mod_shape), SymbolKind::MODULE);

        let tuple_doc = analyze("let t = {x = 1};", None);
        let (tuple_shape, _) = tuple_doc.symbol_table.get(&Rc::from("t")).unwrap();
        assert_eq!(shape_to_symbol_kind(tuple_shape), SymbolKind::STRUCT);

        let int_doc = analyze("let n = 1;", None);
        let (int_shape, _) = int_doc.symbol_table.get(&Rc::from("n")).unwrap();
        assert_eq!(shape_to_symbol_kind(int_shape), SymbolKind::VARIABLE);
    }

    // --- collect_workspace_symbols ---

    #[test]
    fn test_collect_workspace_symbols_empty_query() {
        let mut ws = empty_workspace();
        ws.update_from_content(
            &PathBuf::from("/tmp/test.ucg"),
            "let x = 1;\nlet y = \"hello\";",
        );
        let results = collect_workspace_symbols(&ws, "");
        assert_eq!(results.len(), 2, "empty query should return all symbols");
        let names: Vec<&str> = results.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"x"));
        assert!(names.contains(&"y"));
    }

    #[test]
    fn test_collect_workspace_symbols_filtered() {
        let mut ws = empty_workspace();
        ws.update_from_content(
            &PathBuf::from("/tmp/test.ucg"),
            "let foo = 1;\nlet bar = 2;\nlet foobar = 3;",
        );
        let results = collect_workspace_symbols(&ws, "foo");
        assert_eq!(results.len(), 2, "should match 'foo' and 'foobar'");
    }

    #[test]
    fn test_collect_workspace_symbols_no_match() {
        let mut ws = empty_workspace();
        ws.update_from_content(&PathBuf::from("/tmp/test.ucg"), "let x = 1;");
        let results = collect_workspace_symbols(&ws, "zzz");
        assert!(results.is_empty());
    }

    #[test]
    fn test_collect_workspace_symbols_kind() {
        let mut ws = empty_workspace();
        ws.update_from_content(
            &PathBuf::from("/tmp/test.ucg"),
            "let f = func(x) => x + 1;",
        );
        let results = collect_workspace_symbols(&ws, "f");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].kind, SymbolKind::FUNCTION);
    }
}
