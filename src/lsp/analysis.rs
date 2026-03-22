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

//! Per-document analysis: tokenize → parse → type-check, lenient.
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use lsp_types::{Diagnostic, DiagnosticSeverity, Position as LspPosition, Range};

use crate::ast::typecheck::DeriveShape;
use crate::ast::{Expression, Position, Shape, Statement, Token};
use crate::error::{BuildError, ErrorType};
use crate::iter::OffsetStrIter;
use crate::parse::parse;
use crate::tokenizer::tokenize;

/// Results of analyzing a single UCG document in memory.
pub struct AnalysisResult {
    pub ast: Option<Vec<Statement>>,
    pub diagnostics: Vec<Diagnostic>,
    /// Maps binding name → (shape, definition position).
    pub symbol_table: BTreeMap<Rc<str>, (Shape, Position)>,
    /// All tokens from the document (used for hover/definition position lookup).
    pub tokens: Vec<Token>,
    /// Maps binding name → resolved path for `let x = import "..."` bindings.
    /// Path is absolute for user files; relative (e.g. "std/lists.ucg") for stdlib.
    pub import_map: HashMap<Rc<str>, PathBuf>,
}

impl AnalysisResult {
    pub fn empty() -> Self {
        AnalysisResult {
            ast: None,
            diagnostics: Vec::new(),
            symbol_table: BTreeMap::new(),
            tokens: Vec::new(),
            import_map: HashMap::new(),
        }
    }
}

/// Convert a UCG `BuildError` to an LSP `Diagnostic`.
fn build_error_to_diagnostic(e: &BuildError) -> Diagnostic {
    let range = e
        .pos
        .as_ref()
        .map(|p| ucg_pos_to_range(p))
        .unwrap_or_default();
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        message: e.msg.clone(),
        source: Some("ucg".to_string()),
        ..Default::default()
    }
}

/// Convert a UCG `Position` (1-based) to an LSP single-character `Range` (0-based).
pub fn ucg_pos_to_range(pos: &Position) -> Range {
    let line = pos.line.saturating_sub(1) as u32;
    let col = pos.column.saturating_sub(1) as u32;
    Range {
        start: LspPosition {
            line,
            character: col,
        },
        end: LspPosition {
            line,
            character: col + 1,
        },
    }
}

/// Check if a `Shape` is a type error and if so add it as a diagnostic.
fn collect_shape_errors(shape: &Shape, diagnostics: &mut Vec<Diagnostic>) {
    if let Shape::TypeErr(pos, msg) = shape {
        let e = BuildError::with_pos(msg.clone(), ErrorType::TypeFail, pos.clone());
        diagnostics.push(build_error_to_diagnostic(&e));
    }
}

/// Analyze a UCG document given its text content.
///
/// `working_dir` is the directory containing the document — used to resolve
/// relative import paths to absolute paths for go-to-definition.
///
/// Runs the tokenizer, parser, and type checker leniently:
/// errors at each stage are collected as LSP diagnostics rather than
/// aborting analysis.
pub fn analyze(content: &str, working_dir: Option<&Path>) -> AnalysisResult {
    let mut result = AnalysisResult::empty();

    // --- Tokenize ---
    let tokens = match tokenize(OffsetStrIter::new(content), None) {
        Ok(toks) => toks,
        Err(e) => {
            result.diagnostics.push(build_error_to_diagnostic(&e));
            return result;
        }
    };
    result.tokens = tokens;

    // --- Parse ---
    let ast = match parse(OffsetStrIter::new(content), None) {
        Ok(stmts) => stmts,
        Err(e) => {
            result.diagnostics.push(build_error_to_diagnostic(&e));
            return result;
        }
    };

    // --- Build symbol table from Let bindings + collect type errors + track imports ---
    let mut sym_map: BTreeMap<Rc<str>, Shape> = BTreeMap::new();
    for stmt in &ast {
        if let Statement::Let(def) = stmt {
            let shape = def.value.derive_shape(&mut sym_map);
            let name: Rc<str> = def.name.fragment.clone();
            collect_shape_errors(&shape, &mut result.diagnostics);
            result
                .symbol_table
                .insert(name.clone(), (shape.clone(), def.name.pos.clone()));
            sym_map.insert(name.clone(), shape);

            // Track import bindings for go-to-definition.
            if let Expression::Import(import_def) = &def.value {
                let path_str = import_def.path.fragment.as_ref();
                let resolved = if let Some(dir) = working_dir {
                    dir.join(path_str)
                } else {
                    PathBuf::from(path_str)
                };
                result.import_map.insert(name, resolved);
            }
        }
    }

    result.ast = Some(ast);
    result
}
