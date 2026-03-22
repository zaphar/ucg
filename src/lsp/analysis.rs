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
use crate::ast::{Expression, ImportShape, Position, PositionedItem, Shape, Statement, Token};
use crate::error::{BuildError, ErrorType};
use crate::iter::OffsetStrIter;
use crate::parse::parse;
use crate::tokenizer::{tokenize, CommentMap};

/// Results of analyzing a single UCG document in memory.
pub struct AnalysisResult {
    pub ast: Option<Vec<Statement>>,
    pub diagnostics: Vec<Diagnostic>,
    /// Maps binding name → (shape, definition position) for top-level `let` bindings.
    pub symbol_table: BTreeMap<Rc<str>, (Shape, Position)>,
    /// All tokens from the document (used for hover/definition position lookup).
    pub tokens: Vec<Token>,
    /// Maps binding name → resolved path for `let x = import "..."` bindings.
    /// Path is absolute for user files; relative (e.g. "std/lists.ucg") for stdlib.
    pub import_map: HashMap<Rc<str>, PathBuf>,
    /// Position-keyed shape map for scoped names (function/module args, inner lets).
    /// Key is `(line, column)` using 1-based UCG coordinates matching `Token::pos`.
    /// Indexed by every token occurrence within a name's scope, not just definition
    /// sites — so hovering on a use of `x` inside a function body works too.
    pub token_types: HashMap<(usize, usize), Shape>,
    /// Position-keyed dotted path for tuple field tokens, e.g. `"config.host"`.
    /// Only populated for field name tokens inside tuple literals; absent for
    /// function/module args and inner lets where the bare name is sufficient.
    pub path_map: HashMap<(usize, usize), Rc<str>>,
    /// All comment groups in the document, keyed by the last comment line (1-based).
    pub comment_map: CommentMap,
    /// File-level doc comment: the first consecutive comment block in the file,
    /// provided it is separated from the first binding by at least 2 blank lines.
    pub file_doc: Option<String>,
}

impl AnalysisResult {
    pub fn empty() -> Self {
        AnalysisResult {
            ast: None,
            diagnostics: Vec::new(),
            symbol_table: BTreeMap::new(),
            tokens: Vec::new(),
            import_map: HashMap::new(),
            token_types: HashMap::new(),
            path_map: HashMap::new(),
            comment_map: CommentMap::new(),
            file_doc: None,
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

/// Recursively walk `shape` and replace any `ImportShape::Unresolved` with
/// `ImportShape::Resolved` using the workspace cache `resolved`.
/// This fixes imports that appear nested inside module return types (or tuples),
/// where `derive_shape` cannot access the LSP workspace cache.
fn resolve_imports_in_shape(
    shape: Shape,
    working_dir: Option<&Path>,
    resolved: &HashMap<PathBuf, AnalysisResult>,
) -> Shape {
    match shape {
        Shape::Import(ImportShape::Unresolved(ref pi)) => {
            let import_path = working_dir
                .map(|d| d.join(pi.val.as_ref()))
                .unwrap_or_else(|| PathBuf::from(pi.val.as_ref()));
            if let Some(imported) = resolved.get(&import_path) {
                let tuple_items: crate::ast::TupleShape = imported
                    .symbol_table
                    .iter()
                    .map(|(n, (s, p))| (PositionedItem::new(n.clone(), p.clone()), s.clone()))
                    .collect();
                Shape::Import(ImportShape::Resolved(pi.pos.clone(), tuple_items))
            } else {
                shape
            }
        }
        Shape::Tuple(pi) => {
            let new_val = pi
                .val
                .into_iter()
                .map(|(name, s)| (name, resolve_imports_in_shape(s, working_dir, resolved)))
                .collect();
            Shape::Tuple(PositionedItem::new(new_val, pi.pos))
        }
        Shape::Module(mshape) => {
            let new_ret =
                resolve_imports_in_shape(mshape.ret().clone(), working_dir, resolved);
            Shape::Module(mshape.with_ret(new_ret))
        }
        _ => shape,
    }
}

/// Render a comment group as plain text, stripping the `//` marker.
fn format_comment_group(group: &[crate::ast::Token]) -> String {
    group
        .iter()
        .map(|tok| tok.fragment.trim().to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Return the doc comment for a binding defined at `def_line` (1-based).
/// The comment group must end on the line immediately above (`def_line - 1`);
/// any blank line between comment and binding disqualifies it.
pub fn doc_comment_for_binding(comment_map: &CommentMap, def_line: usize) -> Option<String> {
    if def_line == 0 {
        return None;
    }
    comment_map
        .get(&(def_line - 1))
        .map(|g| format_comment_group(g))
}

/// Return the file-level doc comment: all comment groups that appear before
/// `first_code_line` and are separated from it by at least 2 blank lines
/// (`first_code_line >= group_last_line + 3`).
fn compute_file_doc(comment_map: &CommentMap, first_code_line: usize) -> Option<String> {
    let groups: Vec<String> = comment_map
        .iter()
        .filter(|(&last_line, _)| last_line + 3 <= first_code_line)
        .map(|(_, g)| format_comment_group(g))
        .collect();
    if groups.is_empty() {
        None
    } else {
        Some(groups.join("\n"))
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
pub fn analyze(
    content: &str,
    working_dir: Option<&Path>,
    resolved: &HashMap<PathBuf, AnalysisResult>,
) -> AnalysisResult {
    let mut result = AnalysisResult::empty();

    // --- Tokenize (also capture comments) ---
    let mut comment_map = CommentMap::new();
    let tokens = match tokenize(OffsetStrIter::new(content), Some(&mut comment_map)) {
        Ok(toks) => toks,
        Err(e) => {
            result.diagnostics.push(build_error_to_diagnostic(&e));
            return result;
        }
    };
    result.tokens = tokens;
    result.comment_map = comment_map;

    // --- Parse ---
    let ast = match parse(OffsetStrIter::new(content), None) {
        Ok(stmts) => stmts,
        Err(e) => {
            result.diagnostics.push(build_error_to_diagnostic(&e));
            return result;
        }
    };

    // --- Build symbol table from Let bindings + collect type errors + track imports ---
    // Compute per-let offset ranges so the scope collector can bound token scans.
    // let_ranges[i] = (start_offset, end_offset) for the i-th top-level Let.
    let let_stmts: Vec<&crate::ast::LetDef> = ast
        .iter()
        .filter_map(|s| {
            if let Statement::Let(d) = s {
                Some(d)
            } else {
                None
            }
        })
        .collect();
    let let_ranges: Vec<(usize, usize)> = let_stmts
        .iter()
        .enumerate()
        .map(|(i, def)| {
            let start = def.name.pos.offset;
            let end = let_stmts
                .get(i + 1)
                .map(|next| next.name.pos.offset)
                .unwrap_or(usize::MAX);
            (start, end)
        })
        .collect();

    // Compute file-level doc comment using the 2-blank-line rule.
    let first_code_line = let_stmts
        .first()
        .map(|def| def.name.pos.line)
        .unwrap_or(usize::MAX);
    result.file_doc = compute_file_doc(&result.comment_map, first_code_line);

    let mut sym_map: BTreeMap<Rc<str>, Shape> = BTreeMap::new();
    for (i, stmt) in ast.iter().enumerate() {
        if let Statement::Let(def) = stmt {
            let shape = def.value.derive_shape(&mut sym_map);
            let name: Rc<str> = def.name.fragment.clone();
            collect_shape_errors(&shape, &mut result.diagnostics);
            result
                .symbol_table
                .insert(name.clone(), (shape.clone(), def.name.pos.clone()));
            sym_map.insert(name.clone(), shape.clone());

            // Track import bindings for go-to-definition, and resolve the import
            // shape from the pre-analyzed cache when available.
            if let Expression::Import(import_def) = &def.value {
                let path_str = import_def.path.fragment.as_ref();
                let import_path = if let Some(dir) = working_dir {
                    dir.join(path_str)
                } else {
                    PathBuf::from(path_str)
                };
                result.import_map.insert(name.clone(), import_path.clone());

                if let Some(imported) = resolved.get(&import_path) {
                    let tuple_items: crate::ast::TupleShape = imported
                        .symbol_table
                        .iter()
                        .map(|(n, (s, p))| {
                            (PositionedItem::new(n.clone(), p.clone()), s.clone())
                        })
                        .collect();
                    let resolved_shape = Shape::Import(ImportShape::Resolved(
                        import_def.path.pos.clone(),
                        tuple_items,
                    ));
                    result
                        .symbol_table
                        .insert(name.clone(), (resolved_shape.clone(), def.name.pos.clone()));
                    sym_map.insert(name.clone(), resolved_shape);
                }
            }

            // Resolve any unresolved imports that are nested inside the shape
            // (e.g., import bindings inside a module return type).
            if let Some((shape, pos)) = result.symbol_table.get(&name).map(|(s, p)| (s.clone(), p.clone())) {
                let fully_resolved = resolve_imports_in_shape(shape, working_dir, resolved);
                result.symbol_table.insert(name.clone(), (fully_resolved.clone(), pos));
                sym_map.insert(name.clone(), fully_resolved);
            }

            // Collect scoped names (func/module args, inner lets, tuple fields) into
            // token_types and path_map, using the binding name as the path root.
            let scope_range = let_ranges[i];
            collect_scoped_names(
                &def.value,
                &mut sym_map.clone(),
                scope_range,
                &result.tokens,
                &mut result.token_types,
                &mut result.path_map,
                def.name.fragment.as_ref(),
            );
        }
    }

    // Post-process token_types: resolve any unresolved imports so that hovering
    // on import bindings inside module/func bodies shows the exported fields.
    let resolved_token_types: HashMap<(usize, usize), Shape> = result
        .token_types
        .into_iter()
        .map(|(pos, shape)| (pos, resolve_imports_in_shape(shape, working_dir, resolved)))
        .collect();
    result.token_types = resolved_token_types;

    result.ast = Some(ast);
    result
}

/// Record every token occurrence of `name` within `scope_range` (offset-bounded)
/// into `token_types` with `shape`.
fn record_token_occurrences(
    name: &Rc<str>,
    shape: &Shape,
    scope_range: (usize, usize),
    tokens: &[Token],
    token_types: &mut HashMap<(usize, usize), Shape>,
) {
    use crate::ast::TokenType;
    let (start, end) = scope_range;
    for tok in tokens {
        if tok.typ == TokenType::BAREWORD
            && tok.fragment == *name
            && tok.pos.offset >= start
            && tok.pos.offset < end
        {
            token_types.insert((tok.pos.line, tok.pos.column), shape.clone());
        }
    }
}

/// Walk an expression, collecting all scoped names (function args, module args,
/// inner `let` bindings) into `token_types` using position-keyed entries.
///
/// `scope_range` is the `(start_offset, end_offset)` of the enclosing top-level
/// let binding, used to bound token scans so that two different functions with
/// the same argument name don't overwrite each other's entries.
fn collect_scoped_names(
    expr: &Expression,
    sym_map: &mut BTreeMap<Rc<str>, Shape>,
    scope_range: (usize, usize),
    tokens: &[Token],
    token_types: &mut HashMap<(usize, usize), Shape>,
    path_map: &mut HashMap<(usize, usize), Rc<str>>,
    path: &str,
) {
    match expr {
        Expression::Func(func_def) => {
            let shape = expr.derive_shape(sym_map);
            if let Shape::Func(fdef) = &shape {
                for (arg_name, arg_shape) in fdef.args() {
                    record_token_occurrences(arg_name, arg_shape, scope_range, tokens, token_types);
                }
            }
            // Descend into the body with args in scope so inner lets see them.
            let mut inner_sym = sym_map.clone();
            if let Shape::Func(fdef) = &shape {
                for (arg_name, arg_shape) in fdef.args() {
                    inner_sym.insert(arg_name.clone(), arg_shape.clone());
                }
            }
            collect_scoped_names(
                &func_def.fields,
                &mut inner_sym,
                scope_range,
                tokens,
                token_types,
                path_map,
                path,
            );
        }

        Expression::Module(mod_def) => {
            let shape = expr.derive_shape(sym_map);
            if let Shape::Module(mdef) = &shape {
                for (name_pi, item_shape) in mdef.items() {
                    record_token_occurrences(
                        &name_pi.val,
                        item_shape,
                        scope_range,
                        tokens,
                        token_types,
                    );
                }
            }
            // Walk module body statements.
            // Inject the `mod` binding so inner lets that reference `mod.field`
            // resolve to the correct shapes rather than Shape::Hole.
            let mut inner_sym = sym_map.clone();
            if let Shape::Module(mdef) = &shape {
                let mod_key: Rc<str> = "mod".into();
                let mod_pos = mod_def
                    .arg_set
                    .first()
                    .map(|(tok, _, _)| tok.pos.clone())
                    .unwrap_or_else(|| mod_def.pos.clone());
                inner_sym.insert(
                    mod_key,
                    Shape::Tuple(crate::ast::PositionedItem {
                        pos: mod_pos,
                        val: mdef.items().clone(),
                    }),
                );
            }
            for stmt in &mod_def.statements {
                if let Statement::Let(inner_def) = stmt {
                    let inner_shape = inner_def.value.derive_shape(&mut inner_sym);
                    let inner_name: Rc<str> = inner_def.name.fragment.clone();
                    record_token_occurrences(
                        &inner_name,
                        &inner_shape,
                        scope_range,
                        tokens,
                        token_types,
                    );
                    inner_sym.insert(inner_name, inner_shape.clone());
                    collect_scoped_names(
                        &inner_def.value,
                        &mut inner_sym,
                        scope_range,
                        tokens,
                        token_types,
                        path_map,
                        path,
                    );
                }
            }
            if let Some(out_expr) = &mod_def.out_expr {
                collect_scoped_names(
                    out_expr,
                    &mut inner_sym,
                    scope_range,
                    tokens,
                    token_types,
                    path_map,
                    path,
                );
            }
        }

        Expression::Simple(crate::ast::Value::Tuple(pi)) => {
            for (field_tok, _constraint, field_expr) in &pi.val {
                let field_name: Rc<str> = field_tok.fragment.clone();
                let field_shape = field_expr.derive_shape(sym_map);
                // Register the field name token at its exact definition position.
                token_types.insert(
                    (field_tok.pos.line, field_tok.pos.column),
                    field_shape.clone(),
                );
                // Record the dotted path for hover display.
                let field_path: Rc<str> = format!("{}.{}", path, field_name).into();
                path_map.insert((field_tok.pos.line, field_tok.pos.column), field_path.clone());
                // Recurse into the field value with the extended path.
                collect_scoped_names(
                    field_expr,
                    sym_map,
                    scope_range,
                    tokens,
                    token_types,
                    path_map,
                    &field_path,
                );
            }
        }

        // Descend through expression wrappers that can contain funcs/modules/tuples.
        Expression::Binary(def) => {
            collect_scoped_names(&def.left, sym_map, scope_range, tokens, token_types, path_map, path);
            collect_scoped_names(&def.right, sym_map, scope_range, tokens, token_types, path_map, path);
        }
        Expression::Grouped(inner, _) => {
            collect_scoped_names(inner, sym_map, scope_range, tokens, token_types, path_map, path);
        }
        Expression::Call(call_def) => {
            for arg in &call_def.arglist {
                collect_scoped_names(arg, sym_map, scope_range, tokens, token_types, path_map, path);
            }
        }
        Expression::Copy(copy_def) => {
            for (_, _, field_expr) in &copy_def.fields {
                collect_scoped_names(field_expr, sym_map, scope_range, tokens, token_types, path_map, path);
            }
        }
        Expression::FuncOp(op_def) => {
            use crate::ast::FuncOpDef;
            match op_def {
                FuncOpDef::Map(def) | FuncOpDef::Filter(def) => {
                    collect_scoped_names(&def.func, sym_map, scope_range, tokens, token_types, path_map, path);
                    collect_scoped_names(&def.target, sym_map, scope_range, tokens, token_types, path_map, path);
                }
                FuncOpDef::Reduce(def) => {
                    collect_scoped_names(&def.func, sym_map, scope_range, tokens, token_types, path_map, path);
                    collect_scoped_names(&def.acc, sym_map, scope_range, tokens, token_types, path_map, path);
                    collect_scoped_names(&def.target, sym_map, scope_range, tokens, token_types, path_map, path);
                }
            }
        }
        Expression::Select(sel_def) => {
            collect_scoped_names(&sel_def.val, sym_map, scope_range, tokens, token_types, path_map, path);
            if let Some(default) = &sel_def.default {
                collect_scoped_names(default, sym_map, scope_range, tokens, token_types, path_map, path);
            }
            for (_, _, field_expr) in &sel_def.tuple {
                collect_scoped_names(field_expr, sym_map, scope_range, tokens, token_types, path_map, path);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;
    use std::rc::Rc;

    use super::*;

    /// Test helper: analyze without a resolved-imports cache.
    fn analyze(content: &str, working_dir: Option<&Path>) -> AnalysisResult {
        super::analyze(content, working_dir, &HashMap::new())
    }

    // --- ucg_pos_to_range ---

    #[test]
    fn test_ucg_pos_to_range_line1_col1() {
        let pos = Position { line: 1, column: 1, offset: 0, file: None };
        let range = ucg_pos_to_range(&pos);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 1);
    }

    #[test]
    fn test_ucg_pos_to_range_interior() {
        let pos = Position { line: 5, column: 10, offset: 0, file: None };
        let range = ucg_pos_to_range(&pos);
        assert_eq!(range.start.line, 4);
        assert_eq!(range.start.character, 9);
        assert_eq!(range.end.line, 4);
        assert_eq!(range.end.character, 10);
    }

    // --- analyze: basic ---

    #[test]
    fn test_analyze_empty_document() {
        let result = analyze("", None);
        assert!(result.diagnostics.is_empty());
        assert!(result.symbol_table.is_empty());
        // The tokenizer emits an END sentinel even for empty input, so tokens is non-empty.
        // What matters is the symbol table is empty and there are no diagnostics.
        assert!(result.ast.is_some());
    }

    #[test]
    fn test_analyze_simple_let_int() {
        let result = analyze("let x = 1;", None);
        assert!(result.diagnostics.is_empty(), "unexpected diagnostics: {:?}", result.diagnostics);
        let key: Rc<str> = "x".into();
        assert!(result.symbol_table.contains_key(&key));
        let (shape, pos) = result.symbol_table.get(&key).unwrap();
        assert!(matches!(shape, Shape::Int(_)), "expected Int, got {:?}", shape);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 5);
        assert!(!result.tokens.is_empty());
    }

    #[test]
    fn test_analyze_multiple_lets() {
        let result = analyze("let x = 1;\nlet y = \"hello\";", None);
        assert!(result.diagnostics.is_empty());
        assert!(result.symbol_table.contains_key(&Rc::from("x")));
        assert!(result.symbol_table.contains_key(&Rc::from("y")));
    }

    // --- analyze: error paths ---

    #[test]
    fn test_analyze_parse_error_becomes_diagnostic() {
        // "let = 1;" is a parse error (missing name)
        let result = analyze("let = 1;", None);
        assert!(
            !result.diagnostics.is_empty(),
            "parse error should produce a diagnostic"
        );
        assert_eq!(
            result.diagnostics[0].severity,
            Some(lsp_types::DiagnosticSeverity::ERROR)
        );
    }

    #[test]
    fn test_analyze_type_error_becomes_diagnostic() {
        // Adding an Int and a Str is a type error
        let result = analyze("let x = 1 + \"hello\";", None);
        assert!(
            !result.diagnostics.is_empty(),
            "type error should produce a diagnostic"
        );
    }

    // --- analyze: import tracking ---

    #[test]
    fn test_analyze_import_with_working_dir() {
        let result = analyze(
            r#"let lists = import "std/lists.ucg";"#,
            Some(Path::new("/project")),
        );
        let key: Rc<str> = "lists".into();
        assert!(result.import_map.contains_key(&key));
        let path = result.import_map.get(&key).unwrap();
        assert_eq!(path, &std::path::PathBuf::from("/project/std/lists.ucg"));
        assert!(path.is_absolute());
    }

    #[test]
    fn test_analyze_import_no_working_dir() {
        let result = analyze(r#"let lists = import "std/lists.ucg";"#, None);
        let key: Rc<str> = "lists".into();
        assert!(result.import_map.contains_key(&key));
        let path = result.import_map.get(&key).unwrap();
        // Without working_dir the path is relative
        assert!(!path.is_absolute());
        assert_eq!(path, &std::path::PathBuf::from("std/lists.ucg"));
    }

    // --- analyze: token_types for scoped names ---

    #[test]
    fn test_analyze_func_arg_at_definition_site() {
        // "let f = func(x) => x + 1;"
        //  col:  5        14    20
        let result = analyze("let f = func(x) => x + 1;", None);
        assert!(result.diagnostics.is_empty());
        // x at col 14 (arg def) should be in token_types
        assert!(
            result.token_types.contains_key(&(1, 14)),
            "arg def position (1,14) missing from token_types; keys: {:?}",
            result.token_types.keys().collect::<Vec<_>>()
        );
        // x at col 20 (body use) should also be indexed
        assert!(
            result.token_types.contains_key(&(1, 20)),
            "arg use position (1,20) missing from token_types"
        );
        // Both should have the same Int shape
        let def_shape = result.token_types.get(&(1, 14)).unwrap();
        let use_shape = result.token_types.get(&(1, 20)).unwrap();
        assert!(matches!(def_shape, Shape::Int(_)));
        assert!(matches!(use_shape, Shape::Int(_)));
    }

    #[test]
    fn test_analyze_scope_bounding_two_funcs_same_arg() {
        // Two functions with the same arg name must not share token_types entries.
        // Each `x` token should be attributed to its own function's scope.
        let src = "let f = func(x) => x + 1;\nlet g = func(x) => x + 2;";
        let result = analyze(src, None);
        assert!(result.diagnostics.is_empty());
        // f's x is at line 1 col 14 (def) and col 20 (use)
        // g's x is at line 2 col 14 (def) and col 20 (use)
        assert!(result.token_types.contains_key(&(1, 14)), "f's arg def missing");
        assert!(result.token_types.contains_key(&(1, 20)), "f's arg use missing");
        assert!(result.token_types.contains_key(&(2, 14)), "g's arg def missing");
        assert!(result.token_types.contains_key(&(2, 20)), "g's arg use missing");
    }

    #[test]
    fn test_analyze_module_arg_in_token_types() {
        // "let m = module { x = 0, } => (mod.x) {};"
        // x at the arg def position should be in token_types
        let result = analyze("let m = module { x = 0, } => (mod.x) {};", None);
        assert!(result.diagnostics.is_empty());
        // There should be at least one entry in token_types from the module arg
        assert!(
            !result.token_types.is_empty(),
            "module arg should populate token_types"
        );
    }

    // --- analyze: resolved import shapes ---

    #[test]
    fn test_analyze_with_resolved_import_shapes_fields() {
        // Simulate having already analyzed an imported file.
        let import_path = std::path::PathBuf::from("/tmp/mylib.ucg");
        let mut resolved: HashMap<std::path::PathBuf, AnalysisResult> = HashMap::new();
        let imported = super::analyze("let foo = 42; let bar = \"hi\";", None, &HashMap::new());
        resolved.insert(import_path.clone(), imported);

        let result = super::analyze(
            r#"let lib = import "/tmp/mylib.ucg";"#,
            None,
            &resolved,
        );
        assert!(result.diagnostics.is_empty());
        let (shape, _) = result.symbol_table.get(&Rc::from("lib")).unwrap();
        // Should be Resolved, not Unresolved
        match shape {
            Shape::Import(ImportShape::Resolved(_, fields)) => {
                let names: Vec<&str> = fields.iter().map(|(n, _)| n.val.as_ref()).collect();
                assert!(names.contains(&"foo"), "should include foo field");
                assert!(names.contains(&"bar"), "should include bar field");
            }
            other => panic!("expected ImportShape::Resolved, got {:?}", other),
        }
    }

    #[test]
    fn test_analyze_without_resolved_import_stays_unresolved() {
        // Without a cache entry, the shape stays Unresolved.
        let result = super::analyze(
            r#"let lib = import "/tmp/mylib.ucg";"#,
            None,
            &HashMap::new(),
        );
        let (shape, _) = result.symbol_table.get(&Rc::from("lib")).unwrap();
        assert!(
            matches!(shape, Shape::Import(ImportShape::Unresolved(_))),
            "should remain Unresolved without cache"
        );
    }

    // --- analyze: tuple field path_map ---

    #[test]
    fn test_analyze_tuple_field_in_token_types() {
        // "let t = {x = 1, y = \"hi\"};"
        //          ^ x is at col 10 (1-based)
        let result = analyze("let t = {x = 1, y = \"hi\"};", None);
        assert!(result.diagnostics.is_empty());
        // x should be in token_types
        let x_pos = result
            .token_types
            .iter()
            .find(|(_, s)| matches!(s, Shape::Int(_)));
        assert!(x_pos.is_some(), "x field should be in token_types as Int");
    }

    #[test]
    fn test_analyze_tuple_field_path_map() {
        let result = analyze("let t = {x = 1, y = \"hi\"};", None);
        assert!(result.diagnostics.is_empty());
        // path_map should contain "t.x" and "t.y"
        let paths: Vec<&str> = result.path_map.values().map(|s| s.as_ref()).collect();
        assert!(paths.contains(&"t.x"), "expected t.x in path_map, got: {:?}", paths);
        assert!(paths.contains(&"t.y"), "expected t.y in path_map, got: {:?}", paths);
    }

    #[test]
    fn test_analyze_nested_tuple_field_path_map() {
        let result = analyze("let t = {outer = {inner = 1}};", None);
        assert!(result.diagnostics.is_empty());
        let paths: Vec<&str> = result.path_map.values().map(|s| s.as_ref()).collect();
        assert!(paths.contains(&"t.outer"), "expected t.outer, got: {:?}", paths);
        assert!(paths.contains(&"t.outer.inner"), "expected t.outer.inner, got: {:?}", paths);
    }

    #[test]
    fn test_analyze_func_args_not_in_path_map() {
        // Function args should appear in token_types but NOT in path_map
        let result = analyze("let f = func(x) => x + 1;", None);
        assert!(result.diagnostics.is_empty());
        assert!(
            result.path_map.is_empty(),
            "func args should not populate path_map, got: {:?}",
            result.path_map
        );
    }

    #[test]
    fn test_analyze_import_inside_module_body_resolves_from_cache() {
        // Imports inside module bodies should be resolved from the workspace cache.
        let import_path = std::path::PathBuf::from("/tmp/libmod.ucg");
        let mut resolved: HashMap<std::path::PathBuf, AnalysisResult> = HashMap::new();
        let imported = super::analyze("let val = 42;", None, &HashMap::new());
        resolved.insert(import_path.clone(), imported);

        let result = super::analyze(
            r#"let m = module {} => { let lib = import "/tmp/libmod.ucg"; };"#,
            None,
            &resolved,
        );
        let (shape, _) = result.symbol_table.get(&Rc::from("m")).unwrap();
        // The module return type should be a Tuple containing a Resolved import for 'lib'.
        if let Shape::Module(mshape) = shape {
            if let Shape::Tuple(pi) = mshape.ret() {
                let lib_shape = pi.val.iter().find(|(n, _)| n.val.as_ref() == "lib");
                assert!(lib_shape.is_some(), "lib field should be in module return tuple");
                let (_, lib_s) = lib_shape.unwrap();
                assert!(
                    matches!(lib_s, Shape::Import(ImportShape::Resolved(_, _))),
                    "lib should be Resolved, got {:?}",
                    lib_s
                );
            } else {
                panic!("expected Module return to be Tuple, got {:?}", mshape.ret());
            }
        } else {
            panic!("expected Module shape, got {:?}", shape);
        }
    }

    #[test]
    fn test_token_types_import_inside_module_body_resolves() {
        // Hovering on an import binding INSIDE a module body should show resolved fields,
        // not "import(path)". This requires post-processing of token_types.
        let import_path = std::path::PathBuf::from("/tmp/innerlib.ucg");
        let mut resolved: HashMap<std::path::PathBuf, AnalysisResult> = HashMap::new();
        let imported = super::analyze("let answer = 42;", None, &HashMap::new());
        resolved.insert(import_path.clone(), imported);

        let result = super::analyze(
            r#"let m = module {} => { let lib = import "/tmp/innerlib.ucg"; };"#,
            None,
            &resolved,
        );
        // Find the token_types entry for `lib` — its shape should be Resolved.
        let lib_entry = result
            .token_types
            .values()
            .find(|s| matches!(s, Shape::Import(ImportShape::Resolved(_, _))));
        assert!(
            lib_entry.is_some(),
            "lib in token_types should be Resolved; got: {:?}",
            result.token_types.values().collect::<Vec<_>>()
        );
    }
}
