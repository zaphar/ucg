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

//! Workspace indexer: discovers and analyzes all `.ucg` files under a root directory.
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::ast::TokenType;
use crate::build::stdlib;
use crate::iter::OffsetStrIter;
use crate::tokenizer::tokenize;

use super::analysis::{analyze, AnalysisResult};

pub struct WorkspaceIndex {
    root: PathBuf,
    /// Absolute path → analysis result for every indexed file.
    files: HashMap<PathBuf, AnalysisResult>,
    /// Valid stdlib import paths, e.g. `["std/lists.ucg", "std/strings.ucg", ...]`.
    pub stdlib_keys: Vec<String>,
}

impl WorkspaceIndex {
    pub fn new(root: PathBuf) -> Self {
        let mut stdlib_keys: Vec<String> = stdlib::get_libs().into_keys().collect();
        stdlib_keys.sort();
        WorkspaceIndex {
            root,
            files: HashMap::new(),
            stdlib_keys,
        }
    }

    /// Walk `root` recursively, analyzing every non-test `.ucg` file in
    /// import-dependency order so that each file's imports are already resolved
    /// when it is analyzed.
    pub fn index_all(&mut self) {
        let files = self.discover_files();
        let ordered = topo_sort_files(&files);
        for path in ordered {
            if let Ok(content) = std::fs::read_to_string(&path) {
                let working_dir = path.parent().map(Path::to_path_buf);
                let result = analyze(&content, working_dir.as_deref(), &self.files);
                self.files.insert(path, result);
            }
        }
    }

    /// Collect all indexable `.ucg` files under `root`.
    fn discover_files(&self) -> Vec<PathBuf> {
        let mut files = Vec::new();
        self.walk_dir(&self.root.clone(), &mut files);
        files
    }

    fn walk_dir(&self, dir: &Path, files: &mut Vec<PathBuf>) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                self.walk_dir(&path, files);
            } else if is_ucg_source(&path) {
                files.push(path);
            }
        }
    }

    /// Re-analyze a file from its in-memory content (for open documents).
    /// Uses the existing workspace cache to resolve import shapes.
    pub fn update_from_content(&mut self, path: &PathBuf, content: &str) {
        let working_dir = path.parent().map(Path::to_path_buf);
        let result = analyze(content, working_dir.as_deref(), &self.files);
        self.files.insert(path.clone(), result);
    }

    /// Re-analyze a file by reading it from disk.
    pub fn update_from_disk(&mut self, path: &PathBuf) {
        if let Ok(content) = std::fs::read_to_string(path) {
            self.update_from_content(path, &content);
        }
    }

    /// Look up the analysis result for a given absolute path.
    pub fn get(&self, path: &PathBuf) -> Option<&AnalysisResult> {
        self.files.get(path)
    }

    /// Iterate over all indexed files and their analysis results.
    pub fn iter_files(&self) -> impl Iterator<Item = (&PathBuf, &AnalysisResult)> {
        self.files.iter()
    }

    /// Expose the resolved-imports cache so callers can pass it to `analyze()`.
    pub fn resolved_files(&self) -> &HashMap<PathBuf, AnalysisResult> {
        &self.files
    }
}

/// Scan a file's content for absolute import paths without doing a full analysis.
/// Paths are resolved relative to `file_path`'s parent directory.
/// Stdlib (non-absolute after resolution) imports are excluded.
pub fn scan_imports(content: &str, file_path: &Path) -> Vec<PathBuf> {
    let working_dir = file_path.parent();
    let tokens = match tokenize(OffsetStrIter::new(content), None) {
        Ok(t) => t,
        Err(_) => return vec![],
    };
    let mut imports = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        if tokens[i].typ == TokenType::BAREWORD && tokens[i].fragment.as_ref() == "import" {
            // Skip whitespace
            let mut j = i + 1;
            while j < tokens.len() && tokens[j].typ == TokenType::WS {
                j += 1;
            }
            if j < tokens.len() && tokens[j].typ == TokenType::QUOTED {
                let path_str = tokens[j].fragment.as_ref();
                let resolved = super::analysis::normalize_path(if let Some(dir) = working_dir {
                    dir.join(path_str)
                } else {
                    PathBuf::from(path_str)
                });
                if resolved.is_absolute() {
                    imports.push(resolved);
                }
            }
        }
        i += 1;
    }
    imports
}

/// Sort `files` in dependency order: if A imports B, B appears before A.
/// Files not in the input set (e.g. stdlib) are ignored.
/// Since circular imports are a language error, cycles won't arise in valid code.
pub fn topo_sort_files(files: &[PathBuf]) -> Vec<PathBuf> {
    let file_set: HashSet<&PathBuf> = files.iter().collect();

    // Build adjacency list: file → [workspace deps it imports].
    let mut deps: HashMap<PathBuf, Vec<PathBuf>> = HashMap::new();
    for f in files {
        let content = std::fs::read_to_string(f).unwrap_or_default();
        let imports = scan_imports(&content, f)
            .into_iter()
            .filter(|p| file_set.contains(p))
            .collect();
        deps.insert(f.clone(), imports);
    }

    // Iterative post-order DFS topo sort.
    let mut visited: HashSet<PathBuf> = HashSet::new();
    let mut result: Vec<PathBuf> = Vec::new();

    for start in files {
        if visited.contains(start) {
            continue;
        }
        // Stack entries: (path, already_processed).
        // Push (path, false) first; when popped with false, push (path, true) then
        // push all unvisited deps with false.  When popped with true, emit to result.
        let mut stack: Vec<(PathBuf, bool)> = vec![(start.clone(), false)];
        while let Some((path, processed)) = stack.pop() {
            if processed {
                result.push(path);
                continue;
            }
            if visited.contains(&path) {
                continue;
            }
            visited.insert(path.clone());
            stack.push((path.clone(), true));
            if let Some(children) = deps.get(&path) {
                for child in children {
                    if !visited.contains(child) {
                        stack.push((child.clone(), false));
                    }
                }
            }
        }
    }
    result
}

/// Returns true for `.ucg` files that are not test files (`*_test.ucg`).
fn is_ucg_source(path: &Path) -> bool {
    let name = path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or_default();
    name.ends_with(".ucg") && !name.ends_with("_test.ucg")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_scan_imports_absolute_path() {
        let content = r#"let x = import "/tmp/foo.ucg";"#;
        let file = PathBuf::from("/home/user/project/main.ucg");
        let imports = scan_imports(content, &file);
        assert_eq!(imports, vec![PathBuf::from("/tmp/foo.ucg")]);
    }

    #[test]
    fn test_scan_imports_relative_path_resolved_against_file() {
        let content = r#"let x = import "other.ucg";"#;
        let file = PathBuf::from("/home/user/project/main.ucg");
        let imports = scan_imports(content, &file);
        assert_eq!(imports, vec![PathBuf::from("/home/user/project/other.ucg")]);
    }

    #[test]
    fn test_scan_imports_stdlib_excluded() {
        // "std/lists.ucg" with no parent dir stays relative → excluded.
        let content = r#"let x = import "std/lists.ucg";"#;
        let file = PathBuf::from("main.ucg");
        let imports = scan_imports(content, &file);
        assert!(imports.is_empty(), "stdlib paths should be excluded");
    }

    #[test]
    fn test_scan_imports_no_imports() {
        let content = "let x = 1;";
        let file = PathBuf::from("/tmp/main.ucg");
        let imports = scan_imports(content, &file);
        assert!(imports.is_empty());
    }

    #[test]
    fn test_topo_sort_independent_files_all_appear() {
        // Files with no imports among themselves: all must appear in output.
        let files = vec![PathBuf::from("/a.ucg"), PathBuf::from("/b.ucg")];
        let sorted = topo_sort_files(&files);
        assert_eq!(sorted.len(), 2);
    }
}
