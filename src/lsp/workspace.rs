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
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::build::stdlib;

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

    /// Walk `root` recursively, analyzing every non-test `.ucg` file.
    pub fn index_all(&mut self) {
        let root = self.root.clone();
        self.walk_dir(&root);
    }

    fn walk_dir(&mut self, dir: &Path) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                self.walk_dir(&path);
            } else if is_ucg_source(&path) {
                self.update_from_disk(&path);
            }
        }
    }

    /// Re-analyze a file from its in-memory content (for open documents).
    pub fn update_from_content(&mut self, path: &PathBuf, content: &str) {
        let working_dir = path.parent().map(Path::to_path_buf);
        let result = analyze(content, working_dir.as_deref());
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
}

/// Returns true for `.ucg` files that are not test files (`*_test.ucg`).
fn is_ucg_source(path: &Path) -> bool {
    let name = path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or_default();
    name.ends_with(".ucg") && !name.ends_with("_test.ucg")
}
