// Copyright 2020 Jeremy Wall
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
use std::path::PathBuf;

use crate::ast::walk::Visitor;
use crate::ast::Expression;

pub struct Rewriter {
    base: PathBuf,
    package_root: Option<PathBuf>,
    vendor_dir: String,
}

impl Rewriter {
    pub fn new<P: Into<PathBuf>>(base: P) -> Self {
        Self {
            base: base.into(),
            package_root: None,
            vendor_dir: "vendor".to_string(),
        }
    }

    pub fn with_package_root(mut self, root: Option<PathBuf>, vendor_dir: &str) -> Self {
        self.package_root = root;
        self.vendor_dir = vendor_dir.to_string();
        self
    }

    fn resolve_path(&self, path: &PathBuf) -> PathBuf {
        let main_separator = format!("{}", std::path::MAIN_SEPARATOR);
        let path_str = path.to_string_lossy();

        // std/ paths are special and do not get made into absolute paths.
        let std_prefix = format!("std{}", main_separator);
        if path_str.starts_with(&std_prefix) || path_str.starts_with("std/") {
            return path.clone();
        }

        // vendor/ prefix: resolve against package root's vendor dir
        let vendor_prefix = format!("vendor{}", main_separator);
        if path_str.starts_with(&vendor_prefix) || path_str.starts_with("vendor/") {
            if let Some(ref root) = self.package_root {
                let rest = if path_str.starts_with(&vendor_prefix) {
                    &path_str[vendor_prefix.len()..]
                } else {
                    &path_str["vendor/".len()..]
                };
                return root
                    .join(&self.vendor_dir)
                    .join(rest.replace("/", &main_separator));
            }
            // No package root: fall through to relative resolution
        }

        // Relative paths: resolve against base
        if path.is_relative() {
            self.base.join(path)
        } else {
            path.clone()
        }
    }
}

impl Visitor for Rewriter {
    fn visit_expression(&mut self, expr: &mut Expression) {
        let main_separator = format!("{}", std::path::MAIN_SEPARATOR);
        if let Expression::Include(def) = expr {
            let path = PathBuf::from(def.path.fragment.as_ref());
            let resolved = self.resolve_path(&path);
            if resolved != path {
                def.path.fragment = resolved.to_string_lossy().to_string().into();
            }
        }
        if let Expression::Import(def) = expr {
            let path = PathBuf::from(
                &def.path
                    .fragment
                    .replace("/", &main_separator)
                    .replace("\\", &main_separator),
            );
            let resolved = self.resolve_path(&path);
            if resolved != path {
                def.path.fragment = resolved.to_string_lossy().to_string().into();
            }
        }
    }
}
