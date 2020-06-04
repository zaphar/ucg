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

use crate::ast::walk::{Visitor, Walker};
use crate::ast::Expression;

pub struct Rewriter {
    base: PathBuf,
}

impl Rewriter {
    pub fn new<P: Into<PathBuf>>(base: P) -> Self {
        Self { base: base.into() }
    }
}

impl Visitor for Rewriter {
    fn visit_expression(&mut self, expr: &mut Expression) {
        // Rewrite all paths except for stdlib paths to absolute.
        let main_separator = format!("{}", std::path::MAIN_SEPARATOR);
        if let Expression::Include(ref mut def) = expr {
            let path = PathBuf::from(&def.path.fragment);
            if path.is_relative() {
                def.path.fragment = self.base.join(path).to_string_lossy().to_string();
            }
        }
        if let Expression::Import(ref mut def) = expr {
            let path = PathBuf::from(
                &def.path
                    .fragment
                    .replace("/", &main_separator)
                    .replace("\\", &main_separator),
            );
            // std/ paths are special and do not get made into absolute paths.
            if path.starts_with(format!("std{}", main_separator)) {
                return;
            }
            if path.is_relative() {
                def.path.fragment = self.base.join(path).to_string_lossy().to_string();
            }
        }
    }
}

impl Walker for Rewriter {}
