// Copyright 2019 Jeremy Wall
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
use std::collections::btree_map;
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::rc::Rc;

use super::{Op, OpPointer};

/// A Cache of Op codes.
pub struct Ops {
    ops: BTreeMap<String, Rc<Vec<Op>>>,
}

impl Ops {
    pub fn new() -> Self {
        Self {
            ops: BTreeMap::new(),
        }
    }

    pub fn entry<'a, S: Into<String>>(&'a mut self, path: S) -> Entry<'a> {
        Entry(self.ops.entry(path.into()))
    }
}

pub struct Entry<'a>(btree_map::Entry<'a, String, Rc<Vec<Op>>>);

impl<'a> Entry<'a> {
    pub fn get_pointer_or_else<F: FnOnce() -> Vec<Op>, P: Into<PathBuf>>(
        self,
        f: F,
        path: P,
    ) -> OpPointer {
        let cached = self.0.or_insert_with(|| Rc::new(f())).clone();
        OpPointer::new(cached).with_path(path.into())
    }
}
