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

use super::translate::OpsMap;
use super::Error;
use super::OpPointer;

/// A Cache of Op codes.
pub struct Ops {
    ops: BTreeMap<PathBuf, Rc<OpsMap>>,
}

impl Ops {
    pub fn new() -> Self {
        Self {
            ops: BTreeMap::new(),
        }
    }

    pub fn entry<'a, P: Into<PathBuf>>(&'a mut self, path: P) -> Entry<'a> {
        Entry(self.ops.entry(path.into()))
    }
}

pub struct Entry<'a>(btree_map::Entry<'a, PathBuf, Rc<OpsMap>>);

impl<'a> Entry<'a> {
    pub fn get_pointer_or_else<F: FnOnce() -> Result<OpsMap, Error>, P: Into<PathBuf>>(
        self,
        f: F,
        path: P,
    ) -> Result<OpPointer, Error> {
        let cached = match self.0 {
            btree_map::Entry::Occupied(e) => e.get().clone(),
            btree_map::Entry::Vacant(e) => {
                // TODO(jwall) Check a file cache for the opcodes before
                let v = Rc::new(f()?);
                e.insert(v.clone());
                v
            }
        };
        let mut ptr = OpPointer::new(cached);
        ptr.set_path(path.into());
        Ok(ptr)
    }
}
