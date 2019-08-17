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
use std::path::PathBuf;
use std::rc::Rc;

use super::{Error, Op};

#[derive(Debug, PartialEq, Clone)]
pub struct OpPointer {
    pub ops: Rc<Vec<Op>>,
    pub ptr: Option<usize>,
    pub path: Option<PathBuf>,
}

impl OpPointer {
    pub fn new(ops: Rc<Vec<Op>>) -> Self {
        // If we load an empty program what happens?
        Self {
            ops: ops,
            ptr: None,
            path: None,
        }
    }

    pub fn with_path(mut self, path: PathBuf) -> Self {
        self.path = Some(path);
        self
    }

    pub fn next(&mut self) -> Option<&Op> {
        if let Some(i) = self.ptr {
            let nxt = i + 1;
            if nxt < self.ops.len() {
                self.ptr = Some(nxt);
            } else {
                return None;
            }
        } else if self.ops.len() != 0 {
            self.ptr = Some(0);
        }
        self.op()
    }

    pub fn jump(&mut self, ptr: usize) -> Result<(), Error> {
        if ptr < self.ops.len() {
            self.ptr = Some(ptr);
            return Ok(());
        }
        Err(dbg!(Error {}))
    }

    pub fn op(&self) -> Option<&Op> {
        if let Some(i) = self.ptr {
            return self.ops.get(i);
        }
        None
    }

    pub fn idx(&self) -> Result<usize, Error> {
        match self.ptr {
            Some(ptr) => Ok(ptr),
            None => dbg!(Err(Error {})),
        }
    }

    pub fn snapshot(&self) -> Self {
        Self {
            ops: self.ops.clone(),
            ptr: None,
            path: self.path.clone(),
        }
    }
}
