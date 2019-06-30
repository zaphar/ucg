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

use super::{Error, Op};

pub struct OpPointer {
    ops: Vec<Op>,
    pub ptr: Option<usize>,
}

impl OpPointer {
    pub fn new(ops: Vec<Op>) -> Self {
        // If we load an empty program what happens?
        Self {
            ops: ops,
            ptr: None,
        }
    }

    pub fn next(&mut self) -> Option<usize> {
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
        self.ptr
    }

    pub fn jump(&mut self, ptr: usize) -> Result<(), Error> {
        if ptr < self.ops.len() {
            self.ptr = Some(ptr);
            return Ok(());
        }
        Err(Error {})
    }

    pub fn op(&self) -> Option<&Op> {
        if let Some(i) = self.ptr {
            return self.ops.get(i);
        }
        None
    }
}
