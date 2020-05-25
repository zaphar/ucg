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

use crate::ast::Position;

use super::translate::OpsMap;
use super::{Error, Op};

#[derive(Debug, PartialEq, Clone)]
pub struct OpPointer {
    pub pos_map: Rc<OpsMap>,
    pub ptr: Option<usize>,
    pub path: Option<PathBuf>,
}

impl OpPointer {
    pub fn new(ops: Rc<OpsMap>) -> Self {
        // If we load an empty program what happens?
        Self {
            pos_map: ops,
            ptr: None,
            path: None,
        }
    }

    pub fn set_path(&mut self, path: PathBuf) {
        self.path = Some(path);
    }

    pub fn next(&mut self) -> Option<&Op> {
        if let Some(i) = self.ptr {
            let nxt = i + 1;
            if nxt < self.pos_map.len() {
                self.ptr = Some(nxt);
            } else {
                return None;
            }
        } else if self.pos_map.len() != 0 {
            self.ptr = Some(0);
        }
        self.op()
    }

    pub fn jump(&mut self, ptr: usize) -> Result<(), Error> {
        if ptr < self.pos_map.len() {
            self.ptr = Some(ptr);
            return Ok(());
        }
        Err(Error::new(
            format!("FAULT!!! Invalid Jump!"),
            match self.pos() {
                Some(pos) => pos.clone(),
                None => Position::new(0, 0, 0),
            },
        ))
    }

    pub fn op(&self) -> Option<&Op> {
        if let Some(i) = self.ptr {
            return self.pos_map.ops.get(i);
        }
        None
    }

    pub fn pos(&self) -> Option<&Position> {
        if let Some(i) = self.ptr {
            return self.pos_map.pos.get(i);
        }
        None
    }

    pub fn idx(&self) -> Result<usize, Error> {
        match self.ptr {
            Some(ptr) => Ok(ptr),
            None => Err(Error::new(
                format!("FAULT!!! Position Check failure!"),
                Position::new(0, 0, 0),
            )),
        }
    }

    pub fn snapshot(&self) -> Self {
        Self {
            pos_map: self.pos_map.clone(),
            ptr: None,
            path: self.path.clone(),
        }
    }
}
