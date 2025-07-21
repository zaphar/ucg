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
use std::collections::BTreeMap;
use std::rc::Rc;

use super::Value;
use crate::ast::Position;

#[derive(Clone, PartialEq, Debug)]
pub struct Stack {
    curr: BTreeMap<Rc<str>, (Rc<Value>, Position)>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            curr: BTreeMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<(Rc<Value>, Position)> {
        self.curr.get(name).cloned()
    }

    pub fn remove_symbol(&mut self, name: &str) -> Option<(Rc<Value>, Position)> {
        self.curr.remove(name)
    }

    pub fn is_bound(&self, name: &str) -> bool {
        self.curr.get(name).is_some()
    }

    pub fn add(&mut self, name: Rc<str>, val: Rc<Value>, pos: Position) {
        self.curr.insert(name, (val, pos));
    }

    pub fn symbol_list(&self) -> Vec<Rc<str>> {
        self.curr.keys().cloned().collect()
    }

    pub fn snapshot(&self) -> Self {
        Self {
            curr: self.curr.clone(),
        }
    }
}
