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
use std::collections::{BTreeMap, BTreeSet};
use std::iter::FromIterator;
use std::rc::Rc;

use super::Value;

#[derive(Clone, PartialEq, Debug)]
pub struct Stack {
    curr: BTreeMap<String, Rc<Value>>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            curr: BTreeMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Value>> {
        self.curr.get(name).cloned()
    }

    pub fn is_bound(&self, name: &str) -> bool {
        self.curr.get(name).is_some()
    }

    pub fn add(&mut self, name: String, val: Rc<Value>) {
        self.curr.insert(name, val);
    }

    pub fn symbol_list(&self) -> Vec<&String> {
        self.curr.keys().collect()
    }

    pub fn snapshot(&self) -> Self {
        Self {
            curr: self.curr.clone(),
        }
    }
}
