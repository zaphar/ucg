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

use super::cache;
use super::VM;
use super::{Composite, Error, Hook, Primitive, Value};
use crate::build::AssertCollector;
use Composite::Tuple;
use Primitive::{Bool, Str};

pub struct Builtins {
    op_cache: cache::Ops,
    val_cache: BTreeMap<String, Rc<Value>>,
    assert_results: AssertCollector,
    // TODO(jwall): IO sink for stderr
    // TODO(jwall): IO sink for stdout
}

impl Builtins {
    pub fn new() -> Self {
        Self {
            op_cache: cache::Ops::new(),
            val_cache: BTreeMap::new(),
            assert_results: AssertCollector::new(),
        }
    }

    pub fn handle(&mut self, h: Hook, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        match h {
            Hook::Import => self.import(stack),
            Hook::Include => self.include(stack),
            Hook::Assert => self.assert(stack),
            Hook::Convert => self.convert(stack),
            Hook::Out => self.out(stack),
            Hook::Map => self.map(stack),
            Hook::Filter => self.filter(stack),
            Hook::Reduce => self.reduce(stack),
        }
    }

    fn import(&mut self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        let path = stack.pop();
        if let Some(val) = path {
            if let &Value::P(Str(ref path)) = val.as_ref() {
                if self.val_cache.contains_key(path) {
                    stack.push(self.val_cache[path].clone());
                } else {
                    let op_pointer = self.op_cache.entry(path).get_pointer_or_else(|| {
                        // TODO(jwall): import
                        unimplemented!("Compiling paths are not implemented yet");
                    });
                    let mut vm = VM::with_pointer(op_pointer);
                    vm.run()?;
                    let result = Rc::new(vm.symbols_to_tuple(true));
                    self.val_cache.insert(path.clone(), result.clone());
                    stack.push(result);
                }
                return Ok(());
            }
        }
        return Err(Error {});
    }

    fn include(&self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        // TODO(jwall): include
        let path = stack.pop();
        if let Some(val) = path {
            if let &Value::P(Str(ref path)) = val.as_ref() {}
            unimplemented!("TODO(jwall): Includes are not implemented yet")
        }
        return Err(Error {});
    }

    fn assert(&mut self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        // TODO(jwall): assert
        let tuple = stack.pop();
        if let Some(val) = tuple.clone() {
            if let &Value::C(Tuple(ref tuple)) = val.as_ref() {
                // look for the description field
                let mut desc = None;
                // look for the ok field.
                let mut ok = None;
                for &(ref name, ref val) in tuple.iter() {
                    if name == "description" {
                        desc = Some(val.clone());
                    }
                    if name == "ok" {
                        ok = Some(val.clone());
                    }
                }
                if let (Some(ok), Some(desc)) = (ok, desc) {
                    if let (&Value::P(Bool(ref b)), &Value::P(Str(ref desc))) =
                        (ok.as_ref(), desc.as_ref())
                    {
                        self.assert_results.record_assert_result(desc, *b);
                        return Ok(());
                    }
                }
            }
        }
        let msg = format!(
            "TYPE FAIL - Expected tuple with ok and desc fields got {:?} at line: {} column: {}\n",
            tuple, "TODO", "TODO"
        );
        self.assert_results.record_assert_result(&msg, false);
        return Ok(());
    }

    fn convert(&self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        // TODO(jwall): convert
        let val = stack.pop();
        if let Some(val) = val {
            unimplemented!("TODO(jwall): Conversions are not implemented yet")
        } else {
            Err(Error {})
        }
    }

    fn out(&self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        // TODO(jwall): out
        let val = stack.pop();
        if let Some(val) = val {
            unimplemented!("TODO(jwall): Out expressions are not implemented yet")
        } else {
            Err(Error {})
        }
    }

    fn map(&self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        // TODO(jwall): map (combine these into one?)
        unimplemented!("TODO(jwall): Map expressions are not implemented yet")
    }

    fn filter(&self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        // TODO(jwall): filter
        unimplemented!("TODO(jwall): Filter expressions are not implemented yet")
    }

    fn reduce(&self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        // TODO(jwall): reduce
        unimplemented!("TODO(jwall): Reduce expressions are not implemented yet")
    }
}
