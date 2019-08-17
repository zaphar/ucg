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
use std::cell::RefCell;
use std::convert::{TryFrom, TryInto};
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use regex::Regex;

use super::environment::Environment;
use super::Value::{C, F, P};
use super::VM;
use super::{Composite, Error, Hook, Primitive, Value};
use crate::ast::Position;
use crate::build::ir::Val;
use crate::build::AssertCollector;
use Composite::{List, Tuple};
use Primitive::{Bool, Empty, Int, Str};

pub struct Builtins {
    assert_results: AssertCollector,
    working_dir: PathBuf,
    import_path: Vec<PathBuf>,
}

impl Builtins {
    pub fn new() -> Self {
        Self::with_working_dir(std::env::current_dir().unwrap())
    }

    pub fn with_working_dir<P: Into<PathBuf>>(path: P) -> Self {
        Self {
            assert_results: AssertCollector::new(),
            working_dir: path.into(),
            import_path: Vec::new(),
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            assert_results: AssertCollector::new(),
            working_dir: self.working_dir.clone(),
            import_path: self.import_path.clone(),
        }
    }

    pub fn handle<P: AsRef<Path>, O, E>(
        &mut self,
        path: P,
        h: Hook,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        match h {
            Hook::Import => self.import(stack, env),
            Hook::Include => self.include(stack, env),
            Hook::Assert => self.assert(stack),
            Hook::Convert => self.convert(stack, env),
            Hook::Out => self.out(path, stack, env),
            Hook::Map => self.map(path, stack, env),
            Hook::Filter => self.filter(path, stack, env),
            Hook::Reduce => self.reduce(path, stack, env),
            Hook::Regex => self.regex(stack),
            Hook::Range => self.range(stack),
            Hook::Trace(pos) => self.trace(stack, pos, env),
        }
    }

    fn find_file<P: Into<PathBuf>>(
        &self,
        path: P,
        use_import_path: bool,
    ) -> Result<PathBuf, Error> {
        // Try a relative path first.
        let path = path.into();
        let mut normalized = self.working_dir.clone();
        if path.is_relative() {
            normalized.push(&path);
            // First see if the normalized file exists or not.
            if !normalized.exists() && use_import_path {
                // TODO(jwall): Support importing from a zip file in this
                // import_path?
                // If it does not then look for it in the list of import_paths
                for mut p in self.import_path.iter().cloned() {
                    p.push(&path);
                    if p.exists() {
                        normalized = p;
                        break;
                    }
                }
            }
        } else {
            normalized = path;
        }
        match normalized.canonicalize() {
            Ok(p) => Ok(p),
            Err(_e) => Err(dbg!(Error {})),
        }
    }

    fn get_file_as_string(&self, path: &str) -> Result<String, Error> {
        let sep = format!("{}", std::path::MAIN_SEPARATOR);
        let raw_path = path.replace("/", &sep);
        let normalized = match self.find_file(raw_path, false) {
            Ok(p) => p,
            Err(_e) => {
                return Err(dbg!(Error {}));
            }
        };
        let mut f = File::open(normalized).unwrap();
        let mut contents = String::new();
        f.read_to_string(&mut contents).unwrap();
        Ok(contents)
    }

    fn import<O, E>(
        &mut self,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let path = stack.pop();
        if let Some(val) = path {
            if let &Value::P(Str(ref path)) = val.as_ref() {
                let mut borrowed_env = env.borrow_mut();
                let val_cache = &mut borrowed_env.val_cache;
                if val_cache.contains_key(path) {
                    stack.push(val_cache[path].clone());
                } else {
                    let op_pointer =
                        env.borrow_mut()
                            .op_cache
                            .entry(path)
                            .get_pointer_or_else(|| {
                                // FIXME(jwall): import
                                unimplemented!("Compiling paths are not implemented yet");
                            });
                    let mut vm = VM::with_pointer(path, op_pointer, env.clone());
                    vm.run()?;
                    let result = Rc::new(vm.symbols_to_tuple(true));
                    val_cache.insert(path.clone(), result.clone());
                    stack.push(result);
                }
                return Ok(());
            }
        }
        return Err(dbg!(Error {}));
    }

    fn include<O, E>(
        &self,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        // TODO(jwall): include
        let path = stack.pop();
        let typ = stack.pop();
        let path = if let Some(val) = path {
            if let &Value::P(Str(ref path)) = val.as_ref() {
                path.clone()
            } else {
                return Err(dbg!(Error {}));
            }
        } else {
            return Err(dbg!(Error {}));
        };
        let typ = if let Some(val) = typ.as_ref() {
            if let &Value::P(Str(ref typ)) = val.as_ref() {
                typ.clone()
            } else {
                return Err(dbg!(Error {}));
            }
        } else {
            return Err(dbg!(Error {}));
        };
        if typ == "str" {
            stack.push(Rc::new(P(Str(self.get_file_as_string(&path)?))));
        } else {
            stack.push(Rc::new(
                match env.borrow().importer_registry.get_importer(&typ) {
                    Some(importer) => {
                        let contents = self.get_file_as_string(&path)?;
                        if contents.len() == 0 {
                            eprintln!("including an empty file. Use NULL as the result");
                            P(Empty)
                        } else {
                            match importer.import(contents.as_bytes()) {
                                Ok(v) => v.try_into()?,
                                Err(_e) => return Err(dbg!(Error {})),
                            }
                        }
                    }
                    None => return Err(dbg!(Error {})),
                },
            ));
        }
        return Err(dbg!(Error {}));
    }

    fn assert(&mut self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
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

    fn out<P: AsRef<Path>, O, E>(
        &self,
        path: P,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let val = stack.pop();
        if let Some(val) = val {
            let val = val.try_into()?;
            if let Some(c_type_val) = stack.pop() {
                if let &Value::S(ref c_type) = c_type_val.as_ref() {
                    if let Some(c) = env.borrow().converter_registry.get_converter(c_type) {
                        match c.convert(Rc::new(val), &mut File::create(path)?) {
                            Ok(_) => {
                                // noop
                            }
                            Err(_e) => return Err(dbg!(Error {})),
                        }
                        return Ok(());
                    }
                }
            }
        }
        return Err(dbg!(Error {}));
    }

    fn convert<O, E>(
        &self,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let val = stack.pop();
        if let Some(val) = val {
            let val = val.try_into()?;
            if let Some(c_type_val) = stack.pop() {
                if let &Value::S(ref c_type) = c_type_val.as_ref() {
                    if let Some(c) = env.borrow().converter_registry.get_converter(c_type) {
                        let mut buf: Vec<u8> = Vec::new();
                        match c.convert(Rc::new(val), &mut buf) {
                            Ok(_) => {
                                stack
                                    .push(Rc::new(P(Str(
                                        String::from_utf8_lossy(buf.as_slice()).to_string()
                                    ))));
                            }
                            Err(_e) => return Err(dbg!(Error {})),
                        }
                        return Ok(());
                    }
                }
            }
        }
        return Err(dbg!(Error {}));
    }

    fn map<P: AsRef<Path>, O, E>(
        &self,
        path: P,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        // get the list from the stack
        let list = if let Some(list) = stack.pop() {
            list
        } else {
            return Err(dbg!(Error {}));
        };
        // TODO(jwall): This can also be tuples or strings.
        let elems = match list.as_ref() {
            &C(List(ref elems)) => elems,
            &C(Tuple(ref _flds)) => {
                unimplemented!("TODO Tuple functional operations");
            }
            &P(Str(ref _s)) => {
                unimplemented!("TODO String functional operations");
            }
            _ => return Err(dbg!(Error {})),
        };

        // get the func ptr from the stack
        let fptr = if let Some(ptr) = stack.pop() {
            ptr
        } else {
            return Err(dbg!(Error {}));
        };

        let f = if let &F(ref f) = fptr.as_ref() {
            f
        } else {
            return Err(dbg!(Error {}));
        };

        let mut result_elems = Vec::new();
        for e in elems.iter() {
            // push function argument on the stack.
            stack.push(e.clone());
            // call function and push it's result on the stack.
            result_elems.push(VM::fcall_impl(
                path.as_ref().to_owned(),
                f,
                stack,
                env.clone(),
            )?);
        }
        stack.push(Rc::new(C(List(result_elems))));
        Ok(())
    }

    fn filter<P: AsRef<Path>, O, E>(
        &self,
        path: P,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        // get the list from the stack
        let list = if let Some(list) = stack.pop() {
            list
        } else {
            return Err(dbg!(Error {}));
        };
        // TODO(jwall): This can also be tuples or strings.
        let elems = match list.as_ref() {
            &C(List(ref elems)) => elems,
            &C(Tuple(ref _flds)) => {
                unimplemented!("TODO Tuple functional operations");
            }
            &P(Str(ref _s)) => {
                unimplemented!("TODO String functional operations");
            }
            _ => return Err(dbg!(Error {})),
        };

        // get the func ptr from the stack
        let fptr = if let Some(ptr) = stack.pop() {
            ptr
        } else {
            return Err(dbg!(Error {}));
        };

        let f = if let &F(ref f) = fptr.as_ref() {
            f
        } else {
            return dbg!(Err(Error {}));
        };

        let mut result_elems = Vec::new();
        for e in elems.iter() {
            // push function argument on the stack.
            stack.push(e.clone());
            // call function and push it's result on the stack.
            let condition = VM::fcall_impl(path.as_ref().to_owned(), f, stack, env.clone())?;
            // Check for empty or boolean results and only push e back in
            // if they are non empty and true
            match condition.as_ref() {
                &P(Empty) | &P(Bool(false)) => {
                    continue;
                }
                _ => result_elems.push(e.clone()),
            }
        }
        stack.push(Rc::new(C(List(result_elems))));
        Ok(())
    }

    fn regex(&self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        // 1. get left side (string)
        let left_str = if let Some(val) = stack.pop() {
            if let &P(Str(ref s)) = val.as_ref() {
                s.clone()
            } else {
                return dbg!(Err(Error {}));
            }
        } else {
            return dbg!(Err(Error {}));
        };

        // 2. get right side (string)
        let right_str = if let Some(val) = stack.pop() {
            if let &P(Str(ref s)) = val.as_ref() {
                s.clone()
            } else {
                return dbg!(Err(Error {}));
            }
        } else {
            return dbg!(Err(Error {}));
        };

        // 3. compare via regex
        let rex = Regex::new(&right_str)?;
        stack.push(Rc::new(P(Bool(rex.find(&left_str).is_some()))));
        Ok(())
    }

    fn reduce<P: AsRef<Path>, O, E>(
        &self,
        path: P,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        // get the list from the stack
        let list = if let Some(list) = stack.pop() {
            list
        } else {
            return dbg!(Err(Error {}));
        };
        // TODO(jwall): This can also be tuples or strings.
        let elems = match list.as_ref() {
            &C(List(ref elems)) => elems,
            &C(Tuple(ref _flds)) => {
                unimplemented!("TODO Tuple functional operations");
            }
            &P(Str(ref _s)) => {
                unimplemented!("TODO String functional operations");
            }
            _ => return Err(dbg!(Error {})),
        };

        // Get the accumulator from the stack
        let mut acc = if let Some(acc) = stack.pop() {
            acc
        } else {
            return dbg!(Err(Error {}));
        };
        // get the func ptr from the stack
        let fptr = if let Some(ptr) = stack.pop() {
            ptr
        } else {
            return dbg!(Err(Error {}));
        };

        let f = if let &F(ref f) = fptr.as_ref() {
            f
        } else {
            return dbg!(Err(Error {}));
        };

        for e in elems.iter() {
            // push function arguments on the stack.
            stack.push(e.clone());
            stack.push(acc.clone());
            // call function and push it's result on the stack.
            acc = VM::fcall_impl(path.as_ref().to_owned(), f, stack, env.clone())?;
            // Check for empty or boolean results and only push e back in
            // if they are non empty and true
        }
        // push the acc on the stack as our result
        stack.push(acc);
        Ok(())
    }

    fn range(&self, stack: &mut Vec<Rc<Value>>) -> Result<(), Error> {
        let start = if let Some(start) = stack.pop() {
            start
        } else {
            return dbg!(Err(Error {}));
        };
        let step = if let Some(step) = stack.pop() {
            if let &P(Empty) = step.as_ref() {
                Rc::new(P(Int(1)))
            } else {
                step
            }
        } else {
            return dbg!(Err(Error {}));
        };
        let end = if let Some(end) = stack.pop() {
            end
        } else {
            return dbg!(Err(Error {}));
        };

        let mut elems = Vec::new();
        match (start.as_ref(), step.as_ref(), end.as_ref()) {
            (&P(Int(start)), &P(Int(step)), &P(Int(end))) => {
                let mut num = start;
                loop {
                    if num > end {
                        break;
                    }
                    elems.push(Rc::new(P(Int(num))));
                    num += step;
                }
            }
            _ => {
                return dbg!(Err(Error {}));
            }
        }
        stack.push(Rc::new(C(List(elems))));
        Ok(())
    }

    fn trace<O, E>(
        &mut self,
        stack: &mut Vec<Rc<Value>>,
        pos: Position,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let val = if let Some(val) = dbg!(stack.pop()) {
            val
        } else {
            return Err(dbg!(Error {}));
        };
        let expr = stack.pop();
        let expr_pretty = match expr {
            Some(ref expr) => match dbg!(expr.as_ref()) {
                &P(Str(ref expr)) => expr.clone(),
                _ => return Err(dbg!(Error {})),
            },
            _ => return Err(dbg!(Error {})),
        };
        let writable_val: Val = TryFrom::try_from(val.clone())?;
        if let Err(_) = writeln!(
            &mut env.borrow_mut().stderr,
            "TRACE: {} = {} at {}",
            expr_pretty,
            writable_val,
            pos
        ) {
            return Err(dbg!(Error {}));
        };
        stack.push(val);
        Ok(())
    }
}
