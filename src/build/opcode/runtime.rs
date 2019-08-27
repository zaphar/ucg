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
    validate_mode: bool,
}

impl Builtins {
    pub fn new() -> Self {
        Self::with_working_dir(std::env::current_dir().unwrap())
    }

    pub fn with_working_dir<P: Into<PathBuf>>(path: P) -> Self {
        Self {
            assert_results: AssertCollector::new(),
            working_dir: path.into(),
            // FIXME(jwall): This should probably be injected in.
            import_path: Vec::new(),
            validate_mode: false,
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            assert_results: AssertCollector::new(),
            working_dir: self.working_dir.clone(),
            import_path: self.import_path.clone(),
            validate_mode: self.validate_mode,
        }
    }

    pub fn enable_validate_mode(&mut self) {
        self.validate_mode = true;
    }

    pub fn handle<P: AsRef<Path>, O, E>(
        &mut self,
        path: Option<P>,
        h: Hook,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        match h {
            Hook::Import => self.import(stack, env, pos),
            Hook::Include => self.include(stack, env, pos),
            Hook::Assert => self.assert(stack),
            Hook::Convert => self.convert(stack, env, pos),
            Hook::Out => self.out(path, stack, env, pos),
            Hook::Map => self.map(stack, env, pos),
            Hook::Filter => self.filter(stack, env, pos),
            Hook::Reduce => self.reduce(stack, env, pos),
            Hook::Regex => self.regex(stack, pos),
            Hook::Range => self.range(stack, pos),
            Hook::Trace(pos) => self.trace(stack, pos, env),
        }
    }

    fn find_file<P: Into<PathBuf>>(
        &self,
        path: P,
        use_import_path: bool,
        pos: Position,
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
            Err(_e) => Err(Error::new(
                format!("Invalid path: {}", normalized.to_string_lossy()),
                pos.clone(),
            )),
        }
    }

    fn get_file_as_string(&self, path: &str, pos: Position) -> Result<String, Error> {
        let sep = format!("{}", std::path::MAIN_SEPARATOR);
        let raw_path = path.replace("/", &sep);
        let normalized = self.find_file(raw_path, false, pos)?;
        // TODO(jwall): Proper error here
        let mut f = File::open(normalized)?;
        let mut contents = String::new();
        // TODO(jwall): Proper error here
        f.read_to_string(&mut contents)?;
        Ok(contents)
    }

    // FIXME(jwall): Should probably move this to the runtime.
    //fn detect_import_cycle(&self, path: &str) -> bool {
    //    self.scope
    //        .import_stack
    //        .iter()
    //        .find(|p| *p == path)
    //        .is_some()
    //}
    fn import<O, E>(
        &mut self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let path = stack.pop();
        if let Some((val, path_pos)) = path {
            if let &Value::P(Str(ref path)) = val.as_ref() {
                let mut borrowed_env = env.borrow_mut();
                match borrowed_env.get_cached_path_val(path) {
                    Some(v) => {
                        stack.push((v, path_pos));
                    }
                    None => {
                        let op_pointer = borrowed_env.get_ops_for_path(path)?;
                        let mut vm = VM::with_pointer(op_pointer, env.clone());
                        vm.run()?;
                        let result = Rc::new(vm.symbols_to_tuple(true));
                        borrowed_env.update_path_val(&path, result.clone());
                        stack.push((result, pos));
                    }
                }
                return Ok(());
            }
            return Err(Error::new(format!("Invalid Path {:?}", val), pos));
        }
        unreachable!();
    }

    fn include<O, E>(
        &self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let path = stack.pop();
        let typ = stack.pop();
        let path = if let Some((val, path_pos)) = path {
            if let &Value::P(Str(ref path)) = val.as_ref() {
                path.clone()
            } else {
                return Err(Error::new(format!("Invalid Path {:?}", val), path_pos));
            }
        } else {
            unreachable!();
        };
        let typ = if let Some((val, typ_pos)) = typ {
            if let &Value::P(Str(ref typ)) = val.as_ref() {
                typ.clone()
            } else {
                return Err(Error::new(
                    format!("Expected conversion type but got {:?}", val),
                    typ_pos,
                ));
            }
        } else {
            unreachable!();
        };
        if typ == "str" {
            stack.push((
                Rc::new(P(Str(self.get_file_as_string(&path, pos.clone())?))),
                pos.clone(),
            ));
        } else {
            stack.push((
                Rc::new(match env.borrow().importer_registry.get_importer(&typ) {
                    Some(importer) => {
                        let contents = self.get_file_as_string(&path, pos.clone())?;
                        if contents.len() == 0 {
                            eprintln!("including an empty file. Use NULL as the result");
                            P(Empty)
                        } else {
                            match importer.import(contents.as_bytes()) {
                                Ok(v) => v.into(),
                                Err(e) => return Err(Error::new(format!("{}", e), pos)),
                            }
                        }
                    }
                    None => {
                        return Err(Error::new(format!("No such conversion type {}", &typ), pos))
                    }
                }),
                pos,
            ));
        }
        Ok(())
    }

    fn assert(&mut self, stack: &mut Vec<(Rc<Value>, Position)>) -> Result<(), Error> {
        let tuple = stack.pop();
        if let Some((val, tpl_pos)) = tuple.clone() {
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
            let msg = format!(
                "TYPE FAIL - Expected tuple with ok and desc fields got {:?} at {}\n",
                tuple, tpl_pos
            );
            self.assert_results.record_assert_result(&msg, false);
        } else {
            unreachable!();
        }
        return Ok(());
    }

    fn out<P: AsRef<Path>, O, E>(
        &self,
        path: Option<P>,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let mut writer: Box<dyn std::io::Write> = if let Some(path) = path {
            Box::new(File::create(path)?)
        } else {
            Box::new(std::io::stdout())
        };
        let val = stack.pop();
        if let Some((val, val_pos)) = val {
            let val = val.into();
            let c_type = stack.pop();
            if let Some((c_type_val, c_type_pos)) = c_type {
                if let &Value::S(ref c_type) = c_type_val.as_ref() {
                    if let Some(c) = env.borrow().converter_registry.get_converter(c_type) {
                        if let Err(e) = c.convert(Rc::new(val), &mut writer) {
                            return Err(Error::new(format!("{}", e), pos.clone()));
                        }
                        return Ok(());
                    } else {
                        return Err(Error::new(
                            format!("No such conversion type {:?}", c_type),
                            c_type_pos,
                        ));
                    }
                }
                return Err(Error::new(
                    format!("Not a conversion type {:?}", c_type_val),
                    val_pos,
                ));
            }
        }
        unreachable!();
    }

    fn convert<O, E>(
        &self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let val = stack.pop();
        if let Some((val, val_pos)) = val {
            let val = val.into();
            if let Some((c_type_val, c_typ_pos)) = stack.pop() {
                if let &Value::S(ref c_type) = c_type_val.as_ref() {
                    if let Some(c) = env.borrow().converter_registry.get_converter(c_type) {
                        let mut buf: Vec<u8> = Vec::new();
                        match c.convert(Rc::new(val), &mut buf) {
                            Ok(_) => {
                                stack.push((
                                    Rc::new(P(Str(
                                        String::from_utf8_lossy(buf.as_slice()).to_string()
                                    ))),
                                    pos,
                                ));
                            }
                            Err(_e) => {
                                return Err(Error::new(
                                    format!("No such conversion type {:?}", c_type),
                                    c_typ_pos,
                                ));
                            }
                        }
                        return Ok(());
                    }
                }
            }
            return Err(Error::new(
                format!("Not a conversion type {:?}", val),
                val_pos,
            ));
        }
        unreachable!()
    }

    fn map<O, E>(
        &self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        // get the list from the stack
        let (list, list_pos) = if let Some(list) = stack.pop() {
            list
        } else {
            unreachable!();
        };
        // get the func ptr from the stack
        let (fptr, fptr_pos) = if let Some(ptr) = stack.pop() {
            ptr
        } else {
            unreachable!();
        };

        let f = if let &F(ref f) = fptr.as_ref() {
            f
        } else {
            return Err(Error::new(format!("Not a function!!"), fptr_pos));
        };

        match list.as_ref() {
            &C(List(ref elems)) => {
                let mut result_elems = Vec::new();
                for e in elems.iter() {
                    // push function argument on the stack.
                    stack.push((e.clone(), list_pos.clone()));
                    // call function and push it's result on the stack.
                    let (result, _) = VM::fcall_impl(f, stack, env.clone())?;
                    result_elems.push(result);
                }
                stack.push((Rc::new(C(List(result_elems))), list_pos));
            }
            &C(Tuple(ref _flds)) => {
                let mut new_fields = Vec::new();
                for (ref name, ref val) in _flds {
                    stack.push((val.clone(), list_pos.clone()));
                    stack.push((Rc::new(P(Str(name.clone()))), list_pos.clone()));
                    let (result, result_pos) = VM::fcall_impl(f, stack, env.clone())?;
                    if let &C(List(ref fval)) = result.as_ref() {
                        // we expect them to be a list of exactly 2 items.
                        if fval.len() != 2 {
                            return Err(Error::new(
                                format!(
                                    "Map Functions over tuples must return a list of two items"
                                ),
                                result_pos,
                            ));
                        }
                        let name = match fval[0].as_ref() {
                            &P(Str(ref name)) => name.clone(),
                            _ => return Err(Error::new(
                                format!("Map functions over tuples must return a String as the first list item"),
                                result_pos,
                            )),
                        };
                        new_fields.push((name, fval[1].clone()));
                    }
                }
                stack.push((Rc::new(C(Tuple(new_fields))), pos));
            }
            &P(Str(ref s)) => {
                let mut buf = String::new();
                for c in s.chars() {
                    stack.push((Rc::new(P(Str(c.to_string()))), list_pos.clone()));
                    // call function and push it's result on the stack.
                    let (result, result_pos) = VM::fcall_impl(f, stack, env.clone())?;
                    if let &P(Str(ref s)) = result.as_ref() {
                        buf.push_str(s);
                    } else {
                        return Err(Error::new(
                            format!("Map functions over string should return strings"),
                            result_pos,
                        ));
                    }
                }
                stack.push((Rc::new(P(Str(buf))), pos));
            }
            _ => {
                return Err(Error::new(
                    format!("You can only map over lists, tuples, or strings"),
                    pos,
                ))
            }
        };
        Ok(())
    }

    fn filter<O, E>(
        &self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        // get the list from the stack
        let (list, list_pos) = if let Some(list) = stack.pop() {
            list
        } else {
            unreachable!();
        };
        // get the func ptr from the stack
        let (fptr, fptr_pos) = if let Some(ptr) = stack.pop() {
            ptr
        } else {
            unreachable!();
        };

        let f = if let &F(ref f) = fptr.as_ref() {
            f
        } else {
            return Err(Error::new(format!("Not a function!!"), fptr_pos));
        };

        match list.as_ref() {
            &C(List(ref elems)) => {
                let mut result_elems = Vec::new();
                for e in elems.iter() {
                    // push function argument on the stack.
                    stack.push((e.clone(), list_pos.clone()));
                    // call function and push it's result on the stack.
                    let (condition, _) = VM::fcall_impl(f, stack, env.clone())?;
                    // Check for empty or boolean results and only push e back in
                    // if they are non empty and true
                    match condition.as_ref() {
                        &P(Empty) | &P(Bool(false)) => {
                            continue;
                        }
                        _ => result_elems.push(e.clone()),
                    }
                }
                stack.push((Rc::new(C(List(result_elems))), pos));
            }
            &C(Tuple(ref _flds)) => {
                let mut new_fields = Vec::new();
                for (ref name, ref val) in _flds {
                    stack.push((val.clone(), list_pos.clone()));
                    stack.push((Rc::new(P(Str(name.clone()))), list_pos.clone()));
                    let (condition, _) = VM::fcall_impl(f, stack, env.clone())?;
                    // Check for empty or boolean results and only push e back in
                    // if they are non empty and true
                    match condition.as_ref() {
                        &P(Empty) | &P(Bool(false)) => {
                            continue;
                        }
                        _ => new_fields.push((name.clone(), val.clone())),
                    }
                }
                stack.push((Rc::new(C(Tuple(new_fields))), pos));
            }
            &P(Str(ref s)) => {
                let mut buf = String::new();
                for c in s.chars() {
                    stack.push((Rc::new(P(Str(c.to_string()))), list_pos.clone()));
                    // call function and push it's result on the stack.
                    let (condition, _) = VM::fcall_impl(f, stack, env.clone())?;
                    // Check for empty or boolean results and only push c back in
                    // if they are non empty and true
                    match condition.as_ref() {
                        &P(Empty) | &P(Bool(false)) => {
                            continue;
                        }
                        _ => buf.push(c),
                    }
                }
                stack.push((Rc::new(P(Str(buf))), pos));
            }
            _ => {
                return Err(Error::new(
                    format!("You can only filter over lists, tuples, or strings"),
                    pos,
                ))
            }
        }
        Ok(())
    }

    fn regex(&self, stack: &mut Vec<(Rc<Value>, Position)>, pos: Position) -> Result<(), Error> {
        // 1. get left side (string)
        let left_str = if let Some((val, val_pos)) = stack.pop() {
            if let &P(Str(ref s)) = val.as_ref() {
                s.clone()
            } else {
                return Err(Error::new(
                    format!("Expected string bug got {:?}", val),
                    val_pos,
                ));
            }
        } else {
            unreachable!();
        };

        // 2. get right side (string)
        let right_str = if let Some((val, val_pos)) = stack.pop() {
            if let &P(Str(ref s)) = val.as_ref() {
                s.clone()
            } else {
                return Err(Error::new(
                    format!("Expected string bug got {:?}", val),
                    val_pos,
                ));
            }
        } else {
            unreachable!();
        };

        // 3. compare via regex
        let rex = Regex::new(&right_str)?;
        stack.push((Rc::new(P(Bool(rex.find(&left_str).is_some()))), pos));
        Ok(())
    }

    fn reduce<O, E>(
        &self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        // get the list from the stack
        let (list, list_pos) = if let Some(list) = stack.pop() {
            list
        } else {
            unreachable!();
        };
        // Get the accumulator from the stack
        let (mut acc, mut acc_pos) = if let Some(acc) = stack.pop() {
            acc
        } else {
            unreachable!();
        };
        // get the func ptr from the stack
        let (fptr, fptr_pos) = if let Some(ptr) = stack.pop() {
            ptr
        } else {
            unreachable!();
        };

        let f = if let &F(ref f) = fptr.as_ref() {
            f
        } else {
            return Err(Error::new(format!("Not a function!"), fptr_pos));
        };

        match list.as_ref() {
            &C(List(ref elems)) => {
                for e in elems.iter() {
                    // push function arguments on the stack.
                    stack.push((e.clone(), list_pos.clone()));
                    stack.push((acc.clone(), acc_pos.clone()));
                    // call function and push it's result on the stack.
                    let (new_acc, new_acc_pos) = VM::fcall_impl(f, stack, env.clone())?;
                    acc = new_acc;
                    acc_pos = new_acc_pos;
                }
            }
            &C(Tuple(ref _flds)) => {
                for (ref name, ref val) in _flds.iter() {
                    // push function arguments on the stack.
                    stack.push((val.clone(), list_pos.clone()));
                    stack.push((Rc::new(P(Str(name.clone()))), list_pos.clone()));
                    stack.push((acc.clone(), acc_pos.clone()));
                    // call function and push it's result on the stack.
                    let (new_acc, new_acc_pos) = VM::fcall_impl(f, stack, env.clone())?;
                    acc = new_acc;
                    acc_pos = new_acc_pos;
                }
            }
            &P(Str(ref _s)) => {
                for c in _s.chars() {
                    // push function arguments on the stack.
                    stack.push((Rc::new(P(Str(c.to_string()))), list_pos.clone()));
                    stack.push((acc.clone(), acc_pos.clone()));
                    // call function and push it's result on the stack.
                    let (new_acc, new_acc_pos) = VM::fcall_impl(f, stack, env.clone())?;
                    acc = new_acc;
                    acc_pos = new_acc_pos;
                }
            }
            _ => {
                return Err(Error::new(
                    format!("You can only reduce over lists, tuples, or strings"),
                    pos.clone(),
                ))
            }
        };

        // push the acc on the stack as our result
        stack.push((acc, pos));
        Ok(())
    }

    fn range(&self, stack: &mut Vec<(Rc<Value>, Position)>, pos: Position) -> Result<(), Error> {
        let (start, _) = if let Some(start) = stack.pop() {
            start
        } else {
            unreachable!();
        };
        let (step, _) = if let Some((step, step_pos)) = stack.pop() {
            if let &P(Empty) = step.as_ref() {
                (Rc::new(P(Int(1))), step_pos)
            } else {
                (step, step_pos)
            }
        } else {
            unreachable!();
        };
        let (end, _) = if let Some(end) = stack.pop() {
            end
        } else {
            unreachable!();
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
                return Err(Error::new(
                    format!("Ranges can only be created with Ints"),
                    pos,
                ));
            }
        }
        stack.push((Rc::new(C(List(elems))), pos));
        Ok(())
    }

    fn trace<O, E>(
        &mut self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        pos: Position,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write,
        E: std::io::Write,
    {
        let (val, val_pos) = if let Some(val) = stack.pop() {
            val
        } else {
            unreachable!();
        };
        let expr = stack.pop();
        let expr_pretty = match expr {
            Some((ref expr, _)) => match expr.as_ref() {
                &P(Str(ref expr)) => expr.clone(),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        let writable_val: Val = val.clone().into();
        if let Err(e) = writeln!(
            &mut env.borrow_mut().stderr,
            "TRACE: {} = {} at {}",
            expr_pretty,
            writable_val,
            &val_pos
        ) {
            return Err(Error::new(format!("{}", e), pos));
        };
        stack.push((val, val_pos));
        Ok(())
    }
}
