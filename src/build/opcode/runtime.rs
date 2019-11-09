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
use std::fmt::Debug;
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
use Composite::{List, Tuple};
use Primitive::{Bool, Empty, Int, Str};

pub struct Builtins {
    pub strict: bool,
    import_path: Vec<PathBuf>,
    validate_mode: bool,
}

impl Builtins {
    pub fn new(strict: bool) -> Self {
        // FIXME(jwall): This should probably be injected in.
        Self {
            strict: strict,
            import_path: Vec::new(),
            validate_mode: false,
        }
    }

    pub fn clone(&self) -> Self {
        Self {
            strict: self.strict,
            import_path: self.import_path.clone(),
            validate_mode: self.validate_mode,
        }
    }

    pub fn enable_validate_mode(&mut self) {
        self.validate_mode = true;
    }

    pub fn handle<P, WP, O, E>(
        &mut self,
        path: Option<P>,
        h: Hook,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        import_stack: &mut Vec<String>,
        working_dir: WP,
        pos: Position,
    ) -> Result<(), Error>
    where
        P: AsRef<Path> + Debug,
        WP: Into<PathBuf> + Clone + Debug,
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
    {
        match h {
            Hook::Import => self.import(working_dir, stack, env, import_stack, pos),
            Hook::Include => self.include(working_dir, stack, env, pos),
            Hook::Assert => self.assert(stack, env),
            Hook::Convert => self.convert(stack, env, pos),
            Hook::Out => self.out(path, stack, env, pos),
            Hook::Map => self.map(stack, env, import_stack, pos),
            Hook::Filter => self.filter(stack, env, import_stack, pos),
            Hook::Reduce => self.reduce(stack, env, import_stack, pos),
            Hook::Regex => self.regex(stack, pos),
            Hook::Range => self.range(stack, pos),
            Hook::Trace(pos) => self.trace(stack, pos, env),
        }
    }

    fn normalize_path<P, BP>(
        &self,
        base_path: BP,
        use_import_path: bool,
        path: P,
    ) -> Result<PathBuf, Error>
    where
        BP: Into<PathBuf>,
        P: Into<PathBuf>,
    {
        // Try a relative path first.
        let path = path.into();
        // stdlib paths are special
        if path.starts_with("std/") {
            return Ok(path);
        }
        let mut normalized = base_path.into();
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
        // The canonicalize method on windows is not what we want so we'll
        // do something a little different on windows that we would do on
        // other Operating Systems.
        #[cfg(target_os = "windows")]
        {
            Ok(normalized)
        }
        #[cfg(not(target_os = "windows"))]
        {
            Ok(dbg!(normalized.canonicalize()?))
        }
    }

    fn find_file<P, BP>(
        &self,
        base_path: BP,
        path: P,
        use_import_path: bool,
        pos: Position,
    ) -> Result<PathBuf, Error>
    where
        P: Into<PathBuf>,
        BP: Into<PathBuf>,
    {
        // Try a relative path first.
        // FIXME(jwall): Use import paths if desired.
        let normalized = self.normalize_path(base_path, use_import_path, path)?;
        match normalized.canonicalize() {
            Ok(p) => Ok(p),
            Err(_e) => Err(Error::new(
                format!("Invalid path: {}", normalized.to_string_lossy()),
                pos.clone(),
            )),
        }
    }

    fn get_file_as_string<P: Into<PathBuf>>(
        &self,
        base_path: P,
        path: &str,
        pos: Position,
    ) -> Result<String, Error> {
        let sep = format!("{}", std::path::MAIN_SEPARATOR);
        let raw_path = path.replace("/", &sep);
        // FIXME(jwall): import paths?
        let normalized = self.find_file(base_path, raw_path, false, pos)?;
        // TODO(jwall): Proper error here
        let mut f = File::open(normalized)?;
        let mut contents = String::new();
        // TODO(jwall): Proper error here
        f.read_to_string(&mut contents)?;
        Ok(contents)
    }

    fn import<P, O, E>(
        &mut self,
        base_path: P,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        import_stack: &mut Vec<String>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
        P: Into<PathBuf> + Clone + Debug,
    {
        let path = stack.pop();
        if let Some((val, path_pos)) = path {
            if let &Value::P(Str(ref path)) = val.as_ref() {
                // TODO(jwall): A bit hacky we should probably change import stacks to be pathbufs.
                let normalized =
                    decorate_error!(path_pos => self.normalize_path(base_path, false, path))?;
                // first we chack the cache
                let path = normalized.to_string_lossy().to_string();
                if let Some(val) = env.borrow().get_cached_path_val(&path) {
                    stack.push((val, path_pos));
                    return Ok(());
                }
                if import_stack.iter().find(|p| *p == &path).is_some() {
                    return Err(Error::new(
                        format!("Import cycle detected: {} in {:?}", path, import_stack),
                        pos,
                    ));
                }
                let val = { env.borrow_mut().get_cached_path_val(&path) };
                match val {
                    Some(v) => {
                        stack.push((v, path_pos));
                    }
                    None => {
                        let op_pointer = decorate_error!(path_pos => env.borrow_mut().get_ops_for_path(&normalized))?;
                        // TODO(jwall): What if we don't have a base path?
                        let mut vm = VM::with_pointer(
                            self.strict,
                            op_pointer,
                            env.clone(),
                            normalized.parent().unwrap(),
                        )
                        .with_import_stack(import_stack.clone());
                        vm.run()?;
                        let result = Rc::new(vm.symbols_to_tuple(true));
                        env.borrow_mut().update_path_val(&path, result.clone());
                        stack.push((result, pos));
                    }
                }
                import_stack.push(path.clone());
                return Ok(());
            }
            return Err(Error::new(format!("Invalid Path {:?}", val), pos));
        }
        unreachable!();
    }

    fn include<P, O, E>(
        &self,
        base_path: P,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
        P: Into<PathBuf> + Clone + Debug,
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
                Rc::new(P(Str(self.get_file_as_string(
                    base_path,
                    &path,
                    pos.clone(),
                )?))),
                pos.clone(),
            ));
        } else {
            stack.push((
                Rc::new(match env.borrow().importer_registry.get_importer(&typ) {
                    Some(importer) => {
                        let contents = self.get_file_as_string(base_path, &path, pos.clone())?;
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

    fn assert<O, E>(
        &mut self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
    {
        if let Some((tuple, tpl_pos)) = stack.pop() {
            if let &Value::C(Tuple(ref tuple_flds, _)) = tuple.as_ref() {
                // look for the description field
                let mut desc = None;
                // look for the ok field.
                let mut ok = None;
                for &(ref name, ref val) in tuple_flds.iter() {
                    if name == "desc" {
                        desc = Some(val.as_ref());
                    }
                    if name == "ok" {
                        ok = Some(val.as_ref());
                    }
                }
                let ok = if let Some(&P(Bool(ref b))) = ok {
                    *b
                } else {
                    let msg = format!(
                        "TYPE FAIL - Expected Boolean field ok in tuple {} at {}\n",
                        tuple, tpl_pos
                    );
                    env.borrow_mut().record_assert_result(&msg, false);
                    return Ok(());
                };

                let desc = if let Some(&P(Str(ref desc))) = desc {
                    desc
                } else {
                    let msg = format!(
                        "TYPE FAIL - Expected String field desc in tuple {} at {}\n",
                        tuple, tpl_pos
                    );
                    env.borrow_mut().record_assert_result(&msg, false);
                    return Ok(());
                };
                env.borrow_mut().record_assert_result(desc, ok);
                return Ok(());
            }
            let msg = format!(
                "TYPE FAIL - Expected tuple with ok and desc fields got {} at {}\n",
                tuple, tpl_pos
            );
            env.borrow_mut().record_assert_result(&msg, false);
        } else {
            unreachable!();
        }
        return Ok(());
    }

    fn out<P, O, E>(
        &self,
        path: Option<P>,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
        P: AsRef<Path> + Debug,
    {
        let write_path: Option<PathBuf> = if let Some(path) = path {
            let write_path = path.as_ref().to_path_buf();
            if env.borrow().get_out_lock_for_path(&path) {
                return Err(Error::new(
                    format!("You can only have one output per file"),
                    pos,
                ));
            }
            env.borrow_mut().set_out_lock_for_path(path.as_ref());
            Some(write_path)
        } else {
            if env.borrow().get_out_lock_for_path("/dev/stdout") {
                return Err(Error::new(
                    format!("You can only have one output per file"),
                    pos,
                ));
            }
            env.borrow_mut().set_out_lock_for_path("/dev/stdout");
            None
        };
        let val = stack.pop();
        if let Some((val, val_pos)) = val {
            let val = val.into();
            let c_type = stack.pop();
            if let Some((c_type_val, c_type_pos)) = c_type {
                if let &Value::P(Primitive::Str(ref c_type)) = c_type_val.as_ref() {
                    let stdout = env.borrow().stdout();
                    if let Some(c) = env.borrow().converter_registry.get_converter(c_type) {
                        let mut writer: Box<dyn std::io::Write> = match write_path {
                            Some(p) => {
                                let p = p.with_extension(c.file_ext());
                                Box::new(File::create(&p)?)
                            }
                            None => Box::new(stdout),
                        };
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
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
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
        import_stack: &Vec<String>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
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
            &C(List(ref elems, ref elems_pos_list)) => {
                let mut result_elems = Vec::new();
                let mut pos_elems = Vec::new();
                let mut counter = 0;
                for e in elems.iter() {
                    // push function argument on the stack.
                    let e_pos = elems_pos_list[counter].clone();
                    stack.push((e.clone(), e_pos.clone()));
                    // call function and push it's result on the stack.
                    let (result, result_pos) = decorate_call!(pos =>
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
                    pos_elems.push(result_pos);
                    result_elems.push(result);
                    counter += 1;
                }
                stack.push((Rc::new(C(List(result_elems, pos_elems))), list_pos));
            }
            &C(Tuple(ref flds, ref flds_pos_list)) => {
                let mut new_fields = Vec::new();
                let mut new_flds_pos_list = Vec::new();
                let mut counter = 0;
                for (ref name, ref val) in flds {
                    let name_pos = flds_pos_list[counter].0.clone();
                    let val_pos = flds_pos_list[counter].1.clone();
                    stack.push((Rc::new(P(Str(name.clone()))), name_pos));
                    stack.push((val.clone(), val_pos));
                    let (result, result_pos) = decorate_call!(pos =>
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
                    if let &C(List(ref fval, _)) = result.as_ref() {
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
                        let name_pos = flds_pos_list[counter].0.clone();
                        new_flds_pos_list.push((name_pos, result_pos));
                        new_fields.push((name, fval[1].clone()));
                    }
                    counter += 1;
                }
                stack.push((Rc::new(C(Tuple(new_fields, new_flds_pos_list))), pos));
            }
            &P(Str(ref s)) => {
                let mut buf = String::new();
                for c in s.chars() {
                    stack.push((Rc::new(P(Str(c.to_string()))), list_pos.clone()));
                    // call function and push it's result on the stack.
                    let (result, result_pos) = decorate_call!(pos =>
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
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
        import_stack: &Vec<String>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
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
            &C(List(ref elems, ref elems_pos_list)) => {
                let mut result_elems = Vec::new();
                let mut pos_elems = Vec::new();
                let mut counter = 0;
                for e in elems.iter() {
                    // push function argument on the stack.
                    let e_pos = elems_pos_list[counter].clone();
                    stack.push((e.clone(), e_pos.clone()));
                    // call function and push it's result on the stack.
                    let (condition, _) = decorate_call!(pos =>
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
                    // Check for empty or boolean results and only push e back in
                    // if they are non empty and true
                    counter += 1;
                    match condition.as_ref() {
                        &P(Empty) | &P(Bool(false)) => {
                            continue;
                        }
                        _ => {
                            result_elems.push(e.clone());
                            pos_elems.push(e_pos);
                        }
                    }
                }
                stack.push((Rc::new(C(List(result_elems, pos_elems))), pos));
            }
            &C(Tuple(ref flds, ref pos_list)) => {
                let mut new_fields = Vec::new();
                let mut new_flds_pos_list = Vec::new();
                let mut counter = 0;
                for (ref name, ref val) in flds {
                    let name_pos = pos_list[counter].0.clone();
                    let val_pos = pos_list[counter].1.clone();
                    stack.push((Rc::new(P(Str(name.clone()))), name_pos.clone()));
                    stack.push((val.clone(), val_pos.clone()));
                    let (condition, _) = decorate_call!(pos =>
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
                    // Check for empty or boolean results and only push e back in
                    // if they are non empty and true
                    counter += 1;
                    match condition.as_ref() {
                        &P(Empty) | &P(Bool(false)) => {
                            continue;
                        }
                        _ => {
                            new_fields.push((name.clone(), val.clone()));
                            new_flds_pos_list.push((name_pos, val_pos));
                        }
                    }
                }
                stack.push((Rc::new(C(Tuple(new_fields, new_flds_pos_list))), pos));
            }
            &P(Str(ref s)) => {
                let mut buf = String::new();
                for c in s.chars() {
                    stack.push((Rc::new(P(Str(c.to_string()))), list_pos.clone()));
                    // call function and push it's result on the stack.
                    let (condition, _) = decorate_call!(pos =>
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
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
        import_stack: &Vec<String>,
        pos: Position,
    ) -> Result<(), Error>
    where
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
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
            &C(List(ref elems, ref elems_pos_list)) => {
                let mut counter = 0;
                for e in elems.iter() {
                    let e_pos = elems_pos_list[counter].clone();
                    // push function arguments on the stack.
                    stack.push((acc.clone(), acc_pos.clone()));
                    stack.push((e.clone(), e_pos.clone()));
                    // call function and push it's result on the stack.
                    let (new_acc, new_acc_pos) = decorate_call!(pos =>
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
                    acc = new_acc;
                    acc_pos = new_acc_pos;
                    counter += 1;
                }
            }
            &C(Tuple(ref _flds, ref flds_pos_list)) => {
                let mut counter = 0;
                for (ref name, ref val) in _flds.iter() {
                    let name_pos = flds_pos_list[counter].0.clone();
                    let val_pos = flds_pos_list[counter].1.clone();
                    // push function arguments on the stack.
                    stack.push((acc.clone(), acc_pos.clone()));
                    stack.push((Rc::new(P(Str(name.clone()))), name_pos));
                    stack.push((val.clone(), val_pos));
                    // call function and push it's result on the stack.
                    let (new_acc, new_acc_pos) = decorate_call!(pos =>
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
                    acc = new_acc;
                    acc_pos = new_acc_pos;
                    counter += 1;
                }
            }
            &P(Str(ref s)) => {
                for c in s.chars() {
                    // push function arguments on the stack.
                    stack.push((acc.clone(), acc_pos.clone()));
                    stack.push((Rc::new(P(Str(c.to_string()))), list_pos.clone()));
                    // call function and push it's result on the stack.
                    let (new_acc, new_acc_pos) = decorate_call!(pos => 
                        VM::fcall_impl(f, self.strict, stack, env.clone(), import_stack))?;
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
        let mut pos_list = Vec::new();
        match (start.as_ref(), step.as_ref(), end.as_ref()) {
            (&P(Int(start)), &P(Int(step)), &P(Int(end))) => {
                let mut num = start;
                loop {
                    if num > end {
                        break;
                    }
                    elems.push(Rc::new(P(Int(num))));
                    pos_list.push(pos.clone());
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
        stack.push((Rc::new(C(List(elems, pos_list))), pos));
        Ok(())
    }

    fn trace<O, E>(
        &mut self,
        stack: &mut Vec<(Rc<Value>, Position)>,
        pos: Position,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(), Error>
    where
        O: std::io::Write + Clone,
        E: std::io::Write + Clone,
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
