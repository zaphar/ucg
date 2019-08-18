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
use std::rc::Rc;

use crate::ast::Position;

use super::environment::Environment;
use super::pointer::OpPointer;
use super::runtime;
use super::scope::Stack;
use super::translate::PositionMap;
use super::Composite::{List, Tuple};
use super::Hook;
use super::Primitive::{Bool, Empty, Float, Int, Str};
use super::Value::{C, F, M, P, S, T};
use super::{Error, Op, Primitive, Value};
use super::{Func, Module};

pub struct VM<O, E>
where
    O: std::io::Write,
    E: std::io::Write,
{
    stack: Vec<Rc<Value>>,
    symbols: Stack,
    runtime: runtime::Builtins,
    ops: OpPointer,
    pub env: Rc<RefCell<Environment<O, E>>>,
}

impl<'a, O, E> VM<O, E>
where
    O: std::io::Write,
    E: std::io::Write,
{
    pub fn new(ops: Rc<PositionMap>, env: Rc<RefCell<Environment<O, E>>>) -> Self {
        Self::with_pointer(OpPointer::new(ops), env)
    }

    pub fn with_pointer(ops: OpPointer, env: Rc<RefCell<Environment<O, E>>>) -> Self {
        Self {
            stack: Vec::new(),
            symbols: Stack::new(),
            runtime: runtime::Builtins::new(),
            ops: ops,
            env: env,
        }
    }

    pub fn to_scoped(self, symbols: Stack) -> Self {
        Self {
            stack: Vec::new(),
            symbols: symbols,
            runtime: self.runtime.clone(),
            ops: self.ops.clone(),
            env: self.env.clone(),
        }
    }

    pub fn symbols_to_tuple(&self, include_mod: bool) -> Value {
        let mut flds = Vec::new();
        for sym in self.symbols.symbol_list() {
            if include_mod || sym != "mod" {
                flds.push((sym.clone(), self.symbols.get(sym).unwrap().clone()));
            }
        }
        return C(Tuple(flds));
    }

    pub fn run(&mut self) -> Result<(), Error> {
        loop {
            let op = if let Some(op) = dbg!(self.ops.next()) {
                op.clone()
            } else {
                break;
            };
            let pos = self.ops.pos().unwrap().clone();
            let idx = self.ops.idx()?;
            match op {
                Op::Val(p) => self.push(Rc::new(P(p.clone())))?,
                Op::Sym(s) => self.push(Rc::new(S(s.clone())))?,
                Op::DeRef(s) => self.op_deref(s.clone(), &pos)?,
                Op::Add => self.op_add(&pos)?,
                Op::Mod => self.op_mod(&pos)?,
                Op::Sub => self.op_sub(&pos)?,
                Op::Mul => self.op_mul(&pos)?,
                Op::Div => self.op_div(&pos)?,
                Op::Bind => self.op_bind(true, &pos)?,
                Op::BindOver => self.op_bind(false, &pos)?,
                Op::Equal => self.op_equal()?,
                Op::Not => self.op_not(&pos)?,
                Op::Gt => self.op_gt(&pos)?,
                Op::Lt => self.op_lt(&pos)?,
                Op::GtEq => self.op_gteq(&pos)?,
                Op::LtEq => self.op_lteq(&pos)?,
                // Add a Composite list value to the stack
                Op::InitList => self.push(Rc::new(C(List(Vec::new()))))?,
                // Add a composite tuple value to the stack
                Op::InitTuple => self.push(Rc::new(C(Tuple(Vec::new()))))?,
                Op::Field => self.op_field()?,
                Op::Element => self.op_element()?,
                Op::Index => self.op_index(false, &pos)?,
                Op::SafeIndex => self.op_index(true, &pos)?,
                Op::Cp => self.op_copy(&pos)?,
                //FIXME(jwall): Should this take a user provided message?
                Op::Bang => self.op_bang()?,
                Op::InitThunk(jp) => self.op_thunk(idx, jp)?,
                Op::Noop => {
                    // Do nothing
                }
                Op::Jump(jp) => self.op_jump(jp)?,
                Op::JumpIfTrue(jp) => self.op_jump_if_true(jp, &pos)?,
                Op::JumpIfFalse(jp) => self.op_jump_if_false(jp, &pos)?,
                Op::SelectJump(jp) => self.op_select_jump(jp)?,
                Op::And(jp) => self.op_and(jp, &pos)?,
                Op::Or(jp) => self.op_or(jp, &pos)?,
                Op::Module(mptr) => self.op_module(idx, mptr, &pos)?,
                Op::Func(jptr) => self.op_func(idx, jptr, &pos)?,
                Op::FCall => self.op_fcall(&pos)?,
                Op::NewScope(jp) => self.op_new_scope(jp, self.ops.clone())?,
                Op::Return => {
                    dbg!(&self.stack);
                    return Ok(());
                }
                Op::Pop => {
                    self.pop()?;
                }
                Op::Typ => self.op_typ()?,
                Op::Runtime(h) => self.op_runtime(h, &pos)?,
                Op::Render => self.op_render()?,
            };
        }
        Ok(())
    }

    fn op_typ(&mut self) -> Result<(), Error> {
        let val = self.pop()?;
        let typ_name = match val.as_ref() {
            P(Int(_)) => "int",
            P(Float(_)) => "float",
            P(Bool(_)) => "bool",
            P(Str(_)) => "str",
            P(Empty) => "null",
            C(Tuple(_)) => "tuple",
            C(List(_)) => "list",
            F(_) => "func",
            M(_) => "module",
            S(_) => "sym",
            T(_) => "thunk",
        }
        .to_owned();
        self.push(Rc::new(P(Str(typ_name))))?;
        Ok(())
    }

    fn op_deref(&mut self, name: String, pos: &Position) -> Result<(), Error> {
        let val = self.get_binding(&name, pos)?.clone();
        self.push(val)
    }

    fn op_jump(&mut self, jp: i32) -> Result<(), Error> {
        self.ops.jump(
            self.ops
                .ptr
                .map(|v| (v as i32 + jp) as usize)
                .unwrap_or(jp as usize),
        )?;
        Ok(())
    }

    fn op_and(&mut self, jp: i32, pos: &Position) -> Result<(), Error> {
        let cond = self.pop()?;
        let cc = cond.clone();
        if let &P(Bool(cond)) = cond.as_ref() {
            if !cond {
                self.push(cc)?;
                self.op_jump(jp)?;
            }
        } else {
            return Err(dbg!(Error::new(
                format!("Not a boolean condition {:?} in && expression!", cond),
                pos.clone(),
            )));
        }
        Ok(())
    }

    fn op_or(&mut self, jp: i32, pos: &Position) -> Result<(), Error> {
        let cond = self.pop()?;
        let cc = dbg!(cond.clone());
        if let &P(Bool(cond)) = cond.as_ref() {
            if dbg!(cond) {
                self.push(cc)?;
                self.op_jump(jp)?;
            }
        } else {
            return Err(dbg!(Error::new(
                format!("Not a boolean condition {:?} in || expression!", cond),
                pos.clone(),
            )));
        }
        Ok(())
    }

    fn op_jump_if_true(&mut self, jp: i32, pos: &Position) -> Result<(), Error> {
        let cond = self.pop()?;
        if let &P(Bool(cond)) = cond.as_ref() {
            if cond {
                self.op_jump(jp)?;
            }
        } else {
            return Err(dbg!(Error::new(
                format!("Expected boolean but got {:?}!", cond),
                pos.clone(),
            )));
        }
        Ok(())
    }

    fn op_jump_if_false(&mut self, jp: i32, pos: &Position) -> Result<(), Error> {
        let cond = self.pop()?;
        if let &P(Bool(cond)) = cond.as_ref() {
            if !cond {
                self.op_jump(jp)?;
            }
        } else {
            return Err(dbg!(Error::new(
                format!("Expected boolean but got {:?}!", cond),
                pos.clone(),
            )));
        }
        Ok(())
    }

    fn op_select_jump(&'a mut self, jp: i32) -> Result<(), Error> {
        // pop field value off
        let field_name = dbg!(self.pop())?;
        // pop search value off
        let search = dbg!(self.pop())?;
        // compare them.
        let matched = match (field_name.as_ref(), search.as_ref()) {
            (&S(ref fname), &P(Str(ref sname))) | (&S(ref fname), &S(ref sname)) => fname == sname,
            _ => false,
        };
        if !matched {
            // if they aren't equal then push search value back on and jump
            self.push(dbg!(search))?;
            self.op_jump(dbg!(jp))?;
        }
        Ok(())
    }

    fn op_module(&'a mut self, idx: usize, jptr: i32, pos: &Position) -> Result<(), Error> {
        let mod_val = dbg!(self.pop())?;
        let (result_ptr, flds) = match mod_val.as_ref() {
            &C(Tuple(ref flds)) => (None, flds.clone()),
            &T(ptr) => {
                let tpl_val = self.pop()?;
                if let &C(Tuple(ref flds)) = tpl_val.as_ref() {
                    (Some(ptr), flds.clone())
                } else {
                    return dbg!(Err(Error::new(
                        format!("Expected tuple but got {:?}", tpl_val),
                        pos.clone(),
                    )));
                }
            }
            _ => {
                return dbg!(Err(Error::new(
                    format!("Expected tuple but got {:?}", mod_val),
                    pos.clone(),
                )));
            }
        };
        let mut ops = self.ops.clone();
        let pkg_pos = self.ops.pos().unwrap().clone();
        ops.jump(idx)?;
        let pkg_ptr = if let Some(ref path) = self.ops.path {
            let pkg_ops = vec![
                Op::InitList,
                Op::Func(3),
                Op::Val(Str(path.to_string_lossy().to_string())),
                Op::Runtime(Hook::Import),
                Op::Return,
            ];
            let pos_list = vec![
                pkg_pos.clone(),
                pkg_pos.clone(),
                pkg_pos.clone(),
                pkg_pos.clone(),
                pkg_pos.clone(),
            ];
            Some(OpPointer::new(Rc::new(PositionMap {
                ops: pkg_ops,
                pos: pos_list,
            })))
        } else {
            None
        };
        self.push(Rc::new(M(Module {
            ptr: ops,
            result_ptr: result_ptr,
            flds: flds,
            pkg_ptr: pkg_ptr,
        })))?;
        self.op_jump(jptr)
    }

    fn op_func(&mut self, idx: usize, jptr: i32, pos: &Position) -> Result<(), Error> {
        // get arity from stack
        let mut scope_snapshot = self.symbols.snapshot();
        scope_snapshot.push();
        scope_snapshot.to_open();
        eprintln!("Defining a new function");
        let mut bindings = Vec::new();
        // get imported symbols from stack
        let list_val = self.pop()?;
        if let &C(List(ref elems)) = list_val.as_ref() {
            for e in elems {
                if let &S(ref sym) = e.as_ref() {
                    bindings.push(sym.clone());
                } else {
                    return dbg!(Err(Error::new(
                        format!("Not an argument name {:?}", e),
                        pos.clone(),
                    )));
                }
            }
        } else {
            return dbg!(Err(Error::new(
                format!("Fault!!! Bad Argument List"),
                pos.clone(),
            )));
        }
        let mut ops = self.ops.clone();
        ops.jump(idx)?;
        self.push(Rc::new(F(Func {
            ptr: ops, // where the function starts.
            bindings: bindings,
            snapshot: scope_snapshot,
        })))?;
        eprintln!("Jumping to {} past the function body", jptr);
        self.op_jump(jptr)
    }

    pub fn fcall_impl(
        f: &Func,
        stack: &mut Vec<Rc<Value>>,
        env: Rc<RefCell<Environment<O, E>>>,
        pos: &Position,
    ) -> Result<Rc<Value>, Error> {
        let Func {
            ref ptr,
            ref bindings,
            ref snapshot,
        } = f;
        // use the captured scope snapshot for the function.
        let mut vm = Self::with_pointer(ptr.clone(), env).to_scoped(snapshot.clone());
        for nm in bindings.iter() {
            // now put each argument on our scope stack as a binding.
            // TODO(jwall): This should do a better error if there is
            // nothing on the stack.
            let val = stack.pop().unwrap();
            vm.binding_push(nm.clone(), val, false, pos)?;
        }
        // proceed to the function body
        vm.run()?;
        return vm.pop();
    }

    fn op_new_scope(&mut self, jp: i32, ptr: OpPointer) -> Result<(), Error> {
        let scope_snapshot = self.symbols.snapshot();
        dbg!(&ptr);
        let mut vm = Self::with_pointer(ptr, self.env.clone()).to_scoped(scope_snapshot);
        dbg!(&vm.stack);
        vm.run()?;
        dbg!(&vm.stack);
        self.push(vm.pop()?)?;
        self.op_jump(jp)?;
        Ok(())
    }

    fn op_fcall(&mut self, pos: &Position) -> Result<(), Error> {
        let f = dbg!(self.pop())?;
        if let &F(ref f) = f.as_ref() {
            let val = Self::fcall_impl(f, &mut self.stack, self.env.clone(), pos)?;
            self.push(dbg!(val))?;
        }
        Ok(())
    }

    fn op_thunk(&mut self, idx: usize, jp: i32) -> Result<(), Error> {
        self.push(Rc::new(T(idx)))?;
        self.op_jump(jp)
    }

    fn op_not(&mut self, pos: &Position) -> Result<(), Error> {
        let operand = self.pop()?;
        if let P(Bool(val)) = operand.as_ref() {
            self.push(Rc::new(P(Bool(!val))))?;
            return Ok(());
        }
        return Err(dbg!(Error::new(
            format!("Expected Boolean but got {:?}", operand),
            pos.clone(),
        )));
    }

    fn op_equal(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        self.push(Rc::new(P(Bool(left == right))))?;
        Ok(())
    }

    fn op_gt(&mut self, pos: &Position) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i > ii))))?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f > ff))))?;
            }
            _ => {
                return Err(dbg!(Error::new(
                    format!(
                        "Expected Numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                )));
            }
        }
        Ok(())
    }

    fn op_lt(&mut self, pos: &Position) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i < ii))))?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f < ff))))?;
            }
            _ => {
                return Err(dbg!(Error::new(
                    format!(
                        "Expected Numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                )));
            }
        }
        Ok(())
    }

    fn op_lteq(&mut self, pos: &Position) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i <= ii))))?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f <= ff))))?;
            }
            _ => {
                return Err(dbg!(Error::new(
                    format!(
                        "Expected Numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                )));
            }
        }
        Ok(())
    }

    fn op_gteq(&mut self, pos: &Position) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i >= ii))))?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f >= ff))))?;
            }
            _ => {
                return Err(dbg!(Error::new(
                    format!(
                        "Expected Numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                )));
            }
        }
        Ok(())
    }

    fn op_mod(&mut self, pos: &Position) -> Result<(), Error> {
        // Adds the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.modulus(&left, &right, pos)?)))?;
        Ok(())
    }

    fn op_add(&mut self, pos: &Position) -> Result<(), Error> {
        // Adds the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(self.add(&left, &right, pos)?))?;
        Ok(())
    }

    fn op_sub(&mut self, pos: &Position) -> Result<(), Error> {
        // Subtracts the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.sub(&left, &right, pos)?)))?;
        Ok(())
    }

    fn op_mul(&mut self, pos: &Position) -> Result<(), Error> {
        // Multiplies the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.mul(&left, &right, pos)?)))?;
        Ok(())
    }

    fn op_div(&mut self, pos: &Position) -> Result<(), Error> {
        // Divides the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.div(&left, &right, pos)?)))?;
        Ok(())
    }

    fn op_bind(&mut self, strict: bool, pos: &Position) -> Result<(), Error> {
        // pop val off stack.
        let val = self.pop()?;
        // pop name off stack.
        let name = self.pop()?;
        if let &S(ref name) = name.as_ref() {
            self.binding_push(name.clone(), val, strict, pos)?;
        } else {
            unreachable!();
        }
        Ok(())
    }

    fn op_field(&mut self) -> Result<(), Error> {
        // Add a Composite field value to a tuple on the stack
        // get value from stack
        let val = self.pop()?;
        // get name from stack.
        let name_val = self.pop()?;
        let name = if let &S(ref s) | &P(Str(ref s)) = name_val.as_ref() {
            s
        } else {
            unreachable!();
        };
        // get composite tuple from stack
        let tpl = self.pop()?;
        if let &C(Tuple(ref flds)) = tpl.as_ref() {
            // add name and value to tuple
            let mut flds = flds.clone();
            self.merge_field_into_tuple(&mut flds, name.clone(), val)?;
            // place composite tuple back on stack
            self.push(Rc::new(C(Tuple(flds))))?;
        } else {
            unreachable!();
        };
        Ok(())
    }

    fn op_element(&mut self) -> Result<(), Error> {
        // get element from stack.
        let val = dbg!(self.pop()?);
        // get next value. It should be a Composite list.
        let list = dbg!(self.pop()?);
        if let &C(List(ref elems)) = list.as_ref() {
            // add value to list
            let mut elems = elems.clone();
            elems.push(val);
            // Add that value to the list and put list back on stack.
            self.push(Rc::new(C(List(elems))))?;
        } else {
            unreachable!();
        };
        Ok(())
    }

    fn op_bang(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn op_index(&mut self, safe: bool, pos: &Position) -> Result<(), Error> {
        // left and then right
        let right = dbg!(self.pop()?);
        let left = dbg!(self.pop()?);
        match right.as_ref() {
            &P(Int(i)) => {
                if let &C(List(ref elems)) = left.as_ref() {
                    if i < (elems.len() as i64) && i >= 0 {
                        self.push(elems[i as usize].clone())?;
                        return Ok(());
                    }
                }
            }
            &P(Str(ref s)) => {
                if let &C(Tuple(ref flds)) = left.as_ref() {
                    for &(ref key, ref val) in flds.iter() {
                        if key == s {
                            self.push(val.clone())?;
                            return Ok(());
                        }
                    }
                }
            }
            _ => {
                // noop
            }
        };
        if safe {
            self.push(Rc::new(P(Empty)))?;
            return Ok(());
        }
        return Err(dbg!(Error::new(
            format!("Invalid selector index: {:?} target: {:?}", right, left),
            pos.clone(),
        )));
    }

    fn op_copy(&mut self, pos: &Position) -> Result<(), Error> {
        // get next value. It should be a Module or Tuple.
        let tgt = dbg!(self.pop()?);
        // This value should always be a tuple
        let override_val = dbg!(self.pop()?);
        let overrides = if let &C(Tuple(ref oflds)) = override_val.as_ref() {
            oflds.clone()
        } else {
            unreachable!();
        };
        match tgt.as_ref() {
            &C(Tuple(ref flds)) => {
                let mut flds = flds.clone();
                for (name, val) in overrides {
                    self.merge_field_into_tuple(&mut flds, name, val)?;
                }
                // Put the copy on the Stack
                self.push(Rc::new(C(Tuple(flds))))?;
            }
            &M(Module {
                ref ptr,
                ref result_ptr,
                ref flds,
                ref pkg_ptr,
            }) => {
                let this = M(Module {
                    ptr: ptr.clone(),
                    result_ptr: result_ptr.clone(),
                    flds: flds.clone(),
                    pkg_ptr: pkg_ptr.clone(),
                });

                let mut flds = flds.clone();
                for (name, val) in overrides {
                    self.merge_field_into_tuple(&mut flds, name, val)?;
                }
                self.merge_field_into_tuple(&mut flds, "this".to_owned(), Rc::new(this))?;
                if let Some(ptr) = pkg_ptr {
                    let mut pkg_vm = Self::with_pointer(ptr.clone(), self.env.clone());
                    pkg_vm.run()?;
                    let pkg_func = pkg_vm.pop()?;
                    self.merge_field_into_tuple(&mut flds, "pkg".to_owned(), pkg_func)?;
                }

                let mut vm = Self::with_pointer(ptr.clone(), self.env.clone());
                vm.push(Rc::new(S("mod".to_owned())))?;
                vm.push(Rc::new(C(Tuple(flds))))?;
                vm.run()?;
                if let Some(ptr) = result_ptr {
                    vm.ops.jump(ptr.clone())?;
                    vm.run()?;
                    self.push(dbg!(vm.pop())?)?;
                } else {
                    dbg!(&vm.symbols);
                    self.push(Rc::new(dbg!(vm.symbols_to_tuple(false))))?;
                }
            }
            _ => {
                return Err(dbg!(Error::new(
                    format!("Expected a Tuple or a Module but got {:?}", tgt),
                    pos.clone(),
                )));
            }
        }
        Ok(())
    }

    fn merge_field_into_tuple(
        &self,
        src_fields: &'a mut Vec<(String, Rc<Value>)>,
        name: String,
        value: Rc<Value>,
    ) -> Result<(), Error> {
        for fld in src_fields.iter_mut() {
            if fld.0 == name {
                fld.1 = value;
                return Ok(());
            }
        }
        src_fields.push((name, value));
        Ok(())
    }

    fn push(&mut self, p: Rc<Value>) -> Result<(), Error> {
        self.stack.push(p);
        Ok(())
    }

    pub fn binding_push(
        &mut self,
        name: String,
        val: Rc<Value>,
        strict: bool,
        pos: &Position,
    ) -> Result<(), Error> {
        if self.symbols.is_bound(&name) && strict {
            return Err(dbg!(Error::new(
                format!("Binding {} already exists", name),
                pos.clone(),
            )));
        }
        self.symbols.add(name, val);
        Ok(())
    }

    pub fn get_binding(&'a self, name: &str, pos: &Position) -> Result<Rc<Value>, Error> {
        match self.symbols.get(name) {
            Some(v) => Ok(v),
            None => Err(dbg!(Error::new(
                format!("No such binding {}", name),
                pos.clone()
            ))),
        }
    }

    pub fn pop(&mut self) -> Result<Rc<Value>, Error> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => unreachable!(),
        }
    }

    fn mul(&self, left: &Value, right: &Value, pos: &Position) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), P(Int(ii))) => Int(i * ii),
            (P(Float(f)), P(Float(ff))) => Float(f * ff),
            _ => {
                return Err(dbg!(Error::new(
                    format!(
                        "Expected numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                )))
            }
        })
    }

    fn div(&self, left: &Value, right: &Value, pos: &Position) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), P(Int(ii))) => Int(i / ii),
            (P(Float(f)), P(Float(ff))) => Float(f / ff),
            _ => {
                return Err(dbg!(Error::new(
                    format!(
                        "Expected numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                )))
            }
        })
    }

    fn sub(&self, left: &Value, right: &Value, pos: &Position) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i - ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f - ff),
            _ => {
                return Err(dbg!(Error::new(
                    format!(
                        "Expected numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                )))
            }
        })
    }

    fn modulus(&self, left: &Value, right: &Value, pos: &Position) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i % ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f % ff),
            _ => {
                return Err(dbg!(Error::new(
                    format!(
                        "Expected numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                )))
            }
        })
    }

    fn add(&self, left: &Value, right: &Value, pos: &Position) -> Result<Value, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => P(Int(i + ii)),
            (P(Float(f)), Value::P(Float(ff))) => P(Float(f + ff)),
            (P(Str(s)), Value::P(Str(ss))) => {
                let mut ns = String::new();
                ns.push_str(&s);
                ns.push_str(&ss);
                P(Str(ns))
            }
            (C(List(ref left_list)), C(List(ref right_list))) => {
                let mut new_list = Vec::with_capacity(left_list.len() + right_list.len());
                for v in left_list.iter() {
                    new_list.push(v.clone());
                }
                for v in right_list.iter() {
                    new_list.push(v.clone());
                }
                C(List(new_list))
            }
            _ => {
                return Err(dbg!(Error::new(
                    format!("You can not add {:?} and {:?}", left, right),
                    pos.clone()
                )))
            }
        })
    }

    fn op_runtime(&mut self, h: Hook, pos: &Position) -> Result<(), Error> {
        self.runtime.handle(
            self.ops.path.as_ref(),
            h,
            &mut self.stack,
            self.env.clone(),
            pos,
        )
    }

    fn op_render(&mut self) -> Result<(), Error> {
        let val = self.pop()?;
        self.push(Rc::new(P(Str(val.as_ref().into()))))?;
        Ok(())
    }
}
