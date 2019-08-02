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
use std::path::PathBuf;
use std::rc::Rc;

use super::pointer::OpPointer;
use super::runtime;
use super::scope::Stack;
use super::{Error, Op, Primitive, Value};

use super::Composite::{List, Tuple};
use super::Hook;
use super::Primitive::{Bool, Empty, Float, Int, Str};
use super::Value::{C, F, M, P, S, T};
use super::{Func, Module};

pub struct VM {
    stack: Vec<Rc<Value>>,
    symbols: Stack,
    runtime: Rc<RefCell<runtime::Builtins>>,
    ops: OpPointer,
    // TODO(jwall): This should be optional
    path: PathBuf,
}

impl<'a> VM {
    pub fn new<P: Into<PathBuf>>(path: P, ops: Rc<Vec<Op>>) -> Self {
        Self::with_pointer(path, OpPointer::new(ops))
    }

    pub fn with_pointer<P: Into<PathBuf>>(path: P, ops: OpPointer) -> Self {
        Self {
            stack: Vec::new(),
            symbols: Stack::new(),
            runtime: Rc::new(RefCell::new(runtime::Builtins::new())),
            ops: ops,
            path: path.into(),
        }
    }

    pub fn to_scoped(self, symbols: Stack) -> Self {
        Self {
            stack: Vec::new(),
            symbols: symbols,
            runtime: self.runtime.clone(),
            ops: self.ops.clone(),
            path: self.path.clone(),
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
            let op = if let Some(op) = self.ops.next() {
                op.clone()
            } else {
                break;
            };
            let idx = self.ops.idx()?;
            match op {
                Op::Val(p) => self.push(Rc::new(P(p.clone())))?,
                Op::Sym(s) => self.push(Rc::new(S(s.clone())))?,
                Op::DeRef(s) => self.op_deref(s.clone())?,
                Op::Add => self.op_add()?,
                Op::Mod => self.op_mod()?,
                Op::Sub => self.op_sub()?,
                Op::Mul => self.op_mul()?,
                Op::Div => self.op_div()?,
                Op::Bind => self.op_bind()?,
                Op::Equal => self.op_equal()?,
                Op::Not => self.op_not()?,
                Op::Gt => self.op_gt()?,
                Op::Lt => self.op_lt()?,
                Op::GtEq => self.op_gteq()?,
                Op::LtEq => self.op_lteq()?,
                // Add a Composite list value to the stack
                Op::InitList => self.push(Rc::new(C(List(Vec::new()))))?,
                // Add a composite tuple value to the stack
                Op::InitTuple => self.push(Rc::new(C(Tuple(Vec::new()))))?,
                Op::Field => self.op_field()?,
                Op::Element => self.op_element()?,
                Op::Index => self.op_index()?,
                Op::Cp => self.op_copy()?,
                //TODO(jwall): Should this take a user provided message?
                Op::Bang => return dbg!(Err(Error {})),
                Op::InitThunk(jp) => self.op_thunk(idx, jp)?,
                Op::Noop => {
                    // Do nothing
                }
                Op::Jump(jp) => self.op_jump(jp)?,
                Op::JumpIfTrue(jp) => self.op_jump_if_true(jp)?,
                Op::JumpIfFalse(jp) => self.op_jump_if_false(jp)?,
                Op::SelectJump(jp) => self.op_select_jump(jp)?,
                Op::And(jp) => self.op_and(jp)?,
                Op::Or(jp) => self.op_or(jp)?,
                Op::Module(mptr) => self.op_module(idx, mptr)?,
                Op::Func(jptr) => self.op_func(idx, jptr)?,
                Op::FCall => self.op_fcall()?,
                Op::Return => return Ok(()),
                Op::Pop => {
                    self.pop()?;
                }
                Op::Typ => self.op_typ()?,
                Op::Runtime(h) => self.op_runtime(h)?,
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
            S(_) | T(_) => {
                return Err(dbg!(Error {}));
            }
        }
        .to_owned();
        self.push(Rc::new(P(Str(typ_name))))?;
        Ok(())
    }

    fn op_deref(&mut self, name: String) -> Result<(), Error> {
        let val = self.get_binding(&name)?.clone();
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

    fn op_and(&mut self, jp: i32) -> Result<(), Error> {
        let cond = self.pop()?;
        let cc = cond.clone();
        if let &P(Bool(cond)) = cond.as_ref() {
            if !cond {
                self.push(cc)?;
                self.op_jump(jp)?;
            }
        } else {
            return Err(dbg!(Error {}));
        }
        Ok(())
    }

    fn op_or(&mut self, jp: i32) -> Result<(), Error> {
        let cond = self.pop()?;
        let cc = cond.clone();
        if let &P(Bool(cond)) = cond.as_ref() {
            if cond {
                self.push(cc)?;
                self.op_jump(jp)?;
            }
        }
        Ok(())
    }

    fn op_jump_if_true(&mut self, jp: i32) -> Result<(), Error> {
        let cond = self.pop()?;
        if let &P(Bool(cond)) = cond.as_ref() {
            if cond {
                self.op_jump(jp)?;
            }
        } else {
            return Err(dbg!(Error {}));
        }
        Ok(())
    }

    fn op_jump_if_false(&mut self, jp: i32) -> Result<(), Error> {
        let cond = self.pop()?;
        if let &P(Bool(cond)) = cond.as_ref() {
            if !cond {
                self.op_jump(dbg!(jp))?;
            }
        }
        Ok(())
    }

    fn op_select_jump(&'a mut self, jp: i32) -> Result<(), Error> {
        // pop field value off
        let field_name = dbg!(self.pop())?;
        // pop search value off
        let search = dbg!(self.pop())?;
        // compare them.
        if dbg!(field_name != search) {
            self.op_jump(dbg!(jp))?;
            self.push(dbg!(search))?;
        }
        dbg!(self.ops.ptr.unwrap());
        // if they aren't equal then push search value back on and jump
        Ok(())
    }

    fn op_module(&'a mut self, idx: usize, jptr: usize) -> Result<(), Error> {
        let mod_val = self.pop()?;
        let (result_ptr, flds) = match mod_val.as_ref() {
            &C(Tuple(ref flds)) => (None, flds.clone()),
            &T(ptr) => {
                let tpl_val = self.pop()?;
                if let &C(Tuple(ref flds)) = tpl_val.as_ref() {
                    (Some(ptr), flds.clone())
                } else {
                    return dbg!(Err(Error {}));
                }
            }
            _ => {
                return dbg!(Err(Error {}));
            }
        };
        let mut ops = self.ops.clone();
        ops.jump(idx)?;
        self.push(Rc::new(M(Module {
            ptr: dbg!(ops),
            result_ptr: result_ptr,
            flds: dbg!(flds),
        })))?;
        self.ops.jump(dbg!(jptr))
    }

    fn op_func(&mut self, idx: usize, jptr: usize) -> Result<(), Error> {
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
                    return dbg!(Err(Error {}));
                }
            }
        } else {
            return dbg!(Err(Error {}));
        }
        eprintln!("Pushing function definition on stack");
        let mut ops = self.ops.clone();
        ops.jump(idx)?;
        self.push(Rc::new(dbg!(F(Func {
            ptr: ops, // where the function starts.
            bindings: bindings,
            snapshot: scope_snapshot,
        }))))?;
        eprintln!("Jumping to {} past the function body", jptr);
        self.ops.jump(jptr)
    }

    pub fn fcall_impl<P: Into<PathBuf>>(
        path: P,
        f: &Func,
        stack: &mut Vec<Rc<Value>>,
    ) -> Result<Rc<Value>, Error> {
        let Func {
            ref ptr,
            ref bindings,
            ref snapshot,
        } = f;
        // use the captured scope snapshot for the function.
        let mut vm = Self::with_pointer(path, ptr.clone()).to_scoped(snapshot.clone());
        for nm in bindings.iter() {
            // now put each argument on our scope stack as a binding.
            // TODO(jwall): This should do a better error if there is
            // nothing on the stack.
            let val = stack.pop().unwrap();
            vm.binding_push(nm.clone(), val)?;
        }
        // proceed to the function body
        vm.run()?;
        return vm.pop();
    }

    fn op_fcall(&mut self) -> Result<(), Error> {
        let f = self.pop()?;
        if let &F(ref f) = f.as_ref() {
            let val = Self::fcall_impl(&self.path, f, &mut self.stack)?;
            self.push(val)?;
        }
        Ok(())
    }

    fn op_thunk(&mut self, idx: usize, jp: i32) -> Result<(), Error> {
        self.push(Rc::new(dbg!(T(idx))))?;
        self.op_jump(jp)
    }

    fn op_not(&mut self) -> Result<(), Error> {
        let operand = self.pop()?;
        if let P(Bool(val)) = operand.as_ref() {
            self.push(Rc::new(P(Bool(!val))))?;
            return Ok(());
        }
        return Err(dbg!(Error {}));
    }

    fn op_equal(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        self.push(Rc::new(P(Bool(left == right))))?;
        Ok(())
    }

    fn op_gt(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i > ii))))?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f > ff))))?;
            }
            _ => return Err(dbg!(Error {})),
        }
        Ok(())
    }

    fn op_lt(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i < ii))))?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f < ff))))?;
            }
            _ => return Err(dbg!(Error {})),
        }
        Ok(())
    }

    fn op_lteq(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i <= ii))))?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f <= ff))))?;
            }
            _ => return Err(dbg!(Error {})),
        }
        Ok(())
    }

    fn op_gteq(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i >= ii))))?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f >= ff))))?;
            }
            _ => return Err(dbg!(Error {})),
        }
        Ok(())
    }

    fn op_mod(&mut self) -> Result<(), Error> {
        // Adds the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.modulus(&left, &right)?)))?;
        Ok(())
    }

    fn op_add(&mut self) -> Result<(), Error> {
        // Adds the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.add(&left, &right)?)))?;
        Ok(())
    }

    fn op_sub(&mut self) -> Result<(), Error> {
        // Subtracts the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.sub(&left, &right)?)))?;
        Ok(())
    }

    fn op_mul(&mut self) -> Result<(), Error> {
        // Multiplies the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.mul(&left, &right)?)))?;
        Ok(())
    }

    fn op_div(&mut self) -> Result<(), Error> {
        // Divides the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.div(&left, &right)?)))?;
        Ok(())
    }

    fn op_bind(&mut self) -> Result<(), Error> {
        // pop val off stack.
        let val = dbg!(self.pop())?;
        // pop name off stack.
        let name = dbg!(self.pop())?;
        if let &S(ref name) = name.as_ref() {
            self.binding_push(name.clone(), val)?;
        } else {
            return Err(dbg!(Error {}));
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
            return Err(dbg!(Error {}));
        };
        // get composite tuple from stack
        let tpl = self.pop()?;
        if let &C(Tuple(ref flds)) = tpl.as_ref() {
            // add name and value to tuple
            // TODO(jwall): This is probably memory inefficient and we should
            // optimize it a bit.
            let mut flds = flds.clone();
            self.merge_field_into_tuple(&mut flds, name.clone(), val)?;
            // place composite tuple back on stack
            self.push(Rc::new(C(Tuple(flds))))?;
        } else {
            return Err(dbg!(Error {}));
        };
        Ok(())
    }

    fn op_element(&mut self) -> Result<(), Error> {
        // get element from stack.
        let val = self.pop()?;
        // get next value. It should be a Composite list.
        let tpl = self.pop()?;
        if let &C(List(ref elems)) = tpl.as_ref() {
            // add value to list
            // TODO(jwall): This is probably memory inefficient and we should
            // optimize it a bit.
            let mut elems = elems.clone();
            elems.push(val);
            // Add that value to the list and put list back on stack.
            self.push(Rc::new(C(List(elems))))?;
        } else {
            return Err(dbg!(Error {}));
        };
        Ok(())
    }

    fn find_in_list(&self, index: &Value, elems: &Vec<Rc<Value>>) -> Result<Rc<Value>, Error> {
        let idx = match index {
            P(Int(i)) => i.clone(),
            _ => return dbg!(Err(Error {})),
        };
        match elems.get(idx as usize) {
            Some(v) => Ok(v.clone()),
            None => Err(dbg!(Error {})),
        }
    }

    fn find_in_flds(
        &self,
        index: &Value,
        flds: &Vec<(String, Rc<Value>)>,
    ) -> Result<Rc<Value>, Error> {
        let idx = match index {
            S(p) => p,
            P(Str(p)) => p,
            _ => return dbg!(Err(Error {})),
        };
        for f in flds.iter() {
            if idx == &f.0 {
                return Ok(f.1.clone());
            }
        }
        Err(dbg!(Error {}))
    }

    fn find_in_value(&self, index: &Value, target: &Value) -> Result<Rc<Value>, Error> {
        match target {
            C(Tuple(flds)) => self.find_in_flds(index, flds),
            C(List(elements)) => self.find_in_list(index, elements),
            _ => return Err(dbg!(Error {})),
        }
    }

    fn op_index(&mut self) -> Result<(), Error> {
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
        return Err(dbg!(Error {}));
    }

    fn op_copy(&mut self) -> Result<(), Error> {
        // TODO Use Cow pointers for this?
        // get next value. It should be a Module or Tuple.
        let tgt = dbg!(self.pop())?;
        // This value should always be a tuple
        let override_val = self.pop()?;
        let overrides = if let &C(Tuple(ref oflds)) = override_val.as_ref() {
            oflds.clone()
        } else {
            return Err(dbg!(Error {}));
        };
        match tgt.as_ref() {
            &C(Tuple(ref flds)) => {
                let mut flds = flds.clone();
                for (name, val) in overrides {
                    dbg!(self.merge_field_into_tuple(&mut flds, name, val))?;
                }
                // Put the copy on the Stack
                self.push(Rc::new(C(Tuple(flds))))?;
            }
            &M(Module {
                ref ptr,
                ref result_ptr,
                ref flds,
            }) => {
                //let this = M(Module {
                //    ptr: ptr.clone(),
                //    result_ptr: result_ptr.clone(),
                //    flds: flds.clone(),
                //});
                let mut flds = flds.clone();
                for (name, val) in overrides {
                    self.merge_field_into_tuple(&mut flds, name, val)?;
                }
                // FIXME(jwall): We need to populate the pkg key for modules.
                //self.merge_field_into_tuple(&mut flds, "this".to_owned(), this)?;
                let mut vm = Self::with_pointer(self.path.clone(), ptr.clone());
                vm.push(Rc::new(S("mod".to_owned())))?;
                vm.push(Rc::new(C(Tuple(dbg!(flds)))))?;
                vm.run()?;
                if let Some(ptr) = dbg!(result_ptr) {
                    vm.ops.jump(ptr.clone())?;
                    vm.run()?;
                    self.push(vm.pop()?)?;
                } else {
                    self.push(dbg!(Rc::new(vm.symbols_to_tuple(false))))?;
                }
            }
            _ => {
                return Err(dbg!(Error {}));
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

    pub fn binding_push(&mut self, name: String, val: Rc<Value>) -> Result<(), Error> {
        if self.symbols.is_bound(&name) {
            return Err(dbg!(Error {}));
        }
        self.symbols.add(name, val);
        Ok(())
    }

    pub fn get_binding(&'a self, name: &str) -> Result<Rc<Value>, Error> {
        match self.symbols.get(name) {
            Some(v) => Ok(v),
            None => Err(dbg!(Error {})),
        }
    }

    pub fn pop(&mut self) -> Result<Rc<Value>, Error> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => Err(dbg!(Error {})),
        }
    }

    fn mul(&self, left: &Value, right: &Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), P(Int(ii))) => Int(i * ii),
            (P(Float(f)), P(Float(ff))) => Float(f * ff),
            _ => return Err(dbg!(Error {})),
        })
    }

    fn div(&self, left: &Value, right: &Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), P(Int(ii))) => Int(i / ii),
            (P(Float(f)), P(Float(ff))) => Float(f / ff),
            _ => return Err(dbg!(Error {})),
        })
    }

    fn sub(&self, left: &Value, right: &Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i - ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f - ff),
            _ => return Err(dbg!(Error {})),
        })
    }

    fn modulus(&self, left: &Value, right: &Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i % ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f % ff),
            _ => return Err(dbg!(Error {})),
        })
    }

    fn add(&self, left: &Value, right: &Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i + ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f + ff),
            (P(Str(s)), Value::P(Str(ss))) => {
                let mut ns = String::new();
                ns.push_str(&s);
                ns.push_str(&ss);
                Str(ns)
            }
            _ => return Err(dbg!(Error {})),
        })
    }

    fn op_runtime(&mut self, h: Hook) -> Result<(), Error> {
        self.runtime
            .borrow_mut()
            .handle(&self.path, h, &mut self.stack)
    }
}
