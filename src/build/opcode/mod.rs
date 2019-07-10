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
pub mod pointer;
pub mod scope;

use pointer::OpPointer;
use scope::Stack;

#[derive(Debug, PartialEq, Clone)]
pub enum Primitive {
    // Primitive Types
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Empty,
}

use Primitive::{Bool, Float, Int, Str};

#[derive(Debug, PartialEq, Clone)]
pub enum Composite {
    List(Vec<Value>),
    Tuple(Vec<(String, Value)>),
}

use Composite::{List, Tuple};

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    ptr: usize,
    bindings: Vec<String>,
    snapshot: Stack,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    ptr: usize,
    result_ptr: Option<usize>,
    flds: Vec<(String, Value)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    // Binding names.
    S(String),
    // Primitive Types
    P(Primitive),
    // Composite Types.
    C(Composite),
    // Program Pointer
    T(usize),
    // Function
    F(Func),
    // Module
    M(Module),
}
use Value::{C, F, M, P, S, T};

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    // Stack and Name manipulation.
    Bind, // Bind a Val to a name in the heap
    Pop,  // Pop a Value off the value stack and discard it.
    // Math ops
    Add,
    Sub,
    Div,
    Mul,
    // Comparison Ops
    Equal,
    Gt,
    Lt,
    GtEq,
    LtEq,
    // Primitive Types ops
    Val(Primitive),
    // A bareword for use in bindings or lookups
    Sym(String),
    // Reference a binding on the heap
    DeRef(String),
    // Complex Type ops
    InitTuple,
    Field,
    InitList,
    Element,
    // Copy Operation
    Cp,
    // Control Flow
    Bang,
    Jump(i32),
    JumpIfTrue(i32),
    JumpIfFalse(i32),
    SelectJump(i32),
    // FIXME(jwall): Short circuiting operations
    // - And(usize)
    // - Or(usize)
    // Spacer operation, Does nothing.
    Noop,
    // Pending Computation
    InitThunk(i32), // Basically just used for module return expressions
    Module(usize),
    Func(usize),
    Return,
    // - Call
    FCall,
    // Runtime hooks
    // - Map,
    // - Filter,
    // - Reduce,
    // - Import,
    // - Out,
    // - Assert,
    // - Print,
}

#[derive(Debug)]
pub struct Error {}

pub struct VM<'a> {
    stack: Vec<Value>,
    symbols: Stack,
    ops: OpPointer<'a>,
}

impl<'a> VM<'a> {
    pub fn new(ops: &'a Vec<Op>) -> Self {
        Self {
            stack: Vec::new(),
            symbols: Stack::new(),
            ops: OpPointer::new(ops),
        }
    }

    pub fn to_scoped(&self, symbols: Stack) -> Self {
        Self {
            stack: Vec::new(),
            symbols: symbols,
            ops: self.ops.clone(),
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        while self.ops.next().is_some() {
            let idx = self.ops.ptr.unwrap();
            match dbg!(self.ops.op()).unwrap() {
                Op::Val(p) => self.push(dbg!(P(p.clone())))?,
                Op::Sym(s) => self.push(S(s.clone()))?,
                Op::DeRef(s) => self.op_deref(s.clone())?,
                Op::Add => self.op_add()?,
                Op::Sub => self.op_sub()?,
                Op::Mul => self.op_mul()?,
                Op::Div => self.op_div()?,
                Op::Bind => self.op_bind()?,
                Op::Equal => self.op_equal()?,
                Op::Gt => self.op_gt()?,
                Op::Lt => self.op_lt()?,
                Op::GtEq => self.op_gteq()?,
                Op::LtEq => self.op_lteq()?,
                // Add a Composite list value to the stack
                Op::InitList => self.push(C(List(Vec::new())))?,
                // Add a composite tuple value to the stack
                Op::InitTuple => self.push(C(Tuple(Vec::new())))?,
                Op::Field => self.op_field()?,
                Op::Element => self.op_element()?,
                Op::Cp => self.op_copy()?,
                //TODO(jwall): Should this take a user provided message?
                Op::Bang => return dbg!(Err(Error {})),
                Op::InitThunk(jp) => self.op_thunk(idx, *jp)?,
                Op::Noop => {
                    // Do nothing
                }
                Op::Jump(jp) => self.op_jump(*jp)?,
                Op::JumpIfTrue(jp) => self.op_jump_if_true(*jp)?,
                Op::JumpIfFalse(jp) => self.op_jump_if_false(*jp)?,
                Op::SelectJump(jp) => self.op_select_jump(*jp)?,
                Op::Module(mptr) => self.op_module(idx, *mptr)?,
                Op::Func(jptr) => self.op_func(idx, *jptr)?,
                Op::FCall => self.op_fcall()?,
                Op::Return => return Ok(()),
                Op::Pop => {
                    self.pop()?;
                }
            };
        }
        Ok(())
    }

    fn op_deref(&mut self, name: String) -> Result<(), Error> {
        let val = dbg!(self.get_binding(&name)?.clone());
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

    fn op_jump_if_true(&mut self, jp: i32) -> Result<(), Error> {
        if let P(Bool(cond)) = self.pop()? {
            if cond {
                self.op_jump(jp)?;
            }
        }
        Ok(())
    }

    fn op_jump_if_false(&mut self, jp: i32) -> Result<(), Error> {
        if let P(Bool(cond)) = self.pop()? {
            if !cond {
                self.op_jump(jp)?;
            }
        }
        Ok(())
    }

    fn op_select_jump(&mut self, jp: i32) -> Result<(), Error> {
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

    fn op_module(&mut self, idx: usize, jptr: usize) -> Result<(), Error> {
        let (result_ptr, flds) = match self.pop()? {
            C(Tuple(flds)) => (None, flds),
            T(ptr) => {
                if let C(Tuple(flds)) = self.pop()? {
                    (Some(ptr), flds)
                } else {
                    return dbg!(Err(Error {}));
                }
            }
            _ => {
                return dbg!(Err(Error {}));
            }
        };
        self.push(M(Module {
            ptr: dbg!(idx),
            result_ptr: result_ptr,
            flds: dbg!(flds),
        }))?;
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
        if let C(List(elems)) = self.pop()? {
            for e in elems {
                if let S(sym) = e {
                    bindings.push(sym);
                } else {
                    return dbg!(Err(Error {}));
                }
            }
        } else {
            return dbg!(Err(Error {}));
        }
        eprintln!("Pushing function definition on stack");
        self.push(dbg!(F(Func {
            ptr: idx, // where the function starts.
            bindings: bindings,
            snapshot: scope_snapshot,
        })))?;
        eprintln!("Jumping to {} past the function body", jptr);
        self.ops.jump(jptr)
    }

    fn op_fcall(&mut self) -> Result<(), Error> {
        let f = self.pop()?;
        if let F(Func {
            ptr,
            bindings,
            snapshot,
        }) = f
        {
            // TODO(jwall): This is wasteful. We can do better.
            let mut vm = self.to_scoped(snapshot);
            // use the captured scope snapshot for the function.
            for nm in bindings {
                // now put each argument on our scope stack as a binding.
                let val = self.pop()?;
                vm.binding_push(nm, val)?;
            }
            // proceed to the function body
            vm.ops.jump(ptr)?;
            vm.run()?;
            self.push(vm.pop()?)?;
        } else {
            return dbg!(Err(Error {}));
        }
        Ok(())
    }

    fn op_thunk(&mut self, idx: usize, jp: i32) -> Result<(), Error> {
        self.push(dbg!(T(idx)))?;
        self.op_jump(jp)
    }

    fn op_equal(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        self.push(P(Bool(left == right)))?;
        Ok(())
    }

    fn op_gt(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left, right) {
            (P(Int(i)), P(Int(ii))) => {
                self.push(P(Bool(i > ii)))?;
            }
            (P(Float(f)), P(Float(ff))) => {
                self.push(P(Bool(f > ff)))?;
            }
            _ => return Err(Error {}),
        }
        Ok(())
    }

    fn op_lt(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left, right) {
            (P(Int(i)), P(Int(ii))) => {
                self.push(P(Bool(i < ii)))?;
            }
            (P(Float(f)), P(Float(ff))) => {
                self.push(P(Bool(f < ff)))?;
            }
            _ => return Err(Error {}),
        }
        Ok(())
    }

    fn op_lteq(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left, right) {
            (P(Int(i)), P(Int(ii))) => {
                self.push(P(Bool(i <= ii)))?;
            }
            (P(Float(f)), P(Float(ff))) => {
                self.push(P(Bool(f <= ff)))?;
            }
            _ => return Err(Error {}),
        }
        Ok(())
    }

    fn op_gteq(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left, right) {
            (P(Int(i)), P(Int(ii))) => {
                self.push(P(Bool(i >= ii)))?;
            }
            (P(Float(f)), P(Float(ff))) => {
                self.push(P(Bool(f >= ff)))?;
            }
            _ => return Err(Error {}),
        }
        Ok(())
    }

    fn op_add(&mut self) -> Result<(), Error> {
        // Adds the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(P(self.add(left, right)?))?;
        Ok(())
    }

    fn op_sub(&mut self) -> Result<(), Error> {
        // Subtracts the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(P(self.sub(left, right)?))?;
        Ok(())
    }

    fn op_mul(&mut self) -> Result<(), Error> {
        // Multiplies the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(P(self.mul(left, right)?))?;
        Ok(())
    }

    fn op_div(&mut self) -> Result<(), Error> {
        // Divides the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(P(self.div(left, right)?))?;
        Ok(())
    }

    fn op_bind(&mut self) -> Result<(), Error> {
        // pop val off stack.
        let val = dbg!(self.pop())?;
        // pop name off stack.
        let name = dbg!(self.pop())?;
        if let S(name) = name {
            self.binding_push(name, val)?;
        } else {
            return Err(Error {});
        }
        Ok(())
    }

    fn op_field(&mut self) -> Result<(), Error> {
        // Add a Composite field value to a tuple on the stack
        // get value from stack
        let val = self.pop()?;
        // get name from stack.
        let name = if let S(s) | P(Str(s)) = self.pop()? {
            s
        } else {
            return Err(Error {});
        };
        // get composite tuple from stack
        let tpl = self.pop()?;
        if let C(Tuple(mut flds)) = tpl {
            // add name and value to tuple
            self.merge_field_into_tuple(&mut flds, name, val)?;
            // place composite tuple back on stack
            self.push(C(Tuple(flds)))?;
        } else {
            return Err(Error {});
        };
        Ok(())
    }

    fn op_element(&mut self) -> Result<(), Error> {
        // get element from stack.
        let val = self.pop()?;
        // get next value. It should be a Composite list.
        let tpl = self.pop()?;
        if let C(List(mut elems)) = tpl {
            // add value to list
            elems.push(val);
            // Add that value to the list and put list back on stack.
            self.push(C(List(elems)))?;
        } else {
            return Err(Error {});
        };
        Ok(())
    }

    fn op_copy(&mut self) -> Result<(), Error> {
        // TODO Use Cow pointers for this?
        // get next value. It should be a Module.
        let tgt = self.pop()?;
        match tgt {
            C(Tuple(mut flds)) => {
                let overrides = self.pop()?;
                if let C(Tuple(oflds)) = overrides {
                    for (name, val) in oflds {
                        self.merge_field_into_tuple(&mut flds, name, val)?;
                    }
                } else {
                    return dbg!(Err(Error {}));
                }
                // Put the copy on the Stack
                self.push(C(Tuple(flds)))?;
            }
            M(Module {
                ptr,
                result_ptr,
                mut flds,
            }) => {
                let overrides = dbg!(self.pop()?);
                if let C(Tuple(oflds)) = overrides {
                    for (name, val) in oflds {
                        self.merge_field_into_tuple(&mut flds, name, val)?;
                    }
                } else {
                    return dbg!(Err(Error {}));
                }
                let mut vm = VM::new(self.ops.ops);
                vm.push(S("mod".to_owned()))?;
                vm.push(C(Tuple(flds)))?;
                vm.ops.jump(ptr)?;
                vm.run()?;
                let mut flds = Vec::new();
                if let Some(ptr) = dbg!(result_ptr) {
                    vm.ops.jump(ptr)?;
                    vm.run()?;
                    self.push(vm.pop()?)?;
                } else {
                    for sym in vm.symbols.symbol_list() {
                        if sym != "mod" {
                            flds.push((sym.clone(), vm.symbols.get(sym).unwrap().clone()));
                        }
                    }
                    self.push(dbg!(C(Tuple(flds))))?;
                }
            }
            _ => {
                return Err(Error {});
            }
        }
        Ok(())
    }

    fn merge_field_into_tuple(
        &self,
        src_fields: &mut Vec<(String, Value)>,
        name: String,
        value: Value,
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

    fn push(&mut self, p: Value) -> Result<(), Error> {
        self.stack.push(p);
        Ok(())
    }

    fn binding_push(&mut self, name: String, val: Value) -> Result<(), Error> {
        if self.symbols.is_bound(&name) {
            return Err(Error {});
        }
        self.symbols.add(name, val);
        Ok(())
    }

    pub fn get_binding(&mut self, name: &str) -> Result<&Value, Error> {
        match self.symbols.get(name) {
            Some(v) => Ok(v),
            None => Err(Error {}),
        }
    }

    fn pop(&mut self) -> Result<Value, Error> {
        match self.stack.pop() {
            Some(v) => Ok(v),
            None => Err(Error {}),
        }
    }

    fn mul(&self, left: Value, right: Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), P(Int(ii))) => Int(i * ii),
            (P(Float(f)), P(Float(ff))) => Float(f * ff),
            _ => return Err(Error {}),
        })
    }

    fn div(&self, left: Value, right: Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), P(Int(ii))) => Int(i / ii),
            (P(Float(f)), P(Float(ff))) => Float(f / ff),
            _ => return Err(Error {}),
        })
    }

    fn sub(&self, left: Value, right: Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i - ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f - ff),
            _ => return Err(Error {}),
        })
    }

    fn add(&self, left: Value, right: Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i + ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f + ff),
            (P(Str(s)), Value::P(Str(ss))) => {
                let mut ns = String::new();
                ns.push_str(&s);
                ns.push_str(&ss);
                Str(ns)
            }
            _ => return Err(Error {}),
        })
    }
}

#[cfg(test)]
mod test;
