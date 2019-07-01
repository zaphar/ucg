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

pub mod pointer;
use pointer::OpPointer;

#[derive(Debug, PartialEq, Clone)]
pub enum Primitive {
    // Primitive Types
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Empty,
}

use Primitive::{Bool, Empty, Float, Int, Str};

#[derive(Debug, PartialEq, Clone)]
pub enum Composite {
    List(Vec<Value>),
    Tuple(Vec<(String, Value)>),
}

use Composite::{List, Tuple};

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
}
use Value::{C, P, S, T};

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
    // Complex Type ops
    InitTuple,
    Field,
    InitList,
    Element,
    // Operations
    Cp,
    // Control Flow
    Bang,
    Jump(usize),
    Noop,
    // Pending Computation
    InitThunk(usize),
    Return,
    // - Call
    // Runtime hooks
    // - Map
    // - Filter
    // - Reduce
    // - Import
    // - Out
    // - Assert
    // - Print
}

#[derive(Debug)]
pub struct Error {}

pub struct VM {
    stack: Vec<Value>,
    // TODO(jwall): We may want to preserve order on these.
    symbols: BTreeMap<String, Value>,
    ops: OpPointer,
}

impl VM {
    pub fn new(ops: Vec<Op>) -> Self {
        Self {
            stack: Vec::new(),
            symbols: BTreeMap::new(),
            ops: OpPointer::new(ops),
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        while self.ops.next().is_some() {
            let idx = self.ops.ptr.unwrap();
            match self.ops.op().unwrap() {
                Op::Val(p) => self.primitive_push(p.clone())?,
                Op::Sym(s) => self.push(Value::S(s.clone()))?,
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
                Op::InitList => self.composite_push(List(Vec::new()))?,
                // Add a composite tuple value to the stack
                Op::InitTuple => self.composite_push(Tuple(Vec::new()))?,
                Op::Field => self.op_field()?,
                Op::Element => self.op_element()?,
                Op::Cp => self.op_copy()?,
                Op::Bang => return Err(Error {}),
                Op::InitThunk(jp) => self.op_thunk(idx, *jp)?,
                Op::Noop => {
                    // Do nothing
                }
                Op::Return => {
                    // TODO(jwall): This means we return back to the start of the frame.
                }
                Op::Jump(jp) => self.ops.jump(self.ops.ptr.map(|v| v + jp).unwrap_or(*jp))?,
                Op::Pop => {
                    self.pop()?;
                }
            };
        }
        Ok(())
    }

    fn op_thunk(&mut self, idx: usize, jp: usize) -> Result<(), Error> {
        self.push(Value::T(idx))?;
        self.ops.jump(self.ops.ptr.map(|v| v + jp).unwrap_or(jp))?;
        // TODO(jwall): Skip over the rest until we reach the return?
        Ok(())
    }

    fn op_equal(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        self.push(Value::P(Bool(left == right)))?;
        Ok(())
    }

    fn op_gt(&mut self) -> Result<(), Error> {
        let left = self.pop()?;
        let right = self.pop()?;
        match (left, right) {
            (P(Int(i)), P(Int(ii))) => {
                self.push(Value::P(Bool(i > ii)))?;
            }
            (P(Float(f)), P(Float(ff))) => {
                self.push(Value::P(Bool(f > ff)))?;
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
                self.push(Value::P(Bool(i < ii)))?;
            }
            (P(Float(f)), P(Float(ff))) => {
                self.push(Value::P(Bool(f < ff)))?;
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
                self.push(Value::P(Bool(i <= ii)))?;
            }
            (P(Float(f)), P(Float(ff))) => {
                self.push(Value::P(Bool(f <= ff)))?;
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
                self.push(Value::P(Bool(i >= ii)))?;
            }
            (P(Float(f)), P(Float(ff))) => {
                self.push(Value::P(Bool(f >= ff)))?;
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
        self.primitive_push(self.add(left, right)?)?;
        Ok(())
    }

    fn op_sub(&mut self) -> Result<(), Error> {
        // Subtracts the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.primitive_push(self.sub(left, right)?)?;
        Ok(())
    }

    fn op_mul(&mut self) -> Result<(), Error> {
        // Multiplies the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.primitive_push(self.mul(left, right)?)?;
        Ok(())
    }

    fn op_div(&mut self) -> Result<(), Error> {
        // Divides the previous two items in the stack.
        let left = self.pop()?;
        let right = self.pop()?;
        // Then pushes the result onto the stack.
        self.primitive_push(self.div(left, right)?)?;
        Ok(())
    }

    fn op_bind(&mut self) -> Result<(), Error> {
        // TODO(jwall): Okay this should actually
        // point to a location in the library op code
        // instead of storing in the heap.
        // pop val off stack.
        let val = self.pop()?;
        // pop name off stack.
        let name = self.pop()?;
        if let Value::S(name) = name {
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
        let name = if let Value::S(s) | Value::P(Primitive::Str(s)) = self.pop()? {
            s
        } else {
            return Err(Error {});
        };
        // get composite tuple from stack
        let tpl = self.pop()?;
        if let Value::C(Tuple(mut flds)) = tpl {
            // add name and value to tuple
            self.merge_field_into_tuple(&mut flds, name, val)?;
            // place composite tuple back on stack
            self.composite_push(Tuple(flds))?;
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
        if let Value::C(List(mut elems)) = tpl {
            // add value to list
            elems.push(val);
            // Add that value to the list and put list back on stack.
            self.composite_push(List(elems))?;
        } else {
            return Err(Error {});
        };
        Ok(())
    }

    fn op_copy(&mut self) -> Result<(), Error> {
        // TODO Use Cow pointers for this?
        // get next value. It should be a Composite Tuple.
        if let Value::C(Tuple(flds)) = self.pop()? {
            // Make a copy of the original
            let original = Tuple(flds.clone());
            let copy = Tuple(flds);
            // Put the original on the Stack as well as the copy
            self.composite_push(original)?;
            self.composite_push(copy)?;
        } else {
            return Err(Error {});
        };
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

    fn primitive_push(&mut self, p: Primitive) -> Result<(), Error> {
        self.stack.push(Value::P(p));
        Ok(())
    }

    fn composite_push(&mut self, c: Composite) -> Result<(), Error> {
        self.stack.push(Value::C(c));
        Ok(())
    }

    fn binding_push(&mut self, name: String, val: Value) -> Result<(), Error> {
        // FIXME(jwall): Error if the symbol already exists.
        self.symbols.insert(name, val);
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
            (Value::P(Primitive::Int(i)), Value::P(Primitive::Int(ii))) => Primitive::Int(i * ii),
            (Value::P(Primitive::Float(f)), Value::P(Primitive::Float(ff))) => {
                Primitive::Float(f * ff)
            }
            _ => return Err(Error {}),
        })
    }

    fn div(&self, left: Value, right: Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (Value::P(Primitive::Int(i)), Value::P(Primitive::Int(ii))) => Primitive::Int(i / ii),
            (Value::P(Primitive::Float(f)), Value::P(Primitive::Float(ff))) => {
                Primitive::Float(f / ff)
            }
            _ => return Err(Error {}),
        })
    }

    fn sub(&self, left: Value, right: Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (Value::P(Primitive::Int(i)), Value::P(Primitive::Int(ii))) => Primitive::Int(i - ii),
            (Value::P(Primitive::Float(f)), Value::P(Primitive::Float(ff))) => {
                Primitive::Float(f - ff)
            }
            _ => return Err(Error {}),
        })
    }

    fn add(&self, left: Value, right: Value) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (Value::P(Primitive::Int(i)), Value::P(Primitive::Int(ii))) => Primitive::Int(i + ii),
            (Value::P(Primitive::Float(f)), Value::P(Primitive::Float(ff))) => {
                Primitive::Float(f + ff)
            }
            (Value::P(Primitive::Str(s)), Value::P(Primitive::Str(ss))) => {
                let mut ns = String::new();
                ns.push_str(&s);
                ns.push_str(&ss);
                Primitive::Str(ns)
            }
            _ => return Err(Error {}),
        })
    }
}

#[cfg(test)]
mod test;
