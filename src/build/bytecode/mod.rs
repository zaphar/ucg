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
use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::build::ir::Val;

#[derive(Debug, PartialEq, Clone)]
pub enum Primitive {
    // Primitive Types
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Empty,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Composite {
    List(Vec<Value>),
    Tuple(Vec<(String, Value)>),
    //Thunk(Frame),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    // Binding names.
    S(String),
    // Primitive Types
    P(Primitive),
    // Composite Types.
    C(Composite),
}

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
    // Primitive Types ops
    Val(Primitive),
    // A bareword for use in bindings or lookups
    Sym(String),
    // Complex Type ops
    InitTuple,
    FIELD,
    InitList,
    Element,
    // Operations
    Cp,
    // Push a new frame on the FrameStack
    InitFunc,
    InitMod,
    EndFrame,
    // - Call
    // Functional operations
    // - Map
    // - Filter
    // - Reduce
}

pub struct Heap {}

#[derive(Debug)]
pub struct Error {}

/// The type of Frame environment this is
#[derive(Debug, PartialEq, Clone)]
pub enum FrameType {
    Lib,
    Func,
    Module,
}

#[derive(Debug, PartialEq)]
pub struct Frame {
    names: BTreeMap<String, Value>,
    stack: Vec<Value>,
    typ: FrameType,
}

/// Frames represent a functional computation environment on the stack.
/// Frames in the UCG interpreter are hermetic. They can't see their parent.
impl Frame {
    fn pop(&mut self) -> Result<Value, Error> {
        match self.stack.pop() {
            Some(p) => Ok(p),
            None => Err(Error {}),
        }
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn push_primitive(&mut self, p: Primitive) {
        self.stack.push(Value::P(p));
    }

    fn push_composite(&mut self, c: Composite) {
        self.stack.push(Value::C(c));
    }

    fn push_binding(&mut self, name: String, val: Value) {
        self.names.insert(name, val);
    }

    fn get_binding(&mut self, name: &str) -> Result<&Value, Error> {
        self.names.get(name).ok_or(Error {})
    }
}

pub struct VM {
    stack: Vec<Frame>,
}

impl VM {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn run<I>(&mut self, op_stream: I) -> Result<(), Error>
    where
        I: Iterator<Item = Op>,
    {
        // Init our first stack frame
        self.push_stack(FrameType::Lib);
        for op in op_stream {
            match op {
                Op::Val(p) => {
                    self.primitive_push(p)?;
                }
                Op::Sym(s) => {
                    self.push(Value::S(s))?;
                }
                Op::Add => {
                    // Adds the previous two items in the stack.
                    let left = self.pop()?;
                    let right = self.pop()?;
                    // Then pushes the result onto the stack.
                    self.primitive_push(self.add(left, right)?)?;
                }
                Op::Sub => {
                    // Subtracts the previous two items in the stack.
                    let left = self.pop()?;
                    let right = self.pop()?;
                    // Then pushes the result onto the stack.
                    self.primitive_push(self.sub(left, right)?)?;
                }
                Op::Mul => {
                    // Multiplies the previous two items in the stack.
                    let left = self.pop()?;
                    let right = self.pop()?;
                    // Then pushes the result onto the stack.
                    self.primitive_push(self.mul(left, right)?)?;
                }
                Op::Div => {
                    // Divides the previous two items in the stack.
                    let left = self.pop()?;
                    let right = self.pop()?;
                    // Then pushes the result onto the stack.
                    self.primitive_push(self.div(left, right)?)?;
                }
                Op::Bind => {
                    // pop val off stack.
                    let val = self.pop()?;
                    // pop name off stack.
                    let name = self.pop()?;
                    if let Value::S(name) = name {
                        self.binding_push(name, val)?;
                    } else {
                        return Err(Error {});
                    }
                }
                Op::InitList => {
                    // Add a Composite list value to the stack
                    self.composite_push(Composite::List(Vec::new()))?;
                }
                Op::InitTuple => {
                    // Add a composite tuple value to the stack
                    self.composite_push(Composite::Tuple(Vec::new()))?;
                }
                Op::FIELD => {
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
                    if let Value::C(Composite::Tuple(mut flds)) = tpl {
                        // add name and value to tuple
                        self.merge_field_into_tuple(&mut flds, name, val)?;
                        // place composite tuple back on stack
                        self.composite_push(Composite::Tuple(flds))?;
                    } else {
                        return Err(Error {});
                    };
                }
                Op::Element => {
                    // get element from stack.
                    let val = self.pop()?;
                    // get next value. It should be a Composite list.
                    let tpl = self.pop()?;
                    if let Value::C(Composite::List(mut elems)) = tpl {
                        // add value to list
                        elems.push(val);
                        // Add that value to the list and put list back on stack.
                        self.composite_push(Composite::List(elems))?;
                    } else {
                        return Err(Error {});
                    };
                }
                Op::Cp => {
                    // TODO Use Cow pointers for this?
                    // get next value. It should be a Composite Tuple.
                    if let Value::C(Composite::Tuple(flds)) = self.pop()? {
                        // Make a copy of the original
                        let original = Composite::Tuple(flds.clone());
                        let copy = Composite::Tuple(flds);
                        // Put the original on the Stack as well as the copy
                        self.composite_push(original)?;
                        self.composite_push(copy)?;
                    } else {
                        return Err(Error {});
                    };
                }
                Op::InitFunc => {
                    self.push_stack(FrameType::Func);
                }
                Op::InitMod => {
                    self.push_stack(FrameType::Module);
                }
                Op::EndFrame => {
                    // TODO(jwall): We probably want to push this frame onto the stack
                    // somehow.
                    self.pop_stack();
                }
                Op::Pop => {
                    self.pop()?;
                }
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

    fn push_stack(&mut self, typ: FrameType) {
        self.stack.push(Frame {
            names: BTreeMap::new(),
            stack: Vec::new(),
            typ: typ,
        });
    }

    fn pop_stack(&mut self) -> Option<Frame> {
        self.stack.pop()
    }

    fn to_val(p: Value) -> Result<Val, Error> {
        Ok(match p {
            Value::P(Primitive::Int(i)) => Val::Int(i),
            Value::P(Primitive::Float(f)) => Val::Float(f),
            Value::P(Primitive::Str(s)) => Val::Str(s),
            Value::P(Primitive::Bool(b)) => Val::Boolean(b),
            Value::P(Primitive::Empty) => Val::Empty,
            Value::C(Composite::List(mut elems)) => {
                let mut mapped = Vec::with_capacity(elems.len());
                for val in elems.drain(0..) {
                    mapped.push(Rc::new(Self::to_val(val)?));
                }
                Val::List(mapped)
            }
            Value::C(Composite::Tuple(mut flds)) => {
                let mut mapped = Vec::with_capacity(flds.len());
                for (name, val) in flds.drain(0..) {
                    mapped.push((name, Rc::new(Self::to_val(val)?)));
                }
                Val::Tuple(mapped)
            }
            Value::S(_) => return Err(Error {}),
            //Value::C(Composite::Thunk(_)) => {
            //    // TODO(jwall): This is either a function or a Module
            //    Val::Empty
            //}
        })
    }

    fn push(&mut self, p: Value) -> Result<(), Error> {
        match self.stack.first_mut() {
            Some(f) => return Ok(f.push(p)),
            None => return Err(Error {}),
        };
    }

    fn primitive_push(&mut self, p: Primitive) -> Result<(), Error> {
        match self.stack.first_mut() {
            Some(f) => return Ok(f.push_primitive(p)),
            None => return Err(Error {}),
        };
    }

    fn composite_push(&mut self, c: Composite) -> Result<(), Error> {
        match self.stack.first_mut() {
            Some(f) => return Ok(f.push_composite(c)),
            None => return Err(Error {}),
        };
    }

    fn binding_push(&mut self, name: String, val: Value) -> Result<(), Error> {
        match self.stack.first_mut() {
            Some(f) => return Ok(f.push_binding(name, val)),
            None => return Err(Error {}),
        }
    }

    pub fn get_binding(&mut self, name: &str) -> Result<&Value, Error> {
        match self.stack.first_mut() {
            Some(f) => return Ok(f.get_binding(name)?),
            None => return Err(Error {}),
        }
    }

    fn pop(&mut self) -> Result<Value, Error> {
        match self.stack.first_mut() {
            Some(f) => f.pop(),
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
