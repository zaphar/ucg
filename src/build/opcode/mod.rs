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
use std::convert::{TryFrom, TryInto};
use std::rc::Rc;

mod cache;
mod debug;
pub mod environment;
#[macro_use]
mod error;
pub mod pointer;
mod runtime;
pub mod scope;
pub mod translate;
mod vm;

pub use environment::Environment;
pub use error::Error;
pub use vm::VM;

use pointer::OpPointer;
use scope::Stack;

use crate::ast::Position;

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

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            P(Int(_)) => "Int",
            P(Float(_)) => "Float",
            P(Str(_)) => "String",
            P(Bool(_)) => "Bool",
            P(Empty) => "NULL",
            C(List(_)) => "List",
            C(Tuple(_)) => "Tuple",
            F(_) => "Func",
            M(_) => "Func",
            T(_) => "Expression",
            S(_) => "Symbol",
        }
    }
}

impl From<&Primitive> for String {
    fn from(p: &Primitive) -> Self {
        match p {
            Int(i) => format!("{}", i),
            Float(f) => format!("{}", f),
            Str(s) => format!("{}", s),
            Bool(b) => format!("{}", b),
            Empty => "NULL".to_owned(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Composite {
    List(Vec<Rc<Value>>),
    Tuple(Vec<(String, Rc<Value>)>),
}

use Composite::{List, Tuple};

impl From<&Composite> for String {
    fn from(c: &Composite) -> Self {
        let mut buf = String::new();
        match c {
            &List(ref elems) => {
                buf.push_str("[");
                for e in elems.iter() {
                    let val: String = e.as_ref().into();
                    buf.push_str(&val);
                    buf.push_str(",");
                }
                buf.push_str("]");
            }
            &Tuple(ref flds) => {
                buf.push_str("{");
                for &(ref k, ref v) in flds.iter() {
                    buf.push_str(&k);
                    buf.push_str(" = ");
                    let val: String = v.as_ref().into();
                    buf.push_str(&val);
                    buf.push_str(",");
                }
                buf.push_str("}");
            }
        }
        buf
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    ptr: OpPointer,
    bindings: Vec<String>,
    snapshot: Stack,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    ptr: OpPointer,
    result_ptr: Option<usize>,
    flds: Vec<(String, Rc<Value>)>,
    pkg_ptr: Option<OpPointer>,
}

#[derive(PartialEq, Clone)]
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

impl From<&Value> for String {
    fn from(v: &Value) -> Self {
        match v {
            &S(ref s) => s.clone(),
            &P(ref p) => p.into(),
            &C(ref c) => c.into(),
            &T(_) => "<Thunk>".to_owned(),
            &F(_) => "<Func>".to_owned(),
            &M(_) => "<Module>".to_owned(),
        }
    }
}

use Value::{C, F, M, P, S, T};

#[derive(Debug, PartialEq, Clone)]
pub enum Hook {
    Map,
    Include,
    Filter,
    Reduce,
    Import,
    Out,
    Assert,
    Convert,
    Regex,
    Range,
    Trace(Position),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    // Stack and Name manipulation.
    Bind,     // Bind a Val to a name in the heap
    BindOver, // Overwrite a value in the heap
    Pop,      // Pop a Value off the value stack and discard it.
    NewScope(i32),
    // Math ops
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    // Comparison Ops
    Equal,
    Gt,
    Lt,
    GtEq,
    LtEq,
    // Not,
    Not,
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
    And(i32),
    Or(i32),
    // Spacer operation, Does nothing.
    Index,     // indexing operation
    SafeIndex, // Safe indexing operation. Does Null Coelescing
    Noop,
    // Pending Computation
    InitThunk(i32), // Basically just used for module return expressions
    Module(i32),
    Func(i32),
    Return,
    // Calls
    FCall,
    // TypeSystem
    Typ,
    // Runtime hooks
    Runtime(Hook),
    Render,
    // The self lookup for tuples.
    PushSelf,
    PopSelf,
}

use super::ir::Val;

impl From<Rc<Value>> for Val {
    fn from(val: Rc<Value>) -> Val {
        val.as_ref().into()
    }
}

impl From<Value> for Val {
    fn from(val: Value) -> Val {
        (&val).into()
    }
}

impl From<&Value> for Val {
    fn from(val: &Value) -> Val {
        match val {
            P(Int(i)) => Val::Int(*i),
            P(Float(f)) => Val::Float(*f),
            P(Str(s)) => Val::Str(s.clone()),
            P(Bool(b)) => Val::Boolean(*b),
            C(Tuple(fs)) => {
                let mut flds = Vec::new();
                for &(ref k, ref v) in fs.iter() {
                    let v = v.clone();
                    flds.push((k.clone(), Rc::new(v.into())));
                }
                Val::Tuple(flds)
            }
            C(List(elems)) => {
                let mut els = Vec::new();
                for e in elems.iter() {
                    let e = e.clone();
                    els.push(Rc::new(e.into()));
                }
                Val::List(els)
            }
            S(_) | F(_) | M(_) | T(_) | P(Empty) => Val::Empty,
        }
    }
}

impl From<Rc<Val>> for Value {
    fn from(val: Rc<Val>) -> Self {
        val.as_ref().into()
    }
}

impl From<Val> for Value {
    fn from(val: Val) -> Self {
        (&val).into()
    }
}

impl From<&Val> for Value {
    fn from(val: &Val) -> Self {
        match val {
            Val::Int(i) => P(Int(*i)),
            Val::Float(f) => P(Float(*f)),
            Val::Boolean(b) => P(Bool(*b)),
            Val::Str(s) => P(Str(s.clone())),
            Val::Empty => P(Empty),
            Val::List(els) => {
                let mut lst = Vec::new();
                for e in els.iter() {
                    let e = e.clone();
                    lst.push(Rc::new(e.into()));
                }
                C(List(lst))
            }
            Val::Tuple(flds) => {
                let mut field_list = Vec::new();
                for &(ref key, ref val) in flds.iter() {
                    let val = val.clone();
                    field_list.push((key.clone(), Rc::new(val.into())));
                }
                C(Tuple(field_list))
            }
            Val::Env(flds) => {
                let mut field_list = Vec::new();
                for &(ref key, ref val) in flds.iter() {
                    field_list.push((key.clone(), Rc::new(P(Str(val.clone())))));
                }
                C(Tuple(field_list))
            }
        }
    }
}

#[cfg(test)]
mod test;
