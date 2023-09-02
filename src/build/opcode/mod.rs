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
use std::rc::Rc;

mod cache;
mod debug;
mod display;
pub mod environment;
#[macro_use]
pub mod error;
mod convert;
pub mod pointer;
mod runtime;
pub mod scope;
pub mod translate;
mod vm;

pub use environment::Environment;
pub use error::Error;
pub use vm::VM;

use crate::ast::{CastType, Position};
use pointer::OpPointer;
use scope::Stack;

#[derive(Debug, PartialEq, Clone)]
pub enum Primitive {
    // Primitive Types
    Int(i64),
    Float(f64),
    Str(Rc<str>),
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
            C(List(_, _)) => "List",
            C(Tuple(_, _)) => "Tuple",
            F(_) => "Func",
            M(_) => "Func",
            T(_) => "Expression",
            S(_) => "Symbol",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Composite {
    List(Vec<Rc<Value>>, Vec<Position>),
    Tuple(Vec<(Rc<str>, Rc<Value>)>, Vec<(Position, Position)>),
}

use Composite::{List, Tuple};

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    ptr: OpPointer,
    bindings: Vec<Rc<str>>,
    snapshot: Stack,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    ptr: OpPointer,
    result_ptr: Option<usize>,
    flds: Vec<(Rc<str>, Rc<Value>)>,
    flds_pos_list: Vec<(Position, Position)>,
    pkg_ptr: Option<OpPointer>,
}

#[derive(Clone)]
pub enum Value {
    // Binding names.
    S(Rc<str>),
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
    // Primitive casts
    Cast(CastType),
    // A bareword for use in bindings or lookups
    Sym(Rc<str>),
    // Reference a binding on the heap
    DeRef(Rc<str>),
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
    Exist,
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

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (P(left), P(right)) => left == right,
            (C(List(left, _)), C(List(right, _))) => left == right,
            (C(Tuple(left, _)), C(Tuple(right, _))) => {
                if left.len() != right.len() {
                    return false;
                }
                for (ref lk, ref lv) in left.iter() {
                    let mut found = false;
                    for (ref rk, ref rv) in right.iter() {
                        if lk == rk {
                            found = true;
                            if lv != rv {
                                return false;
                            }
                        }
                    }
                    if !found {
                        return false;
                    }
                }
                true
            }
            (F(left), F(right)) => left == right,
            (M(left), M(right)) => left == right,
            (T(_), T(_)) | (S(_), S(_)) => false,
            (_, _) => false,
        }
    }
}
