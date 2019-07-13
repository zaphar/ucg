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
mod vm;

pub use vm::VM;

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

#[derive(Debug, PartialEq, Clone)]
pub enum Composite {
    List(Vec<Value>),
    Tuple(Vec<(String, Value)>),
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
    Index, // indexing operation
    Noop,
    // Pending Computation
    InitThunk(i32), // Basically just used for module return expressions
    Module(usize),
    Func(usize),
    Return,
    // Calls
    FCall,
    // Runtime hooks
    // - Map,
    // - Filter,
    // - Reduce,
    // - Import,
    // - Out,
    // - Assert,
    // - Convert,
}

#[derive(Debug)]
pub struct Error {}

#[cfg(test)]
mod test;
