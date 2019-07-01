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

use super::Composite::{List, Tuple};
use super::Op::{
    Add, Bind, Cp, Div, Element, Equal, Field, InitList, InitThunk, InitTuple, Jump, JumpIfTrue,
    Mul, Noop, Sub, Sym, Val,
};
use super::Primitive::{Bool, Float, Int, Str};
use super::Value::{C, P, T};
use super::VM;

#[test]
fn test_math_ops() {
    let mut cases = vec![
        // 1+1;
        (vec![Val(Int(1)), Val(Int(1)), Add], P(Int(2))),
        // 1-1;
        (vec![Val(Int(1)), Val(Int(1)), Sub], P(Int(0))),
        // 2*2;
        (vec![Val(Int(2)), Val(Int(2)), Mul], P(Int(4))),
        // 6/3;
        (vec![Val(Int(2)), Val(Int(6)), Div], P(Int(3))),
        // 1.0+1.0;
        (vec![Val(Float(1.0)), Val(Float(1.0)), Add], P(Float(2.0))),
        // 1.0-1.0;
        (vec![Val(Float(1.0)), Val(Float(1.0)), Sub], P(Float(0.0))),
        // 2.0*2.0;
        (vec![Val(Float(2.0)), Val(Float(2.0)), Mul], P(Float(4.0))),
        // 6.0/3.0;
        (vec![Val(Float(2.0)), Val(Float(6.0)), Div], P(Float(3.0))),
        // string concatenation
        (
            vec![Val(Str("bar".to_owned())), Val(Str("foo".to_owned())), Add],
            P(Str("foobar".to_owned())),
        ),
        // Composite operations
        (
            vec![
                Val(Int(1)),
                Val(Int(1)),
                Add, // 1 + 1
                Val(Int(1)),
                Add, // 2 + 1
                Val(Int(1)),
                Add, // 3 + 1
            ],
            P(Int(4)),
        ),
    ];
    for case in cases.drain(0..) {
        let mut vm = VM::new(case.0);
        vm.run().unwrap();
        assert_eq!(vm.pop().unwrap(), case.1);
    }
}

#[test]
fn test_bind_op() {
    let mut cases = vec![(
        vec![Sym("foo".to_owned()), Val(Int(1)), Bind],
        ("foo", P(Int(1))),
        vec![Sym("foo".to_owned()), Val(Int(1)), Val(Int(1)), Add, Bind],
        ("foo", P(Int(2))),
    )];

    for case in cases.drain(0..) {
        let mut vm = VM::new(case.0);
        vm.run().unwrap();
        let (name, result) = case.1;
        let v = vm.get_binding(name).unwrap();
        assert_eq!(&result, v);
    }
}

#[test]
fn test_list_ops() {
    let mut cases = vec![
        (vec![InitList], C(List(Vec::new()))),
        (
            vec![InitList, Val(Int(1)), Element],
            C(List(vec![P(Int(1))])),
        ),
        (
            vec![InitList, Val(Int(2)), Element, Val(Int(1)), Element],
            C(List(vec![P(Int(2)), P(Int(1))])),
        ),
        (
            vec![
                InitList,
                Val(Int(1)),
                Element,
                Val(Int(1)),
                Val(Int(1)),
                Add,
                Element,
            ],
            C(List(vec![P(Int(1)), P(Int(2))])),
        ),
    ];
    for case in cases.drain(0..) {
        let mut vm = VM::new(case.0);
        vm.run().unwrap();
        assert_eq!(vm.pop().unwrap(), case.1);
    }
}

#[test]
fn test_tuple_ops() {
    let mut cases = vec![
        (vec![InitTuple], C(Tuple(Vec::new()))),
        (
            vec![InitTuple, Val(Str("foo".to_owned())), Val(Int(1)), Field],
            C(Tuple(vec![("foo".to_owned(), P(Int(1)))])),
        ),
        (
            vec![
                InitTuple,
                Sym("bar".to_owned()),
                Val(Str("quux".to_owned())),
                Field,
                Val(Str("foo".to_owned())),
                Val(Int(1)),
                Field,
            ],
            C(Tuple(vec![
                ("bar".to_owned(), P(Str("quux".to_owned()))),
                ("foo".to_owned(), P(Int(1))),
            ])),
        ),
        (
            vec![
                InitTuple,
                Sym("bar".to_owned()),
                Val(Str("quux".to_owned())),
                Field,
                Val(Str("foo".to_owned())),
                Val(Int(1)),
                Field,
                Cp,
                Val(Str("foo".to_owned())),
                Val(Int(2)),
                Field,
            ],
            C(Tuple(vec![
                ("bar".to_owned(), P(Str("quux".to_owned()))),
                ("foo".to_owned(), P(Int(2))),
            ])),
        ),
        (
            vec![
                InitTuple,
                Sym("bar".to_owned()),
                Val(Str("ux".to_owned())),
                Val(Str("qu".to_owned())),
                Add,
                Field,
                Val(Str("foo".to_owned())),
                Val(Int(1)),
                Field,
                Cp,
                Val(Str("foo".to_owned())),
                Val(Int(2)),
                Field,
            ],
            C(Tuple(vec![
                ("bar".to_owned(), P(Str("quux".to_owned()))),
                ("foo".to_owned(), P(Int(2))),
            ])),
        ),
    ];
    for case in cases.drain(0..) {
        let mut vm = VM::new(case.0);
        vm.run().unwrap();
        assert_eq!(vm.pop().unwrap(), case.1);
    }
}

#[test]
fn test_jump_ops() {
    let mut cases = vec![
        (vec![InitThunk(1), Val(Int(1)), Noop], T(0)),
        (vec![Jump(1), Val(Int(1)), Noop, Val(Int(1))], P(Int(1))),
    ];
    for case in cases.drain(0..) {
        let mut vm = VM::new(case.0);
        vm.run().unwrap();
        assert_eq!(vm.pop().unwrap(), case.1);
    }
}

#[test]
fn test_equality_ops() {
    let mut cases = vec![
        (
            vec![
                Val(Str("foo".to_owned())),
                Val(Str("foo".to_owned())),
                Equal,
            ],
            P(Bool(true)),
        ),
        (
            vec![
                Val(Str("bar".to_owned())),
                Val(Str("foo".to_owned())),
                Equal,
            ],
            P(Bool(false)),
        ),
        (vec![Val(Int(1)), Val(Int(1)), Equal], P(Bool(true))),
        (vec![Val(Int(1)), Val(Int(2)), Equal], P(Bool(false))),
        (vec![Val(Bool(true)), Val(Bool(true)), Equal], P(Bool(true))),
        (
            vec![Val(Bool(false)), Val(Bool(false)), Equal],
            P(Bool(true)),
        ),
        (
            vec![Val(Bool(true)), Val(Bool(false)), Equal],
            P(Bool(false)),
        ),
        (
            vec![
                InitTuple,
                Val(Str("foo".to_owned())),
                Val(Int(1)),
                Field,
                InitTuple,
                Val(Str("foo".to_owned())),
                Val(Int(1)),
                Field,
                Equal,
            ],
            P(Bool(true)),
        ),
        (
            vec![
                InitTuple,
                Val(Str("foo".to_owned())),
                Val(Int(1)),
                Field,
                InitTuple,
                Val(Str("bar".to_owned())),
                Val(Int(1)),
                Field,
                Equal,
            ],
            P(Bool(false)),
        ),
        (
            vec![
                InitList,
                Val(Str("foo".to_owned())),
                Element,
                InitList,
                Val(Str("foo".to_owned())),
                Element,
                Equal,
            ],
            P(Bool(true)),
        ),
        (
            vec![
                InitList,
                Val(Str("foo".to_owned())),
                Element,
                InitList,
                Val(Str("bar".to_owned())),
                Element,
                Equal,
            ],
            P(Bool(false)),
        ),
    ];
    for case in cases.drain(0..) {
        let mut vm = VM::new(case.0);
        vm.run().unwrap();
        assert_eq!(vm.pop().unwrap(), case.1);
    }
}
