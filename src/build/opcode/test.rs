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

use super::scope::Stack;
use super::Composite::{List, Tuple};
use super::Op::{
    Add, Bang, Bind, Cp, DeRef, Div, Element, Equal, FCall, Field, Func, InitList, InitThunk,
    InitTuple, Jump, JumpIfFalse, JumpIfNotEqual, JumpIfTrue, Module, Mul, Noop, Return, Sub, Sym,
    Val,
};
use super::Primitive::{Bool, Float, Int, Str};
use super::Value::{C, P, T};
use super::VM;

macro_rules! assert_cases {
    (__impl__ $cases:expr) => {
        for case in $cases.drain(0..) {
            let mut vm = VM::new(&case.0);
            vm.run().unwrap();
            assert_eq!(dbg!(vm.pop()).unwrap(), case.1);
        }
    };

    (($input:expr, $result:expr), $($tok:tt)*) => {
        assert_cases!(__impl__ vec![($input, $result), $($tok)*])
    };

    ($($input:expr => $result:expr, )* ) => {
        assert_cases!(
            $(($input, $result),)*
        )
    }
}

#[test]
fn test_math_ops() {
    assert_cases!(
        // 1+1;
        vec![Val(Int(1)), Val(Int(1)), Add] => P(Int(2)),
        // 1-1;
        vec![Val(Int(1)), Val(Int(1)), Sub] => P(Int(0)),
        // 2*2;
        vec![Val(Int(2)), Val(Int(2)), Mul] => P(Int(4)),
        // 6/3;
        vec![Val(Int(2)), Val(Int(6)), Div] => P(Int(3)),
        // 1.0+1.0;
        vec![Val(Float(1.0)), Val(Float(1.0)), Add] => P(Float(2.0)),
        // 1.0-1.0;
        vec![Val(Float(1.0)), Val(Float(1.0)), Sub] => P(Float(0.0)),
        // 2.0*2.0;
       vec![Val(Float(2.0)), Val(Float(2.0)), Mul] => P(Float(4.0)),
        // 6.0/3.0;
        vec![Val(Float(2.0)), Val(Float(6.0)), Div] => P(Float(3.0)),
        // string concatenation
        vec![Val(Str("bar".to_owned())), Val(Str("foo".to_owned())), Add] => P(Str("foobar".to_owned())),
        // Composite operations
        vec![
            Val(Int(1)),
            Val(Int(1)),
            Add, // 1 + 1
            Val(Int(1)),
            Add, // 2 + 1
            Val(Int(1)),
            Add, // 3 + 1
        ] => P(Int(4)),
    );
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
        let mut vm = VM::new(&case.0);
        vm.run().unwrap();
        let (name, result) = case.1;
        let v = vm.get_binding(name).unwrap();
        assert_eq!(&result, v);
    }
}

#[test]
fn test_list_ops() {
    assert_cases!(
        vec![InitList] => C(List(Vec::new())),
        vec![InitList, Val(Int(1)), Element] => C(List(vec![P(Int(1))])),
        vec![InitList, Val(Int(2)), Element, Val(Int(1)), Element] => C(List(vec![P(Int(2)), P(Int(1))])),
        vec![
            InitList,
            Val(Int(1)),
            Element,
            Val(Int(1)),
            Val(Int(1)),
            Add,
            Element,
        ] => C(List(vec![P(Int(1)), P(Int(2))])),
    );
}

#[test]
fn test_tuple_ops() {
    assert_cases!(
        vec![InitTuple] => C(Tuple(Vec::new())),
        vec![InitTuple, Val(Str("foo".to_owned())), Val(Int(1)), Field] => C(Tuple(vec![("foo".to_owned(), P(Int(1)))])),
        vec![
            InitTuple,
            Sym("bar".to_owned()),
            Val(Str("quux".to_owned())),
            Field,
            Val(Str("foo".to_owned())),
            Val(Int(1)),
            Field,
        ] => C(Tuple(vec![
            ("bar".to_owned(), P(Str("quux".to_owned()))),
            ("foo".to_owned(), P(Int(1))),
        ])),
        vec![
            InitTuple,
            Sym("bar".to_owned()),
            Val(Str("quux".to_owned())),
            Field,
            Val(Str("foo".to_owned())),
            Val(Int(1)),
            Field,
            Val(Str("foo".to_owned())),
            Val(Int(2)),
            Field,
        ] => C(Tuple(vec![
            ("bar".to_owned(), P(Str("quux".to_owned()))),
            ("foo".to_owned(), P(Int(2))),
        ])),
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
            Val(Str("foo".to_owned())),
            Val(Int(2)),
            Field,
        ] => C(Tuple(vec![
            ("bar".to_owned(), P(Str("quux".to_owned()))),
            ("foo".to_owned(), P(Int(2))),
        ])),
        vec![
            InitTuple, // Override tuple
            Val(Str("foo".to_owned())),
            Val(Int(2)),
            Field,
            InitTuple, // Target tuple
            Sym("bar".to_owned()),
            Val(Str("ux".to_owned())),
            Val(Str("qu".to_owned())),
            Add,
            Field,
            Val(Str("foo".to_owned())),
            Val(Int(1)),
            Field,
            Cp, // Do the tuple copy operation
        ] => C(Tuple(vec![
            ("bar".to_owned(), P(Str("quux".to_owned()))),
            ("foo".to_owned(), P(Int(2))),
        ])),
    );
}

#[test]
fn test_jump_ops() {
    assert_cases!(
        vec![Jump(1), Val(Int(1)), Noop, Val(Int(1))] => P(Int(1)),
    );
}

#[test]
fn test_equality_ops() {
    assert_cases![
        vec![
            Val(Str("foo".to_owned())),
            Val(Str("foo".to_owned())),
            Equal,
        ] => P(Bool(true)),
        vec![
            Val(Str("bar".to_owned())),
            Val(Str("foo".to_owned())),
            Equal,
        ] => P(Bool(false)),
        vec![Val(Int(1)), Val(Int(1)), Equal] => P(Bool(true)),
        vec![Val(Int(1)), Val(Int(2)), Equal] => P(Bool(false)),
        vec![Val(Bool(true)), Val(Bool(true)), Equal] => P(Bool(true)),
        vec![Val(Bool(false)), Val(Bool(false)), Equal] => P(Bool(true)),
        vec![Val(Bool(true)), Val(Bool(false)), Equal] => P(Bool(false)),
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
        ] => P(Bool(true)),
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
        ] => P(Bool(false)),
        vec![
            InitList,
            Val(Str("foo".to_owned())),
            Element,
            InitList,
            Val(Str("foo".to_owned())),
            Element,
            Equal,
        ] => P(Bool(true)),
        vec![
            InitList,
            Val(Str("foo".to_owned())),
            Element,
            InitList,
            Val(Str("bar".to_owned())),
            Element,
            Equal,
        ] => P(Bool(false)),
    ];
}

#[test]
fn test_conditional_jump_ops() {
    assert_cases![
        vec![
            Val(Bool(false)),
            JumpIfTrue(2),
            Val(Bool(true)),
            JumpIfTrue(2),
            Val(Int(1)),
            Jump(1),
            Val(Int(2)),
            Noop,
        ] => P(Int(2)),
        vec![
            Val(Bool(true)),
            JumpIfTrue(2),
            Val(Bool(false)),
            JumpIfTrue(2),
            Val(Int(1)),
            Jump(1),
            Val(Int(2)),
            Noop,
        ] => P(Int(1)),
        vec![
            Val(Int(1)),
            Val(Int(1)),
            Equal,
            JumpIfTrue(2),
            Val(Bool(false)),
            JumpIfTrue(2),
            Val(Int(1)),
            Jump(1),
            Val(Int(2)),
            Noop,
        ] => P(Int(1)),
        vec![
            Val(Int(1)),
            Val(Int(2)),
            Equal,
            JumpIfFalse(2),
            Val(Bool(false)),
            JumpIfTrue(2),
            Val(Int(1)),
            Jump(1),
            Val(Int(2)),
            Noop,
        ] => P(Int(1)),
    ];
}

#[test]
fn test_function_definition_and_call() {
    assert_cases![
        vec![
            Sym("f".to_owned()),     // 0
            InitList,                // 1
            Sym("arg".to_owned()),   // 2
            Element,                 // 3
            Func(6),                 // 4
            DeRef("arg".to_owned()), // 5
            Return,                  // 6
            Bind,                    // 7
            Val(Int(1)),             // 8
            DeRef("f".to_owned()),   // 9
            FCall,                   // 10
        ] => P(Int(1)),
        vec![
            Sym("closed".to_owned()),   // 0
            Val(Int(1)),                // 1
            Bind,                       // 2
            Sym("f".to_owned()),        // 3
            InitList,                   // 4
            Sym("arg".to_owned()),      // 5
            Element,                    // 6
            Func(11),                   // 7
            DeRef("arg".to_owned()),    // 8
            DeRef("closed".to_owned()), // 9
            Add,                        // 10
            Return,                     // 11
            Bind,                       // 12
            Val(Int(1)),                // 13
            DeRef("f".to_owned()),      // 14
            FCall,                      // 16
        ] => P(Int(2)),
    ];
}

#[test]
fn test_module_call() {
    assert_cases![
        vec![
            InitTuple,               // 0 // override tuple
            Sym("one".to_owned()),   // 1
            Val(Int(11)),            // 2
            Field,                   // 3
            Sym("m".to_owned()),     // 4 // binding name for module
            InitTuple,               // 5 // Module tuple bindings
            Sym("one".to_owned()),   // 6
            Val(Int(1)),             // 7
            Field,                   // 8
            Sym("two".to_owned()),   // 9
            Val(Int(2)),             // 10
            Field,                   // 11
            Module(17),              // 12 // Module definition
            Bind,                    // 13
            Sym("foo".to_owned()),   // 14
            DeRef("mod".to_owned()), // 15
            Bind,                    // 16 // bind mod tuple to foo
            Return,                  // 17 // end the module
            Bind,                    // 18 // bind module to the binding name
            DeRef("m".to_owned()),   // 19
            Cp,                      // 20
        ] => C(Tuple(vec![(
            "foo".to_owned(),
            C(Tuple(vec![
                ("one".to_owned(), P(Int(11))),
                ("two".to_owned(), P(Int(2))),
            ])),
        )])),
        vec![
            InitTuple,               // 0 // override tuple
            Sym("one".to_owned()),   // 1
            Val(Int(11)),            // 2
            Field,                   // 3
            Sym("m".to_owned()),     // 4 // binding name for module
            InitTuple,               // 5 // Module tuple bindings
            Sym("one".to_owned()),   // 6
            Val(Int(1)),             // 7
            Field,                   // 8
            Sym("two".to_owned()),   // 9
            Val(Int(2)),             // 10
            Field,                   // 11
            InitThunk(2),            // 12 // Module Return expression
            Val(Int(1)),             // 13
            Return,                  // 14
            Module(20),              // 15 // Module definition
            Bind,                    // 16
            Sym("foo".to_owned()),   // 17
            DeRef("mod".to_owned()), // 18
            Bind,                    // 19 // bind mod tuple to foo
            Return,                  // 20 // end the module
            Bind,                    // 21 // bind module to the binding name
            DeRef("m".to_owned()),   // 22
            Cp,                      // 23
        ] => P(Int(1)),
    ];
}

#[test]
fn test_select_short_circuit() {
    assert_cases![
        vec![
            Sym("field".to_owned()),              // 0 // search field
            Sym("not_field".to_owned()),          // 1 // first field to compare
            JumpIfNotEqual(2),                    // 2
            Val(Str("not our value".to_owned())), // 3
            Jump(4),                              // 4
            Sym("field".to_owned()),              // 5 // second field to compare
            JumpIfNotEqual(2),                    // 6
            Val(Int(1)),                          // 7
            Jump(1),                              // 8
            Bang,                                 // 9
        ] => P(Int(1)),
    ];
}

#[test]
fn test_scope_stacks() {
    let mut stack = Stack::new();
    stack.add("one".to_owned(), P(Int(1)));
    let mut val = stack.get("one").unwrap();
    assert_eq!(val, &P(Int(1)));
    stack.push();
    assert!(stack.get("one").is_none());
    stack.to_open();
    val = stack.get("one").unwrap();
    assert_eq!(val, &P(Int(1)));
}
