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

use super::scope::Stack;
use super::Composite::{List, Tuple};
use super::Op::{
    Add, Bang, Bind, BindOver, Cp, DeRef, Div, Element, Equal, FCall, Field, Func, Index, InitList,
    InitThunk, InitTuple, Jump, JumpIfFalse, JumpIfTrue, Module, Mul, NewScope, Noop, Pop, Render,
    Return, SelectJump, Sub, Sym, Typ, Val,
};
use super::Primitive::{Bool, Empty, Float, Int, Str};
use super::Value::{C, P};
use super::VM;

macro_rules! assert_cases {
    (__impl__ $cases:expr) => {
        for case in $cases.drain(0..) {
            let mut vm = VM::new("foo.ucg", Rc::new(case.0));
            vm.run().unwrap();
            assert_eq!(dbg!(vm.pop()).unwrap(), Rc::new(case.1));
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
fn math_ops() {
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
fn new_scopes() {
    assert_cases![
        vec![
            Sym("foo".to_owned()),
            Val(Int(1)),
            Bind,
            NewScope(5),
            Sym("foo".to_owned()),
            Val(Int(2)),
            BindOver,
            DeRef("foo".to_owned()),
            Return,
        ] => P(Int(2)),
        vec![
            Sym("bar".to_owned()),
            Val(Int(1)),
            Bind,
            NewScope(5),
            Sym("foo".to_owned()),
            Val(Int(2)),
            Bind,
            DeRef("bar".to_owned()),
            Return,
        ] => P(Int(1)),
    ];
}

#[test]
fn bind_op() {
    let mut cases = vec![(
        vec![Sym("foo".to_owned()), Val(Int(1)), Bind],
        ("foo", P(Int(1))),
        vec![Sym("foo".to_owned()), Val(Int(1)), Val(Int(1)), Add, Bind],
        ("foo", P(Int(2))),
    )];

    for case in cases.drain(0..) {
        let mut vm = VM::new("bar.ucg", Rc::new(case.0));
        vm.run().unwrap();
        let (name, result) = case.1;
        let v = vm.get_binding(name).unwrap();
        assert_eq!(&result, v.as_ref());
    }
}

#[test]
fn list_ops() {
    assert_cases!(
        vec![InitList] => C(List(Vec::new())),
        vec![InitList, Val(Int(1)), Element] => C(List(vec![Rc::new(P(Int(1)))])),
        vec![InitList, Val(Int(2)), Element, Val(Int(1)), Element] => C(List(vec![Rc::new(P(Int(2))), Rc::new(P(Int(1)))])),
        vec![
            InitList,
            Val(Int(1)),
            Element,
            Val(Int(1)),
            Val(Int(1)),
            Add,
            Element,
        ] => C(List(vec![Rc::new(P(Int(1))), Rc::new(P(Int(2)))])),
    );
}

#[test]
fn tuple_ops() {
    assert_cases!(
        vec![InitTuple] => C(Tuple(Vec::new())),
        vec![
            InitTuple,
            Val(Str("foo".to_owned())),
            Val(Int(1)),
            Field,
        ] => C(Tuple(vec![
            ("foo".to_owned(), Rc::new(P(Int(1)))),
        ])),
        vec![
            InitTuple,
            Sym("bar".to_owned()),
            Val(Str("quux".to_owned())),
            Field,
            Val(Str("foo".to_owned())),
            Val(Int(1)),
            Field,
        ] => C(Tuple(vec![
            ("bar".to_owned(), Rc::new(P(Str("quux".to_owned())))),
            ("foo".to_owned(), Rc::new(P(Int(1)))),
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
            ("bar".to_owned(), Rc::new(P(Str("quux".to_owned())))),
            ("foo".to_owned(), Rc::new(P(Int(2)))),
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
            ("bar".to_owned(), Rc::new(P(Str("quux".to_owned())))),
            ("foo".to_owned(), Rc::new(P(Int(2)))),
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
            ("bar".to_owned(), Rc::new(P(Str("quux".to_owned())))),
            ("foo".to_owned(), Rc::new(P(Int(2)))),
        ])),
    );
}

#[test]
fn jump_ops() {
    assert_cases!(
        vec![Jump(1), Val(Int(1)), Noop, Val(Int(1))] => P(Int(1)),
    );
}

#[test]
fn equality_ops() {
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
fn conditional_jump_ops() {
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
fn function_definition_and_call() {
    assert_cases![
        vec![
            Sym("f".to_owned()),     // 0
            InitList,                // 1
            Sym("arg".to_owned()),   // 2
            Element,                 // 3
            Func(2),                 // 4
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
            Func(4),                    // 7
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
fn module_call() {
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
            Module(5),               // 12 // Module body definition
            Bind,                    // 13 // bind the mod tuple
            Sym("foo".to_owned()),   // 14
            DeRef("mod".to_owned()), // 15
            Bind,                    // 16 // bind mod tuple to foo
            Return,                  // 17 // end the module
            Bind,                    // 18 // bind module to the binding name
            DeRef("m".to_owned()),   // 19
            Cp,                      // 20 // Call the module
        ] => C(Tuple(vec![
            (
                "foo".to_owned(),
                Rc::new(C(Tuple(vec![
                    ("one".to_owned(), Rc::new(P(Int(11)))),
                    ("two".to_owned(), Rc::new(P(Int(2)))),
                ])))
            ),
        ])),
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
            Module(5),              // 15 // Module definition
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
fn select_short_circuit() {
    assert_cases![
        vec![
            Sym("field".to_owned()),              // 0 // search field
            Sym("not_field".to_owned()),          // 1 // first field to compare
            SelectJump(2),                        // 2
            Val(Str("not our value".to_owned())), // 3 // expression for first field
            Jump(4),                              // 4
            Sym("field".to_owned()),              // 5 // second field to compare
            SelectJump(2),                        // 6
            Val(Int(1)),                          // 7 // expression for second field
            Jump(2),                              // 8
            Pop,                                  // 9 // pop the search field off
            Bang,                                 // 10 // default case
        ] => P(Int(1)),
        vec![
            Sym("field".to_owned()),              // 0 // search field
            Sym("not_field".to_owned()),          // 1 // first field to compare
            SelectJump(2),                        // 2
            Val(Str("not our value".to_owned())), // 3 // expression for first field
            Jump(4),                              // 4
            Sym("als not field".to_owned()),      // 5 // second field to compare
            SelectJump(2),                        // 6
            Val(Int(2)),                          // 7 // expression for second field
            Jump(1),                              // 8
            Pop,                                  // 9 // pop the search field off
            Val(Int(1)),                          // 10 // default case
        ] => P(Int(1)),
    ];
}

#[test]
fn index_operation() {
    assert_cases![
        vec![
            InitTuple,
            Sym("foo".to_owned()),
            InitTuple,
            Sym("bar".to_owned()),
            Val(Int(1)),
            Field,
            Field,
            Val(Str("foo".to_owned())),
            Index,
            Val(Str("bar".to_owned())),
            Index,
        ] => P(Int(1)),
        vec![
            InitList,
            Val(Str("foo".to_owned())),
            Element,
            Val(Str("bar".to_owned())),
            Element,
            Val(Int(0)),
            Index,
        ] => P(Str("foo".to_owned())),
        vec![
            InitTuple,
            Sym("field".to_owned()),
            InitList,
            Val(Str("foo".to_owned())),
            Element,
            Val(Str("bar".to_owned())),
            Element,
            Field,
            Val(Str("field".to_owned())),
            Index,
            Val(Int(0)),
            Index,
        ] => P(Str("foo".to_owned())),
    ];
}

#[test]
fn type_comparisons() {
    assert_cases![
        vec![
            Val(Str("foo".to_owned())),
            Typ,
        ] => P(Str("str".to_owned())),
        vec![
            Val(Int(1)),
            Typ,
        ] => P(Str("int".to_owned())),
        vec![
            Val(Float(1.0)),
            Typ,
        ] => P(Str("float".to_owned())),
        vec![
            Val(Bool(true)),
            Typ,
        ] => P(Str("bool".to_owned())),
        vec![
            Val(Empty),
            Typ,
        ] => P(Str("null".to_owned())),
        vec![
            InitTuple,
            Typ,
        ] => P(Str("tuple".to_owned())),
        vec![
            InitList,
            Typ,
        ] => P(Str("list".to_owned())),
        vec![
            Val(Str("str".to_owned())),
            Val(Str("foo".to_owned())),
            Typ,
            Equal,
        ] => P(Bool(true)),
    ];
}

#[test]
fn scope_stacks() {
    let mut stack = Stack::new();
    stack.add("one".to_owned(), Rc::new(P(Int(1))));
    let mut val = stack.get("one").unwrap();
    assert_eq!(val.as_ref(), &P(Int(1)));
    stack.push();
    assert!(stack.get("one").is_none());
    stack.to_open();
    val = stack.get("one").unwrap();
    assert_eq!(val.as_ref(), &P(Int(1)));
}

use super::translate;
use crate::ast::{Expression, Position, PositionedItem, Statement, Value as ASTValue};
use crate::iter::OffsetStrIter;
use crate::parse::parse;

macro_rules! assert_parse_cases {
    (__impl__ $cases:expr) => {
        for case in $cases.drain(0..) {
            let stmts = parse(OffsetStrIter::from(dbg!(case.0)), None).unwrap();
            // TODO(jwall): preprocessor
            let ops = Rc::new(translate::AST::translate(stmts));
            assert!(ops.len() > 0);
            let mut vm = VM::new("foo.ucg", ops.clone());
            vm.run().unwrap();
            assert_eq!(dbg!(vm.pop()).unwrap(), Rc::new(case.1));
        }
    };

    ( ($input:expr, $result:expr), $( $tok:tt )* ) => {
        assert_parse_cases!(__impl__ vec![($input, $result), $($tok)*])
    };

    ( $( $input:expr => $result:expr, )* ) => {
        assert_parse_cases!($(($input, $result),)*)
    }
}

#[test]
fn simple_expr_scalar_value() {
    assert_parse_cases!(
        "1;" => P(Int(1)),
        "(1);" => P(Int(1)),
        "1.0;" => P(Float(1.0)),
        "true;" => P(Bool(true)),
        "NULL;" => P(Empty),
        "\"foo\";" => P(Str("foo".to_owned())),
    )
}

#[test]
fn simple_binary_expr() {
    assert_parse_cases!(
        "1+1;" => P(Int(2)),
        "2-1;" => P(Int(1)),
        "2*2;" => P(Int(4)),
        "6/2;" => P(Int(3)),
        "4 %% 2;" => P(Int(0)),
        "5 %% 2;" => P(Int(1)),
        "1.0+1.0;" => P(Float(2.0)),
        "\"foo\"+\"bar\";" => P(Str("foobar".to_owned())),
        "1==1;" => P(Bool(true)),
        "1>1;" => P(Bool(false)),
        "1<1;" => P(Bool(false)),
        "2>1;" => P(Bool(true)),
        "2<1;" => P(Bool(false)),
        "2<=1;" => P(Bool(false)),
        "2>=1;" => P(Bool(true)),
        "1!=1;" => P(Bool(false)),
        "\"foo\" ~ \"bar\";" => P(Bool(false)),
        "\"foo\" !~ \"bar\";" => P(Bool(true)),
        "\"foo\" is \"str\";" => P(Bool(true)),
        "true && true;" => P(Bool(true)),
        "true && false;" => P(Bool(false)),
        "false && false;" => P(Bool(false)),
        "false && true;" => P(Bool(false)),
        "false || false;" => P(Bool(false)),
        "true || false;" => P(Bool(true)),
        "false || true;" => P(Bool(true)),
        "true || true;" => P(Bool(true)),
        "foo in {foo = 1};" => P(Bool(true)),
        "bar in {foo = 1};" => P(Bool(false)),
    )
}

#[test]
fn simple_let_statements() {
    assert_parse_cases![
        "let foo = 1; foo;" => P(Int(1)),
        "let foo = 1 + 1; foo;" => P(Int(2)),
    ];
}

#[test]
fn dot_expressions() {
    assert_parse_cases![
        "let foo = [0,1,2]; foo.0;" => P(Int(0)),
        "let foo = [0,1,2]; foo.2;" => P(Int(2)),
        "let tpl = { foo = 1 }; tpl.foo;" => P(Int(1)),
        "let tpl = { foo = { bar = 2 } }; tpl.foo.bar;" => P(Int(2)),
        "let tpl = { foo = [ 3 ] }; tpl.foo.0;" => P(Int(3)),
    ];
}

#[test]
fn simple_not_expr() {
    assert_parse_cases!(
        "not 1==1;" => P(Bool(false)),
        "not 1!=1;" => P(Bool(true)),
    )
}

#[test]
fn simple_render_operation() {
    assert_cases![
        vec![
            Val(Int(1)),
            Render,
        ] => P(Str("1".to_owned())),
        vec![
            Val(Float(1.1)),
            Render,
        ] => P(Str("1.1".to_owned())),
        vec![
            Val(Bool(true)),
            Render,
        ] => P(Str("true".to_owned())),
        vec![
            Val(Bool(false)),
            Render,
        ] => P(Str("false".to_owned())),
        vec![
            Val(Empty),
            Render,
        ] => P(Str("NULL".to_owned())),
        vec![
            Val(Str("foo".to_owned())),
            Render,
        ] => P(Str("foo".to_owned())),
    ];
}

#[test]
fn simple_format_expressions() {
    assert_parse_cases![
        "\"@\" % (1);" => P(Str("1".to_owned())),
        "\"@\" % (1.1);" => P(Str("1.1".to_owned())),
        "\"@\" % (\"foo\");" => P(Str("foo".to_owned())),
        "\"@\" % (NULL);" => P(Str("NULL".to_owned())),
        "\"@ @ @\" % (1, 2, 3);" => P(Str("1 2 3".to_owned())),
        "\"@ \\\\@\" % (1);" => P(Str("1 @".to_owned())),
        "\"@{item.num}\" % {num=1};" => P(Str("1".to_owned())),
    ];
}

#[test]
fn simple_functions() {
    assert_parse_cases![
        "let f = func(val) => val; f(1);" => P(Int(1)),
        "let f = func(val1, val2) => val1 + val2; f(1, 1);" => P(Int(2)),
    ];
}
