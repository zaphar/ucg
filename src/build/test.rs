// Copyright 2017 Jeremy Wall <jeremy@marzhillstudios.com>
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
use super::{Builder, CallDef, MacroDef, SelectDef, Val};
use ast::*;

use std;
use std::rc::Rc;

fn test_expr_to_val(mut cases: Vec<(Expression, Val)>, b: Builder) {
    for tpl in cases.drain(0..) {
        assert_eq!(b.eval_expr(&tpl.0).unwrap(), Rc::new(tpl.1));
    }
}

#[test]
fn test_eval_div_expr() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Div,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                    pos: Position::new(1, 0),
                }),
                Val::Int(1),
            ),
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Div,
                    left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                    right: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                    pos: Position::new(1, 0),
                }),
                Val::Float(1.0),
            ),
        ],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_div_expr_fail() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![(
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Div,
                left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                pos: Position::new(1, 0),
            }),
            Val::Float(1.0),
        )],
        b,
    );
}

#[test]
fn test_eval_mul_expr() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Mul,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                    pos: Position::new(1, 0),
                }),
                Val::Int(4),
            ),
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Mul,
                    left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                    right: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                    pos: Position::new(1, 0),
                }),
                Val::Float(4.0),
            ),
        ],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_mul_expr_fail() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![(
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Mul,
                left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(20, 1, 1)))),
                pos: Position::new(1, 0),
            }),
            Val::Float(1.0),
        )],
        b,
    );
}

#[test]
fn test_eval_subtract_expr() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Sub,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                    pos: Position::new(1, 0),
                }),
                Val::Int(1),
            ),
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Sub,
                    left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                    right: Box::new(Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))),
                    pos: Position::new(1, 0),
                }),
                Val::Float(1.0),
            ),
        ],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_subtract_expr_fail() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![(
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Sub,
                left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                pos: Position::new(1, 0),
            }),
            Val::Float(1.0),
        )],
        b,
    );
}

#[test]
fn test_eval_add_expr() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Add,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                    pos: Position::new(1, 0),
                }),
                Val::Int(2),
            ),
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Add,
                    left: Box::new(Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))),
                    right: Box::new(Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))),
                    pos: Position::new(1, 0),
                }),
                Val::Float(2.0),
            ),
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Add,
                    left: Box::new(Expression::Simple(Value::Str(value_node!(
                        "foo".to_string(),
                        1,
                        1
                    )))),
                    right: Box::new(Expression::Simple(Value::Str(value_node!(
                        "bar".to_string(),
                        1,
                        1
                    )))),
                    pos: Position::new(1, 0),
                }),
                Val::Str("foobar".to_string()),
            ),
            (
                Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Add,
                    left: Box::new(Expression::Simple(Value::List(ListDef {
                        elems: vec![Expression::Simple(Value::Str(value_node!(
                            "foo".to_string(),
                            1,
                            1
                        )))],
                        pos: Position::new(1, 1),
                    }))),
                    right: Box::new(Expression::Simple(Value::List(ListDef {
                        elems: vec![Expression::Simple(Value::Str(value_node!(
                            "bar".to_string(),
                            1,
                            1
                        )))],
                        pos: Position::new(1, 1),
                    }))),
                    pos: Position::new(1, 0),
                }),
                Val::List(vec![
                    Rc::new(Val::Str("foo".to_string())),
                    Rc::new(Val::Str("bar".to_string())),
                ]),
            ),
        ],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_add_expr_fail() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![(
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                pos: Position::new(1, 0),
            }),
            Val::Float(1.0),
        )],
        b,
    );
}

#[test]
fn test_eval_nested_tuple() {
    test_expr_to_val(
        vec![
            (
                Expression::Simple(Value::Tuple(value_node!(
                    vec![(
                        Token::new("foo", TokenType::BAREWORD, 1, 1),
                        Expression::Simple(Value::Tuple(value_node!(Vec::new(), 1, 1))),
                    )],
                    1,
                    1
                ))),
                Val::Tuple(vec![(
                    Positioned::new("foo".to_string(), 1, 1),
                    Rc::new(Val::Tuple(Vec::new())),
                )]),
            ),
            (
                Expression::Simple(Value::Tuple(value_node!(
                    vec![(
                        Token::new("foo", TokenType::BAREWORD, 1, 1),
                        Expression::Simple(Value::Tuple(value_node!(
                            vec![(
                                Token::new("bar".to_string(), TokenType::BAREWORD, 1, 5),
                                Expression::Simple(Value::Tuple(value_node!(vec![], 1, 10))),
                            )],
                            1,
                            1
                        ))),
                    )],
                    1,
                    1
                ))),
                Val::Tuple(vec![(
                    Positioned::new("foo".to_string(), 1, 1),
                    Rc::new(Val::Tuple(vec![(
                        Positioned::new("bar".to_string(), 1, 10),
                        Rc::new(Val::Tuple(vec![])),
                    )])),
                )]),
            ),
            (
                Expression::Simple(Value::Tuple(value_node!(
                    vec![(
                        Token::new("foo", TokenType::BAREWORD, 1, 1),
                        Expression::Simple(Value::Tuple(value_node!(
                            vec![(
                                Token::new("bar".to_string(), TokenType::BAREWORD, 1, 5),
                                Expression::Simple(Value::Tuple(value_node!(
                                    vec![(
                                        Token::new("quux".to_string(), TokenType::BAREWORD, 1, 1),
                                        Expression::Simple(Value::Int(value_node!(3, 1, 1))),
                                    )],
                                    1,
                                    10
                                ))),
                            )],
                            1,
                            1
                        ))),
                    )],
                    1,
                    1
                ))),
                Val::Tuple(vec![(
                    Positioned::new("foo".to_string(), 1, 1),
                    Rc::new(Val::Tuple(vec![(
                        Positioned::new("bar".to_string(), 1, 10),
                        Rc::new(Val::Tuple(vec![(
                            Positioned::new("quux".to_string(), 1, 1),
                            Rc::new(Val::Int(3)),
                        )])),
                    )])),
                )]),
            ),
        ],
        Builder::new(std::env::current_dir().unwrap()),
    );
}

#[test]
fn test_eval_simple_expr() {
    test_expr_to_val(
        vec![
            (
                Expression::Simple(Value::Int(value_node!(1, 1, 1))),
                Val::Int(1),
            ),
            (
                Expression::Simple(Value::Float(value_node!(2.0, 1, 1))),
                Val::Float(2.0),
            ),
            (
                Expression::Simple(Value::Str(value_node!("foo".to_string(), 1, 1))),
                Val::Str("foo".to_string()),
            ),
            (
                Expression::Simple(Value::Tuple(value_node!(
                    vec![(
                        make_tok!("bar", 1, 1),
                        Expression::Simple(Value::Int(value_node!(1, 1, 1))),
                    )],
                    1,
                    1
                ))),
                Val::Tuple(vec![(
                    value_node!("bar".to_string(), 1, 1),
                    Rc::new(Val::Int(1)),
                )]),
            ),
        ],
        Builder::new(std::env::current_dir().unwrap()),
    );
}

#[test]
fn test_eval_simple_lookup_expr() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("var1".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Int(1)));
    test_expr_to_val(
        vec![(
            Expression::Simple(Value::Symbol(value_node!("var1".to_string(), 1, 1))),
            Val::Int(1),
        )],
        b,
    );
}

#[test]
fn test_eval_simple_lookup_error() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("var1".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Int(1)));
    let expr = Expression::Simple(Value::Symbol(value_node!("var".to_string(), 1, 1)));
    assert!(b.eval_expr(&expr).is_err());
}

#[test]
fn test_eval_selector_expr() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("var1".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Tuple(vec![(
            value_node!("lvl1".to_string(), 1, 0),
            Rc::new(Val::Tuple(vec![(
                value_node!("lvl2".to_string(), 1, 0),
                Rc::new(Val::Int(3)),
            )])),
        )])));
    b.out
        .entry(value_node!("var2".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Int(2)));
    b.out
        .entry(value_node!("var3".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Tuple(vec![(
            value_node!("lvl1".to_string(), 1, 0),
            Rc::new(Val::Int(4)),
        )])));

    test_expr_to_val(
        vec![
            (
                Expression::Simple(Value::Selector(make_selector!(make_expr!("var1")))),
                Val::Tuple(vec![(
                    value_node!("lvl1".to_string(), 1, 0),
                    Rc::new(Val::Tuple(vec![(
                        value_node!("lvl2".to_string(), 1, 0),
                        Rc::new(Val::Int(3)),
                    )])),
                )]),
            ),
            (
                Expression::Simple(Value::Selector(
                    make_selector!(make_expr!("var1") => "lvl1"),
                )),
                Val::Tuple(vec![(
                    value_node!("lvl2".to_string(), 1, 0),
                    Rc::new(Val::Int(3)),
                )]),
            ),
            (
                Expression::Simple(Value::Selector(
                    make_selector!(make_expr!("var1") => "lvl1", "lvl2"),
                )),
                Val::Int(3),
            ),
            (
                Expression::Simple(Value::Selector(make_selector!(make_expr!("var2")))),
                Val::Int(2),
            ),
            (
                Expression::Simple(Value::Selector(
                    make_selector!(make_expr!("var3") => "lvl1"),
                )),
                Val::Int(4),
            ),
        ],
        b,
    );
}

#[test]
fn test_eval_selector_list_expr() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("var1".to_string(), 1, 1))
        .or_insert(Rc::new(Val::List(vec![
            Rc::new(Val::Str("val1".to_string())),
            Rc::new(Val::Tuple(vec![(
                value_node!("var2".to_string(), 1, 1),
                Rc::new(Val::Int(1)),
            )])),
        ])));

    test_expr_to_val(
        vec![(
            Expression::Simple(Value::Selector(
                make_selector!(make_expr!("var1") =>  "0" => 1, 1),
            )),
            Val::Str("val1".to_string()),
        )],
        b,
    );
}

// TODO(jwall): Eval for tuple and list.
// Include nested for each.
#[test]
#[should_panic(expected = "Unable to find tpl1")]
fn test_expr_copy_no_such_tuple() {
    let b = Builder::new(std::env::current_dir().unwrap());
    test_expr_to_val(
        vec![(
            Expression::Copy(CopyDef {
                selector: make_selector!(make_expr!("tpl1")),
                fields: Vec::new(),
                pos: Position::new(1, 0),
            }),
            Val::Tuple(Vec::new()),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected Tuple got Int(1)")]
fn test_expr_copy_not_a_tuple() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("tpl1".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Int(1)));
    test_expr_to_val(
        vec![(
            Expression::Copy(CopyDef {
                selector: make_selector!(make_expr!("tpl1")),
                fields: Vec::new(),
                pos: Position::new(1, 0),
            }),
            Val::Tuple(Vec::new()),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected type Integer for field fld1 but got String")]
fn test_expr_copy_field_type_error() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("tpl1".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Tuple(vec![(
            value_node!("fld1".to_string(), 1, 0),
            Rc::new(Val::Int(1)),
        )])));
    test_expr_to_val(
        vec![(
            Expression::Copy(CopyDef {
                selector: make_selector!(make_expr!("tpl1")),
                fields: vec![(
                    make_tok!("fld1", 1, 1),
                    Expression::Simple(Value::Str(value_node!("2".to_string(), 1, 1))),
                )],
                pos: Position::new(1, 0),
            }),
            Val::Tuple(vec![(
                value_node!("fld1".to_string(), 1, 1),
                Rc::new(Val::Str("2".to_string())),
            )]),
        )],
        b,
    );
}

#[test]
fn test_expr_copy() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("tpl1".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Tuple(vec![(
            value_node!("fld1".to_string(), 1, 0),
            Rc::new(Val::Int(1)),
        )])));
    test_expr_to_val(
        vec![
            (
                Expression::Copy(CopyDef {
                    selector: make_selector!(make_expr!("tpl1")),
                    fields: vec![(
                        make_tok!("fld2", 1, 1),
                        Expression::Simple(Value::Str(value_node!("2".to_string(), 1, 1))),
                    )],
                    pos: Position::new(1, 0),
                }),
                // Add a new field to the copy
                Val::Tuple(
                    // NOTE(jwall): The order of these is important in order to ensure
                    // that the compare assertion is correct. The ordering has no
                    // semantics though so at some point we should probably be less restrictive.
                    vec![
                        (value_node!("fld1".to_string(), 1, 0), Rc::new(Val::Int(1))),
                        (
                            value_node!("fld2".to_string(), 1, 1),
                            Rc::new(Val::Str("2".to_string())),
                        ),
                    ],
                ),
            ),
            // Overwrite a field in the copy
            (
                Expression::Copy(CopyDef {
                    selector: make_selector!(make_expr!("tpl1")),
                    fields: vec![
                        (
                            make_tok!("fld1", 1, 1),
                            Expression::Simple(Value::Int(value_node!(3, 1, 1))),
                        ),
                        (
                            make_tok!("fld2", 1, 1),
                            Expression::Simple(Value::Str(value_node!("2".to_string(), 1, 1))),
                        ),
                    ],
                    pos: Position::new(1, 0),
                }),
                Val::Tuple(vec![
                    (value_node!("fld1".to_string(), 1, 0), Rc::new(Val::Int(3))),
                    (
                        value_node!("fld2".to_string(), 1, 0),
                        Rc::new(Val::Str("2".to_string())),
                    ),
                ]),
            ),
            // The source tuple is still unmodified.
            (
                Expression::Simple(Value::Selector(make_selector!(make_expr!["tpl1"]))),
                Val::Tuple(vec![(
                    value_node!("fld1".to_string(), 1, 0),
                    Rc::new(Val::Int(1)),
                )]),
            ),
        ],
        b,
    );
}

#[test]
fn test_macro_call() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("tstmac".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Macro(MacroDef {
            argdefs: vec![value_node!("arg1".to_string(), 1, 0)],
            fields: vec![(
                make_tok!("foo", 1, 1),
                Expression::Simple(Value::Symbol(value_node!("arg1".to_string(), 1, 1))),
            )],
            pos: Position::new(1, 0),
        })));
    test_expr_to_val(
        vec![(
            Expression::Call(CallDef {
                macroref: make_selector!(make_expr!("tstmac")),
                arglist: vec![Expression::Simple(Value::Str(value_node!(
                    "bar".to_string(),
                    1,
                    1
                )))],
                pos: Position::new(1, 0),
            }),
            Val::Tuple(vec![(
                value_node!("foo".to_string(), 1, 1),
                Rc::new(Val::Str("bar".to_string())),
            )]),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Unable to find arg1")]
fn test_macro_hermetic() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("arg1".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Str("bar".to_string())));
    b.out
        .entry(value_node!("tstmac".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Macro(MacroDef {
            argdefs: vec![value_node!("arg2".to_string(), 1, 0)],
            fields: vec![(
                make_tok!("foo", 1, 1),
                Expression::Simple(Value::Symbol(value_node!("arg1".to_string(), 1, 1))),
            )],
            pos: Position::new(1, 0),
        })));
    test_expr_to_val(
        vec![(
            Expression::Call(CallDef {
                macroref: make_selector!(make_expr!("tstmac")),
                arglist: vec![Expression::Simple(Value::Str(value_node!(
                    "bar".to_string(),
                    1,
                    1
                )))],
                pos: Position::new(1, 1),
            }),
            Val::Tuple(vec![(
                value_node!("foo".to_string(), 1, 0),
                Rc::new(Val::Str("bar".to_string())),
            )]),
        )],
        b,
    );
}

#[test]
fn test_select_expr() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("foo".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Str("bar".to_string())));
    b.out
        .entry(value_node!("baz".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Str("boo".to_string())));
    test_expr_to_val(
        vec![
            (
                Expression::Select(SelectDef {
                    val: Box::new(Expression::Simple(Value::Symbol(value_node!(
                        "foo".to_string(),
                        1,
                        1
                    )))),
                    default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                    tuple: vec![
                        (
                            make_tok!("foo", 1, 1),
                            Expression::Simple(Value::Str(value_node!("2".to_string(), 1, 1))),
                        ),
                        (
                            make_tok!("bar", 1, 1),
                            Expression::Simple(Value::Int(value_node!(2, 1, 1))),
                        ),
                    ],
                    pos: Position::new(1, 0),
                }),
                Val::Int(2),
            ),
            (
                Expression::Select(SelectDef {
                    val: Box::new(Expression::Simple(Value::Symbol(value_node!(
                        "baz".to_string(),
                        1,
                        1
                    )))),
                    default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                    tuple: vec![
                        (
                            make_tok!("bar", 1, 1),
                            Expression::Simple(Value::Int(value_node!(2, 1, 1))),
                        ),
                        (
                            make_tok!("quux", 1, 1),
                            Expression::Simple(Value::Str(value_node!("2".to_string(), 1, 1))),
                        ),
                    ],
                    pos: Position::new(1, 0),
                }),
                // If the field doesn't exist then we get the default.
                Val::Int(1),
            ),
        ],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected String but got Integer in Select expression")]
fn test_select_expr_not_a_string() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.out
        .entry(value_node!("foo".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Int(4)));
    test_expr_to_val(
        vec![(
            Expression::Select(SelectDef {
                val: Box::new(Expression::Simple(Value::Symbol(value_node!(
                    "foo".to_string(),
                    1,
                    1
                )))),
                default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                tuple: vec![
                    (
                        make_tok!("bar", 1, 1),
                        Expression::Simple(Value::Int(value_node!(2, 1, 1))),
                    ),
                    (
                        make_tok!("quux", 1, 1),
                        Expression::Simple(Value::Str(value_node!("2".to_string(), 1, 1))),
                    ),
                ],
                pos: Position::new(1, 0),
            }),
            Val::Int(2),
        )],
        b,
    );
}

#[test]
fn test_let_statement() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    let stmt = Statement::Let(LetDef {
        name: make_tok!("foo", 1, 1),
        value: Expression::Simple(Value::Str(value_node!("bar".to_string(), 1, 1))),
    });
    b.build_stmt(&stmt).unwrap();
    test_expr_to_val(
        vec![(
            Expression::Simple(Value::Symbol(value_node!("foo".to_string(), 1, 1))),
            Val::Str("bar".to_string()),
        )],
        b,
    );
}

#[test]
fn test_build_file_string() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.eval_string("let foo = 1;").unwrap();
    let key = value_node!("foo".to_string(), 1, 0);
    assert!(b.out.contains_key(&key));
}

#[test]
fn test_asset_symbol_lookups() {
    let mut b = Builder::new(std::env::current_dir().unwrap());
    b.assets
        .entry(value_node!("foo".to_string(), 1, 0))
        .or_insert(Rc::new(Val::Tuple(vec![(
            value_node!("bar".to_string(), 1, 0),
            Rc::new(Val::Tuple(vec![(
                value_node!("quux".to_string(), 1, 0),
                Rc::new(Val::Int(1)),
            )])),
        )])));
    test_expr_to_val(
        vec![(
            Expression::Simple(Value::Symbol(value_node!("foo".to_string(), 1, 1))),
            Val::Tuple(vec![(
                value_node!("bar".to_string(), 1, 0),
                Rc::new(Val::Tuple(vec![(
                    value_node!("quux".to_string(), 1, 0),
                    Rc::new(Val::Int(1)),
                )])),
            )]),
        )],
        b,
    );
}
