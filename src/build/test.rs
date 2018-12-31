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
use super::assets::MemoryCache;
use super::{CallDef, FileBuilder, MacroDef, SelectDef, Val};
use crate::ast::*;

use std;
use std::cell::RefCell;
use std::rc::Rc;

fn test_expr_to_val(mut cases: Vec<(Expression, Val)>, mut b: FileBuilder) {
    for tpl in cases.drain(0..) {
        assert_eq!(b.eval_expr(&tpl.0).unwrap(), Rc::new(tpl.1));
    }
}

#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_div_expr_fail() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    test_expr_to_val(
        vec![(
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Div,
                left: Box::new(Expression::Simple(Value::Float(value_node!(
                    2.0,
                    Position::new(1, 1, 1)
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(
                    2,
                    Position::new(1, 1, 1)
                )))),
                pos: Position::new(1, 0, 0),
            }),
            Val::Float(1.0),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_mul_expr_fail() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    test_expr_to_val(
        vec![(
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Mul,
                left: Box::new(Expression::Simple(Value::Float(value_node!(
                    2.0,
                    Position::new(1, 1, 1)
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(
                    20,
                    Position::new(1, 1, 1)
                )))),
                pos: Position::new(1, 0, 0),
            }),
            Val::Float(1.0),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_subtract_expr_fail() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    test_expr_to_val(
        vec![(
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Sub,
                left: Box::new(Expression::Simple(Value::Float(value_node!(
                    2.0,
                    Position::new(1, 1, 1)
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(
                    2,
                    Position::new(1, 1, 1)
                )))),
                pos: Position::new(1, 0, 0),
            }),
            Val::Float(1.0),
        )],
        b,
    );
}
#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_add_expr_fail() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    test_expr_to_val(
        vec![(
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Float(value_node!(
                    2.0,
                    Position::new(1, 1, 1)
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(
                    2,
                    Position::new(1, 1, 1)
                )))),
                pos: Position::new(1, 0, 0),
            }),
            Val::Float(1.0),
        )],
        b,
    );
}

#[test]
fn test_eval_simple_lookup_error() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let mut b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    b.build_output
        .entry(value_node!("var1".to_string(), Position::new(1, 0, 0)))
        .or_insert(Rc::new(Val::Int(1)));
    let expr = Expression::Simple(Value::Symbol(value_node!(
        "var".to_string(),
        Position::new(1, 1, 1)
    )));
    assert!(b.eval_expr(&expr).is_err());
}

// Include nested for each.
#[test]
#[should_panic(expected = "Unable to find binding tpl1")]
fn test_expr_copy_no_such_tuple() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    test_expr_to_val(
        vec![(
            Expression::Copy(CopyDef {
                selector: make_selector!(
                    make_expr!("tpl1", Position::new(1, 1, 1)),
                    Position::new(1, 1, 1)
                ),
                fields: Vec::new(),
                pos: Position::new(1, 0, 0),
            }),
            Val::Tuple(Vec::new()),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected Tuple or Module got Int(1)")]
fn test_expr_copy_not_a_tuple() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let mut b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    b.build_output
        .entry(value_node!("tpl1".to_string(), Position::new(1, 0, 0)))
        .or_insert(Rc::new(Val::Int(1)));
    test_expr_to_val(
        vec![(
            Expression::Copy(CopyDef {
                selector: make_selector!(
                    make_expr!("tpl1", Position::new(1, 1, 1)),
                    Position::new(1, 1, 1)
                ),
                fields: Vec::new(),
                pos: Position::new(1, 0, 0),
            }),
            Val::Tuple(Vec::new()),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected type Integer for field fld1 but got String")]
fn test_expr_copy_field_type_error() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let mut b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    b.build_output
        .entry(value_node!("tpl1".to_string(), Position::new(1, 0, 0)))
        .or_insert(Rc::new(Val::Tuple(vec![(
            value_node!("fld1".to_string(), Position::new(1, 0, 0)),
            Rc::new(Val::Int(1)),
        )])));
    test_expr_to_val(
        vec![(
            Expression::Copy(CopyDef {
                selector: make_selector!(
                    make_expr!("tpl1", Position::new(1, 1, 1)),
                    Position::new(1, 1, 1)
                ),
                fields: vec![(
                    make_tok!("fld1", Position::new(1, 1, 1)),
                    Expression::Simple(Value::Str(value_node!(
                        "2".to_string(),
                        Position::new(1, 1, 1)
                    ))),
                )],
                pos: Position::new(1, 0, 0),
            }),
            Val::Tuple(vec![(
                value_node!("fld1".to_string(), Position::new(1, 1, 1)),
                Rc::new(Val::Str("2".to_string())),
            )]),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Unable to find binding arg1")]
fn test_macro_hermetic() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let mut b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    b.build_output
        .entry(value_node!("arg1".to_string(), Position::new(1, 0, 0)))
        .or_insert(Rc::new(Val::Str("bar".to_string())));
    b.build_output
        .entry(value_node!("tstmac".to_string(), Position::new(1, 0, 0)))
        .or_insert(Rc::new(Val::Macro(MacroDef {
            argdefs: vec![value_node!("arg2".to_string(), Position::new(1, 0, 0))],
            fields: vec![(
                make_tok!("foo", Position::new(1, 1, 1)),
                Expression::Simple(Value::Symbol(value_node!(
                    "arg1".to_string(),
                    Position::new(1, 1, 1)
                ))),
            )],
            pos: Position::new(1, 0, 0),
        })));
    test_expr_to_val(
        vec![(
            Expression::Call(CallDef {
                macroref: make_selector!(
                    make_expr!("tstmac", Position::new(1, 1, 1)),
                    Position::new(1, 1, 1)
                ),
                arglist: vec![Expression::Simple(Value::Str(value_node!(
                    "bar".to_string(),
                    Position::new(1, 1, 1)
                )))],
                pos: Position::new(1, 1, 1),
            }),
            Val::Tuple(vec![(
                value_node!("foo".to_string(), Position::new(1, 1, 1)),
                Rc::new(Val::Str("bar".to_string())),
            )]),
        )],
        b,
    );
}

#[test]
#[should_panic(expected = "Expected String but got Integer in Select expression")]
fn test_select_expr_not_a_string() {
    let i_paths = Vec::new();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    let mut b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, cache);
    b.build_output
        .entry(value_node!("foo".to_string(), Position::new(1, 0, 0)))
        .or_insert(Rc::new(Val::Int(4)));
    test_expr_to_val(
        vec![(
            Expression::Select(SelectDef {
                val: Box::new(Expression::Simple(Value::Symbol(value_node!(
                    "foo".to_string(),
                    Position::new(1, 1, 1)
                )))),
                default: Box::new(Expression::Simple(Value::Int(value_node!(
                    1,
                    Position::new(1, 1, 1)
                )))),
                tuple: vec![
                    (
                        make_tok!("bar", Position::new(1, 1, 1)),
                        Expression::Simple(Value::Int(value_node!(2, Position::new(1, 1, 1)))),
                    ),
                    (
                        make_tok!("quux", Position::new(1, 1, 1)),
                        Expression::Simple(Value::Str(value_node!(
                            "2".to_string(),
                            Position::new(1, 1, 1)
                        ))),
                    ),
                ],
                pos: Position::new(1, 0, 0),
            }),
            Val::Int(2),
        )],
        b,
    );
}
