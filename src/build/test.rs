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
use super::{FileBuilder, Val};
use crate::ast::*;

use std;
use std::rc::Rc;

fn test_expr_to_val<'a, O, E>(mut cases: Vec<(Expression, Val)>, mut b: FileBuilder<'a, O, E>)
where
    O: std::io::Write + Clone,
    E: std::io::Write + Clone,
{
    for tpl in cases.drain(0..) {
        assert_eq!(b.eval_expr(tpl.0).unwrap(), Rc::new(tpl.1));
    }
}

#[test]
#[should_panic(expected = "Expected Float")]
fn test_eval_div_expr_fail() {
    let i_paths = Vec::new();
    let out: Vec<u8> = Vec::new();
    let err: Vec<u8> = Vec::new();
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, out, err);
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
    let out: Vec<u8> = Vec::new();
    let err: Vec<u8> = Vec::new();
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, out, err);
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
    let out: Vec<u8> = Vec::new();
    let err: Vec<u8> = Vec::new();
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, out, err);
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
    let out: Vec<u8> = Vec::new();
    let err: Vec<u8> = Vec::new();
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, out, err);
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

// Include nested for each.
#[test]
#[should_panic(expected = "No such binding tpl1")]
fn test_expr_copy_no_such_tuple() {
    let i_paths = Vec::new();
    let out: Vec<u8> = Vec::new();
    let err: Vec<u8> = Vec::new();
    let b = FileBuilder::new(std::env::current_dir().unwrap(), &i_paths, out, err);
    test_expr_to_val(
        vec![(
            Expression::Copy(CopyDef {
                selector: Value::Symbol(PositionedItem::new(
                    "tpl1".to_string(),
                    Position::new(1, 1, 1),
                )),
                fields: Vec::new(),
                pos: Position::new(1, 0, 0),
            }),
            Val::Tuple(Vec::new()),
        )],
        b,
    );
}
