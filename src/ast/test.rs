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

use super::*;

#[test]
pub fn test_macro_validation_happy_path() {
    let def = MacroDef {
        argdefs: vec![value_node!("foo".to_string(), 1, 0)],
        fields: vec![(
            make_tok!("f1", 1, 1),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Symbol(value_node!(
                    "foo".to_string(),
                    1,
                    1
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                pos: Position::new(1, 0),
            }),
        )],
        pos: Position::new(1, 0),
    };
    assert!(def.validate_symbols().unwrap() == ());
}

#[test]
pub fn test_macro_validation_fail() {
    let def = MacroDef {
        argdefs: vec![value_node!("foo".to_string(), 1, 0)],
        fields: vec![(
            make_tok!("f1", 1, 1),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Symbol(value_node!(
                    "bar".to_string(),
                    1,
                    1
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                pos: Position::new(1, 0),
            }),
        )],
        pos: Position::new(1, 0),
    };
    let mut expected = HashSet::new();
    expected.insert("bar".to_string());
    assert_eq!(def.validate_symbols().err().unwrap(), expected);
}

#[test]
pub fn test_macro_validation_selector_happy_path() {
    let def = MacroDef {
        argdefs: vec![value_node!("foo".to_string(), 1, 0)],
        fields: vec![(
            make_tok!("f1", 1, 1),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Selector(
                    make_selector!(make_expr!("foo", 1, 1) => [
                    make_tok!("quux", 1, 1) ] => 1, 1),
                ))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                pos: Position::new(1, 0),
            }),
        )],
        pos: Position::new(1, 0),
    };
    assert!(def.validate_symbols().unwrap() == ());
}

#[test]
pub fn test_macro_validation_selector_fail() {
    let def = MacroDef {
        argdefs: vec![value_node!("foo".to_string(), 1, 0)],
        fields: vec![(
            make_tok!("f1", 1, 1),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Selector(
                    make_selector!(make_expr!("bar", 1, 1) => [
                    make_tok!("quux", 1, 1) ] => 1, 1),
                ))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                pos: Position::new(1, 0),
            }),
        )],
        pos: Position::new(1, 0),
    };
    let mut expected = HashSet::new();
    expected.insert("bar".to_string());
    assert_eq!(def.validate_symbols(), Err(expected));
}
