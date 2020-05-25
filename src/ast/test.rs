// Copyright 2020 Jeremy Wall
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
use abortable_parser::iter::SliceIter;
use abortable_parser::Result as ParseResult;

use crate::ast::{Expression, ListDef, Position, PositionedItem, Shape, Token, TokenType, Value};
use crate::iter::OffsetStrIter;
use crate::parse::expression;
use crate::tokenizer::tokenize;

macro_rules! assert_shape {
    ($input:expr => $shape:expr) => {{
        let iter = OffsetStrIter::new($input);
        let tokenized = match tokenize(iter, None) {
            Ok(toks) => toks,
            Err(err) => panic!(format!("failed to parse {} with error {}", $input, err)),
        };
        let toks = SliceIter::new(&tokenized);
        if let ParseResult::Complete(_, ref expr) = expression(toks) {
            assert_eq!(
                match expr.derive_shape() {
                    Ok(shape) => shape,
                    Err(err) => {
                        panic!(format!("failed to parse {} with error {}", $input, err))
                    }
                },
                $shape
            );
        } else {
            assert!(false, format!("failed to parse expression! {:?}", $input));
        };
    }};
}

#[test]
fn derive_shape_values() {
    let value_cases = vec![
        (
            Value::Empty(Position::new(0, 0, 0)),
            Shape::Empty(Position::new(0, 0, 0)),
        ),
        (
            Value::Boolean(PositionedItem::new(false, Position::new(0, 1, 2))),
            Shape::Boolean(PositionedItem::new(false, Position::new(0, 1, 2))),
        ),
        (
            Value::Boolean(PositionedItem::new(true, Position::new(0, 1, 2))),
            Shape::Boolean(PositionedItem::new(true, Position::new(0, 1, 2))),
        ),
        (
            Value::Int(PositionedItem::new(1, Position::new(0, 1, 2))),
            Shape::Int(PositionedItem::new(1, Position::new(0, 1, 2))),
        ),
        (
            Value::Float(PositionedItem::new(2.0, Position::new(0, 1, 2))),
            Shape::Float(PositionedItem::new(2.0, Position::new(0, 1, 2))),
        ),
        (
            Value::Str(PositionedItem::new(
                "foo".to_owned(),
                Position::new(0, 1, 2),
            )),
            Shape::Str(PositionedItem::new(
                "foo".to_owned(),
                Position::new(0, 1, 2),
            )),
        ),
        (
            Value::Tuple(PositionedItem::new(
                vec![(
                    Token::new("foo", TokenType::BAREWORD, Position::new(0, 0, 0)),
                    Expression::Simple(Value::Int(PositionedItem::new(3, Position::new(0, 0, 0)))),
                )],
                Position::new(0, 0, 0),
            )),
            Shape::Tuple(PositionedItem::new(
                vec![(
                    Token::new("foo", TokenType::BAREWORD, Position::new(0, 0, 0)),
                    Shape::Int(PositionedItem::new(3, Position::new(0, 0, 0))),
                )],
                Position::new(0, 0, 0),
            )),
        ),
        (
            Value::List(ListDef {
                elems: vec![Expression::Simple(Value::Int(PositionedItem::new(
                    3,
                    Position::new(0, 0, 0),
                )))],
                pos: Position::new(0, 0, 0),
            }),
            Shape::List(PositionedItem::new(
                vec![Shape::Int(PositionedItem::new(3, Position::new(0, 0, 0)))],
                Position::new(0, 0, 0),
            )),
        ),
    ];

    for (val, shape) in value_cases {
        assert_eq!(val.derive_shape().unwrap(), shape);
    }
}

#[test]
fn derive_shape_expressions() {
    let expr_cases = vec![
        (
            "3;",
            Shape::Int(PositionedItem::new(3, Position::new(0, 0, 0))),
        ),
        (
            "(3);",
            Shape::Int(PositionedItem::new(3, Position::new(0, 0, 0))),
        ),
        (
            "\"foo {}\" % (1);",
            Shape::Str(PositionedItem::new("".to_owned(), Position::new(0, 0, 0))),
        ),
        (
            "not true;",
            Shape::Boolean(PositionedItem::new(false, Position::new(1, 0, 0))),
        ),
        (
            "0:1;",
            Shape::List(PositionedItem::new(
                vec![Shape::Int(PositionedItem::new(0, Position::new(0, 0, 0)))],
                Position::new(0, 0, 0),
            )),
        ),
        (
            "int(\"1\");",
            Shape::Int(PositionedItem::new(0, Position::new(0, 0, 0))),
        ),
        (
            "float(1);",
            Shape::Float(PositionedItem::new(0.0, Position::new(0, 0, 0))),
        ),
        (
            "str(1);",
            Shape::Str(PositionedItem::new("".to_owned(), Position::new(0, 0, 0))),
        ),
        (
            "bool(\"true\");",
            Shape::Boolean(PositionedItem::new(true, Position::new(0, 0, 0))),
        ),
    ];

    for (expr, shape) in expr_cases {
        assert_shape!(expr => shape);
    }
}
