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
use crate::ast::{
    Expression, FormatArgs, FormatDef, ListDef, NotDef, Position, PositionedItem, Shape, Token,
    TokenType, Value,
};

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
            Expression::Simple(Value::Int(PositionedItem::new(3, Position::new(0, 0, 0)))),
            Shape::Int(PositionedItem::new(3, Position::new(0, 0, 0))),
        ),
        (
            Expression::Grouped(
                Box::new(Expression::Simple(Value::Int(PositionedItem::new(
                    3,
                    Position::new(0, 0, 0),
                )))),
                Position::new(0, 0, 0),
            ),
            Shape::Int(PositionedItem::new(3, Position::new(0, 0, 0))),
        ),
        (
            Expression::Format(FormatDef {
                template: "".to_owned(),
                args: FormatArgs::List(Vec::new()),
                pos: Position::new(0, 0, 0),
            }),
            Shape::Str(PositionedItem::new("".to_owned(), Position::new(0, 0, 0))),
        ),
        (
            Expression::Not(NotDef {
                expr: Box::new(Expression::Simple(Value::Boolean(PositionedItem::new(
                    true,
                    Position::new(0, 0, 0),
                )))),
                pos: Position::new(1, 0, 0),
            }),
            Shape::Boolean(PositionedItem::new(true, Position::new(1, 0, 0))),
        ),
    ];

    for (expr, shape) in expr_cases {
        assert_eq!(expr.derive_shape().unwrap(), shape);
    }
}
