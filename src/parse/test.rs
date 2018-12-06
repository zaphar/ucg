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
use crate::tokenizer::tokenize;

use abortable_parser::{Result, SliceIter};

use crate::iter::OffsetStrIter;

macro_rules! assert_parse {
    ($parsemac:ident($i:expr), $out:expr) => {
        assert_parse!($i, $parsemac, $out)
    };
    ($i:expr, $f:expr, $out:expr) => {{
        let input = OffsetStrIter::new($i);
        match tokenize(input.clone()) {
            Err(e) => assert!(false, format!("Tokenizer Error: {:?}", e)),
            Ok(val) => match $f(SliceIter::new(val.as_slice())) {
                Result::Complete(_, result) => assert_eq!(result, $out),
                other => assert!(false, format!("Expected Done got {:?}", other)),
            },
        }
    };};
}

macro_rules! assert_fail {
    ($parsemac:ident($i:expr)) => {
        assert_fail!($i, $parsemac)
    };
    ($i:expr, $f:expr) => {{
        let input = OffsetStrIter::new($i);
        match tokenize(input.clone()) {
            Err(_) => assert!(true),
            Ok(val) => {
                let result = $f(SliceIter::new(val.as_slice()));
                assert!(result.is_fail(), format!("Not an abort: {:?}", result))
            }
        }
    }};
}

macro_rules! assert_abort {
    ($parsemac:ident($i:expr)) => {
        assert_abort!($i, $parsemac)
    };
    ($i:expr, $f:expr) => {{
        let input = OffsetStrIter::new($i);
        match tokenize(input.clone()) {
            Err(_) => assert!(true),
            Ok(val) => {
                let result = $f(SliceIter::new(val.as_slice()));
                assert!(result.is_abort(), format!("Not a fail: {:?}", result))
            }
        }
    }};
}

#[test]
fn test_null_parsing() {
    assert_parse!(empty_value("NULL "), Value::Empty(Position::new(1, 1, 0)));
    assert_parse!(value("NULL "), Value::Empty(Position::new(1, 1, 0)));
    assert_parse!(
        simple_expression("NULL "),
        Expression::Simple(Value::Empty(Position::new(1, 1, 0)))
    );
    assert_parse!(
        expression("NULL,"),
        Expression::Simple(Value::Empty(Position::new(1, 1, 0)))
    );
}

#[test]
fn test_boolean_parsing() {
    assert_parse!(
        boolean_value("true"),
        Value::Boolean(PositionedItem::new(true, Position::new(1, 1, 0)))
    );
    assert_parse!(
        boolean_value("false"),
        Value::Boolean(PositionedItem::new(false, Position::new(1, 1, 0)))
    );
    assert_fail!(boolean_value("truth"));
}

#[test]
fn test_symbol_parsing() {
    assert_parse!(
        symbol("foo"),
        Value::Symbol(value_node!("foo".to_string(), Position::new(1, 1, 0)))
    );
    assert_parse!(
        symbol("foo-bar"),
        Value::Symbol(value_node!("foo-bar".to_string(), Position::new(1, 1, 0)))
    );
    assert_parse!(
        symbol("foo_bar"),
        Value::Symbol(value_node!("foo_bar".to_string(), Position::new(1, 1, 0)))
    );
}

#[test]
fn test_selector_parsing() {
    assert_fail!(selector_value("foo."));
    assert_parse!(
        selector_value("foo.bar "),
        Value::Selector(
            make_selector!(make_expr!("foo".to_string(), Position::new(1, 1, 0)) => [
                                      make_tok!("bar", Position::new(1, 5, 4))] =>
                                    Position::new(1, 1, 0))
        )
    );
    assert_parse!(
        selector_value("foo.0 "),
        Value::Selector(
            make_selector!(make_expr!("foo".to_string(), Position::new(1, 1, 0)) => [
                                      make_tok!(DIGIT => "0", Position::new(1, 5, 4))] =>
                                    Position::new(1, 1, 0))
        )
    );
    assert_parse!(
        selector_value("foo.bar;"),
        Value::Selector(make_selector!(make_expr!("foo", Position::new(1, 1, 0)) =>
                                        [
                                           make_tok!("bar", Position::new(1, 5, 4))
                                        ] =>
                                        Position::new(1, 1, 0)))
    );
    assert_parse!(
        selector_value("({foo=1}).foo "),
        Value::Selector(
            make_selector!(Expression::Grouped(Box::new(Expression::Simple(
            Value::Tuple(value_node!(
                vec![(make_tok!("foo", Position::new(1, 3, 2)), Expression::Simple(Value::Int(PositionedItem::new(1, Position::new(1, 7, 6)))))],
                Position::new(1, 3, 3)))
            ))) => [ make_tok!("foo", Position::new(1, 11, 10)) ] => Position::new(1, 2, 1))
        )
    );
}

#[test]
fn test_statement_parse() {
    let stmt = "import \"foo\" as foo;";
    assert_parse!(
        statement(stmt),
        Statement::Import(ImportDef {
            path: make_tok!(QUOT => "foo", Position::new(1, 8, 7)),
            name: make_tok!("foo", Position::new(1, 17, 16)),
        })
    );

    assert_abort!(import_statement("import \"foo\""));

    assert_parse!(
        statement("let foo = 1.0 ;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Simple(Value::Float(value_node!(1.0, Position::new(1, 11, 10)))),
        })
    );

    assert_parse!(
        statement("let foo = 1 + 1 * 2;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Int(value_node!(
                    1,
                    Position::new(1, 11, 10)
                )))),
                right: Box::new(Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Mul,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(
                        1,
                        Position::new(1, 15, 14)
                    )))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(
                        2,
                        Position::new(1, 19, 18)
                    )))),
                    pos: Position::new(1, 15, 14),
                })),
                pos: Position::new(1, 11, 10),
            }),
        })
    );

    assert_parse!(
        statement("let foo = (1 + 1) * 2;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Mul,
                left: Box::new(Expression::Grouped(Box::new(Expression::Binary(
                    BinaryOpDef {
                        kind: BinaryExprType::Add,
                        left: Box::new(Expression::Simple(Value::Int(value_node!(
                            1,
                            Position::new(1, 12, 11)
                        )))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(
                            1,
                            Position::new(1, 16, 15)
                        )))),
                        pos: Position::new(1, 12, 11),
                    },
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(
                    2,
                    Position::new(1, 21, 20)
                )))),
                pos: Position::new(1, 12, 11),
            }),
        })
    );

    assert_parse!(
        statement("let foo = 1 * 1 + 2;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Mul,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(
                        1,
                        Position::new(1, 11, 10)
                    )))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(
                        1,
                        Position::new(1, 15, 14)
                    )))),
                    pos: Position::new(1, 11, 10),
                })),
                right: Box::new(Expression::Simple(Value::Int(value_node!(
                    2,
                    Position::new(1, 19, 18)
                )))),
                pos: Position::new(1, 11, 10),
            }),
        })
    );

    assert_parse!(
        statement("// comment\nlet foo = 1.0 ;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(2, 5, 15)),
            value: Expression::Simple(Value::Float(value_node!(1.0, Position::new(2, 11, 21)))),
        })
    );

    assert_parse!(
        statement("1.0;"),
        Statement::Expression(Expression::Simple(Value::Float(value_node!(
            1.0,
            Position::new(1, 1, 0)
        ))))
    );
}

#[test]
fn test_import_statement_parse() {
    assert_abort!(import_statement("import"));
    assert_abort!(import_statement("import \"foo\""));
    assert_abort!(import_statement("import \"foo\" as"));
    assert_abort!(import_statement("import \"foo\" as foo"));

    let import_stmt = "import \"foo\" as foo;";
    assert_parse!(
        import_statement(import_stmt),
        Statement::Import(ImportDef {
            path: make_tok!(QUOT => "foo", Position::new(1, 8, 7)),
            name: make_tok!("foo", Position::new(1, 17, 16)),
        })
    );
}

#[test]
fn test_let_statement_parse() {
    assert_fail!(let_statement("foo"));
    assert_abort!(let_statement("let \"foo\""));
    assert_abort!(let_statement("let 1"));
    assert_abort!(let_statement("let"));
    assert_abort!(let_statement("let foo"));
    assert_abort!(let_statement("let foo ="));
    assert_abort!(let_statement("let foo = "));
    assert_abort!(let_statement("let foo = 1"));

    assert_parse!(
        let_statement("let foo = 1.0 ;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Simple(Value::Float(value_node!(1.0, Position::new(1, 11, 10)))),
        })
    );

    assert_parse!(
        let_statement("let foo = // comment\n1.0 ;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Simple(Value::Float(value_node!(1.0, Position::new(2, 1, 22)))),
        })
    );

    assert_parse!(
        let_statement("let foo = 1.0 // comment\n;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Simple(Value::Float(value_node!(1.0, Position::new(1, 11, 10)))),
        })
    );

    assert_parse!(
        let_statement("let foo= 1.0;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Simple(Value::Float(value_node!(1.0, Position::new(1, 10, 9)))),
        })
    );

    assert_parse!(
        let_statement("let foo =1.0;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", Position::new(1, 5, 4)),
            value: Expression::Simple(Value::Float(value_node!(1.0, Position::new(1, 10, 9)))),
        })
    );
}

#[test]
fn test_out_statement_parse() {
    assert_abort!(out_statement("out"));
    assert_abort!(out_statement("out json"));
    assert_abort!(out_statement("out json foo"));
    assert_parse!(
        out_statement("out json 1.0;"),
        Statement::Output(
            Token {
                pos: Position {
                    line: 1,
                    column: 5,
                    offset: 4
                },
                fragment: "json".to_string(),
                typ: TokenType::BAREWORD
            },
            Expression::Simple(Value::Float(value_node!(1.0, Position::new(1, 10, 9))))
        )
    );
}

#[test]
fn test_assert_statement_parse() {
    assert_fail!(out_statement("assert"));
    assert_fail!(out_statement("assert |"));
    assert_fail!(out_statement("assert |foo"));
    assert_parse!(
        assert_statement("assert |foo|;"),
        Statement::Assert(Token {
            pos: Position {
                line: 1,
                column: 8,
                offset: 7
            },
            fragment: "foo".to_string(),
            typ: TokenType::PIPEQUOTE
        })
    );
}
#[test]
fn test_expression_statement_parse() {
    assert_fail!(expression_statement("foo"));
    assert_parse!(
        expression_statement("1.0;"),
        Statement::Expression(Expression::Simple(Value::Float(value_node!(
            1.0,
            Position::new(1, 1, 0)
        ))))
    );
    assert_parse!(
        expression_statement("1.0 ;"),
        Statement::Expression(Expression::Simple(Value::Float(value_node!(
            1.0,
            Position::new(1, 1, 0)
        ))))
    );
    assert_parse!(
        expression_statement(" 1.0;"),
        Statement::Expression(Expression::Simple(Value::Float(value_node!(
            1.0,
            Position::new(1, 2, 1)
        ))))
    );
    assert_parse!(
        expression_statement("foo;"),
        Statement::Expression(Expression::Simple(Value::Selector(make_selector!(
            make_expr!("foo", Position::new(1, 1, 0)),
            Position::new(1, 1, 0)
        ))))
    );
    assert_parse!(
        expression_statement("foo ;"),
        Statement::Expression(Expression::Simple(Value::Selector(make_selector!(
            make_expr!("foo", Position::new(1, 2, 1)),
            Position::new(1, 1, 0)
        ))))
    );
    assert_parse!(
        expression_statement(" foo;"),
        Statement::Expression(Expression::Simple(Value::Selector(make_selector!(
            make_expr!("foo", Position::new(1, 2, 1)),
            Position::new(1, 2, 1)
        ))))
    );
    assert_parse!(
        expression_statement("\"foo\";"),
        Statement::Expression(Expression::Simple(Value::Str(value_node!(
            "foo".to_string(),
            Position::new(1, 1, 0)
        ))))
    );
    assert_parse!(
        expression_statement("\"foo\" ;"),
        Statement::Expression(Expression::Simple(Value::Str(value_node!(
            "foo".to_string(),
            Position::new(1, 1, 0)
        ))))
    );
    assert_parse!(
        expression_statement(" \"foo\";"),
        Statement::Expression(Expression::Simple(Value::Str(value_node!(
            "foo".to_string(),
            Position::new(1, 2, 1)
        ))))
    );
}

#[test]
fn test_expression_parse() {
    assert_parse!(
        expression("NULL "),
        Expression::Simple(Value::Empty(Position::new(1, 1, 0)))
    );
    assert_parse!(
        expression("\"foo\""),
        Expression::Simple(Value::Str(value_node!(
            "foo".to_string(),
            Position::new(1, 1, 0)
        )))
    );
    assert_parse!(
        expression("1"),
        Expression::Simple(Value::Int(value_node!(1, Position::new(1, 1, 0))))
    );
    assert_parse!(
        expression("foo "),
        Expression::Simple(Value::Selector(make_selector!(
            make_expr!("foo", Position::new(1, 1, 0)),
            Position::new(1, 1, 0)
        )))
    );
    assert_parse!(
        expression("foo.bar "),
        Expression::Simple(Value::Selector(
            make_selector!(make_expr!("foo", Position::new(1, 1, 0)) =>
                                                             [ make_tok!("bar", Position::new(1, 5, 4)) ] =>
                                                             Position::new(1, 1, 0))
        ))
    );
    assert_parse!(
        expression("{foo=1}.foo "),
        Expression::Simple(Value::Selector(
            make_selector!(Expression::Simple(Value::Tuple(
            value_node!(vec![
                    (
                    make_tok!("foo", Position::new(1, 2, 1)),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 6, 5)))),
                    ),
                ],
                Position::new(1, 1, 0)),
        )) =>
        [ make_tok!("foo", Position::new(1, 9, 8)) ] =>
        Position::new(1, 1, 0))
        ))
    );
    assert_parse!(
        expression("[1, 2].1 "),
        Expression::Simple(Value::Selector(
            make_selector!(Expression::Simple(Value::List(
            ListDef{
                elems: vec![
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 2, 1)))),
                    Expression::Simple(Value::Int(value_node!(2, Position::new(1, 5, 4)))),
                ],
                pos: Position::new(1, 1, 0),
            })) =>
            [ make_tok!(DIGIT => "1", Position::new(1, 8, 7)) ] =>
            Position::new(1, 1, 0))
        ))
    );
    assert_parse!(
        expression("1 + 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 5, 4)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1 - 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Sub,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 5, 4)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1 / 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Div,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 5, 4)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("(1 / 1)"),
        Expression::Grouped(Box::new(Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Div,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 2, 1)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 6, 5)
            )))),
            pos: Position::new(1, 2, 1),
        })))
    );
    assert_parse!(
        expression("1 / 1 + 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Div,
                left: Box::new(Expression::Simple(Value::Int(value_node!(
                    1,
                    Position::new(1, 1, 0)
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(
                    1,
                    Position::new(1, 5, 4)
                )))),
                pos: Position::new(1, 1, 0),
            })),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 9, 8)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("(1 + 1) * 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Mul,
            left: Box::new(Expression::Grouped(Box::new(Expression::Binary(
                BinaryOpDef {
                    kind: BinaryExprType::Add,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(
                        1,
                        Position::new(1, 2, 1)
                    )))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(
                        1,
                        Position::new(1, 6, 5)
                    )))),
                    pos: Position::new(1, 2, 1),
                }
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 11, 10)
            )))),
            pos: Position::new(1, 2, 1),
        })
    );
    assert_parse!(
        expression("1 > 1"),
        Expression::Compare(ComparisonDef {
            kind: CompareType::GT,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 5, 4)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1 < 1"),
        Expression::Compare(ComparisonDef {
            kind: CompareType::LT,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 5, 4)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1 <= 1"),
        Expression::Compare(ComparisonDef {
            kind: CompareType::LTEqual,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 5, 4)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1 >= 1"),
        Expression::Compare(ComparisonDef {
            kind: CompareType::GTEqual,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 5, 4)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1+1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 3, 2)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1-1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Sub,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 3, 2)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1*1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Mul,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 3, 2)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("1/1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Div,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 1, 0)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 3, 2)
            )))),
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("macro (arg1, arg2) => { foo = arg1 }"),
        Expression::Macro(MacroDef {
            argdefs: vec![
                value_node!("arg1".to_string(), Position::new(1, 8, 7)),
                value_node!("arg2".to_string(), Position::new(1, 14, 13)),
            ],
            fields: vec![(
                make_tok!("foo", Position::new(1, 25, 24)),
                Expression::Simple(Value::Selector(make_selector!(
                    make_expr!("arg1", Position::new(1, 31, 30)),
                    Position::new(1, 31, 30)
                ))),
            )],
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("select foo, 1, { foo = 2 }"),
        Expression::Select(SelectDef {
            val: Box::new(Expression::Simple(Value::Selector(make_selector!(
                make_expr!("foo", Position::new(1, 8, 7)),
                Position::new(1, 8, 7)
            )))),
            default: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 13, 12)
            )))),
            tuple: vec![(
                make_tok!("foo", Position::new(1, 18, 17)),
                Expression::Simple(Value::Int(value_node!(2, Position::new(1, 24, 23)))),
            )],
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("foo.bar (1, \"foo\")"),
        Expression::Call(CallDef {
            macroref: make_selector!(make_expr!("foo", Position::new(1, 1, 0))  =>
                                     [ make_tok!("bar", Position::new(1, 5, 4)) ] =>
                                     Position::new(1, 1, 0)),
            arglist: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 10, 9)))),
                Expression::Simple(Value::Str(value_node!(
                    "foo".to_string(),
                    Position::new(1, 13, 12)
                ))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        expression("(1 + 1)"),
        Expression::Grouped(Box::new(Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 2, 1)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 6, 5)
            )))),
            pos: Position::new(1, 2, 1),
        })))
    );
    assert_parse!(
        expression("[1, 1]"),
        Expression::Simple(Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 2, 1)))),
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 5, 4)))),
            ],
            pos: Position::new(1, 1, 0),
        }))
    );
}

#[test]
fn test_format_parse() {
    assert_fail!(format_expression("\"foo"));
    assert_fail!(format_expression("\"foo\""));
    assert_fail!(format_expression("\"foo\" %"));
    assert_fail!(format_expression("\"foo\" % (, 2"));

    assert_parse!(
        format_expression("\"foo @ @\" % (1, 2)"),
        Expression::Format(FormatDef {
            template: "foo @ @".to_string(),
            args: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 14, 13)))),
                Expression::Simple(Value::Int(value_node!(2, Position::new(1, 17, 16)))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        format_expression("\"foo @ @\"%(1, 2)"),
        Expression::Format(FormatDef {
            template: "foo @ @".to_string(),
            args: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 12, 11)))),
                Expression::Simple(Value::Int(value_node!(2, Position::new(1, 15, 14)))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );
}

#[test]
fn test_call_parse() {
    assert_fail!(call_expression("foo"));
    assert_fail!(call_expression("foo ("));
    assert_fail!(call_expression("foo (1"));
    assert_fail!(call_expression("foo (1,"));
    assert_fail!(call_expression("foo (1,2"));

    assert_parse!(
        call_expression("foo (1, \"foo\")"),
        Expression::Call(CallDef {
            macroref: make_selector!(
                make_expr!("foo", Position::new(1, 1, 0)),
                Position::new(1, 1, 0)
            ),
            arglist: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 6, 5)))),
                Expression::Simple(Value::Str(value_node!(
                    "foo".to_string(),
                    Position::new(1, 9, 8)
                ))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        call_expression("foo.bar (1, \"foo\")"),
        Expression::Call(CallDef {
            macroref: make_selector!(make_expr!("foo", Position::new(1, 1, 0)) => [ make_tok!("bar", Position::new(1, 5, 4)) ] => Position::new(1, 1, 0)),
            arglist: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 10, 9)))),
                Expression::Simple(Value::Str(value_node!(
                    "foo".to_string(),
                    Position::new(1, 13, 12)
                ))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        call_expression("foo ()"),
        Expression::Call(CallDef {
            macroref: make_selector!(
                make_expr!("foo", Position::new(1, 1, 0)),
                Position::new(1, 1, 0)
            ),
            arglist: Vec::new(),
            pos: Position::new(1, 1, 0),
        })
    );
}

#[test]
fn test_select_parse() {
    assert_fail!(select_expression("select"));
    assert_fail!(select_expression("select foo"));
    assert_fail!(select_expression("select foo, 1"));
    assert_fail!(select_expression("select foo, 1, {"));

    assert_parse!(
        select_expression("select foo, 1, { foo = 2 }"),
        Expression::Select(SelectDef {
            val: Box::new(Expression::Simple(Value::Selector(make_selector!(
                make_expr!("foo", Position::new(1, 8, 7)),
                Position::new(1, 8, 7)
            )))),
            default: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 13, 12)
            )))),
            tuple: vec![(
                make_tok!("foo", Position::new(1, 18, 17)),
                Expression::Simple(Value::Int(value_node!(2, Position::new(1, 24, 23)))),
            )],
            pos: Position::new(1, 1, 0),
        })
    );
}

#[test]
fn test_module_expression_parsing() {
    assert_fail!(module_expression("foo"));
    assert_fail!(module_expression("module"));
    assert_fail!(module_expression("module("));
    assert_fail!(module_expression("module["));
    assert_fail!(module_expression("module {"));
    assert_fail!(module_expression("module {}"));
    assert_fail!(module_expression("module {} =>"));
    assert_fail!(module_expression("module {} => {"));

    assert_parse!(
        module_expression("module {} => {}"),
        Expression::Module(ModuleDef {
            pos: Position::new(1, 1, 0),
            arg_set: Vec::new(),
            arg_tuple: None,
            statements: Vec::new(),
        })
    );
}

#[test]
fn test_macro_expression_parsing() {
    assert_fail!(macro_expression("foo"));
    assert_fail!(macro_expression("macro \"foo\""));
    assert_fail!(macro_expression("macro 1"));
    assert_fail!(macro_expression("macro"));
    assert_fail!(macro_expression("macro ("));
    assert_fail!(macro_expression("macro (arg"));
    assert_fail!(macro_expression("macro (arg, arg2"));
    assert_fail!(macro_expression("macro (arg1, arg2) =>"));
    assert_fail!(macro_expression("macro (arg1, arg2) => {"));
    assert_fail!(macro_expression("macro (arg1, arg2) => { foo"));
    assert_fail!(macro_expression("macro (arg1, arg2) => { foo ="));

    assert_parse!(
        macro_expression("macro () => {foo=1,bar=2}"),
        Expression::Macro(MacroDef {
            argdefs: Vec::new(),
            fields: vec![
                (
                    make_tok!("foo", Position::new(1, 14, 13)),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 18, 17)))),
                ),
                (
                    make_tok!("bar", Position::new(1, 20, 19)),
                    Expression::Simple(Value::Int(value_node!(2, Position::new(1, 24, 23)))),
                ),
            ],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        macro_expression("macro (arg1, arg2) => {foo=1,bar=2}"),
        Expression::Macro(MacroDef {
            argdefs: vec![
                value_node!("arg1".to_string(), Position::new(1, 8, 7)),
                value_node!("arg2".to_string(), Position::new(1, 14, 13)),
            ],
            fields: vec![
                (
                    make_tok!("foo", Position::new(1, 24, 23)),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 28, 27)))),
                ),
                (
                    make_tok!("bar", Position::new(1, 30, 29)),
                    Expression::Simple(Value::Int(value_node!(2, Position::new(1, 34, 33)))),
                ),
            ],
            pos: Position::new(1, 1, 0),
        })
    );
}

#[test]
fn test_copy_parse() {
    assert_fail!(copy_expression("{}"));
    assert_fail!(copy_expression("foo"));
    assert_fail!(copy_expression("foo{"));

    assert_parse!(
        copy_expression("foo{}"),
        Expression::Copy(CopyDef {
            selector: make_selector!(
                make_expr!("foo", Position::new(1, 1, 0)),
                Position::new(1, 1, 0)
            ),
            fields: Vec::new(),
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        copy_expression("foo{bar=1}"),
        Expression::Copy(CopyDef {
            selector: make_selector!(
                make_expr!("foo", Position::new(1, 1, 0)),
                Position::new(1, 1, 0)
            ),
            fields: vec![(
                make_tok!("bar", Position::new(1, 5, 4)),
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
            )],
            pos: Position::new(1, 1, 0),
        })
    );
    assert_parse!(
        copy_expression("foo{bar=1,}"),
        Expression::Copy(CopyDef {
            selector: make_selector!(
                make_expr!("foo", Position::new(1, 1, 0)),
                Position::new(1, 1, 0)
            ),
            fields: vec![(
                make_tok!("bar", Position::new(1, 5, 4)),
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
            )],
            pos: Position::new(1, 1, 0),
        })
    );
}

#[test]
fn test_grouped_expression_parse() {
    assert_fail!(grouped_expression("foo"));
    assert_fail!(grouped_expression("(foo"));
    assert_parse!(
        grouped_expression("(foo)"),
        Expression::Grouped(Box::new(Expression::Simple(Value::Selector(
            make_selector!(
                make_expr!("foo", Position::new(1, 2, 1)),
                Position::new(1, 2, 1)
            )
        ))))
    );
    assert_parse!(
        grouped_expression("(1 + 1)"),
        Expression::Grouped(Box::new(Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 2, 1)
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(
                1,
                Position::new(1, 6, 5)
            )))),
            pos: Position::new(1, 2, 1),
        })))
    );
}

#[test]
fn test_list_value_parse() {
    assert_fail!(list_value("foo"));
    assert_fail!(list_value("[foo"));
    assert_fail!(list_value("// commen\n[foo"));

    assert_parse!(
        list_value("[foo]"),
        Value::List(ListDef {
            elems: vec![Expression::Simple(Value::Selector(make_selector!(
                make_expr!("foo", Position::new(1, 2, 1)),
                Position::new(1, 2, 1)
            )))],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        list_value("[1, 1]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 2, 1)))),
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 5, 4)))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        list_value("[1, 1,]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 2, 1)))),
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 5, 4)))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        list_value("// comment\n[1, 1]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(2, 2, 12)))),
                Expression::Simple(Value::Int(value_node!(1, Position::new(2, 5, 14)))),
            ],
            pos: Position::new(2, 1, 11),
        })
    );

    assert_parse!(
        list_value("[// comment\n1, 1]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(2, 2, 12)))),
                Expression::Simple(Value::Int(value_node!(1, Position::new(2, 5, 15)))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        list_value("[1, // comment\n1]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 2, 1)))),
                Expression::Simple(Value::Int(value_node!(1, Position::new(2, 1, 14)))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );

    assert_parse!(
        list_value("[1, 1 // comment\n]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 2, 1)))),
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 5, 4)))),
            ],
            pos: Position::new(1, 1, 0),
        })
    );
}

#[test]
fn test_tuple_parse() {
    assert_fail!(tuple("{"));
    assert_fail!(tuple("{ foo"));
    assert_fail!(tuple("{ foo ="));
    assert_fail!(tuple("{ foo = 1"));
    assert_fail!(tuple("{ foo = 1,"));
    assert_fail!(tuple("{ foo = 1, bar ="));
    assert_fail!(tuple("// comment\n{ foo = 1, bar ="));

    assert_parse!(
        tuple("{ }"),
        Value::Tuple(value_node!(vec![], Position::new(1, 1, 0)))
    );

    assert_parse!(
        tuple("{ foo = 1 }"),
        Value::Tuple(value_node!(
            vec![(
                make_tok!("foo", Position::new(1, 3, 2)),
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
            )],
            Position::new(1, 1, 0)
        ))
    );

    assert_parse!(
        tuple("// comment\n{ foo = 1 }"),
        Value::Tuple(value_node!(
            vec![(
                make_tok!("foo", Position::new(2, 3, 13)),
                Expression::Simple(Value::Int(value_node!(1, Position::new(2, 9, 19)))),
            )],
            Position::new(1, 1, 0)
        ))
    );

    assert_parse!(
        tuple("{// comment\n foo = 1 }"),
        Value::Tuple(value_node!(
            vec![(
                make_tok!("foo", Position::new(2, 2, 13)),
                Expression::Simple(Value::Int(value_node!(1, Position::new(2, 8, 19)))),
            )],
            Position::new(1, 1, 0)
        ))
    );

    assert_parse!(
        tuple("{ foo = 1// comment\n }"),
        Value::Tuple(value_node!(
            vec![(
                make_tok!("foo", Position::new(1, 3, 2)),
                Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
            )],
            Position::new(1, 1, 0)
        ))
    );

    assert_parse!(
        tuple("{ foo = 1, bar = \"1\" }"),
        Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", Position::new(1, 3, 2)),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
                ),
                (
                    make_tok!("bar", Position::new(1, 12, 11)),
                    Expression::Simple(Value::Str(value_node!(
                        "1".to_string(),
                        Position::new(1, 18, 17)
                    ))),
                ),
            ],
            Position::new(1, 1, 0)
        ))
    );
    assert_parse!(
        tuple("{ foo = 1, // comment\nbar = \"1\" }"),
        Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", Position::new(1, 3, 2)),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
                ),
                (
                    make_tok!("bar", Position::new(2, 1, 22)),
                    Expression::Simple(Value::Str(value_node!(
                        "1".to_string(),
                        Position::new(2, 7, 28)
                    ))),
                ),
            ],
            Position::new(1, 1, 0)
        ))
    );
    assert_parse!(
        tuple("{ foo = 1, bar = {} }"),
        Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", Position::new(1, 3, 2)),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
                ),
                (
                    make_tok!("bar", Position::new(1, 12, 11)),
                    Expression::Simple(Value::Tuple(value_node!(
                        Vec::new(),
                        Position::new(1, 17, 16)
                    ))),
                ),
            ],
            Position::new(1, 1, 0)
        ))
    );
    assert_parse!(
        tuple("{ foo = 1, bar = {}, }"),
        Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", Position::new(1, 3, 2)),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
                ),
                (
                    make_tok!("bar", Position::new(1, 12, 11)),
                    Expression::Simple(Value::Tuple(value_node!(
                        Vec::new(),
                        Position::new(1, 17, 16)
                    ))),
                ),
            ],
            Position::new(1, 1, 0)
        ))
    );

    assert_parse!(
        expression("{ foo = 1, lst = [1, 2, 3], }"),
        Expression::Simple(Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", Position::new(1, 3, 2)),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9, 8)))),
                ),
                (
                    make_tok!("lst", Position::new(1, 12, 11)),
                    Expression::Simple(Value::List(ListDef {
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(
                                1,
                                Position::new(1, 19, 18)
                            ))),
                            Expression::Simple(Value::Int(value_node!(
                                2,
                                Position::new(1, 22, 21)
                            ))),
                            Expression::Simple(Value::Int(value_node!(
                                3,
                                Position::new(1, 25, 24)
                            ))),
                        ],
                        pos: Position {
                            line: 1,
                            column: 18,
                            offset: 17,
                        },
                    })),
                ),
            ],
            Position::new(1, 1, 0)
        )))
    );
}

#[test]
fn test_field_list_parse() {
    let mut f_list = "foo = 1, quux = 2;";
    assert_parse!(
        field_list(f_list),
        vec![
            (
                make_tok!("foo", Position::new(1, 1, 0)),
                make_expr!(1 => int, Position::new(1, 7, 6))
            ),
            (
                make_tok!("quux", Position::new(1, 10, 9)),
                make_expr!(2 => int, Position::new(1, 17, 16))
            ),
        ]
    );

    f_list = "foo = 1, // comment\nquux = 2;";
    assert_parse!(
        field_list(f_list),
        vec![
            (
                make_tok!("foo", Position::new(1, 1, 0)),
                make_expr!(1 => int, Position::new(1, 7, 6))
            ),
            (
                make_tok!("quux", Position::new(2, 1, 20)),
                make_expr!(2 => int, Position::new(2, 8, 27))
            ),
        ]
    );

    f_list = "foo = 1,\n// comment\nquux = 2;";
    assert_parse!(
        field_list(f_list),
        vec![
            (
                make_tok!("foo", Position::new(1, 1, 0)),
                make_expr!(1 => int, Position::new(1, 7, 6))
            ),
            (
                make_tok!("quux", Position::new(3, 1, 20)),
                make_expr!(2 => int, Position::new(3, 8, 28))
            ),
        ]
    );
    f_list = "foo = 1,\nquux = [1, 2],";
    assert_parse!(
        field_list(f_list),
        vec![
            (
                make_tok!("foo", Position::new(1, 1, 0)),
                make_expr!(1 => int, Position::new(1, 7, 6))
            ),
            (
                make_tok!("quux", Position::new(2, 1, 9)),
                Expression::Simple(Value::List(ListDef {
                    elems: vec![
                        Expression::Simple(Value::Int(value_node!(1, Position::new(2, 9, 17)))),
                        Expression::Simple(Value::Int(value_node!(2, Position::new(2, 12, 20)))),
                    ],
                    pos: Position::new(2, 8, 16),
                })),
            ),
        ]
    );
}

#[test]
fn test_field_value_parse() {
    assert_fail!(field_value("foo"));
    assert_fail!(field_value("// comment\nfoo"));
    assert_fail!(field_value("foo ="));

    assert_parse!(
        field_value("foo = 1"),
        (
            make_tok!("foo", Position::new(1, 1, 0)),
            Expression::Simple(Value::Int(value_node!(1, Position::new(1, 7, 6))))
        )
    );
    assert_parse!(
        field_value("foo = 1 // foo comment\n"),
        (
            make_tok!("foo", Position::new(1, 1, 0)),
            Expression::Simple(Value::Int(value_node!(1, Position::new(1, 7, 6))))
        )
    );
    assert_parse!(
        field_value("foo // foo comment\n = 1"),
        (
            make_tok!("foo", Position::new(1, 1, 0)),
            Expression::Simple(Value::Int(value_node!(1, Position::new(2, 4, 4))))
        )
    );
    assert_parse!(
        field_value("// foo comment\nfoo = 1"),
        (
            make_tok!("foo", Position::new(2, 1, 15)),
            Expression::Simple(Value::Int(value_node!(1, Position::new(2, 7, 21))))
        )
    );
    assert_parse!(
        field_value("foo = \"1\""),
        (
            make_tok!("foo", Position::new(1, 1, 0)),
            Expression::Simple(Value::Str(value_node!(
                "1".to_string(),
                Position::new(1, 7, 6)
            )))
        )
    );
    assert_parse!(
        field_value("foo = bar "),
        (
            make_tok!("foo", Position::new(1, 1, 0)),
            Expression::Simple(Value::Selector(make_selector!(
                make_expr!("bar", Position::new(1, 7, 6)),
                Position::new(1, 7, 6)
            )))
        )
    );
    assert_parse!(
        field_value("foo = bar.baz "),
        (
            make_tok!("foo", Position::new(1, 1, 0)),
            Expression::Simple(Value::Selector(
                make_selector!(make_expr!("bar", Position::new(1, 7, 6)) => [ make_tok!("baz", Position::new(1, 11, 10)) ] => Position::new(1, 7, 6)),
            ))
        )
    );
    assert_parse!(
        field_value("foo = [1,2], "),
        (
            make_tok!("foo", Position::new(1, 1, 0)),
            Expression::Simple(Value::List(ListDef {
                elems: vec![
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 8, 7)))),
                    Expression::Simple(Value::Int(value_node!(2, Position::new(1, 10, 9)))),
                ],
                pos: Position::new(1, 7, 6),
            }))
        )
    );
}

#[test]
fn test_number_parsing() {
    assert_fail!(number("."));
    assert_fail!(number(". "));
    assert_parse!(
        number("1.0"),
        Value::Float(value_node!(1.0, Position::new(1, 1, 0)))
    );
    assert_parse!(
        number("1."),
        Value::Float(value_node!(1.0, Position::new(1, 1, 0)))
    );
    assert_parse!(
        number("1"),
        Value::Int(value_node!(1, Position::new(1, 1, 0)))
    );
    assert_parse!(
        number(".1"),
        Value::Float(value_node!(0.1, Position::new(1, 1, 0)))
    );
}

#[test]
fn test_parse() {
    let bad_input = OffsetStrIter::new("import mylib as lib;");
    let bad_result = parse(bad_input);
    assert!(bad_result.is_err());

    // Valid parsing tree
    let input = OffsetStrIter::new("import \"mylib\" as lib;let foo = 1;1+1;");
    let result = parse(input);
    assert!(result.is_ok(), format!("Expected Ok, Got {:?}", result));
    let tpl = result.unwrap();
    assert_eq!(
        tpl,
        vec![
            Statement::Import(ImportDef {
                path: make_tok!(QUOT => "mylib", Position::new(1, 8, 7)),
                name: make_tok!("lib", Position::new(1, 19, 18)),
            }),
            Statement::Let(LetDef {
                name: make_tok!("foo", Position::new(1, 27, 26)),
                value: Expression::Simple(Value::Int(value_node!(1, Position::new(1, 33, 32)))),
            }),
            Statement::Expression(Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Int(value_node!(
                    1,
                    Position::new(1, 35, 34)
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(
                    1,
                    Position::new(1, 37, 36)
                )))),
                pos: Position::new(1, 35, 34),
            })),
        ]
    );
}
