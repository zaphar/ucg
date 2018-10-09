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
use tokenizer::{tokenize, TokenIter};

use nom_locate::LocatedSpan;

macro_rules! assert_parse {
    ($parsemac:ident($i:expr), $out:expr) => {
        assert_parse!($i, $parsemac, $out)
    };
    ($i:expr, $f:expr, $out:expr) => {{
        let input = LocatedSpan::new($i);
        match tokenize(input) {
            Err(e) => assert!(false, format!("Tokenizer Error: {:?}", e)),
            Ok(val) => match $f(TokenIter {
                source: val.as_slice(),
            }) {
                Ok((_, result)) => assert_eq!(result, $out),
                other => assert!(false, format!("Expected Ok got {:?}", other)),
            },
        }
    };};
}

macro_rules! assert_error {
    ($parsemac:ident($i:expr)) => {
        assert_error!($i, $parsemac)
    };
    ($i:expr, $f:expr) => {{
        let input = LocatedSpan::new($i);
        match tokenize(input) {
            Err(_) => assert!(true),
            Ok(val) => {
                let result = $f(TokenIter {
                    source: val.as_slice(),
                });
                assert!(result.is_err(), format!("Not an error: {:?}", result))
            }
        }
    }};
}

#[test]
fn test_null_parsing() {
    assert_parse!(empty_value("NULL "), Value::Empty(Position::new(1, 1)));
    assert_parse!(value("NULL "), Value::Empty(Position::new(1, 1)));
    assert_parse!(
        simple_expression("NULL "),
        Expression::Simple(Value::Empty(Position::new(1, 1)))
    );
    assert_parse!(
        expression("NULL,"),
        Expression::Simple(Value::Empty(Position::new(1, 1)))
    );
}

#[test]
fn test_boolean_parsing() {
    assert_parse!(
        boolean_value("true"),
        Value::Boolean(Positioned::new(true, 1, 1))
    );
    assert_parse!(
        boolean_value("false"),
        Value::Boolean(Positioned::new(false, 1, 1))
    );
    assert_error!(boolean_value("truth"));
}

#[test]
fn test_symbol_parsing() {
    assert_parse!(
        symbol("foo"),
        Value::Symbol(value_node!("foo".to_string(), 1, 1))
    );
    assert_parse!(
        symbol("foo-bar"),
        Value::Symbol(value_node!("foo-bar".to_string(), 1, 1))
    );
    assert_parse!(
        symbol("foo_bar"),
        Value::Symbol(value_node!("foo_bar".to_string(), 1, 1))
    );
}

#[test]
fn test_selector_parsing() {
    assert_error!(selector_value("foo."));
    assert_parse!(
        selector_value("foo.bar "),
        Value::Selector(make_selector!(make_expr!("foo".to_string(), 1, 1) => [
                                      make_tok!("bar", 1, 5)] =>
                                    1, 1))
    );
    assert_parse!(
        selector_value("foo.0 "),
        Value::Selector(make_selector!(make_expr!("foo".to_string(), 1, 1) => [
                                      make_tok!(DIGIT => "0", 1, 5)] =>
                                    1, 1))
    );
    assert_parse!(
        selector_value("foo.bar;"),
        Value::Selector(make_selector!(make_expr!("foo", 1, 1) =>
                                        [
                                           make_tok!("bar", 1, 5)
                                        ] =>
                                        1, 1))
    );
    assert_parse!(
        selector_value("({foo=1}).foo "),
        Value::Selector(
            make_selector!(Expression::Grouped(Box::new(Expression::Simple(
            Value::Tuple(value_node!(
                vec![(make_tok!("foo", 1, 3), Expression::Simple(Value::Int(Positioned::new(1, 1, 7))))],
                1, 3))
            ))) => [ make_tok!("foo", 1, 11) ] => 1, 2)
        )
    );
}

#[test]
fn test_statement_parse() {
    let stmt = "import \"foo\" as foo;";
    assert_parse!(
        statement(stmt),
        Statement::Import(ImportDef {
            path: make_tok!(QUOT => "foo", 1,8),
            name: make_tok!("foo", 1, 17),
        })
    );

    assert_error!(import_statement("import \"foo\""));

    assert_parse!(
        statement("let foo = 1.0 ;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Simple(Value::Float(value_node!(1.0, 1, 11))),
        })
    );

    assert_parse!(
        statement("let foo = 1 + 1 * 2;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 11)))),
                right: Box::new(Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Mul,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 15)))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 19)))),
                    pos: Position::new(1, 15),
                })),
                pos: Position::new(1, 11),
            }),
        })
    );

    assert_parse!(
        statement("let foo = (1 + 1) * 2;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Mul,
                left: Box::new(Expression::Grouped(Box::new(Expression::Binary(
                    BinaryOpDef {
                        kind: BinaryExprType::Add,
                        left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 12)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 16)))),
                        pos: Position::new(1, 12),
                    },
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 21)))),
                pos: Position::new(1, 12),
            }),
        })
    );

    assert_parse!(
        statement("let foo = 1 * 1 + 2;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Binary(BinaryOpDef {
                    kind: BinaryExprType::Mul,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 11)))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 15)))),
                    pos: Position::new(1, 11),
                })),
                right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 19)))),
                pos: Position::new(1, 11),
            }),
        })
    );

    assert_parse!(
        statement("// comment\nlet foo = 1.0 ;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 2, 5),
            value: Expression::Simple(Value::Float(value_node!(1.0, 2, 11))),
        })
    );

    assert_parse!(
        statement("1.0;"),
        Statement::Expression(Expression::Simple(Value::Float(value_node!(1.0, 1, 1))))
    );
}

#[test]
fn test_import_statement_parse() {
    assert_error!(import_statement("import"));
    assert_error!(import_statement("import \"foo\""));
    assert_error!(import_statement("import \"foo\" as"));
    assert_error!(import_statement("import \"foo\" as foo"));

    let import_stmt = "import \"foo\" as foo;";
    assert_parse!(
        import_statement(import_stmt),
        Statement::Import(ImportDef {
            path: make_tok!(QUOT => "foo", 1, 8),
            name: make_tok!("foo", 1, 17),
        })
    );
}

#[test]
fn test_let_statement_parse() {
    assert_error!(let_statement("foo"));
    assert_error!(let_statement("let \"foo\""));
    assert_error!(let_statement("let 1"));
    assert_error!(let_statement("let"));
    assert_error!(let_statement("let foo"));
    assert_error!(let_statement("let foo ="));
    assert_error!(let_statement("let foo = "));
    assert_error!(let_statement("let foo = 1"));

    assert_parse!(
        let_statement("let foo = 1.0 ;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Simple(Value::Float(value_node!(1.0, 1, 11))),
        })
    );

    assert_parse!(
        let_statement("let foo = // comment\n1.0 ;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Simple(Value::Float(value_node!(1.0, 2, 1))),
        })
    );

    assert_parse!(
        let_statement("let foo = 1.0 // comment\n;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Simple(Value::Float(value_node!(1.0, 1, 11))),
        })
    );

    assert_parse!(
        let_statement("let foo= 1.0;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Simple(Value::Float(value_node!(1.0, 1, 10))),
        })
    );

    assert_parse!(
        let_statement("let foo =1.0;"),
        Statement::Let(LetDef {
            name: make_tok!("foo", 1, 5),
            value: Expression::Simple(Value::Float(value_node!(1.0, 1, 10))),
        })
    );
}

#[test]
fn test_out_statement_parse() {
    assert_error!(out_statement("out"));
    assert_error!(out_statement("out json"));
    assert_error!(out_statement("out json foo"));
    assert_parse!(
        out_statement("out json 1.0;"),
        Statement::Output(
            Token {
                pos: Position { line: 1, column: 5 },
                fragment: "json".to_string(),
                typ: TokenType::BAREWORD
            },
            Expression::Simple(Value::Float(value_node!(1.0, 1, 10)))
        )
    );
}

#[test]
fn test_assert_statement_parse() {
    assert_error!(out_statement("assert"));
    assert_error!(out_statement("assert |"));
    assert_error!(out_statement("assert |foo"));
    assert_parse!(
        assert_statement("assert |foo|;"),
        Statement::Assert(Token {
            pos: Position { line: 1, column: 8 },
            fragment: "foo".to_string(),
            typ: TokenType::PIPEQUOTE
        })
    );
}
#[test]
fn test_expression_statement_parse() {
    assert_error!(expression_statement("foo"));
    assert_parse!(
        expression_statement("1.0;"),
        Statement::Expression(Expression::Simple(Value::Float(value_node!(1.0, 1, 1))))
    );
    assert_parse!(
        expression_statement("1.0 ;"),
        Statement::Expression(Expression::Simple(Value::Float(value_node!(1.0, 1, 1))))
    );
    assert_parse!(
        expression_statement(" 1.0;"),
        Statement::Expression(Expression::Simple(Value::Float(value_node!(1.0, 1, 2))))
    );
    assert_parse!(
        expression_statement("foo;"),
        Statement::Expression(Expression::Simple(Value::Selector(make_selector!(
            make_expr!("foo", 1, 1),
            1,
            1
        ))))
    );
    assert_parse!(
        expression_statement("foo ;"),
        Statement::Expression(Expression::Simple(Value::Selector(make_selector!(
            make_expr!("foo", 1, 2),
            1,
            1
        ))))
    );
    assert_parse!(
        expression_statement(" foo;"),
        Statement::Expression(Expression::Simple(Value::Selector(make_selector!(
            make_expr!("foo", 1, 2),
            1,
            2
        ))))
    );
    assert_parse!(
        expression_statement("\"foo\";"),
        Statement::Expression(Expression::Simple(Value::Str(value_node!(
            "foo".to_string(),
            1,
            1
        ))))
    );
    assert_parse!(
        expression_statement("\"foo\" ;"),
        Statement::Expression(Expression::Simple(Value::Str(value_node!(
            "foo".to_string(),
            1,
            1
        ))))
    );
    assert_parse!(
        expression_statement(" \"foo\";"),
        Statement::Expression(Expression::Simple(Value::Str(value_node!(
            "foo".to_string(),
            1,
            2
        ))))
    );
}

#[test]
fn test_expression_parse() {
    assert_parse!(
        expression("NULL "),
        Expression::Simple(Value::Empty(Position::new(1, 1)))
    );
    assert_parse!(
        expression("\"foo\""),
        Expression::Simple(Value::Str(value_node!("foo".to_string(), 1, 1)))
    );
    assert_parse!(
        expression("1"),
        Expression::Simple(Value::Int(value_node!(1, 1, 1)))
    );
    assert_parse!(
        expression("foo "),
        Expression::Simple(Value::Selector(make_selector!(
            make_expr!("foo", 1, 1),
            1,
            1
        )))
    );
    assert_parse!(
        expression("foo.bar "),
        Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 1) =>
                                                             [ make_tok!("bar", 1, 5) ] =>
                                                             1, 1)))
    );
    assert_parse!(
        expression("{foo=1}.foo "),
        Expression::Simple(Value::Selector(
            make_selector!(Expression::Simple(Value::Tuple(
            value_node!(vec![
                    (
                    make_tok!("foo", 1, 2),
                    Expression::Simple(Value::Int(value_node!(1, 1, 6))),
                    ),
                ],
                1, 1),
        )) =>
        [ make_tok!("foo", 1, 9) ] =>
        1, 1)
        ))
    );
    assert_parse!(
        expression("[1, 2].1 "),
        Expression::Simple(Value::Selector(
            make_selector!(Expression::Simple(Value::List(
            ListDef{
                elems: vec![
                    Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                    Expression::Simple(Value::Int(value_node!(2, 1, 5))),
                ],
                pos: Position::new(1, 1),
            })) =>
            [ make_tok!(DIGIT => "1", 1, 8) ] =>
            1, 1)
        ))
    );
    assert_parse!(
        expression("1 + 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1 - 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Sub,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1 / 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Div,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("(1 / 1)"),
        Expression::Grouped(Box::new(Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Div,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 2)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 6)))),
            pos: Position::new(1, 2),
        })))
    );
    assert_parse!(
        expression("1 / 1 + 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Div,
                left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
                pos: Position::new(1, 1),
            })),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 9)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("(1 + 1) * 1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Mul,
            left: Box::new(Expression::Grouped(Box::new(Expression::Binary(
                BinaryOpDef {
                    kind: BinaryExprType::Add,
                    left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 2)))),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 6)))),
                    pos: Position::new(1, 2),
                }
            )))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 11)))),
            pos: Position::new(1, 2),
        })
    );
    assert_parse!(
        expression("1 > 1"),
        Expression::Compare(ComparisonDef {
            kind: CompareType::GT,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1 < 1"),
        Expression::Compare(ComparisonDef {
            kind: CompareType::LT,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1 <= 1"),
        Expression::Compare(ComparisonDef {
            kind: CompareType::LTEqual,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1 >= 1"),
        Expression::Compare(ComparisonDef {
            kind: CompareType::GTEqual,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1+1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 3)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1-1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Sub,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 3)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1*1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Mul,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 3)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("1/1"),
        Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Div,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 3)))),
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("macro (arg1, arg2) => { foo = arg1 }"),
        Expression::Macro(MacroDef {
            argdefs: vec![
                value_node!("arg1".to_string(), 1, 8),
                value_node!("arg2".to_string(), 1, 14),
            ],
            fields: vec![(
                make_tok!("foo", 1, 25),
                Expression::Simple(Value::Selector(make_selector!(
                    make_expr!("arg1", 1, 31),
                    1,
                    31
                ))),
            )],
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("select foo, 1, { foo = 2 }"),
        Expression::Select(SelectDef {
            val: Box::new(Expression::Simple(Value::Selector(make_selector!(
                make_expr!("foo", 1, 8),
                1,
                8
            )))),
            default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 13)))),
            tuple: vec![(
                make_tok!("foo", 1, 18),
                Expression::Simple(Value::Int(value_node!(2, 1, 24))),
            )],
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("foo.bar (1, \"foo\")"),
        Expression::Call(CallDef {
            macroref: make_selector!(make_expr!("foo", 1, 1)  =>
                                     [ make_tok!("bar", 1, 5) ] =>
                                     1, 1),
            arglist: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 10))),
                Expression::Simple(Value::Str(value_node!("foo".to_string(), 1, 13))),
            ],
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        expression("(1 + 1)"),
        Expression::Grouped(Box::new(Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 2)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 6)))),
            pos: Position::new(1, 2),
        })))
    );
    assert_parse!(
        expression("[1, 1]"),
        Expression::Simple(Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                Expression::Simple(Value::Int(value_node!(1, 1, 5))),
            ],
            pos: Position::new(1, 1),
        }))
    );
}

#[test]
fn test_format_parse() {
    assert_error!(format_expression("\"foo"));
    assert_error!(format_expression("\"foo\""));
    assert_error!(format_expression("\"foo\" %"));
    assert_error!(format_expression("\"foo\" % (, 2"));

    assert_parse!(
        format_expression("\"foo @ @\" % (1, 2)"),
        Expression::Format(FormatDef {
            template: "foo @ @".to_string(),
            args: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 14))),
                Expression::Simple(Value::Int(value_node!(2, 1, 17))),
            ],
            pos: Position::new(1, 1),
        })
    );

    assert_parse!(
        format_expression("\"foo @ @\"%(1, 2)"),
        Expression::Format(FormatDef {
            template: "foo @ @".to_string(),
            args: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 12))),
                Expression::Simple(Value::Int(value_node!(2, 1, 15))),
            ],
            pos: Position::new(1, 1),
        })
    );
}

#[test]
fn test_call_parse() {
    assert_error!(call_expression("foo"));
    assert_error!(call_expression("foo ("));
    assert_error!(call_expression("foo (1"));
    assert_error!(call_expression("foo (1,"));
    assert_error!(call_expression("foo (1,2"));

    assert_parse!(
        call_expression("foo (1, \"foo\")"),
        Expression::Call(CallDef {
            macroref: make_selector!(make_expr!("foo", 1, 1), 1, 1),
            arglist: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 6))),
                Expression::Simple(Value::Str(value_node!("foo".to_string(), 1, 9))),
            ],
            pos: Position::new(1, 1),
        })
    );

    assert_parse!(
        call_expression("foo.bar (1, \"foo\")"),
        Expression::Call(CallDef {
            macroref: make_selector!(make_expr!("foo") => [ make_tok!("bar", 1, 5) ] => 1, 1),
            arglist: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 10))),
                Expression::Simple(Value::Str(value_node!("foo".to_string(), 1, 13))),
            ],
            pos: Position::new(1, 1),
        })
    );
}

#[test]
fn test_select_parse() {
    assert_error!(select_expression("select"));
    assert_error!(select_expression("select foo"));
    assert_error!(select_expression("select foo, 1"));
    assert_error!(select_expression("select foo, 1, {"));

    assert_parse!(
        select_expression("select foo, 1, { foo = 2 }"),
        Expression::Select(SelectDef {
            val: Box::new(Expression::Simple(Value::Selector(make_selector!(
                make_expr!("foo", 1, 8),
                1,
                8
            )))),
            default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 13)))),
            tuple: vec![(
                make_tok!("foo", 1, 18),
                Expression::Simple(Value::Int(value_node!(2, 1, 24))),
            )],
            pos: Position::new(1, 1),
        })
    );
}

#[test]
fn test_macro_expression_parsing() {
    assert_error!(macro_expression("foo"));
    assert_error!(macro_expression("macro \"foo\""));
    assert_error!(macro_expression("macro 1"));
    assert_error!(macro_expression("macro"));
    assert_error!(macro_expression("macro ("));
    assert_error!(macro_expression("macro (arg"));
    assert_error!(macro_expression("macro (arg, arg2"));
    assert_error!(macro_expression("macro (arg1, arg2) =>"));
    assert_error!(macro_expression("macro (arg1, arg2) => {"));
    assert_error!(macro_expression("macro (arg1, arg2) => { foo"));
    assert_error!(macro_expression("macro (arg1, arg2) => { foo ="));

    assert_parse!(
        macro_expression("macro (arg1, arg2) => {foo=1,bar=2}"),
        Expression::Macro(MacroDef {
            argdefs: vec![
                value_node!("arg1".to_string(), 1, 8),
                value_node!("arg2".to_string(), 1, 14),
            ],
            fields: vec![
                (
                    make_tok!("foo", 1, 24),
                    Expression::Simple(Value::Int(value_node!(1, 1, 28))),
                ),
                (
                    make_tok!("bar", 1, 30),
                    Expression::Simple(Value::Int(value_node!(2, 1, 34))),
                ),
            ],
            pos: Position::new(1, 1),
        })
    );
}

#[test]
fn test_copy_parse() {
    assert_error!(copy_expression("{}"));
    assert_error!(copy_expression("foo"));
    assert_error!(copy_expression("foo{"));

    assert_parse!(
        copy_expression("foo{}"),
        Expression::Copy(CopyDef {
            selector: make_selector!(make_expr!("foo", 1, 1), 1, 1),
            fields: Vec::new(),
            pos: Position::new(1, 1),
        })
    );

    assert_parse!(
        copy_expression("foo{bar=1}"),
        Expression::Copy(CopyDef {
            selector: make_selector!(make_expr!("foo", 1, 1), 1, 1),
            fields: vec![(
                make_tok!("bar", 1, 5),
                Expression::Simple(Value::Int(value_node!(1, 1, 9))),
            )],
            pos: Position::new(1, 1),
        })
    );
    assert_parse!(
        copy_expression("foo{bar=1,}"),
        Expression::Copy(CopyDef {
            selector: make_selector!(make_expr!("foo", 1, 1), 1, 1),
            fields: vec![(
                make_tok!("bar", 1, 5),
                Expression::Simple(Value::Int(value_node!(1, 1, 9))),
            )],
            pos: Position::new(1, 1),
        })
    );
}

#[test]
fn test_grouped_expression_parse() {
    assert_error!(grouped_expression("foo"));
    assert_error!(grouped_expression("(foo"));
    assert_parse!(
        grouped_expression("(foo)"),
        Expression::Grouped(Box::new(Expression::Simple(Value::Selector(
            make_selector!(make_expr!("foo", 1, 2), 1, 2)
        ))))
    );
    assert_parse!(
        grouped_expression("(1 + 1)"),
        Expression::Grouped(Box::new(Expression::Binary(BinaryOpDef {
            kind: BinaryExprType::Add,
            left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 2)))),
            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 6)))),
            pos: Position::new(1, 2),
        })))
    );
}

#[test]
fn test_list_value_parse() {
    assert_error!(list_value("foo"));
    assert_error!(list_value("[foo"));
    assert_error!(list_value("// commen\n[foo"));

    assert_parse!(
        list_value("[foo]"),
        Value::List(ListDef {
            elems: vec![Expression::Simple(Value::Selector(make_selector!(
                make_expr!("foo", 1, 2),
                1,
                2
            )))],
            pos: Position::new(1, 1),
        })
    );

    assert_parse!(
        list_value("[1, 1]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                Expression::Simple(Value::Int(value_node!(1, 1, 5))),
            ],
            pos: Position::new(1, 1),
        })
    );

    assert_parse!(
        list_value("[1, 1,]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                Expression::Simple(Value::Int(value_node!(1, 1, 5))),
            ],
            pos: Position::new(1, 1),
        })
    );

    assert_parse!(
        list_value("// comment\n[1, 1]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, 2, 2))),
                Expression::Simple(Value::Int(value_node!(1, 2, 5))),
            ],
            pos: Position::new(2, 1),
        })
    );

    assert_parse!(
        list_value("[// comment\n1, 1]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, 2, 2))),
                Expression::Simple(Value::Int(value_node!(1, 2, 5))),
            ],
            pos: Position::new(1, 1),
        })
    );

    assert_parse!(
        list_value("[1, // comment\n1]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                Expression::Simple(Value::Int(value_node!(1, 2, 1))),
            ],
            pos: Position::new(1, 1),
        })
    );

    assert_parse!(
        list_value("[1, 1 // comment\n]"),
        Value::List(ListDef {
            elems: vec![
                Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                Expression::Simple(Value::Int(value_node!(1, 1, 5))),
            ],
            pos: Position::new(1, 1),
        })
    );
}

#[test]
fn test_tuple_parse() {
    assert_error!(tuple("{"));
    assert_error!(tuple("{ foo"));
    assert_error!(tuple("{ foo ="));
    assert_error!(tuple("{ foo = 1"));
    assert_error!(tuple("{ foo = 1,"));
    assert_error!(tuple("{ foo = 1, bar ="));
    assert_error!(tuple("// comment\n{ foo = 1, bar ="));

    assert_parse!(tuple("{ }"), Value::Tuple(value_node!(vec![], 1, 1)));

    assert_parse!(
        tuple("{ foo = 1 }"),
        Value::Tuple(value_node!(
            vec![(
                make_tok!("foo", 1, 3),
                Expression::Simple(Value::Int(value_node!(1, 1, 9))),
            )],
            1,
            1
        ))
    );

    assert_parse!(
        tuple("// comment\n{ foo = 1 }"),
        Value::Tuple(value_node!(
            vec![(
                make_tok!("foo", 2, 3),
                Expression::Simple(Value::Int(value_node!(1, 2, 9))),
            )],
            1,
            1
        ))
    );

    assert_parse!(
        tuple("{// comment\n foo = 1 }"),
        Value::Tuple(value_node!(
            vec![(
                make_tok!("foo", 2, 2),
                Expression::Simple(Value::Int(value_node!(1, 2, 8))),
            )],
            1,
            1
        ))
    );

    assert_parse!(
        tuple("{ foo = 1// comment\n }"),
        Value::Tuple(value_node!(
            vec![(
                make_tok!("foo", 1, 3),
                Expression::Simple(Value::Int(value_node!(1, 1, 9))),
            )],
            1,
            1
        ))
    );

    assert_parse!(
        tuple("{ foo = 1, bar = \"1\" }"),
        Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", 1, 3),
                    Expression::Simple(Value::Int(value_node!(1, 1, 9))),
                ),
                (
                    make_tok!("bar", 1, 12),
                    Expression::Simple(Value::Str(value_node!(
                        "1".to_string(),
                        Position::new(1, 18)
                    ))),
                ),
            ],
            1,
            1
        ))
    );
    assert_parse!(
        tuple("{ foo = 1, // comment\nbar = \"1\" }"),
        Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", 1, 3),
                    Expression::Simple(Value::Int(value_node!(1, 1, 9))),
                ),
                (
                    make_tok!("bar", 2, 1),
                    Expression::Simple(Value::Str(value_node!(
                        "1".to_string(),
                        Position::new(2, 7)
                    ))),
                ),
            ],
            1,
            1
        ))
    );
    assert_parse!(
        tuple("{ foo = 1, bar = {} }"),
        Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", 1, 3),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9)))),
                ),
                (
                    make_tok!("bar", 1, 12),
                    Expression::Simple(Value::Tuple(value_node!(Vec::new(), Position::new(1, 17)))),
                ),
            ],
            1,
            1
        ))
    );
    assert_parse!(
        tuple("{ foo = 1, bar = {}, }"),
        Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", 1, 3),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9)))),
                ),
                (
                    make_tok!("bar", 1, 12),
                    Expression::Simple(Value::Tuple(value_node!(Vec::new(), Position::new(1, 17)))),
                ),
            ],
            1,
            1
        ))
    );

    assert_parse!(
        expression("{ foo = 1, lst = [1, 2, 3], }"),
        Expression::Simple(Value::Tuple(value_node!(
            vec![
                (
                    make_tok!("foo", 1, 3),
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9)))),
                ),
                (
                    make_tok!("lst", 1, 12),
                    Expression::Simple(Value::List(ListDef {
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, Position::new(1, 19)))),
                            Expression::Simple(Value::Int(value_node!(2, Position::new(1, 22)))),
                            Expression::Simple(Value::Int(value_node!(3, Position::new(1, 25)))),
                        ],
                        pos: Position {
                            line: 1,
                            column: 18,
                        },
                    })),
                ),
            ],
            1,
            1
        )))
    );
}

#[test]
fn test_field_list_parse() {
    let mut f_list = "foo = 1, quux = 2;";
    assert_parse!(
        field_list(f_list),
        vec![
            (make_tok!("foo", 1, 1), make_expr!(1 => int, 1, 7)),
            (make_tok!("quux", 1, 10), make_expr!(2 => int, 1, 17)),
        ]
    );

    f_list = "foo = 1, // comment\nquux = 2;";
    assert_parse!(
        field_list(f_list),
        vec![
            (make_tok!("foo", 1, 1), make_expr!(1 => int, 1, 7)),
            (make_tok!("quux", 2, 1), make_expr!(2 => int, 2, 8)),
        ]
    );

    f_list = "foo = 1,\n// comment\nquux = 2;";
    assert_parse!(
        field_list(f_list),
        vec![
            (make_tok!("foo", 1, 1), make_expr!(1 => int, 1, 7)),
            (make_tok!("quux", 3, 1), make_expr!(2 => int, 3, 8)),
        ]
    );
    f_list = "foo = 1,\nquux = [1, 2],";
    assert_parse!(
        field_list(f_list),
        vec![
            (make_tok!("foo", 1, 1), make_expr!(1 => int, 1, 7)),
            (
                make_tok!("quux", 2, 1),
                Expression::Simple(Value::List(ListDef {
                    elems: vec![
                        Expression::Simple(Value::Int(value_node!(1, Position::new(2, 9)))),
                        Expression::Simple(Value::Int(value_node!(2, Position::new(2, 12)))),
                    ],
                    pos: Position::new(2, 8),
                })),
            ),
        ]
    );
}

#[test]
fn test_field_value_parse() {
    assert_error!(field_value("foo"));
    assert_error!(field_value("// comment\nfoo"));
    assert_error!(field_value("foo ="));

    assert_parse!(
        field_value("foo = 1"),
        (
            make_tok!("foo", 1, 1),
            Expression::Simple(Value::Int(value_node!(1, 1, 7)))
        )
    );
    assert_parse!(
        field_value("foo = 1 // foo comment\n"),
        (
            make_tok!("foo", 1, 1),
            Expression::Simple(Value::Int(value_node!(1, 1, 7)))
        )
    );
    assert_parse!(
        field_value("foo // foo comment\n = 1"),
        (
            make_tok!("foo", 1, 1),
            Expression::Simple(Value::Int(value_node!(1, 2, 4)))
        )
    );
    assert_parse!(
        field_value("// foo comment\nfoo = 1"),
        (
            make_tok!("foo", 2, 1),
            Expression::Simple(Value::Int(value_node!(1, 2, 7)))
        )
    );
    assert_parse!(
        field_value("foo = \"1\""),
        (
            make_tok!("foo", 1, 1),
            Expression::Simple(Value::Str(value_node!("1".to_string(), 1, 7)))
        )
    );
    assert_parse!(
        field_value("foo = bar "),
        (
            make_tok!("foo", 1, 1),
            Expression::Simple(Value::Selector(make_selector!(
                make_expr!("bar", 1, 7),
                1,
                7
            )))
        )
    );
    assert_parse!(
        field_value("foo = bar.baz "),
        (
            make_tok!("foo", 1, 1),
            Expression::Simple(Value::Selector(
                make_selector!(make_expr!("bar", 1, 7) => [ make_tok!("baz", 1, 11) ] => 1, 7),
            ))
        )
    );
    assert_parse!(
        field_value("foo = [1,2], "),
        (
            make_tok!("foo", 1, 1),
            Expression::Simple(Value::List(ListDef {
                elems: vec![
                    Expression::Simple(Value::Int(value_node!(1, Position::new(1, 8)))),
                    Expression::Simple(Value::Int(value_node!(2, Position::new(1, 10)))),
                ],
                pos: Position::new(1, 7),
            }))
        )
    );
}

#[test]
fn test_number_parsing() {
    assert_error!(number("."));
    assert_error!(number(". "));
    assert_parse!(number("1.0"), Value::Float(value_node!(1.0, 1, 1)));
    assert_parse!(number("1."), Value::Float(value_node!(1.0, 1, 1)));
    assert_parse!(number("1"), Value::Int(value_node!(1, 1, 1)));
    assert_parse!(number(".1"), Value::Float(value_node!(0.1, 1, 1)));
}

#[test]
fn test_parse() {
    let bad_input = LocatedSpan::new("import mylib as lib;");
    let bad_result = parse(bad_input);
    assert!(bad_result.is_err());

    // Valid parsing tree
    let input = LocatedSpan::new("import \"mylib\" as lib;let foo = 1;1+1;");
    let result = parse(input);
    assert!(result.is_ok(), format!("Expected Ok, Got {:?}", result));
    let tpl = result.unwrap();
    assert_eq!(
        tpl,
        vec![
            Statement::Import(ImportDef {
                path: make_tok!(QUOT => "mylib", 1, 8),
                name: make_tok!("lib", 1, 19),
            }),
            Statement::Let(LetDef {
                name: make_tok!("foo", 1, 27),
                value: Expression::Simple(Value::Int(value_node!(1, 1, 33))),
            }),
            Statement::Expression(Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 35)))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 37)))),
                pos: Position::new(1, 35),
            })),
        ]
    );
}
