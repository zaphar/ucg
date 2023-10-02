use std::convert::Into;

use abortable_parser::SliceIter;

use crate::ast::walk::Walker;
use crate::ast::{Position, PositionedItem};
use crate::ast::{Token, TokenType};
use crate::parse::{expression, parse};
use crate::tokenizer::tokenize;

use super::*;

macro_rules! assert_type_fail {
    ($e:expr, $msg:expr, $pos:expr) => {{
        let mut checker = Checker::new();
        let mut expr = parse($e.into(), None).unwrap();
        checker.walk_statement_list(expr.iter_mut().collect());
        let result = dbg!(checker.result());
        assert!(result.is_err(), "We expect this to fail a typecheck.");
        let err = result.unwrap_err();
        assert_eq!(err.msg, $msg);
        assert_eq!(err.pos.unwrap(), $pos);
    }};
}

macro_rules! assert_type_success {
    ($e:expr, $shape:expr) => {{
        assert_type_success!($e, $shape, BTreeMap::new());
    }};
    ($e:expr, $shape:expr, $sym_table:expr) => {{
        let mut checker = Checker::new().with_symbol_table($sym_table);
        let mut expr = parse($e.into(), None).unwrap();
        checker.walk_statement_list(expr.iter_mut().collect());
        let maybe_shape = checker.pop_shape();
        let result = checker.result();
        assert!(result.is_ok(), "We expect this to typecheck successfully.");
        assert!(maybe_shape.is_some(), "We got a shape out of it");
        assert_eq!(maybe_shape.unwrap(), $shape);
    }};
}

#[test]
fn simple_binary_typecheck() {
    assert_type_success!("1 + 1;", Shape::Int(Position::new(1, 1, 0)));
    assert_type_success!("\"\" + \"\";", Shape::Str(Position::new(1, 1, 0)));
    assert_type_success!("1.0 + 1.0;", Shape::Float(Position::new(1, 1, 0)));
    assert_type_success!(
        "[] + [];",
        Shape::List(crate::ast::NarrowedShape::new(vec![], 1, 1, 0))
    );
    assert_type_success!(
        "{} + {};",
        Shape::Tuple(PositionedItem::new(vec![], Position::new(1, 1, 0)))
    );
    // TODO(jwall): + isn't valid for tuples.
    assert_type_success!(
        "{foo = 1} + {foo = 1};",
        Shape::Tuple(PositionedItem::new(
            vec![(
                Token {
                    typ: TokenType::BAREWORD,
                    fragment: "foo".into(),
                    pos: Position::new(1, 2, 1)
                },
                Shape::Int(Position::new(1, 8, 7))
            ),],
            Position::new(1, 1, 0)
        ))
    );
    assert_type_success!(
        "[1] + [2];",
        Shape::List(crate::ast::NarrowedShape::new(
            vec![Shape::Int(Position::new(1, 2, 1))],
            1,
            1,
            0
        ))
    );
    assert_type_success!(
        "[1, 1.0] + [1, 2.0];",
        Shape::List(crate::ast::NarrowedShape::new(
            vec![
                Shape::Int(Position::new(1, 2, 1)),
                Shape::Float(Position::new(1, 5, 4)),
            ],
            1,
            1,
            0
        ))
    );
}

// TODO Test that leverages symbol tables and let bindings.
#[test]
fn simple_binary_typefail() {
    assert_type_fail!(
        "1 + true;",
        "Expected int but got boolean",
        Position::new(1, 5, 4)
    );
    assert_type_fail!(
        "1 + \"\";",
        "Expected int but got str",
        Position::new(1, 5, 4)
    );
    assert_type_fail!(
        "1 + [];",
        "Expected int but got list",
        Position::new(1, 5, 4)
    );
    assert_type_fail!(
        "1 + {};",
        "Expected int but got tuple",
        Position::new(1, 5, 4)
    );
    assert_type_fail!(
        "[1] + [1.0];",
        "Incompatible List Shapes",
        Position::new(1, 7, 6)
    );
    // TODO(jwall): + isn't valid for tuples.
    assert_type_fail!(
        "{foo = 1} + {foo = 1.0};",
        "Incompatible Tuple Shapes",
        Position::new(1, 13, 12)
    );
    assert_type_fail!(
        "{foo = 1} + {bar = 1};",
        "Incompatible Tuple Shapes",
        Position::new(1, 13, 12)
    );
}

#[test]
fn multiple_binary_typefail() {
    assert_type_fail!(
        "1 + 1 + true;",
        "Expected int but got boolean",
        Position::new(1, 9, 8)
    );
}

macro_rules! infer_symbol_test {
    ($e:expr, $sym_list:expr, $sym_init_list:expr) => {{
        let expr = $e.into();
        let mut checker = Checker::new();
        for (idx, shape) in $sym_init_list.iter().enumerate() {
            let symbol = $sym_list[idx].0.clone();
            checker
                .symbol_table
                .insert(symbol.clone(), shape.clone());
        }
        let tokens = tokenize(expr, None).unwrap();
        let token_iter = SliceIter::new(&tokens);
        let expr = expression(token_iter);
        if let abortable_parser::Result::Complete(_, mut expr) = expr {
            checker.walk_expression(&mut expr);
            dbg!(&checker.symbol_table);
            for (sym, shape) in $sym_list {
                assert_eq!(
                    checker.symbol_table[&sym],
                     shape,
                );
            }
        } else {
            assert!(false, "Expression failed to parse");
        }
    }}
}

#[test]
fn infer_symbol_type_test() {
    // foo should be determined to be int
    let foo = Into::<Rc<str>>::into("foo");
    let bar = Into::<Rc<str>>::into("bar");
    let table = vec![
        (
            "1 + foo",
            vec![(foo.clone(), Shape::Int(Position::new(0, 0, 0)))],
            vec![Shape::Hole(PositionedItem::new(
                foo.clone(),
                Position::new(0, 0, 0),
            ))],
        ),
        (
            "bar + foo",
            vec![
                (foo.clone(), Shape::Float(Position::new(0, 0, 0))),
                (bar.clone(), Shape::Float(Position::new(0, 0, 0))),
            ],
            vec![
                Shape::Hole(PositionedItem::new(foo.clone(), Position::new(0, 0, 0))),
                Shape::Float(Position::new(0, 0, 0)),
            ],
        ),
    ];
    for (expr, sym_list, sym_init_list) in table {
        infer_symbol_test!(expr, sym_list, sym_init_list)
    }
}

#[test]
fn infer_func_type_test() {
    let mut args = BTreeMap::new();
    let foo = Into::<Rc<str>>::into("foo");
    let bar = Into::<Rc<str>>::into("bar");
    args.insert(
        foo.clone(),
        Shape::Int(Position {
            file: None,
            line: 1,
            column: 6,
            offset: 5,
        }),
    );
    let ret = Shape::Int(Position {
        file: None,
        line: 1,
        column: 1,
        offset: 0,
    })
    .into();
    assert_type_success!(
        "func(foo) => foo + 1;",
        Shape::Func(FuncShapeDef { args, ret })
    );
    let mut symbol_table = BTreeMap::new();
    symbol_table.insert(
        bar.clone(),
        Shape::Int(Position {
                file: None,
                line: 1,
                column: 20,
                offset: 19,
        }),
    );
    let mut args = BTreeMap::new();
    args.insert(
        foo.clone(),
        Shape::Int(Position {
                file: None,
                line: 1,
                column: 6,
                offset: 5,
        }),
    );
    assert_type_success!(
        "func(foo) => foo + bar;",
        Shape::Func(FuncShapeDef {
            args: args,
            ret: Shape::Int(Position {
                    file: None,
                    line: 1,
                    column: 1,
                    offset: 0,
            })
            .into()
        }),
        symbol_table
    );
}

//#[test]
//fn infer_select_shape() {
//    assert_type_success!(
//        r#"select () => { true = "foo", false = 1 }"#,
//        Shape::Narrowed(NarrowedShape { pos: Position { file: None, line: 0, column: 0, offset: 0 }, types: vec![
//            Shape::Str(PositionedItem { pos: , val: () })
//        ] }))
//}
