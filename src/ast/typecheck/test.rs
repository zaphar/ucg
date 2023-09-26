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
    ($e:expr, $shap:expr) => {{
        let mut checker = Checker::new();
        let mut expr = parse($e.into(), None).unwrap();
        checker.walk_statement_list(expr.iter_mut().collect());
        let maybe_shape = checker.pop_shape();
        let result = checker.result();
        assert!(result.is_ok(), "We expect this to typecheck successfully.");
        assert!(
            result.unwrap().is_empty(),
            "We don't expect a symbol table entry."
        );
        assert!(maybe_shape.is_some(), "We got a shape out of it");
        assert_eq!(maybe_shape.unwrap(), $shap);
    }};
}

#[test]
fn simple_binary_typecheck() {
    assert_type_success!(
        "1 + 1;",
        Shape::Int(PositionedItem::new(1, Position::new(1, 1, 0)))
    );
    assert_type_success!(
        "\"\" + \"\";",
        Shape::Str(PositionedItem::new("".into(), Position::new(1, 1, 0)))
    );
    assert_type_success!(
        "1.0 + 1.0;",
        Shape::Float(PositionedItem::new(1.0, Position::new(1, 1, 0)))
    );
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
                Shape::Int(PositionedItem::new_with_pos(1, Position::new(1, 8, 7)))
            ),],
            Position::new(1, 1, 0)
        ))
    );
    assert_type_success!(
        "[1] + [2];",
        Shape::List(crate::ast::NarrowedShape::new(
            vec![Shape::Int(PositionedItem::new_with_pos(
                1,
                Position::new(1, 1, 0)
            ))],
            1,
            1,
            0
        ))
    );
    assert_type_success!(
        "[1, 1.0] + [1, 2.0];",
        Shape::List(crate::ast::NarrowedShape::new(
            vec![
                Shape::Int(PositionedItem::new_with_pos(1, Position::new(1, 1, 0))),
                Shape::Float(PositionedItem::new_with_pos(1.0, Position::new(1, 1, 0))),
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

#[test]
fn infer_symbol_type_test() {
    // foo should be determined to be int
    let expr = "1 + foo".into();
    let symbol: Rc<str> = "foo".into();
    let mut checker = Checker::new();
    checker
        .symbol_table
        .insert(symbol.clone(), Shape::Hole(PositionedItem::new(symbol.clone(), Position::new(0, 0, 0))));
    let tokens = tokenize(expr, None).unwrap();
    let token_iter = SliceIter::new(&tokens);
    let expr = expression(token_iter);
    if let abortable_parser::Result::Complete(_, mut expr) = expr {
        checker.walk_expression(&mut expr);
        dbg!(&checker.symbol_table);
        assert_eq!(
            checker.symbol_table[&symbol],
            Shape::Int(PositionedItem::new(1, Position::new(0, 0, 0)))
        );
    }
}
