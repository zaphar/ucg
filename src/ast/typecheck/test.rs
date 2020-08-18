use std::convert::Into;

use crate::ast::walk::Walker;
use crate::ast::Position;
use crate::parse;

use super::Checker;

#[test]
fn simple_binary_typecheck() {
    let mut checker = Checker::new();
    let expr_str = "1 + 1;";
    let mut expr = parse(expr_str.into(), None).unwrap();
    checker.walk_statement_list(expr.iter_mut().collect());
    let result = checker.result();
    assert!(result.is_ok(), "We expect this to typecheck successfully.");
    assert!(
        result.unwrap().is_empty(),
        "We don't expect a symbol table entry."
    );
}

#[test]
fn simple_binary_typefail() {
    let mut checker = Checker::new();
    let expr_str = "1 + true;";
    let mut expr = parse(expr_str.into(), None).unwrap();
    checker.walk_statement_list(expr.iter_mut().collect());
    let result = checker.result();
    assert!(result.is_err(), "We expect this to fail a typecheck.");
    let err = result.unwrap_err();
    assert_eq!(err.msg, "Expected int but got boolean");
    assert_eq!(err.pos.unwrap(), Position::new(1, 5, 4));
}

#[test]
fn multiple_binary_typefail() {
    let mut checker = Checker::new();
    let expr_str = "1 + 1 + true;";
    let mut expr = parse(expr_str.into(), None).unwrap();
    checker.walk_statement_list(expr.iter_mut().collect());
    let result = checker.result();
    assert!(result.is_err(), "We expect this to fail a typecheck.");
    let err = result.unwrap_err();
    assert_eq!(err.msg, "Expected int but got boolean");
    assert_eq!(err.pos.unwrap(), Position::new(1, 9, 8));
}
