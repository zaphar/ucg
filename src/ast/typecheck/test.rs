use std::convert::Into;

use crate::ast::walk::Walker;
use crate::ast::Position;
use crate::parse;

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
    }}
}

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

// TODO Test that leverages symbol tables and let bindings.
#[test]
fn simple_binary_typefail() {
    assert_type_fail!("1 + true;", "Expected int but got boolean", Position::new(1, 5, 4));
    assert_type_fail!("1 + \"\";", "Expected int but got str", Position::new(1, 5, 4));
    assert_type_fail!("1 + [];", "Expected int but got list", Position::new(1, 5, 4));
    assert_type_fail!("1 + {};", "Expected int but got tuple", Position::new(1, 5, 4));
}

#[test]
fn multiple_binary_typefail() {
    assert_type_fail!("1 + 1 + true;", "Expected int but got boolean", Position::new(1, 9, 8));
}
