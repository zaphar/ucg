use std::convert::Into;
use std::rc::Rc;

use abortable_parser::SliceIter;

use crate::ast::walk::Walker;
use crate::ast::{Position, PositionedItem};
use crate::parse::{expression, parse};
use crate::tokenizer::tokenize;

use super::*;

macro_rules! assert_type_fail {
    ($e:expr_2021, $msg:expr_2021, $pos:expr_2021) => {{
        let mut checker = Checker::new();
        let mut expr = parse($e.into(), None).unwrap();
        checker.walk_statement_list(expr.iter_mut().collect());
        let result = checker.result();
        assert!(result.is_err(), "We expect this to fail a typecheck.");
        let err = result.unwrap_err();
        assert_eq!(err.msg, $msg);
        assert_eq!(err.pos.unwrap(), $pos);
    }};
}

macro_rules! assert_type_ok {
    ($e:expr_2021) => {{
        let mut checker = Checker::new();
        let mut stmts = parse($e.into(), None).unwrap();
        checker.walk_statement_list(stmts.iter_mut().collect());
        let result = checker.result();
        assert!(
            result.is_ok(),
            "We expect this to typecheck successfully. {:?}",
            result
        );
    }};
}

macro_rules! assert_type_err {
    ($e:expr_2021) => {{
        let mut checker = Checker::new();
        let mut stmts = parse($e.into(), None).unwrap();
        checker.walk_statement_list(stmts.iter_mut().collect());
        let result = checker.result();
        assert!(result.is_err(), "We expect this to fail a typecheck.");
    }};
}

macro_rules! assert_type_success {
    ($e:expr_2021, $shape:expr_2021) => {{
        assert_type_success!($e, $shape, BTreeMap::new());
    }};
    ($e:expr_2021, $shape:expr_2021, $sym_table:expr_2021) => {{
        let mut checker = Checker::new().with_symbol_table($sym_table);
        let mut expr = parse($e.into(), None).unwrap();
        checker.walk_statement_list(expr.iter_mut().collect());
        let maybe_shape = checker.pop_shape();
        let result = checker.result();
        assert!(
            result.is_ok(),
            "We expect this to typecheck successfully. {:?}",
            result
        );
        assert!(maybe_shape.is_some(), "We got a shape out of it");
        assert_eq!(maybe_shape.unwrap(), $shape);
    }};
    ($e:expr_2021, $shape:expr_2021, $sym_table:expr_2021, $expected_sym:expr_2021) => {{
        let mut checker = Checker::new().with_symbol_table($sym_table);
        let mut expr = parse($e.into(), None).unwrap();
        checker.walk_statement_list(expr.iter_mut().collect());
        let maybe_shape = checker.pop_shape();
        assert_eq!(checker.symbol_table[$expected_sym], $shape);
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
                PositionedItem::new("foo".into(), Position::new(1, 2, 1)),
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
    ($e:expr_2021, $sym_list:expr_2021, $sym_init_list:expr_2021) => {{
        let expr = $e.into();
        let mut symbol_table = BTreeMap::new();
        for (idx, shape) in $sym_init_list.iter().enumerate() {
            let symbol = $sym_list[idx].0.clone();
            symbol_table.insert(symbol.clone(), shape.clone());
        }
        let tokens = tokenize(expr, None).unwrap();
        let token_iter = SliceIter::new(&tokens);
        let expr = expression(token_iter);
        if let abortable_parser::Result::Complete(_, expr) = expr {
            // Use DeriveShape directly instead of walking with the Checker,
            // since the Checker only drives shape derivation from visit_statement.
            expr.derive_shape(&mut symbol_table);
            for (sym, shape) in $sym_list {
                assert_eq!(symbol_table[&sym], shape,);
            }
        } else {
            assert!(false, "Expression failed to parse");
        }
    }};
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
        (
            "bar.foo",
            vec![(
                bar.clone(),
                Shape::Tuple(PositionedItem::new(
                    vec![(
                        PositionedItem::new(foo.clone(), Position::new(1, 5, 4)),
                        Shape::Narrowed(NarrowedShape {
                            pos: Position::new(0, 0, 0),
                            types: NarrowingShape::Any,
                        }),
                    )],
                    Position::new(0, 0, 0),
                )),
            )],
            vec![Shape::Hole(PositionedItem::new(
                bar.clone(),
                Position::new(0, 0, 0),
            ))],
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
        Shape::Func(FuncShapeDef {
            args,
            arg_order: vec![foo.clone()],
            ret,
        })
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
            arg_order: vec![foo.clone()],
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

#[test]
fn infer_select_shape() {
    assert_type_success!(
        r#"select (foo) => { true = "foo", false = 1 };"#,
        Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::Str(Position::new(1, 26, 25)),
                Shape::Int(Position::new(1, 41, 40)),
            ],
            Position {
                file: None,
                line: 1,
                column: 1,
                offset: 0
            },
        ))
    );
    assert_type_success!(
        r#"select (foo) => { true = "foo", false = { } };"#,
        Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::Str(Position::new(1, 26, 25)),
                Shape::Tuple(PositionedItem {
                    pos: Position {
                        file: None,
                        line: 1,
                        column: 41,
                        offset: 40
                    },
                    val: vec![],
                }),
            ],
            Position {
                file: None,
                line: 1,
                column: 1,
                offset: 0
            },
        ))
    );
    assert_type_success!(
        r#"select (foo) => { true = { }, false = { } };"#,
        Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![Shape::Tuple(PositionedItem {
                pos: Position {
                    file: None,
                    line: 1,
                    column: 26,
                    offset: 25
                },
                val: vec![],
            }),],
            Position {
                file: None,
                line: 1,
                column: 1,
                offset: 0
            },
        ))
    );
    assert_type_success!(
        r#"select (foo) => { true = { foo = 1, }, false = { bar = "foo" } };"#,
        Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::Tuple(PositionedItem {
                    pos: Position {
                        file: None,
                        line: 1,
                        column: 26,
                        offset: 25
                    },
                    val: vec![(
                        PositionedItem::new(
                            "foo".into(),
                            Position {
                                file: None,
                                line: 1,
                                column: 28,
                                offset: 27
                            }
                        ),
                        Shape::Int(Position {
                            file: None,
                            line: 1,
                            column: 34,
                            offset: 33
                        })
                    )]
                }),
                Shape::Tuple(PositionedItem {
                    pos: Position {
                        file: None,
                        line: 1,
                        column: 48,
                        offset: 47
                    },
                    val: vec![(
                        PositionedItem::new(
                            "bar".into(),
                            Position {
                                file: None,
                                line: 1,
                                column: 50,
                                offset: 49
                            }
                        ),
                        Shape::Str(Position {
                            file: None,
                            line: 1,
                            column: 56,
                            offset: 55
                        })
                    )]
                })
            ],
            Position {
                file: None,
                line: 1,
                column: 1,
                offset: 0
            },
        ))
    );
    assert_type_success!(
        r#"select (foo) => { true = { foo = 1, bar = "quux" }, false = { bar = "foo" } };"#,
        Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::Tuple(PositionedItem {
                    pos: Position {
                        file: None,
                        line: 1,
                        column: 26,
                        offset: 25
                    },
                    val: vec![
                        (
                            PositionedItem::new(
                                "foo".into(),
                                Position {
                                    file: None,
                                    line: 1,
                                    column: 28,
                                    offset: 27
                                }
                            ),
                            Shape::Int(Position {
                                file: None,
                                line: 1,
                                column: 34,
                                offset: 33
                            })
                        ),
                        (
                            PositionedItem::new(
                                "bar".into(),
                                Position {
                                    file: None,
                                    line: 1,
                                    column: 37,
                                    offset: 36
                                }
                            ),
                            Shape::Str(Position {
                                file: None,
                                line: 1,
                                column: 43,
                                offset: 42
                            })
                        ),
                    ]
                }),
                Shape::Tuple(PositionedItem {
                    pos: Position {
                        file: None,
                        line: 1,
                        column: 61,
                        offset: 60
                    },
                    val: vec![(
                        PositionedItem::new(
                            "bar".into(),
                            Position {
                                file: None,
                                line: 1,
                                column: 63,
                                offset: 62
                            }
                        ),
                        Shape::Str(Position {
                            file: None,
                            line: 1,
                            column: 69,
                            offset: 68
                        })
                    )]
                })
            ],
            Position {
                file: None,
                line: 1,
                column: 1,
                offset: 0
            },
        ))
    );
    assert_type_success!(
        r#"select (foo) => { true = [ "quux" ], false = [ 1 ] };"#,
        Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::List(NarrowedShape::new_with_pos(
                    vec![Shape::Str(Position {
                        file: None,
                        line: 1,
                        column: 28,
                        offset: 27
                    })],
                    Position {
                        file: None,
                        line: 1,
                        column: 26,
                        offset: 25
                    },
                )),
                Shape::List(NarrowedShape::new_with_pos(
                    vec![Shape::Int(Position {
                        file: None,
                        line: 1,
                        column: 48,
                        offset: 47
                    })],
                    Position {
                        file: None,
                        line: 1,
                        column: 46,
                        offset: 45
                    },
                ))
            ],
            Position {
                file: None,
                line: 1,
                column: 1,
                offset: 0
            },
        ))
    );
}

fn parse_expression(expr: &str) -> Option<Expression> {
    let tokens = tokenize(expr.into(), None).unwrap();
    let token_iter = SliceIter::new(&tokens);
    let expr = expression(token_iter);
    if let abortable_parser::Result::Complete(_, expr) = expr {
        return Some(expr);
    }
    None
}

#[test]
fn func_type_equivalence() {
    let mut symbol_table = BTreeMap::new();
    let expr1 = "func(arg1) => arg1 + 1;";
    let expr2 = "func(arg2) => arg2 + 1;";
    let shape1 = parse_expression(expr1)
        .unwrap()
        .derive_shape(&mut symbol_table);
    let shape2 = parse_expression(expr2)
        .unwrap()
        .derive_shape(&mut symbol_table);
    assert!(shape1.equivalent(&shape2, &mut symbol_table));
}

#[test]
fn let_stmt_inference() {
    let int_stmt = "let foo = 1;";
    assert_type_success!(
        int_stmt,
        Shape::Int(Position::new(1, 11, 10)),
        BTreeMap::new(),
        &Rc::<str>::from("foo")
    );
    let float_stmt = "let foo = 1.0;";
    assert_type_success!(
        float_stmt,
        Shape::Float(Position::new(1, 11, 10)),
        BTreeMap::new(),
        &Rc::<str>::from("foo")
    );
}

#[test]
fn test_module_inference() {
    let simple_mod_stmt = include_str!("simple_mod.ucg");
    assert_type_success!(
        simple_mod_stmt,
        Shape::Module(ModuleShape {
            items: vec![],
            ret: Box::new(Shape::Int(Position {
                file: None,
                line: 2,
                column: 7,
                offset: 14
            }))
        })
    );
    let complex_mod_stmt = include_str!("complex_module.ucg");
    assert_type_success!(
        complex_mod_stmt,
        Shape::Module(ModuleShape {
            items: vec![(
                PositionedItem {
                    pos: Position {
                        file: None,
                        line: 2,
                        column: 3,
                        offset: 10
                    },
                    val: "arg".into()
                },
                Shape::Int(Position {
                    file: None,
                    line: 2,
                    column: 7,
                    offset: 14
                })
            )],
            ret: Box::new(Shape::Int(Position {
                file: None,
                line: 2,
                column: 7,
                offset: 14
            }))
        })
    );
    let infer_mod_arg_stmt = include_str!("infer_module_arg.ucg");
    assert_type_success!(
        infer_mod_arg_stmt,
        Shape::Module(ModuleShape {
            items: vec![(
                PositionedItem {
                    pos: Position {
                        file: None,
                        line: 2,
                        column: 3,
                        offset: 10
                    },
                    val: "arg".into()
                },
                Shape::Narrowed(NarrowedShape {
                    pos: Position {
                        file: None,
                        line: 2,
                        column: 3,
                        offset: 10
                    },
                    types: NarrowingShape::Any,
                })
            )],
            ret: Box::new(Shape::Int(Position {
                file: None,
                line: 4,
                column: 27,
                offset: 57
            }))
        })
    );
}

#[test]
fn test_dot_expression_tuple_field() {
    // Access a known field on a tuple
    assert_type_success!(
        "let t = {foo = 1}; let x = t.foo;",
        Shape::Int(Position::new(1, 16, 15))
    );
}

#[test]
fn test_dot_expression_tuple_field_not_found() {
    assert_type_fail!(
        "let t = {foo = 1}; let x = t.bar;",
        "Field 'bar' not found in tuple",
        Position::new(1, 30, 29)
    );
}

#[test]
fn test_dot_expression_list_int_index() {
    // Accessing a list by int index gives a narrowed element type
    assert_type_success!(
        "let l = [1, 2]; let x = l.0;",
        Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::Int(Position::new(1, 10, 9)),
                Shape::Int(Position::new(1, 13, 12)),
            ],
            Position::new(1, 9, 8)
        ))
    );
}

#[test]
fn test_fail_expression() {
    // fail produces a bottom/any type
    let mut symbol_table = BTreeMap::new();
    let expr = parse_expression(r#"fail "error""#).unwrap();
    let shape = expr.derive_shape(&mut symbol_table);
    assert!(
        matches!(
            shape,
            Shape::Narrowed(NarrowedShape {
                types: NarrowingShape::Any,
                ..
            })
        ),
        "fail should produce an Any type, got: {:?}",
        shape
    );
}

#[test]
fn test_debug_expression() {
    // debug is transparent - returns inner expression's shape
    assert_type_success!("let x = TRACE 42;", Shape::Int(Position::new(1, 15, 14)));
}

#[test]
fn test_call_expression() {
    // Calling a function returns its return type
    assert_type_success!(
        "let f = func(x) => x + 1; let y = f(2);",
        Shape::Int(Position::new(1, 9, 8))
    );
}

#[test]
fn test_call_wrong_arity() {
    assert_type_fail!(
        "let f = func(x) => x + 1; let y = f(1, 2);",
        "Function expects 1 arguments but got 2",
        Position::new(1, 35, 34)
    );
}

#[test]
fn test_call_wrong_arg_type() {
    // Calling a constrained function with a wrong-type argument should produce a type error.
    assert_type_err!("let f = func(x :: 0) => x + 1; let y = f(\"hello\");");
}

#[test]
fn test_call_correct_arg_type() {
    // Calling a constrained function with the correct type should succeed.
    assert_type_success!(
        "let f = func(x :: 0) => x + 1; let y = f(2);",
        Shape::Int(Position::new(1, 9, 8))
    );
}

#[test]
fn test_func_narrowing() {
    // Two functions with same arity and compatible types can narrow
    let mut symbol_table = BTreeMap::new();
    let f1 = parse_expression("func(x) => x + 1")
        .unwrap()
        .derive_shape(&mut symbol_table);
    let f2 = parse_expression("func(y) => y + 2")
        .unwrap()
        .derive_shape(&mut symbol_table);
    let result = f1.narrow(&f2, &mut symbol_table);
    assert!(
        !matches!(result, Shape::TypeErr(_, _)),
        "Compatible functions should narrow successfully, got: {:?}",
        result
    );
}

#[test]
fn test_assert_type_check() {
    // Assert with bare boolean should fail - assert requires {ok=<bool>, desc=<str>}
    assert_type_err!("assert 1 == 1;");
}

#[test]
fn test_assert_tuple_form() {
    // UCG asserts commonly use tuple form: assert { ok = expr, desc = "..." };
    assert_type_success!(
        r#"assert { ok = 1 == 1, desc = "test" };"#,
        Shape::Tuple(PositionedItem::new(
            vec![
                (
                    PositionedItem::new("ok".into(), Position::new(1, 10, 9)),
                    Shape::Boolean(Position::new(1, 15, 14)),
                ),
                (
                    PositionedItem::new("desc".into(), Position::new(1, 23, 22)),
                    Shape::Str(Position::new(1, 30, 29)),
                ),
            ],
            Position::new(1, 8, 7),
        ))
    );
}

#[test]
fn test_map_expression() {
    // map(func, list) returns a list with the function's return type
    let mapper = "func (x) => x + 1";
    let list = "[1, 2, 3]";
    let code = format!(
        "let m = {}; let l = {}; let result = map(m, l);",
        mapper, list
    );
    assert_type_success!(
        code.as_str(),
        Shape::List(NarrowedShape::new_with_pos(
            vec![Shape::Int(Position::new(1, 9, 8))],
            Position::new(1, 60, 59)
        ))
    );
}

#[test]
fn test_filter_expression() {
    // filter(func, list) returns the same list type
    let filtrator = "func (x) => x > 1";
    let list = "[1, 2, 3]";
    let code = format!(
        "let f = {}; let l = {}; let result = filter(f, l);",
        filtrator, list
    );
    assert_type_success!(
        code.as_str(),
        Shape::List(NarrowedShape::new_with_pos(
            vec![
                Shape::Int(Position::new(1, 37, 36)),
                Shape::Int(Position::new(1, 40, 39)),
                Shape::Int(Position::new(1, 43, 42)),
            ],
            Position::new(1, 36, 35)
        ))
    );
}

#[test]
fn test_type_error_caught_before_vm() {
    // Verify that a deliberate type error is caught during type checking.
    // This would fail at VM execution time, but the type checker should
    // catch it first.
    assert_type_fail!(
        "let x = 1 + true;",
        "Expected int but got boolean",
        Position::new(1, 13, 12)
    );
}

#[test]
fn test_chained_dot_access() {
    // Access a nested tuple field: t.inner.x
    assert_type_success!(
        "let t = {inner = {x = 42}}; let v = t.inner.x;",
        Shape::Int(Position::new(1, 23, 22))
    );
}

#[test]
fn test_copy_expression() {
    // Copy a tuple with overrides
    assert_type_success!(
        "let t = {foo = 1, bar = 2}; let u = t{foo = 3};",
        Shape::Tuple(PositionedItem::new(
            vec![
                (
                    PositionedItem::new("foo".into(), Position::new(1, 10, 9)),
                    Shape::Int(Position::new(1, 16, 15)),
                ),
                (
                    PositionedItem::new("bar".into(), Position::new(1, 19, 18)),
                    Shape::Int(Position::new(1, 25, 24)),
                ),
                (
                    PositionedItem::new("foo".into(), Position::new(1, 39, 38)),
                    Shape::Int(Position::new(1, 45, 44)),
                ),
            ],
            Position::new(1, 37, 36),
        ))
    );
}

#[test]
fn test_multi_statement_inference() {
    // Multiple let statements build up the symbol table correctly
    assert_type_success!(
        "let x = 1; let y = 2; let z = x + y;",
        Shape::Int(Position::new(1, 9, 8))
    );
}

#[test]
fn test_select_expression() {
    // Select expression returns narrowed union of branch types
    assert_type_success!(
        r#"let x = true; let result = select (x) => { true = 1, false = 2 };"#,
        Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![Shape::Int(Position::new(1, 51, 50)),],
            Position::new(1, 28, 27),
        ))
    );
}

#[test]
fn test_import_dot_access() {
    // Import shapes should allow dot access (returns unconstrained)
    let mut symbol_table = BTreeMap::new();
    let expr = parse_expression(r#"import "foo.ucg""#).unwrap();
    let shape = expr.derive_shape(&mut symbol_table);
    assert!(
        matches!(shape, Shape::Import(ImportShape::Unresolved(_))),
        "import should produce Import shape, got: {:?}",
        shape
    );
}

#[test]
fn test_reduce_expression() {
    // reduce(func, acc, list) returns the accumulator's type
    let reducer = "func (acc, x) => acc + x";
    let list = "[1, 2, 3]";
    let code = format!(
        "let r = {}; let l = {}; let result = reduce(r, 0, l);",
        reducer, list
    );
    assert_type_success!(code.as_str(), Shape::Int(Position::new(1, 77, 76)));
}

#[test]
fn test_import_resolution() {
    // Import resolution should resolve the imported file's shape
    // when a working directory is provided.
    let fixture_dir =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/ast/typecheck");
    let code = r#"let imp = import "import_fixture.ucg"; let v = imp.val;"#;
    let mut checker = Checker::new().with_working_dir(&fixture_dir);
    let mut stmts = parse(code.into(), None).unwrap();
    checker.walk_statement_list(stmts.iter_mut().collect());
    let result = checker.result();
    assert!(
        result.is_ok(),
        "Import resolution should succeed: {:?}",
        result
    );
    let sym_table = result.unwrap();
    // imp should be a resolved import
    let imp_shape = &sym_table["imp"];
    assert!(
        matches!(imp_shape, Shape::Import(ImportShape::Resolved(_, _))),
        "imp should be a resolved import, got: {:?}",
        imp_shape
    );
    // v should be the type of imp.val, which is Int
    let v_shape = &sym_table["v"];
    assert!(
        matches!(v_shape, Shape::Int(_)),
        "v should be Int (from imported val = 42), got: {:?}",
        v_shape
    );
}

#[test]
fn test_import_cycle_detection() {
    // Create a checker with an import stack that already contains the target file,
    // simulating a cycle.
    let fixture_dir =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/ast/typecheck");
    let cycle_path = fixture_dir.join("import_fixture.ucg");
    let code = r#"let imp = import "import_fixture.ucg";"#;
    let mut checker = Checker::new()
        .with_working_dir(&fixture_dir)
        .with_import_stack(vec![cycle_path]);
    let mut stmts = parse(code.into(), None).unwrap();
    checker.walk_statement_list(stmts.iter_mut().collect());
    let result = checker.result();
    assert!(result.is_err(), "Import cycle should be detected");
    let err = result.unwrap_err();
    assert!(
        err.msg.contains("Import cycle detected"),
        "Error should mention import cycle, got: {}",
        err.msg
    );
}

#[test]
fn test_import_caching() {
    // Verify that importing the same file twice uses the cache
    let fixture_dir =
        std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/ast/typecheck");
    let code = r#"let imp1 = import "import_fixture.ucg"; let imp2 = import "import_fixture.ucg";"#;
    let mut checker = Checker::new().with_working_dir(&fixture_dir);
    let mut stmts = parse(code.into(), None).unwrap();
    checker.walk_statement_list(stmts.iter_mut().collect());
    let result = checker.result();
    assert!(result.is_ok(), "Double import should succeed: {:?}", result);
    let sym_table = result.unwrap();
    // Both should be resolved imports with the same shape
    assert!(
        matches!(
            &sym_table["imp1"],
            Shape::Import(ImportShape::Resolved(_, _))
        ),
        "imp1 should be resolved"
    );
    assert!(
        matches!(
            &sym_table["imp2"],
            Shape::Import(ImportShape::Resolved(_, _))
        ),
        "imp2 should be resolved"
    );
}

// Shape constraint tests

#[test]
fn test_let_shape_constraint() {
    assert_type_ok!("let x :: 0 = 42;");
}

#[test]
fn test_let_shape_constraint_mismatch() {
    assert_type_err!("let x :: 0 = \"wrong\";");
}

#[test]
fn test_named_shape_constraint() {
    assert_type_ok!("let MyShape = {foo = 0}; let x :: MyShape = {foo = 1};");
}

#[test]
fn test_named_shape_constraint_mismatch() {
    assert_type_err!("let MyShape = {foo = 0}; let x :: MyShape = {foo = \"wrong\"};");
}

#[test]
fn test_func_arg_constraint() {
    // Func with constrained arg: the constraint `0` at col 11 gives arg shape Int
    // but the actual shape uses the position from the constraint expression parse
    let args: BTreeMap<Rc<str>, Shape> =
        BTreeMap::from([("x".into(), Shape::Int(Position::new(1, 6, 5)))]);
    let ret = Box::new(Shape::Int(Position::new(1, 1, 0)));
    assert_type_success!(
        "func(x :: 0) => x + 1;",
        Shape::Func(FuncShapeDef {
            args,
            arg_order: vec!["x".into()],
            ret,
        })
    );
}

#[test]
fn test_tuple_field_constraint() {
    assert_type_ok!("{foo :: 0 = 42};");
}

#[test]
fn test_tuple_field_constraint_mismatch() {
    assert_type_err!("{foo :: 0 = \"wrong\"};");
}

#[test]
fn test_string_constraint() {
    assert_type_ok!("let x :: \"\" = \"hello\";");
}

#[test]
fn test_string_constraint_mismatch() {
    assert_type_err!("let x :: \"\" = 42;");
}

#[test]
fn test_bool_constraint() {
    assert_type_ok!("let x :: true = false;");
}

#[test]
fn test_bool_constraint_mismatch() {
    assert_type_err!("let x :: true = 42;");
}

// --- include expression ---

#[test]
fn test_include_expression_shape() {
    // derive_include_shape always returns Narrowed(Tuple | List) regardless of path
    let mut sym_table = BTreeMap::new();
    let expr = parse_expression("include json \"some/path.ucg\"").unwrap();
    let shape = expr.derive_shape(&mut sym_table);
    assert!(
        matches!(shape, Shape::Narrowed(_)),
        "include should produce a Narrowed shape, got {:?}",
        shape
    );
    if let Shape::Narrowed(ns) = &shape {
        if let NarrowingShape::Narrowed(types) = &ns.types {
            assert_eq!(types.len(), 2, "should have two candidates: Tuple and List");
            assert!(
                types.iter().any(|s| matches!(s, Shape::Tuple(_))),
                "should include Tuple"
            );
            assert!(
                types.iter().any(|s| matches!(s, Shape::List(_))),
                "should include List"
            );
        } else {
            panic!("Expected Narrowed candidates, got Any");
        }
    }
}

#[test]
fn test_include_in_let_binding() {
    // include expression used as a let binding typechecks without error
    assert_type_ok!("let x = include json \"some/path.ucg\";");
}

// --- module without out_expr ---

#[test]
fn test_module_no_out_expr_uses_last_let() {
    // A module with no explicit out_expr falls back to the last let binding's shape.
    assert_type_ok!("let m = module {} => { let x = 1; };");
}

#[test]
fn test_module_no_out_expr_empty_body() {
    // A module with no out_expr and no body produces an Any return type.
    assert_type_ok!("let m = module {} => {};");
}

// --- module output constraint ---

#[test]
fn test_module_out_constraint_ok() {
    // Constraint matches the out expression's type — should succeed.
    assert_type_ok!("let m = module {} => (1 :: 0) {};");
}

#[test]
fn test_module_out_constraint_mismatch() {
    // Constraint clashes with the out expression's type — should fail.
    assert_type_err!("let m = module {} => (1 :: \"\") {};");
}

// --- copy on module ---

#[test]
fn test_copy_module_ok() {
    // Copying a module with a correctly-typed field override should succeed.
    assert_type_ok!("let m = module { x = 0, } => (mod.x) {};\nlet n = m { x = 42 };");
}

#[test]
fn test_copy_module_wrong_field_type() {
    // Copying a module with a wrong-typed override should produce a type error.
    assert_type_err!("let m = module { x = 0, } => (mod.x) {};\nlet n = m { x = \"wrong\" };");
}

#[test]
fn test_copy_non_copyable_type() {
    // Copying an Int binding should produce a type error.
    assert_type_err!("let x = 1;\nlet y = x { foo = 1 };");
}

// --- output / convert statements ---

#[test]
fn test_output_statement() {
    assert_type_ok!("out json {x = 1};");
}

#[test]
fn test_convert_statement() {
    assert_type_ok!("convert json {x = 1};");
}

// --- assert statement ---

#[test]
fn test_assert_statement_bool() {
    // Bare boolean is not a valid assert shape - must be {ok=<bool>, desc=<str>}
    assert_type_err!("assert true;");
}

#[test]
fn test_assert_statement_tuple() {
    assert_type_ok!("assert {ok = true, desc = \"passes\"};");
}

// --- calling non-callable type ---

#[test]
fn test_call_non_callable() {
    assert_type_err!("let x = 1;\nlet y = x(1);");
}

// --- function call-site constraint checking ---

#[test]
fn test_func_call_constraint_ok() {
    // Calling a constrained function with matching types should succeed.
    assert_type_ok!("let add = func(a :: 0, b :: 0) => a + b;\nlet result = add(1, 2);");
}

#[test]
fn test_func_call_constraint_mismatch() {
    // Calling a constrained function with wrong type should fail.
    assert_type_err!("let add = func(a :: 0, b :: 0) => a + b;\nlet result = add(1, \"wrong\");");
}

#[test]
fn test_func_call_constraint_all_args_wrong() {
    // All arguments wrong type.
    assert_type_err!("let add = func(a :: 0, b :: 0) => a + b;\nlet result = add(\"x\", \"y\");");
}

#[test]
fn test_func_call_constraint_string_ok() {
    // String-constrained function called with strings should succeed.
    assert_type_ok!(
        "let greet = func(name :: \"\") => \"hello \" + name;\nlet r = greet(\"world\");"
    );
}

#[test]
fn test_func_call_constraint_string_mismatch() {
    // String-constrained function called with int should fail.
    assert_type_err!("let greet = func(name :: \"\") => \"hello \" + name;\nlet r = greet(42);");
}

#[test]
fn test_func_call_constraint_tuple_ok() {
    // Tuple-constrained function called with matching tuple should succeed.
    assert_type_ok!("let f = func(cfg :: {host = \"\", port = 0}) => cfg.host;\nlet r = f({host = \"localhost\", port = 80});");
}

#[test]
fn test_func_call_constraint_tuple_mismatch() {
    // Tuple-constrained function called with wrong field type should fail.
    assert_type_err!("let f = func(cfg :: {host = \"\", port = 0}) => cfg.host;\nlet r = f({host = \"localhost\", port = \"wrong\"});");
}

#[test]
fn test_func_call_mixed_constrained_unconstrained() {
    // Only some args constrained — unconstrained args accept anything.
    assert_type_ok!("let f = func(a :: 0, b) => a;\nlet r = f(1, \"anything\");");
}

#[test]
fn test_func_call_wrong_arg_count() {
    // Wrong number of arguments should fail.
    assert_type_err!("let f = func(a :: 0, b :: 0) => a + b;\nlet r = f(1);");
}

// --- module instantiation constraint checking ---

#[test]
fn test_module_copy_constraint_ok() {
    // Module with constrained args, instantiated with matching types.
    assert_type_ok!("let m = module { host :: \"\" = \"localhost\", port :: 0 = 80, } => (mod.host) {};\nlet n = m { host = \"example.com\", port = 443 };");
}

#[test]
fn test_module_copy_constraint_mismatch() {
    // Module with constrained args, instantiated with wrong type.
    assert_type_err!("let m = module { host :: \"\" = \"localhost\", port :: 0 = 80, } => (mod.host) {};\nlet n = m { host = \"example.com\", port = \"wrong\" };");
}

#[test]
fn test_module_copy_constraint_partial_override() {
    // Only override some args — defaults should still pass.
    assert_type_ok!("let m = module { host :: \"\" = \"localhost\", port :: 0 = 80, } => (mod.host) {};\nlet n = m { host = \"example.com\" };");
}

#[test]
fn test_module_copy_constraint_wrong_field() {
    // Override with wrong type for the constrained field.
    assert_type_err!("let m = module { host :: \"\" = \"localhost\", port :: 0 = 80, } => (mod.host) {};\nlet n = m { host = 42 };");
}

#[test]
fn test_module_copy_no_constraints_same_type() {
    // Module without explicit constraints — default value's type is the implicit shape.
    assert_type_ok!("let m = module { x = 0, } => (mod.x) {};\nlet n = m { x = 42 };");
}

#[test]
fn test_module_copy_no_constraints_wrong_type() {
    // Module without explicit constraints — default value's type is still enforced.
    assert_type_err!("let m = module { x = 0, } => (mod.x) {};\nlet n = m { x = \"wrong\" };");
}

// --- recursive constraint tests ---

#[test]
fn test_recursive_constraint_basic_base_case() {
    // A recursive constraint should accept values matching the base case.
    assert_type_ok!(
        "constraint node = \"\" | {name=\"\", children=[]};\nlet x :: node = \"leaf\";"
    );
}

#[test]
fn test_recursive_constraint_one_level_deep() {
    // A recursive constraint should accept a one-level-deep structure.
    assert_type_ok!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet x :: node = {name=\"root\", children=[\"leaf\"]};"
    );
}

#[test]
fn test_recursive_constraint_two_levels_deep() {
    // A recursive constraint should accept a two-level-deep structure.
    assert_type_ok!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet x :: node = {name=\"root\", children=[{name=\"child\", children=[\"leaf\"]}]};"
    );
}

#[test]
fn test_recursive_constraint_three_levels_deep() {
    // Three levels of nesting should work.
    assert_type_ok!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet x :: node = {name=\"a\", children=[{name=\"b\", children=[{name=\"c\", children=[]}]}]};"
    );
}

#[test]
fn test_recursive_constraint_mismatch_at_top() {
    // Wrong type at the top level should fail.
    assert_type_err!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet x :: node = 42;"
    );
}

#[test]
fn test_recursive_constraint_mismatch_nested() {
    // Wrong type nested inside should fail.
    assert_type_err!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet x :: node = {name=\"root\", children=[42]};"
    );
}

#[test]
fn test_recursive_constraint_mismatch_deep_nested() {
    // Wrong type two levels deep should fail.
    assert_type_err!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet x :: node = {name=\"root\", children=[{name=\"child\", children=[42]}]};"
    );
}

#[test]
fn test_recursive_constraint_empty_list() {
    // Empty list should satisfy a recursive list constraint.
    assert_type_ok!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet x :: node = {name=\"leaf\", children=[]};"
    );
}

#[test]
fn test_recursive_constraint_in_module_param() {
    // Recursive constraint used as a module parameter constraint.
    assert_type_ok!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet m = module { root :: node = {name=\"default\", children=[]}, } => (mod.root) {};"
    );
}

#[test]
fn test_recursive_constraint_in_func_arg() {
    // Recursive constraint used as a function argument constraint.
    assert_type_ok!(
        "constraint node = \"\" | {name=\"\", children=[node]};\nlet f = func(n :: node) => n;"
    );
}

#[test]
fn test_recursive_constraint_two_compatible() {
    // Two recursive constraints with the same structure should be compatible.
    assert_type_ok!(
        "constraint a = \"\" | {name=\"\", children=[a]};\nconstraint b = \"\" | {name=\"\", children=[b]};\nlet x :: a = \"leaf\";\nlet y :: b = x;"
    );
}

#[test]
fn test_recursive_constraint_two_incompatible() {
    // Two recursive constraints with different base types should not be compatible.
    assert_type_err!(
        "constraint a = \"\" | {name=\"\", children=[a]};\nconstraint b = 0 | {label=\"\", items=[b]};\nlet x :: a = \"leaf\";\nlet y :: b = x;"
    );
}

#[test]
fn test_non_recursive_constraint_still_works() {
    // A named constraint that doesn't self-reference should still work as before.
    assert_type_ok!(
        "constraint port = in 1..65535;\nlet p :: port = 8080;"
    );
}

#[test]
fn test_non_recursive_constraint_rejects_bad_value() {
    assert_type_err!(
        "constraint level = \"debug\" | \"info\" | \"warn\";\nlet x :: level = 42;"
    );
}

#[test]
fn test_recursive_constraint_unconstructible_direct_field() {
    // constraint a = {foo = a} is unconstructible — no base case.
    assert_type_err!(
        "constraint a = {foo = a};"
    );
}

#[test]
fn test_recursive_constraint_unconstructible_nested_field() {
    // Self-reference in a nested tuple field with no list/alternation guard.
    assert_type_err!(
        "constraint a = {outer = {inner = a}};"
    );
}

#[test]
fn test_recursive_constraint_valid_in_list() {
    // Self-reference inside a list is valid (list can be empty).
    assert_type_ok!(
        "constraint a = {children = [a]};"
    );
}

#[test]
fn test_recursive_constraint_valid_in_alternation() {
    // Self-reference as one arm of alternation with a base case is valid.
    assert_type_ok!(
        "constraint a = \"\" | {next = a};"
    );
}

#[test]
fn test_recursive_constraint_unconstructible_all_arms_recursive() {
    // All arms of the alternation reference self — no base case.
    assert_type_err!(
        "constraint a = {foo = a} | {bar = a};"
    );
}

// --- chained numeric index then field access ---

#[test]
fn test_chained_numeric_index_then_field() {
    assert_type_ok!(
        "let l = [{name=\"a\"}, {name=\"b\"}];\nlet x = l.0.name;"
    );
}

#[test]
fn test_chained_field_numeric_field() {
    assert_type_ok!(
        "let t = {items = [{label=\"x\"}]};\nlet x = t.items.0.label;"
    );
}
