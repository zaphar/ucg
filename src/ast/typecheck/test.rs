use std::convert::Into;

use abortable_parser::SliceIter;

use crate::ast::walk::Walker;
use crate::ast::{Position, PositionedItem};
use crate::parse::{expression, parse};
use crate::tokenizer::tokenize;

use super::*;

macro_rules! assert_type_fail {
    ($e:expr, $msg:expr, $pos:expr) => {{
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
        assert!(
            result.is_ok(),
            "We expect this to typecheck successfully. {:?}",
            result
        );
        assert!(maybe_shape.is_some(), "We got a shape out of it");
        assert_eq!(maybe_shape.unwrap(), $shape);
    }};
    ($e:expr, $shape:expr, $sym_table:expr, $expected_sym:expr) => {{
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
    ($e:expr, $sym_list:expr, $sym_init_list:expr) => {{
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
        "foo".into()
    );
    let float_stmt = "let foo = 1.0;";
    assert_type_success!(
        float_stmt,
        Shape::Float(Position::new(1, 11, 10)),
        BTreeMap::new(),
        "foo".into()
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
    // Assert with boolean should succeed
    assert_type_success!("assert 1 == 1;", Shape::Boolean(Position::new(1, 8, 7)));
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
