use super::*;

#[test]
pub fn test_macro_validation_happy_path() {
    let def = MacroDef {
        argdefs: vec![value_node!("foo".to_string(), 1, 0)],
        fields: vec![(
            make_tok!("f1", 1, 1),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Symbol(value_node!(
                    "foo".to_string(),
                    1,
                    1
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                pos: Position::new(1, 0),
            }),
        )],
        pos: Position::new(1, 0),
    };
    assert!(def.validate_symbols().unwrap() == ());
}

#[test]
pub fn test_macro_validation_fail() {
    let def = MacroDef {
        argdefs: vec![value_node!("foo".to_string(), 1, 0)],
        fields: vec![(
            make_tok!("f1", 1, 1),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Symbol(value_node!(
                    "bar".to_string(),
                    1,
                    1
                )))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                pos: Position::new(1, 0),
            }),
        )],
        pos: Position::new(1, 0),
    };
    let mut expected = HashSet::new();
    expected.insert("bar".to_string());
    assert_eq!(def.validate_symbols().err().unwrap(), expected);
}

#[test]
pub fn test_macro_validation_selector_happy_path() {
    let def = MacroDef {
        argdefs: vec![value_node!("foo".to_string(), 1, 0)],
        fields: vec![(
            make_tok!("f1", 1, 1),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Selector(
                    make_selector!(make_expr!("foo", 1, 1) => [
                    make_tok!("quux", 1, 1) ] => 1, 1),
                ))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                pos: Position::new(1, 0),
            }),
        )],
        pos: Position::new(1, 0),
    };
    assert!(def.validate_symbols().unwrap() == ());
}

#[test]
pub fn test_macro_validation_selector_fail() {
    let def = MacroDef {
        argdefs: vec![value_node!("foo".to_string(), 1, 0)],
        fields: vec![(
            make_tok!("f1", 1, 1),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::Add,
                left: Box::new(Expression::Simple(Value::Selector(
                    make_selector!(make_expr!("bar", 1, 1) => [
                    make_tok!("quux", 1, 1) ] => 1, 1),
                ))),
                right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                pos: Position::new(1, 0),
            }),
        )],
        pos: Position::new(1, 0),
    };
    let mut expected = HashSet::new();
    expected.insert("bar".to_string());
    assert_eq!(def.validate_symbols(), Err(expected));
}
