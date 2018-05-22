use super::{Builder, Val};

fn assert_build<S: Into<String>>(input: S, assert: &str) {
    let mut b = Builder::new();
    b.build_file_string(input.into()).unwrap();
    let result = b.eval_string(assert).unwrap();
    if let &Val::Boolean(ok) = result.as_ref() {
        assert!(ok, format!("'{}' is not true", assert));
    } else {
        assert!(
            false,
            format!("'{}' does not evaluate to a boolean: {:?}", assert, result)
        );
    }
}

#[test]
fn test_comparisons() {
    let input = "
    let one = 1;
    let two = 2;
    let foo = \"foo\";
    let bar = \"bar\";
    let tpl1 = {
        foo = \"bar\",
        one = 1
    };
    let tpl2 = tpl1{};
    let tpl3 = {
        bar = \"foo\",
        two = 1
    };
    let list = [1, 2, 3];
    let list2 = list;
    let list3 = [1, 2];
    ";
    assert_build(input, "one == one;");
    assert_build(input, "one >= one;");
    assert_build(input, "two > one;");
    assert_build(input, "two >= two;");
    assert_build(input, "tpl1 == tpl2;");
    assert_build(input, "tpl1 != tpl3;");
    assert_build(input, "list == list2;");
    assert_build(input, "list != list3;");
}

#[test]
fn test_deep_comparison() {
    let input = "
    let tpl1 = {
        foo = \"bar\",
        lst = [1, 2, 3],
        inner = {
            fld = \"value\"
        }
    };
    let copy = tpl1;
    let extra = tpl1{one = 1};
    let less = {
        foo = \"bar\"
    };
    ";

    assert_build(input, "tpl1.inner == copy.inner;");
    assert_build(input, "tpl1.inner.fld == copy.inner.fld;");
    assert_build(input, "tpl1.lst == copy.lst;");
    assert_build(input, "tpl1.foo == copy.foo;");
    assert_build(input, "tpl1 == copy;");
    assert_build(input, "tpl1 != extra;");
    assert_build(input, "tpl1 != less;");
}

#[test]
fn test_expression_comparisons() {
    assert_build("", "2 == 1+1;");
    assert_build("", "(1+1) == 2;");
    assert_build("", "(1+1) == (1+1);");
    assert_build("", "(\"foo\" + \"bar\") == \"foobar\";");
}

#[test]
fn test_binary_operator_precedence() {
    //assert_build("let result = 2 * 2 + 1;", "result == 6;");
    assert_build("let result = 2 + 2 * 1;", "result == 4;");
    assert_build("let result = (2 * 2) + 1;", "result == 5;");
}
