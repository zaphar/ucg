use super::Builder;

fn assert_build(input: &str) {
    let mut b = Builder::new("<Eval>");
    b.enable_validate_mode();
    b.eval_string(input).unwrap();
    if !b.assert_collector.success {
        assert!(false, b.assert_collector.failures);
    }
}

#[test]
fn test_comparisons() {
    assert_build(
        "let one = 1;
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
    assert \"one == one\";
    assert \"one == one\";
    assert \"one >= one\";
    assert \"two > one\";
    assert \"two >= two\";
    assert \"tpl1 == tpl2\";
    assert \"tpl1 != tpl3\";
    assert \"list == list2\";
    assert \"list != list3\";
    ",
    );
}

#[test]
fn test_deep_comparison() {
    assert_build(
        "let tpl1 = {
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
    assert \"tpl1.inner == copy.inner\";
    assert \"tpl1.inner.fld == copy.inner.fld\";
    assert \"tpl1.lst == copy.lst\";
    assert \"tpl1.foo == copy.foo\";
    assert \"tpl1 == copy\";
    assert \"tpl1 != extra\";
    assert \"tpl1 != less\";
    ",
    );
}

#[test]
fn test_expression_comparisons() {
    assert_build("assert \"2 == 1+1\";");
    assert_build("assert \"(1+1) == 2\";");
    assert_build("assert \"(1+1) == (1+1)\";");
    assert_build(
        "let want = \"foo\";
    assert \"select want, 1, { foo=2, } == 2\";",
    );
}

#[test]
fn test_binary_operator_precedence() {
    assert_build(
        "let result = 2 * 2 + 1;
    assert \"result == 5\";",
    );
    assert_build(
        "let result = 2 + 2 * 3;
    assert \"result == 8\";",
    );
    assert_build(
        "let result = 2 * (2 + 1);
    assert \"result == 6\";",
    );
}
