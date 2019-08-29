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

use std::cell::RefCell;
use std::rc::Rc;

use regex::Regex;

use super::assets::MemoryCache;
use super::FileBuilder;
use crate::convert::ConverterRegistry;

fn assert_build(input: &str) {
    let i_paths = Vec::new();
    let out_buffer: Vec<u8> = Vec::new();
    let err_buffer: Vec<u8> = Vec::new();
    let mut b = FileBuilder::new("<Eval>", &i_paths, out_buffer, err_buffer);
    b.enable_validate_mode();
    b.eval_string(input).unwrap();
    // FIXME(jwall): What do we want to do with the assert collector?
    //if !b.assert_collector.success {
    //    assert!(false, b.assert_collector.failures);
    //}
}

fn assert_build_failure(input: &str, expect: Vec<Regex>) {
    let i_paths = Vec::new();
    let out_buffer: Vec<u8> = Vec::new();
    let err_buffer: Vec<u8> = Vec::new();
    let mut b = FileBuilder::new("<Eval>", &i_paths, out_buffer, err_buffer);
    b.enable_validate_mode();
    let err = b.eval_string(input);
    match err {
        Ok(_) => {
            for r in expect.iter() {
                // FIXME(jwall): Assert collector...
                //if !b.assert_collector.success {
                //    if let None = r.find(&b.assert_collector.failures) {
                //        assert!(
                //            false,
                //            "[{}] was not found in Assertion Failures:\n{}",
                //            r, b.assert_collector.failures
                //        );
                //    }
                //} else {
                //    assert!(false, "Building input Did not panic!");
                //}
            }
        }
        Err(ref err) => {
            for r in expect.iter() {
                let stack_trace = format!("{}", err);
                // Look for each expect to match the string.
                if let None = r.find(&stack_trace) {
                    assert!(
                        false,
                        "[{}] was not found in stacktrace:\n{}",
                        r, stack_trace
                    );
                }
            }
        }
    }
}

#[test]
fn test_tuples() {
    assert_build(include_str!("../../integration_tests/tuple_test.ucg"));
}

#[test]
fn test_lists() {
    assert_build(include_str!("../../integration_tests/list_test.ucg"));
}

#[test]
fn test_comparisons() {
    assert_build(include_str!("../../integration_tests/comparisons_test.ucg"));
}

#[test]
fn test_funcs() {
    assert_build(include_str!("../../integration_tests/macros_test.ucg"));
}

#[test]
fn test_modules() {
    assert_build(include_str!("../../integration_tests/modules_test.ucg"));
}

#[test]
fn test_selectors() {
    assert_build(include_str!("../../integration_tests/selectors_test.ucg"));
}

#[test]
fn test_empty_value() {
    assert_build(include_str!("../../integration_tests/empty_test.ucg"));
}

#[test]
fn test_select_expressions() {
    assert_build(include_str!(
        "../../integration_tests/select_expressions_test.ucg"
    ));
}

#[test]
fn test_binary_operator_precedence() {
    assert_build(include_str!(
        "../../integration_tests/operator_precedence_test.ucg"
    ));
}

#[test]
fn test_functional_processing() {
    assert_build(include_str!(
        "../../integration_tests/functional_processing_test.ucg"
    ));
}

#[test]
fn test_concatenation() {
    assert_build(include_str!(
        "../../integration_tests/concatenation_test.ucg"
    ));
}

#[test]
fn test_format() {
    assert_build(include_str!("../../integration_tests/format_test.ucg"));
}

#[test]
fn test_type_checks() {
    assert_build(include_str!("../../integration_tests/types_test.ucg"));
}

#[test]
#[should_panic(expected = "UserDefined: I am a failure!")]
fn test_declarative_failures_are_caused_by_msg() {
    assert_build("fail \"I am a failure!\";");
}

#[test]
#[should_panic(expected = "1 is a failure!")]
fn test_declarative_failures_can_with_format_expr() {
    assert_build("fail \"@ is a failure!\" % (1);");
}

#[test]
fn test_assert_just_keyword_compile_failures() {
    assert_build_failure(
        "assert ",
        vec![
            Regex::new(r"Expected Tuple \{ok=<bool>, desc=<str>\} at <eval> line: 1, column: 8")
                .unwrap(),
        ],
    );
}

#[test]
fn test_assert_partial_tuple_compile_failures() {
    assert_build_failure(
        "assert {",
        vec![
            Regex::new(r"Expected Tuple \{ok=<bool>, desc=<str>\} at <eval> line: 1, column: 8")
                .unwrap(),
            Regex::new(r"Expected \(\}\) but got \(\) at <eval> line: 1, column: 9").unwrap(),
        ],
    );
}

#[test]
fn test_assert_partial_tuple_missing_ok_compile_failures() {
    assert_build_failure(
        "assert {};",
        vec![
            Regex::new(r"0 - NOT OK: TYPE FAIL - Expected Boolean field ok in tuple \{").unwrap(),
            Regex::new(r"line: 1, column: 8").unwrap(),
        ],
    );
}

#[test]
fn test_assert_partial_tuple_bad_ok_compile_failures() {
    assert_build_failure(
        "assert { ok = 1, };",
        vec![
            Regex::new(r"0 - NOT OK: TYPE FAIL - Expected Boolean field ok in tuple \{").unwrap(),
            Regex::new(r"line: 1, column: 8").unwrap(),
        ],
    );
}

#[test]
fn test_assert_partial_tuple_missing_desc_compile_failures() {
    assert_build_failure(
        "assert { ok=true, };",
        vec![
            Regex::new(r"0 - NOT OK: TYPE FAIL - Expected String field desc in tuple \{").unwrap(),
            Regex::new(r"line: 1, column: 8").unwrap(),
        ],
    );
}

#[test]
fn test_assert_partial_tuple_bad_desc_compile_failures() {
    assert_build_failure(
        "assert { ok=true, desc = 1 };",
        vec![
            Regex::new(r"0 - NOT OK: TYPE FAIL - Expected String field desc in tuple \{").unwrap(),
            Regex::new(r"line: 1, column: 8").unwrap(),
        ],
    );
}

#[test]
fn test_import_missing_path_compile_failure() {
    assert_build_failure(
        "import ;",
        vec![Regex::new(r"Expected import path at <eval> line: 1, column: 8").unwrap()],
    )
}

#[test]
fn test_import_path_not_a_string_compile_failure() {
    assert_build_failure(
        "import 1;",
        vec![Regex::new(r"Expected import path at <eval> line: 1, column: 8").unwrap()],
    )
}

#[test]
fn test_binary_operator_missing_operand_compile_failure() {
    assert_build_failure(
        "1 +",
        vec![
            Regex::new(r"Missing operand for binary expression at <eval> line: 1, column: 4")
                .unwrap(),
        ],
    )
}

#[test]
fn test_binary_sum_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 + \"foo\";",
        vec![
            Regex::new(r"Expected Int but got String\(foo\)").unwrap(),
            Regex::new(r"line: 1 column: 5").unwrap(),
        ],
    )
}

#[test]
fn test_binary_minus_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 - \"foo\";",
        vec![
            Regex::new(r"Expected Int but got String\(foo\)").unwrap(),
            Regex::new(r"line: 1 column: 5").unwrap(),
        ],
    )
}

#[test]
fn test_binary_mul_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 * \"foo\";",
        vec![
            Regex::new(r"Expected Int but got String\(foo\)").unwrap(),
            Regex::new(r"line: 1 column: 5").unwrap(),
        ],
    )
}

#[test]
fn test_binary_div_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 / \"foo\";",
        vec![
            Regex::new(r"Expected Int but got String\(foo\)").unwrap(),
            Regex::new(r"line: 1 column: 5").unwrap(),
        ],
    )
}

#[test]
fn test_binary_gt_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 > \"foo\";",
        vec![
            Regex::new(
                r"Expected numeric values of the same type but got Int\(1\) at line: 1 column: 1 and String\(foo\) at line: 1 column: 5 for expression",
            )
            .unwrap(),
            Regex::new(r"line: 1 column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_binary_lt_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 < \"foo\";",
        vec![
            Regex::new(
                r"Expected numeric values of the same type but got Int\(1\) at line: 1 column: 1 and String\(foo\) at line: 1 column: 5 for expression",
            )
            .unwrap(),
            Regex::new(r"line: 1 column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_binary_lteq_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 <= \"foo\";",
        vec![
            Regex::new(
                r"Expected numeric values of the same type but got Int\(1\) at line: 1 column: 1 and String\(foo\) at line: 1 column: 6 for expression",
            )
            .unwrap(),
            Regex::new(r"line: 1 column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_binary_gteq_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 >= \"foo\";",
        vec![
            Regex::new(
                r"Expected numeric values of the same type but got Int\(1\) at line: 1 column: 1 and String\(foo\) at line: 1 column: 6 for expression",
            )
            .unwrap(),
            Regex::new(r"line: 1 column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_binary_eqeq_operator_wrong_type_on_rhs_compile_failure() {
    assert_build_failure(
        "1 == \"foo\";",
        vec![
            Regex::new(
                r"Expected numeric values of the same type but got Int\(1\) at line: 1 column: 1 and String\(foo\) at line: 1 column: 5 for expression",
            )
            .unwrap(),
            Regex::new(r"line: 1 column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_incomplete_tuple_compile_failure() {
    assert_build_failure(
        "{;",
        vec![
            Regex::new(r"Expected \(\}\) but got \(;\)").unwrap(),
            Regex::new(r"at <eval> line: 1 column: 2").unwrap(),
        ],
    )
}

#[test]
fn test_incomplete_tuple_key_value_missing_eq_compile_failure() {
    assert_build_failure(
        "{foo",
        vec![
            Regex::new(r"Expected \(=\) but got \(\)").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 5").unwrap(),
        ],
    )
}

#[test]
fn test_incomplete_tuple_key_value_missing_value_compile_failure() {
    assert_build_failure(
        "{foo=",
        vec![
            Regex::new(r"Not a Bareword").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 6").unwrap(),
        ],
    )
}

#[test]
fn test_format_expr_missing_expr_compile_error() {
    assert_build_failure(
        "\"foo\" %",
        vec![
            Regex::new(r"Expected format arguments").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 8").unwrap(),
        ],
    )
}

#[test]
fn test_format_expr_unclosed_parens_compile_failure() {
    assert_build_failure(
        "\"foo\" % (1",
        vec![
            Regex::new(r"Expected \(\)\) but got \(\)").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 9").unwrap(),
        ],
    )
}

#[test]
fn test_list_unclosed_bracket_compile_failure() {
    assert_build_failure(
        "[1",
        vec![
            Regex::new(r"Expected \(\]\) but got \(\)").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 3").unwrap(),
        ],
    )
}

#[test]
fn test_out_missing_type_compile_failure() {
    assert_build_failure(
        "out",
        vec![
            Regex::new(r"Expected converter name").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 4").unwrap(),
        ],
    )
}

#[test]
fn test_out_missing_out_expr_compile_failure() {
    assert_build_failure(
        "out json",
        vec![
            Regex::new(r"Expected Expression to export").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 9").unwrap(),
        ],
    )
}

#[test]
fn test_out_multiple_times_compile_failure() {
    assert_build_failure(
        "out json {};\nout json {};",
        vec![
            Regex::new(r"You can only have one output per file").unwrap(),
            Regex::new(r"at <eval> line: 2, column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_let_missing_name_compile_failure() {
    assert_build_failure(
        "let ",
        vec![
            Regex::new(r"Expected name for binding").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 5").unwrap(),
        ],
    )
}

#[test]
fn test_let_missing_equal_compile_failure() {
    assert_build_failure(
        "let foo ",
        vec![
            Regex::new(r"Expected \(=\) but got \(\)").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 9").unwrap(),
        ],
    )
}

#[test]
fn test_let_missing_expression_compile_failure() {
    assert_build_failure(
        "let foo =",
        vec![
            Regex::new(r"Expected Expression to bind").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 10").unwrap(),
        ],
    )
}

#[test]
fn test_let_missing_semicolon_compile_failure() {
    assert_build_failure(
        "let foo = 1",
        vec![
            Regex::new(r"Expected \(;\) but got \(\)").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 12").unwrap(),
        ],
    )
}

#[test]
fn test_copy_expression_not_a_tuple_compile_failure() {
    assert_build_failure(
        "let foo = 1;\nfoo{};",
        vec![
            Regex::new(r"Expected Tuple or Module but got \(1\)").unwrap(),
            Regex::new(r"line: 2 column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_copy_expression_wrong_field_type_compile_failure() {
    assert_build_failure(
        "let foo = {bar=1};\nfoo{bar=[]};",
        vec![
            Regex::new(r"Expected type Integer for field bar but got \(List\)").unwrap(),
            Regex::new(r"at <eval> line: 2, column: 5").unwrap(),
        ],
    )
}

#[test]
fn test_func_call_wrong_argument_length_compile_failure() {
    assert_build_failure(
        "let foo = func() => 1;\nfoo(1);",
        vec![
            Regex::new(r"Func called with too many args").unwrap(),
            Regex::new(r"at <eval> line: 2, column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_func_call_wrong_argument_type_compile_failure() {
    assert_build_failure(
        "let foo = func(i) => 1 + i;\nfoo(\"bar\");",
        vec![
            Regex::new(r"Func evaluation failed").unwrap(),
            Regex::new(r"at <eval> line: 1, column: 26").unwrap(),
            Regex::new(r"Expected Integer but got \(.bar.\)").unwrap(),
            Regex::new(r"at <eval> line: 2, column: 1").unwrap(),
        ],
    )
}

#[test]
fn test_select_missed_case_string_no_default_compile_failure() {
    assert_build_failure(
        "select \"a\", { b = 1, };",
        vec![
            Regex::new(r"Unhandled select case with no default").unwrap(),
            Regex::new(r"line: 1 column: 8").unwrap(),
        ],
    )
}

#[test]
fn test_select_missed_case_boolean_no_default_compile_failure() {
    assert_build_failure(
        "select true, { false = 1, };",
        vec![
            Regex::new(r"Unhandled select case with no default").unwrap(),
            Regex::new(r"line: 1 column: 8").unwrap(),
        ],
    )
}

#[test]
fn test_bad_import_path_compile_failure() {
    assert_build_failure(
        "let bad = import \"no/such/path.ucg\";",
        vec![
            Regex::new(r"OSError: Path not found").unwrap(),
            Regex::new(r"line: 1 column: 18").unwrap(),
        ],
    )
}
