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

fn assert_build(input: &str) {
    let i_paths = Vec::new();
    let cache = MemoryCache::new();
    let mut b = FileBuilder::new("<Eval>", &i_paths, Rc::new(RefCell::new(cache)));
    b.enable_validate_mode();
    b.eval_string(input).unwrap();
    if !b.assert_collector.success {
        assert!(false, b.assert_collector.failures);
    }
}

fn assert_build_failure(input: &str, expect: Vec<Regex>) {
    let i_paths = Vec::new();
    let cache = MemoryCache::new();
    let mut b = FileBuilder::new("<Eval>", &i_paths, Rc::new(RefCell::new(cache)));
    b.enable_validate_mode();
    let err = b.eval_string(input);
    match err {
        Ok(_) => {
            for r in expect.iter() {
                if !b.assert_collector.success {
                    if let None = r.find(&b.assert_collector.failures) {
                        panic!(
                            "[{}] was not found in Assertion Failures:\n{}",
                            r, b.assert_collector.failures
                        );
                    }
                } else {
                    panic!("Building input Did not panic!");
                }
            }
        }
        Err(ref err) => {
            for r in expect.iter() {
                let stack_trace = format!("{}", err);
                // Look for each expect to match the string.
                if let None = r.find(&stack_trace) {
                    panic!("[{}] was not found in stacktrace:\n{}", r, stack_trace);
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
#[should_panic(expected = "UserDefined")]
fn test_declarative_failures_are_user_defined() {
    assert_build("fail \"I am a failure!\";");
}

#[test]
#[should_panic(expected = "Caused By:\n\tI am a failure!")]
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
            Regex::new(r"line: 1, column: 1").unwrap(),
            Regex::new(r"Expected Tuple \{ok=<bool>, desc=<str>\}: at <eval> line: 1, column: 8")
                .unwrap(),
            Regex::new(r"Expected Expression: at <eval> line: 1, column: 8").unwrap(),
        ],
    );
}

#[test]
fn test_assert_partial_tuple_compile_failures() {
    assert_build_failure(
        "assert {",
        vec![
            Regex::new(r"line: 1, column: 1").unwrap(),
            Regex::new(r"Expected Tuple \{ok=<bool>, desc=<str>\}: at <eval> line: 1, column: 8")
                .unwrap(),
            Regex::new(r"Expected \(\}\) Instead is \(\): at <eval> line: 1, column: 9").unwrap(),
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
