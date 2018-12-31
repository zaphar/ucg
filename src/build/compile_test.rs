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
fn test_macros() {
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
fn test_list_operations() {
    assert_build(include_str!("../../integration_tests/list_ops_test.ucg"));
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
