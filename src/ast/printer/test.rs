// Copyright 2019 Jeremy Wall
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use crate::ast::printer::*;
use crate::iter::OffsetStrIter;
use crate::parse::*;

fn assert_parse(input: &str) -> Vec<Statement> {
    parse(OffsetStrIter::new(input)).unwrap()
}

#[test]
fn test_simple_value_printing() {
    let input = "1;";
    let stmts = assert_parse("1;");
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(0, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_simple_quoted_printing() {
    let input = "\"foo\";";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(0, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_escaped_quoted_printing() {
    let input = "\"f\\\\o\\\"o\";";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(0, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_empty_tuple_printing() {
    let input = "{};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_empty_list_printing() {
    let input = "[];";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_non_empty_tuple_printing() {
    let input = "{\n  foo = 1,\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_nested_empty_tuple_printing() {
    let input = "{\n  foo = {},\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_list_nested_empty_tuple_printing() {
    let input = "[\n  {},\n];";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_nested_non_empty_tuple_printing() {
    let input = "{\n  foo = {\n    bar = 1,\n  },\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_nested_non_empty_list_printing() {
    let input = "[\n  [\n    1,\n  ],\n];";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_simple_quoted_field_tuple_printing() {
    let input = "{\n  \"foo\" = {\n    bar = 1,\n  },\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(
        String::from_utf8(buffer).unwrap(),
        format!("{}\n", "{\n  foo = {\n    bar = 1,\n  },\n};")
    );
}

#[test]
fn test_special_quoted_field_tuple_printing() {
    let input = "{\n  \"foo bar\" = {\n    bar = 1,\n  },\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_let_statement_printing() {
    let input = "let tpl = {\n  \"foo bar\" = {\n    bar = 1,\n  },\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}
