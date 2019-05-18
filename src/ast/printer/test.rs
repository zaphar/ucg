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
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(0, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_simple_selector_printing() {
    let input = "foo.bar.quux;";
    let stmts = assert_parse(input);
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

#[test]
fn test_call_expr_printing() {
    let input = "call(\n  foo,\n  bar,\n);";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_call_expr_one_arg_printing() {
    let input = "call(foo);";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_copy_expr_printing() {
    let input = "copy{\n  foo = 1,\n  bar = 2,\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_copy_expr_one_arg_printing() {
    let input = "copy{\n  foo = 1,\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_out_expr_printing() {
    let input = "out json {\n  foo = 1,\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_select_expr_no_default_printing() {
    let input = "select true, {\n  true = 1,\n  false = 2,\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_select_expr_with_default_printing() {
    let input = "select true, 3, {\n  true = 1,\n  false = 2,\n};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_not_expr_printing() {
    let input = "not true;";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_fail_expr_printing() {
    let input = "fail \"AHHh\";";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_trace_expr_printing() {
    let input = "TRACE \"AHHh\";";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_module_no_out_expr_printing() {
    let input = "let m = module {
  hostname = \"\",
  mem = 2048,
  cpu = 2,
} => {
  let config = {
    hostname = mod.hostname,
    \"memory_size\" = mod.mem,
    \"cpu_count\" = mod.cpu,
  };
};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_module_with_out_expr_printing() {
    let input = "let m = module {
  hostname = \"\",
  mem = 2048,
  cpu = 2,
} => (config) {
  let config = {
    hostname = mod.hostname,
    \"memory_size\" = mod.mem,
    \"cpu_count\" = mod.cpu,
  };
};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_func_expr_printing() {
    let input = "let f = func (foo, bar) => {
  foo = foo,
  bar = bar,
};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_func_expr_single_arg_printing() {
    let input = "let f = func (foo) => {
  foo = foo,
};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_format_expr_single_arg_printing() {
    let input = "\"what? @{item.foo}\" % {
  foo = 1,
};";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}

#[test]
fn test_format_expr_list_arg_printing() {
    let input = "\"what? @ @\" % (
  1,
  2);";
    let stmts = assert_parse(input);
    let mut buffer: Vec<u8> = Vec::new();
    let mut printer = AstPrinter::new(2, &mut buffer);
    printer.render(&stmts);
    assert!(printer.err.is_none());
    assert_eq!(String::from_utf8(buffer).unwrap(), format!("{}\n", input));
}
