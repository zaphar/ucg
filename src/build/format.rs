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

//! The format string logic for ucg format expressions.
use std::error::Error;
use std::str::Chars;

use abortable_parser::iter::SliceIter;
use abortable_parser::Result as ParseResult;

use crate::ast::*;
use crate::iter;
use crate::parse;
use crate::tokenizer;

pub trait FormatRenderer {
    fn render(&self, pos: &Position) -> Result<String, Box<dyn Error>>;
}

pub type TemplateResult = Result<Vec<TemplatePart>, Box<dyn Error>>;

pub trait TemplateParser {
    fn parse(&self, input: &str) -> TemplateResult;
}

pub struct SimpleTemplate();

impl SimpleTemplate {
    pub fn new() -> Self {
        Self()
    }
}

impl TemplateParser for SimpleTemplate {
    fn parse(&self, input: &str) -> TemplateResult {
        let mut result = Vec::new();
        let mut count = 0;
        let mut should_escape = false;
        let mut buf: Vec<char> = Vec::new();
        for c in input.chars() {
            if c == '@' && !should_escape {
                result.push(TemplatePart::Str(buf));
                buf = Vec::new();
                // This is a placeholder in our template.
                result.push(TemplatePart::PlaceHolder(count));
                count += 1;
            } else if c == '\\' && !should_escape {
                should_escape = true;
                continue;
            } else {
                buf.push(c);
            }
            should_escape = false;
        }
        if buf.len() != 0 {
            result.push(TemplatePart::Str(buf));
        }
        Ok(result)
    }
}

pub struct ExpressionTemplate();

impl ExpressionTemplate {
    pub fn new() -> Self {
        ExpressionTemplate()
    }

    fn consume_expr(&self, iter: &mut Chars) -> Result<Expression, Box<dyn Error>> {
        let mut result = String::new();
        let mut brace_count = 0;
        loop {
            let c = match iter.next() {
                Some(c) => c,
                None => break,
            };
            if c == '{' {
                brace_count += 1;
                // We ignore the starting brace
                if brace_count == 1 {
                    continue;
                }
            }
            if c == '}' {
                brace_count -= 1;
                // We ignore the closing brace
                if brace_count == 0 {
                    continue;
                }
            }
            if brace_count == 0 {
                break;
            }
            result.push(c);
        }
        let str_iter = iter::OffsetStrIter::new(&result);
        let toks = match tokenizer::tokenize(str_iter, None) {
            Ok(toks) => toks,
            Err(_e) => panic!("TODO(jwall): make this not a thing"),
        };

        let i = SliceIter::new(&toks);
        match parse::expression(i) {
            ParseResult::Complete(_, expr) => Ok(expr),
            ParseResult::Abort(e) | ParseResult::Fail(e) => {
                panic!("TODO(jwall): make this not a thing")
            }
            ParseResult::Incomplete(_ei) => panic!("TODO(jwall): make this not a thing"),
        }
    }
}

impl TemplateParser for ExpressionTemplate {
    fn parse(&self, input: &str) -> TemplateResult {
        let mut parts = Vec::new();
        let mut should_escape = false;
        let mut iter = input.chars();
        let mut buf: Vec<char> = Vec::new();
        loop {
            let c = match iter.next() {
                Some(c) => c,
                None => break,
            };
            if c == '@' && !should_escape {
                parts.push(TemplatePart::Str(buf));
                buf = Vec::new();
                // consume our expression here
                parts.push(TemplatePart::Expression(self.consume_expr(&mut iter)?));
            } else if c == '\\' && !should_escape {
                should_escape = true;
                continue;
            } else {
                buf.push(c);
            }
            should_escape = false;
        }
        if buf.len() != 0 {
            parts.push(TemplatePart::Str(buf));
        }
        Ok(parts)
    }
}
