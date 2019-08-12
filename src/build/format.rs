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
use std::cell::RefCell;
use std::clone::Clone;
use std::error::Error;
use std::str::Chars;

use crate::ast::*;
use crate::build::assets;
use crate::build::{FileBuilder, Val};
use crate::error;

pub trait FormatRenderer {
    fn render(&self, pos: &Position) -> Result<String, Box<dyn Error>>;
}

/// Implements the logic for format strings in UCG format expressions.
pub struct SimpleFormatter<V: Into<String> + Clone> {
    tmpl: String,
    args: Vec<V>,
}

impl<V: Into<String> + Clone> SimpleFormatter<V> {
    /// Constructs a Formatter with a template and args.
    pub fn new<S: Into<String>>(tmpl: S, args: Vec<V>) -> Self {
        SimpleFormatter {
            tmpl: tmpl.into(),
            args: args,
        }
    }
}

impl<V: Into<String> + Clone> FormatRenderer for SimpleFormatter<V> {
    /// Renders a formatter to a string or returns an error.
    ///
    /// If the formatter has the wrong number of arguments for the number of replacements
    /// it will return an error. Otherwise it will return the formatted string.
    fn render(&self, pos: &Position) -> Result<String, Box<dyn Error>> {
        let mut buf = String::new();
        let mut should_escape = false;
        let mut count = 0;
        for c in self.tmpl.chars() {
            if c == '@' && !should_escape {
                if count == self.args.len() {
                    return Err(error::BuildError::with_pos(
                        "Too few arguments to string \
                         formatter.",
                        error::ErrorType::FormatError,
                        pos.clone(),
                    )
                    .to_boxed());
                }
                let arg = self.args[count].clone();
                let strval = arg.into();
                buf.push_str(&strval);
                count += 1;
                should_escape = false;
            } else if c == '\\' && !should_escape {
                eprintln!("found an escape char {}", self.tmpl);
                should_escape = true;
            } else {
                buf.push(c);
                should_escape = false;
            }
        }
        if self.args.len() != count {
            return Err(error::BuildError::with_pos(
                format!(
                    "Too many arguments to string \
                     formatter. Expected {} got {}",
                    count,
                    self.args.len()
                ),
                error::ErrorType::FormatError,
                pos.clone(),
            )
            .to_boxed());
        }
        return Ok(buf);
    }
}

pub struct ExpressionFormatter<'a, C>
where
    C: assets::Cache,
{
    tmpl: String,
    builder: RefCell<FileBuilder<'a, C>>,
}

impl<'a, C> ExpressionFormatter<'a, C>
where
    C: assets::Cache,
{
    pub fn new<S: Into<String>>(tmpl: S, builder: FileBuilder<'a, C>) -> Self {
        ExpressionFormatter {
            tmpl: tmpl.into(),
            builder: RefCell::new(builder),
        }
    }

    fn consume_expr(
        &self,
        builder: &mut FileBuilder<'a, C>,
        iter: &mut Chars,
        pos: &Position,
    ) -> Result<Val, Box<dyn Error>> {
        // we expect the next char to be { or we error.
        let mut expr_string = String::new();
        let mut brace_count = 0;
        match iter.next() {
            Some(c) => {
                if c == '{' {
                    brace_count += 1;
                } else {
                    return Err(error::BuildError::with_pos(
                        format!(
                            "Invalid syntax for format string expected '{{' but got {}",
                            c
                        ),
                        error::ErrorType::FormatError,
                        pos.clone(),
                    )
                    .to_boxed());
                }
            }
            None => {
                return Err(error::BuildError::with_pos(
                    "Invalid syntax for format string expected '{' but string ended",
                    error::ErrorType::FormatError,
                    pos.clone(),
                )
                .to_boxed());
            }
        };
        loop {
            let c = match iter.next() {
                Some(c) => c,
                None => break,
            };
            if c == '{' {
                brace_count += 1;
            }
            if c == '}' {
                brace_count -= 1;
                // if brace_count is 0 then this is the end of expression.
                if brace_count != 0 {
                    // if it is not zero then this character is just part of
                    // the embedded expression.
                    expr_string.push(c);
                    continue;
                }
                // empty expressions are an error
                if expr_string.is_empty() {
                    return Err(error::BuildError::with_pos(
                        "Got an empty expression in format string",
                        error::ErrorType::FormatError,
                        pos.clone(),
                    )
                    .to_boxed());
                }
                if !expr_string.ends_with(";") {
                    expr_string.push(';');
                }
                // we are done and it is time to compute the expression and return it.
                return Ok(builder.eval_string(&expr_string)?.as_ref().clone());
            } else {
                expr_string.push(c);
            }
        }
        return Err(error::BuildError::with_pos(
            "Expected '}' but got end of string",
            error::ErrorType::FormatError,
            pos.clone(),
        )
        .to_boxed());
    }
}

impl<'a, C> FormatRenderer for ExpressionFormatter<'a, C>
where
    C: assets::Cache,
{
    fn render(&self, pos: &Position) -> Result<String, Box<dyn Error>> {
        let mut buf = String::new();
        let mut should_escape = false;
        let mut iter = self.tmpl.chars();
        loop {
            let c = match iter.next() {
                Some(c) => c,
                None => break,
            };
            if c == '@' && !should_escape {
                // This is kind of wasteful. Can we do better?
                let val = self.consume_expr(&mut self.builder.borrow_mut(), &mut iter, pos)?;
                let strval: String = val.into();
                buf.push_str(&strval);
                should_escape = false;
            } else if c == '\\' && !should_escape {
                should_escape = true;
            } else {
                buf.push(c);
                should_escape = false;
            }
        }
        return Ok(buf);
    }
}

#[cfg(test)]
mod test {
    use super::{FormatRenderer, SimpleFormatter};
    use crate::ast::Position;

    #[test]
    fn test_format_happy_path() {
        let formatter = SimpleFormatter::new("foo @ @ \\@", vec!["bar", "quux"]);
        let pos = Position::new(0, 0, 0);
        assert_eq!(formatter.render(&pos).unwrap(), "foo bar quux @");
    }

    #[test]
    fn test_format_happy_wrong_too_few_args() {
        let formatter = SimpleFormatter::new("foo @ @ \\@", vec!["bar"]);
        let pos = Position::new(0, 0, 0);
        assert!(formatter.render(&pos).is_err());
    }

    #[test]
    fn test_format_happy_wrong_too_many_args() {
        let formatter = SimpleFormatter::new("foo @ @ \\@", vec!["bar", "quux", "baz"]);
        let pos = Position::new(0, 0, 0);
        assert!(formatter.render(&pos).is_err());
    }
}
