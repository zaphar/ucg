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
use std::clone::Clone;
use std::error::Error;

use crate::ast::*;
use crate::error;

/// Implements the logic for format strings in UCG format expressions.
pub struct Formatter<V: Into<String> + Clone> {
    tmpl: String,
    args: Vec<V>,
}

impl<V: Into<String> + Clone> Formatter<V> {
    /// Constructs a Formatter with a template and args.
    pub fn new<S: Into<String>>(tmpl: S, args: Vec<V>) -> Self {
        Formatter {
            tmpl: tmpl.into(),
            args: args,
        }
    }

    /// Renders a formatter to a string or returns an error.
    ///
    /// If the formatter has the wrong number of arguments for the number of replacements
    /// it will return an error. Otherwise it will return the formatted string.
    pub fn render(&self, pos: &Position) -> Result<String, Box<dyn Error>> {
        let mut buf = String::new();
        let mut should_escape = false;
        let mut count = 0;
        for c in self.tmpl.chars() {
            if c == '@' && !should_escape {
                if count == self.args.len() {
                    return Err(Box::new(error::BuildError::new(
                        "Too few arguments to string \
                         formatter.",
                        error::ErrorType::FormatError,
                        pos.clone(),
                    )));
                }
                let arg = self.args[count].clone();
                let strval = arg.into();
                buf.push_str(&strval);
                count += 1;
            } else if c == '\\' && !should_escape {
                should_escape = true;
            } else {
                buf.push(c);
            }
        }
        if self.args.len() != count {
            return Err(Box::new(error::BuildError::new(
                "Too many arguments to string \
                 formatter.",
                error::ErrorType::FormatError,
                pos.clone(),
            )));
        }
        return Ok(buf);
    }
}

#[cfg(test)]
mod test {
    use super::Formatter;
    use crate::ast::Position;

    #[test]
    fn test_format_happy_path() {
        let formatter = Formatter::new("foo @ @ \\@", vec!["bar", "quux"]);
        let pos = Position::new(0, 0, 0);
        assert_eq!(formatter.render(&pos).unwrap(), "foo bar quux @");
    }

    #[test]
    fn test_format_happy_wrong_too_few_args() {
        let formatter = Formatter::new("foo @ @ \\@", vec!["bar"]);
        let pos = Position::new(0, 0, 0);
        assert!(formatter.render(&pos).is_err());
    }

    #[test]
    fn test_format_happy_wrong_too_many_args() {
        let formatter = Formatter::new("foo @ @ \\@", vec!["bar", "quux", "baz"]);
        let pos = Position::new(0, 0, 0);
        assert!(formatter.render(&pos).is_err());
    }
}
