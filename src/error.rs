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

//! Errors for use by the ucg compiler.
use std::error;
use std::fmt;
use std::fmt::Debug;

use abortable_parser::Positioned;

use crate::ast::*;

/// ErrorType defines the various types of errors that can result from compiling UCG into an
/// output format.
pub enum ErrorType {
    // Build Errors
    TypeFail,
    DuplicateBinding,
    Unsupported,
    NoSuchSymbol,
    BadArgLen,
    FormatError,
    ReservedWordError,
    // Parsing Errors
    ParseError,
    AssertError,
    // User Defined Declarative Errors
    UserDefined,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            &ErrorType::TypeFail => "TypeFail",
            &ErrorType::DuplicateBinding => "DuplicateBinding",
            &ErrorType::Unsupported => "Unsupported",
            &ErrorType::NoSuchSymbol => "NoSuchSymbol",
            &ErrorType::BadArgLen => "BadArgLen",
            &ErrorType::FormatError => "FormatError",
            &ErrorType::ReservedWordError => "ReservedWordError",
            &ErrorType::ParseError => "ParseError",
            &ErrorType::AssertError => "AssertError",
            &ErrorType::UserDefined => "UserDefined",
        };
        w.write_str(name)
    }
}

/// Error defines an Error type for parsing and building UCG code.
pub struct BuildError {
    pub err_type: ErrorType,
    pub pos: Position,
    pub msg: String,
    _pkgonly: (),
}

impl BuildError {
    pub fn new<S: Into<String>>(msg: S, t: ErrorType, pos: Position) -> Self {
        BuildError {
            err_type: t,
            pos: pos,
            msg: msg.into(),
            _pkgonly: (),
        }
    }

    fn render(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(
            w,
            "{} at line: {} column: {}\nCaused By:\n\t{} ",
            self.err_type, self.pos.line, self.pos.column, self.msg
        )?;
        Ok(())
    }
}

impl Debug for BuildError {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        self.render(w)
    }
}

impl fmt::Display for BuildError {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        self.render(w)
    }
}

impl error::Error for BuildError {
    fn description(&self) -> &str {
        &self.msg
    }
}

#[derive(Debug)]
pub struct StackPrinter<C: Positioned> {
    pub err: abortable_parser::Error<C>,
}

impl<C: Positioned> StackPrinter<C> {
    pub fn render(&self, w: &mut fmt::Formatter) -> fmt::Result {
        let mut curr_err = Some(&self.err);
        let mut tabstop = "";
        loop {
            match curr_err {
                // our exit condition;
                None => break,
                Some(err) => {
                    let context = err.get_context();
                    write!(
                        w,
                        "{}{}: line: {}, column: {}\n",
                        tabstop,
                        err.get_msg(),
                        context.line(),
                        context.column(),
                    )?;
                    tabstop = "\t";
                    curr_err = err.get_cause();
                    if curr_err.is_some() {
                        write!(w, "Caused by: \n")?;
                    }
                }
            }
        }
        Ok(())
    }
}

impl<C: Positioned> fmt::Display for StackPrinter<C> {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        self.render(w)
    }
}
