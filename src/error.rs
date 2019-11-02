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

use crate::ast::*;
use crate::iter::FilePositioned;

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
    IncludeError,
    ImportError,
    ReservedWordError,
    // Parsing Errors
    ParseError,
    AssertError,
    OSError,
    // Conversion errors
    ConvertError,
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
            &ErrorType::IncludeError => "IncludeError",
            &ErrorType::ImportError => "ImportError",
            &ErrorType::ReservedWordError => "ReservedWordError",
            &ErrorType::ParseError => "ParseError",
            &ErrorType::AssertError => "AssertError",
            &ErrorType::OSError => "OSError",
            &ErrorType::ConvertError => "ConvertError",
            &ErrorType::UserDefined => "UserDefined",
        };
        w.write_str(name)
    }
}

/// Error defines an Error type for parsing and building UCG code.
pub struct BuildError {
    pub err_type: ErrorType,
    pub pos: Option<Position>,
    pub msg: String,
    pub cause: Option<Box<dyn error::Error>>,
    // This field is only present to prevent people from constructing these
    // outside of the module they are defined in.
    _pkgonly: (),
}

impl BuildError {
    pub fn with_pos<S: Into<String>>(msg: S, t: ErrorType, pos: Position) -> Self {
        BuildError {
            err_type: t,
            pos: Some(pos),
            msg: msg.into(),
            cause: None,
            _pkgonly: (),
        }
    }

    pub fn new<S: Into<String>>(msg: S, t: ErrorType) -> Self {
        BuildError {
            err_type: t,
            pos: None,
            msg: msg.into(),
            cause: None,
            _pkgonly: (),
        }
    }

    pub fn wrap_cause(mut self, cause: Box<dyn error::Error>) -> Self {
        self.cause = Some(cause);
        self
    }

    pub fn to_boxed(self) -> Box<Self> {
        Box::new(self)
    }

    fn render(&self, w: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref pos) = self.pos {
            // FIXME(jwall): we should still be printing the file here.
            let file = match pos.file {
                Some(ref pb) => pb.to_string_lossy().to_string(),
                None => "<eval>".to_string(),
            };
            write!(w, "{}: {} at {}", self.err_type, self.msg, pos,)?;
        } else {
            write!(w, "{}: {}", self.err_type, self.msg)?;
        }
        if let Some(ref cause) = self.cause {
            write!(w, "\nCaused By:\n\t{}", cause)?;
        }
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

impl<'a, C> std::convert::From<abortable_parser::Error<C>> for BuildError
where
    C: FilePositioned + 'a,
    C: abortable_parser::Offsetable + Debug,
{
    fn from(e: abortable_parser::Error<C>) -> BuildError {
        BuildError::from(&e)
    }
}

impl<'a, C> std::convert::From<&'a abortable_parser::Error<C>> for BuildError
where
    C: FilePositioned + 'a,
    C: abortable_parser::Offsetable + Debug,
{
    fn from(e: &'a abortable_parser::Error<C>) -> BuildError {
        let ctx = e.get_context();
        let position = Position::new(ctx.line(), ctx.column(), ctx.get_offset());
        let err = BuildError::with_pos(e.get_msg(), ErrorType::ParseError, position);
        match e.get_cause() {
            None => err,
            Some(cause) => err.wrap_cause(Box::new(BuildError::from(cause))),
        }
    }
}
