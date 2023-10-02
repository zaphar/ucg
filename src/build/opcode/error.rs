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
use std::convert::From;
use std::fmt;
use std::fmt::Display;
use std::io;
use std::rc::Rc;

use crate::ast::Position;
use crate::build::opcode::convert;

#[derive(Debug)]
pub struct Error {
    message: Rc<str>,
    pos: Option<Position>,
    call_stack: Vec<Position>,
}

impl Error {
    pub fn new(msg: Rc<str>, pos: Position) -> Self {
        Self {
            message: msg,
            pos: Some(pos),
            call_stack: Vec::new(),
        }
    }

    pub fn with_pos(mut self, pos: Position) -> Self {
        self.pos = Some(pos);
        self
    }

    pub fn push_call_stack(&mut self, pos: Position) {
        self.call_stack.push(pos);
    }
}

macro_rules! decorate_error {
    ($pos:expr => $result:expr) => {
        match $result {
            Ok(v) => Ok(v),
            Err(e) => Err(e.with_pos($pos.clone())),
        }
    };
}

macro_rules! decorate_call {
    ($pos:expr => $result:expr) => {
        match $result {
            Ok(v) => Ok(v),
            Err(mut e) => {
                e.push_call_stack($pos.clone());
                Err(e)
            }
        }
    };
}

impl From<regex::Error> for Error {
    fn from(e: regex::Error) -> Self {
        Error {
            message: format!("{}", e).into(),
            pos: None,
            call_stack: Vec::new(),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        let msg = match e.kind() {
            io::ErrorKind::NotFound | io::ErrorKind::Other => {
                format!("OSError: Path not found: {}", e)
            }
            _ => format!("{}", e),
        }
        .into();
        Error {
            message: msg,
            pos: None,
            call_stack: Vec::new(),
        }
    }
}

impl From<convert::Error> for Error {
    fn from(e: convert::Error) -> Self {
        Error {
            message: e.message().into(),
            pos: None,
            call_stack: Vec::new(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref pos) = self.pos {
            write!(f, "{} at {}", self.message, pos)?;
        } else {
            write!(f, "{}", self.message)?;
        }
        if !self.call_stack.is_empty() {
            for p in self.call_stack.iter() {
                write!(f, "\nVIA: {}", p)?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for Error {}
