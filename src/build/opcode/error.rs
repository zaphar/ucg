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

use crate::ast::Position;

#[derive(Debug)]
pub struct Error {
    message: String,
    pos: Option<Position>,
}

impl Error {
    pub fn new(msg: String, pos: Position) -> Self {
        Self {
            message: msg,
            pos: Some(pos),
        }
    }

    pub fn with_pos(mut self, pos: Position) -> Self {
        self.pos = Some(pos);
        self
    }
}

macro_rules! decorate_error {
    ($pos:expr => $result:expr) => {
        match $result {
            Ok(v) => Ok(v),
            Err(e) => {
                Err(e.with_pos($pos.clone()))
            }
        }
    };
}

impl From<regex::Error> for Error {
    fn from(e: regex::Error) -> Self {
        Error {
            message: format!("{}", e),
            pos: None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        let msg = match e.kind() {
            io::ErrorKind::NotFound => format!("OSError: Path not found: {}", e),
            _ => format!("{}", e),
        };
        Error {
            message: msg,
            pos: None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref pos) = self.pos {
            write!(f, "{} at {}", self.message, pos)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl std::error::Error for Error {}
