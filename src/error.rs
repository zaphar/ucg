
use std::error;
use std::fmt;

use ast::*;

pub enum ErrorType {
    // Build Errors
    TypeFail,
    DuplicateBinding,
    IncompleteParsing,
    Unsupported,
    NoSuchSymbol,
    BadArgLen,
    FormatError,
    // Parsing Errors
    UnexpectedToken,
    EmptyExpression,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        let name = match self {
            &ErrorType::TypeFail => "TypeFail",
            &ErrorType::DuplicateBinding => "DuplicateBinding",
            &ErrorType::IncompleteParsing => "IncompleteParsing",
            &ErrorType::Unsupported => "Unsupported",
            &ErrorType::NoSuchSymbol => "NoSuchSymbol",
            &ErrorType::BadArgLen => "BadArgLen",
            &ErrorType::FormatError => "FormatError",
            &ErrorType::UnexpectedToken => "UnexpectedToken",
            &ErrorType::EmptyExpression => "EmptyExpression",
        };
        w.write_str(name)
    }
}

pub struct Error {
    pub err_type: ErrorType,
    pub pos: Position,
    pub msg: String,
    _pkgonly: (),
}

impl Error {
    pub fn new<S: Into<String>>(msg: S, t: ErrorType, pos: Position) -> Self {
        Error {
            err_type: t,
            pos: pos,
            msg: msg.into(),
            _pkgonly: (),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(
            w,
            "{}: \"{}\" {}:{}",
            self.err_type,
            self.msg,
            self.pos.line,
            self.pos.column
        )
    }
}

impl fmt::Display for Error {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(
            w,
            "{}: \"{}\" {}:{}",
            self.err_type,
            self.msg,
            self.pos.line,
            self.pos.column
        )
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        &self.msg
    }
}
