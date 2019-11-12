use std::convert::TryFrom;

use super::Primitive;
use crate::ast::CastType;

pub struct Error {
    val: Primitive,
    cast_type: CastType,
}

impl Error {
    pub fn message(&self) -> String {
        format!("No cast from {} to {}", self.val, self.cast_type)
    }
}

impl From<&Primitive> for String {
    fn from(p: &Primitive) -> Self {
        match p {
            Primitive::Int(i) => format!("{}", i),
            Primitive::Float(f) => format!("{}", f),
            Primitive::Str(s) => format!("{}", s),
            Primitive::Bool(b) => format!("{}", b),
            Primitive::Empty => "NULL".to_owned(),
        }
    }
}

impl TryFrom<&Primitive> for i64 {
    type Error = Error;

    fn try_from(p: &Primitive) -> Result<Self, Self::Error> {
        match p {
            Primitive::Bool(_) | Primitive::Empty => Err(Error {
                val: p.clone(),
                cast_type: CastType::Int,
            }),
            Primitive::Str(s) => match s.parse::<i64>() {
                Ok(i) => Ok(i),
                Err(_) => Err(Error {
                    val: Primitive::Str(s.clone()),
                    cast_type: CastType::Int,
                }),
            },
            Primitive::Float(f) => Ok(*f as i64),
            Primitive::Int(i) => Ok(i.clone()),
        }
    }
}

impl TryFrom<&Primitive> for f64 {
    type Error = Error;

    fn try_from(p: &Primitive) -> Result<Self, Self::Error> {
        match p {
            Primitive::Bool(_) | Primitive::Empty => Err(Error {
                val: p.clone(),
                cast_type: CastType::Int,
            }),
            Primitive::Str(s) => match s.parse::<f64>() {
                Ok(f) => Ok(f),
                Err(_) => Err(Error {
                    val: Primitive::Str(s.clone()),
                    cast_type: CastType::Int,
                }),
            },
            Primitive::Int(i) => Ok(*i as f64),
            Primitive::Float(f) => Ok(f.clone()),
        }
    }
}

impl TryFrom<&Primitive> for bool {
    type Error = Error;

    fn try_from(p: &Primitive) -> Result<Self, Self::Error> {
        match p {
            Primitive::Empty | Primitive::Int(_) | Primitive::Float(_) => Err(Error {
                val: p.clone(),
                cast_type: CastType::Int,
            }),
            Primitive::Bool(b) => Ok(*b),
            Primitive::Str(s) => match s.as_str() {
                "true" => Ok(true),
                "false" => Ok(false),
                _ => Err(Error {
                    val: Primitive::Str(s.clone()),
                    cast_type: CastType::Int,
                }),
            },
        }
    }
}
