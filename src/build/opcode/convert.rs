use std::convert::{From, TryFrom};
use std::rc::Rc;

use super::{Composite, Primitive, Val, Value};
use crate::ast::{CastType, Position};

use Composite::{List, Tuple};
use Primitive::{Bool, Empty, Float, Int, Str};
use Value::{C, F, M, P, S, T};

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

impl From<Rc<Value>> for Val {
    fn from(val: Rc<Value>) -> Val {
        val.as_ref().into()
    }
}

impl From<Value> for Val {
    fn from(val: Value) -> Val {
        (&val).into()
    }
}

impl From<&Value> for Val {
    fn from(val: &Value) -> Val {
        match val {
            P(Int(i)) => Val::Int(*i),
            P(Float(f)) => Val::Float(*f),
            P(Str(s)) => Val::Str(s.clone()),
            P(Bool(b)) => Val::Boolean(*b),
            C(Tuple(fs, _)) => {
                let mut flds = Vec::new();
                for &(ref k, ref v) in fs.iter() {
                    let v = v.clone();
                    flds.push((k.clone(), Rc::new(v.into())));
                }
                Val::Tuple(flds)
            }
            C(List(elems, _)) => {
                let mut els = Vec::new();
                for e in elems.iter() {
                    let e = e.clone();
                    els.push(Rc::new(e.into()));
                }
                Val::List(els)
            }
            S(_) | F(_) | M(_) | T(_) | P(Empty) => Val::Empty,
        }
    }
}

impl From<Rc<Val>> for Value {
    fn from(val: Rc<Val>) -> Self {
        val.as_ref().into()
    }
}

impl From<Val> for Value {
    fn from(val: Val) -> Self {
        (&val).into()
    }
}

impl From<&Val> for Value {
    fn from(val: &Val) -> Self {
        match val {
            Val::Int(i) => P(Int(*i)),
            Val::Float(f) => P(Float(*f)),
            Val::Boolean(b) => P(Bool(*b)),
            Val::Str(s) => P(Str(s.clone())),
            Val::Empty => P(Empty),
            Val::List(els) => {
                let mut lst = Vec::new();
                let mut positions = Vec::new();
                for e in els.iter() {
                    let e = e.clone();
                    lst.push(Rc::new(e.into()));
                    positions.push(Position::new(0, 0, 0));
                }
                // TODO(jwall): This should have a set of
                // Positions of the same length.
                C(List(lst, positions))
            }
            Val::Tuple(flds) => {
                let mut field_list = Vec::new();
                let mut positions = Vec::new();
                for &(ref key, ref val) in flds.iter() {
                    let val = val.clone();
                    field_list.push((key.clone(), Rc::new(val.into())));
                    positions.push((Position::new(0, 0, 0), Position::new(0, 0, 0)));
                }
                C(Tuple(field_list, positions))
            }
            Val::Env(flds) => {
                let mut field_list = Vec::new();
                let mut positions = Vec::new();
                for &(ref key, ref val) in flds.iter() {
                    field_list.push((key.clone(), Rc::new(P(Str(val.clone())))));
                    positions.push((Position::new(0, 0, 0), Position::new(0, 0, 0)));
                }
                C(Tuple(field_list, positions))
            }
        }
    }
}

impl From<&Composite> for String {
    fn from(c: &Composite) -> Self {
        let mut buf = String::new();
        match c {
            &List(ref elems, _) => {
                buf.push_str("[");
                for e in elems.iter() {
                    let val: String = e.as_ref().into();
                    buf.push_str(&val);
                    buf.push_str(",");
                }
                buf.push_str("]");
            }
            &Tuple(ref flds, _) => {
                buf.push_str("{");
                for &(ref k, ref v) in flds.iter() {
                    buf.push_str(&k);
                    buf.push_str(" = ");
                    let val: String = v.as_ref().into();
                    buf.push_str(&val);
                    buf.push_str(",");
                }
                buf.push_str("}");
            }
        }
        buf
    }
}

impl From<&Value> for String {
    fn from(v: &Value) -> Self {
        match v {
            &S(ref s) => s.clone(),
            &P(ref p) => p.into(),
            &C(ref c) => c.into(),
            &T(_) => "<Thunk>".to_owned(),
            &F(_) => "<Func>".to_owned(),
            &M(_) => "<Module>".to_owned(),
        }
    }
}
