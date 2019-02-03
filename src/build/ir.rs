//! The ir module holds the definitions of our ucg Intermediate Representation before it is converted
//! to an output artifact.
use std::convert::From;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::string::ToString;

use crate::ast::*;
use crate::error;

/// The Intermediate representation of a compiled UCG AST.
#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Empty,
    Boolean(bool),
    Int(i64),
    Float(f64),
    Str(String),
    List(Vec<Rc<Val>>),
    Tuple(Vec<(PositionedItem<String>, Rc<Val>)>),
    Env(Vec<(String, String)>),
    Func(FuncDef),
    Module(ModuleDef),
}

impl Val {
    /// Returns the Type of a Val as a string.
    pub fn type_name(&self) -> String {
        match self {
            &Val::Empty => "EmptyValue".to_string(),
            &Val::Boolean(_) => "Boolean".to_string(),
            &Val::Int(_) => "Integer".to_string(),
            &Val::Float(_) => "Float".to_string(),
            &Val::Str(_) => "String".to_string(),
            &Val::List(_) => "List".to_string(),
            &Val::Tuple(_) => "Tuple".to_string(),
            &Val::Env(_) => "Env".to_string(),
            &Val::Func(_) => "Func".to_string(),
            &Val::Module(_) => "Module".to_string(),
        }
    }

    /// Returns true if called with a Val of the same type as itself.
    pub fn type_equal(&self, target: &Self) -> bool {
        enum_type_equality!(
            self,
            target,
            &Val::Empty,
            &Val::Boolean(_),
            &Val::Int(_),
            &Val::Float(_),
            &Val::Str(_),
            &Val::List(_),
            &Val::Tuple(_),
            &Val::Env(_),
            &Val::Func(_),
            &Val::Module(_)
        )
    }

    pub fn equal(&self, target: &Self, pos: Position) -> Result<bool, error::BuildError> {
        // first we do a type equality comparison
        match (self, target) {
            // Empty values are always equal.
            (&Val::Empty, &Val::Empty) => Ok(true),
            (&Val::Int(ref i), &Val::Int(ref ii)) => Ok(i == ii),
            (&Val::Float(ref f), &Val::Float(ref ff)) => Ok(f == ff),
            (&Val::Boolean(ref b), &Val::Boolean(ref bb)) => Ok(b == bb),
            (&Val::Str(ref s), &Val::Str(ref ss)) => Ok(s == ss),
            (&Val::List(ref ldef), &Val::List(ref rdef)) => {
                if ldef.len() != rdef.len() {
                    Ok(false)
                } else {
                    for (i, lv) in ldef.iter().enumerate() {
                        if !lv.equal(rdef[i].as_ref(), pos.clone())? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                }
            }
            (&Val::Tuple(ref ldef), &Val::Tuple(ref rdef)) => {
                if ldef.len() != rdef.len() {
                    Ok(false)
                } else {
                    for (i, lv) in ldef.iter().enumerate() {
                        let field_target = &rdef[i];
                        if lv.0.val != field_target.0.val {
                            // field name equality
                            return Ok(false);
                        } else {
                            // field value equality.
                            if !lv.1.equal(field_target.1.as_ref(), lv.0.pos.clone())? {
                                return Ok(false);
                            }
                        }
                    }
                    Ok(true)
                }
            }
            (&Val::Func(_), &Val::Func(_)) => Err(error::BuildError::new(
                "Func are not comparable",
                error::ErrorType::TypeFail,
                pos,
            )),
            (&Val::Module(_), &Val::Module(_)) => Err(error::BuildError::new(
                "Module are not comparable",
                error::ErrorType::TypeFail,
                pos,
            )),
            // EMPTY is always comparable for equality.
            (&Val::Empty, _) => Ok(false),
            (_, &Val::Empty) => Ok(false),
            (me, tgt) => Err(error::BuildError::new(
                format!("Expected {} but got {}", me.type_name(), tgt),
                error::ErrorType::TypeFail,
                pos,
            )),
        }
    }

    /// Returns the fields if this Val is a tuple. None otherwise.
    pub fn get_fields(&self) -> Option<&Vec<(PositionedItem<String>, Rc<Val>)>> {
        if let &Val::Tuple(ref fs) = self {
            Some(fs)
        } else {
            None
        }
    }

    pub fn is_int(&self) -> bool {
        if let &Val::Int(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_empty(&self) -> bool {
        if let &Val::Empty = self {
            return true;
        }
        return false;
    }

    pub fn is_float(&self) -> bool {
        if let &Val::Float(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_string(&self) -> bool {
        if let &Val::Str(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_tuple(&self) -> bool {
        if let &Val::Tuple(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_env(&self) -> bool {
        if let &Val::Env(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_list(&self) -> bool {
        if let &Val::List(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_bool(&self) -> bool {
        if let &Val::Boolean(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_func(&self) -> bool {
        if let &Val::Func(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_str(&self) -> bool {
        if let &Val::Str(_) = self {
            return true;
        }
        return false;
    }
    pub fn is_module(&self) -> bool {
        if let &Val::Module(_) = self {
            return true;
        }
        return false;
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Val::Boolean(b) => write!(f, "{}", b),
            &Val::Empty => write!(f, "NULL"),
            &Val::Float(ref ff) => write!(f, "{}", ff),
            &Val::Int(ref i) => write!(f, "{}", i),
            // TODO(jwall): Escape quotes in the string.
            &Val::Str(ref s) => write!(f, "\"{}\"", s),
            &Val::List(ref def) => {
                write!(f, "[")?;
                for v in def.iter() {
                    write!(f, "{}, ", v)?;
                }
                write!(f, "]")
            }
            &Val::Func(_) => write!(f, "Func(..)"),
            &Val::Module(_) => write!(f, "Module{{..}}"),
            &Val::Tuple(ref def) => {
                write!(f, "{{\n")?;
                for v in def.iter() {
                    write!(f, "\t{} = {},\n", v.0.val, v.1)?;
                }
                write!(f, "}}")
            }
            &Val::Env(ref def) => {
                write!(f, "{{\n")?;
                for v in def.iter() {
                    write!(f, "\t{}=\"{}\"\n", v.0, v.1)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl From<Val> for String {
    fn from(v: Val) -> String {
        match v {
            Val::Int(ref i) => format!("{}", i),
            Val::Float(ref f) => format!("{}", f),
            Val::Str(ref s) => s.to_string(),
            Val::Boolean(ref b) => format!("{}", b),
            Val::Empty => "NULL".to_string(),
            val => format!("{}", val),
        }
    }
}

impl From<String> for Val {
    fn from(s: String) -> Val {
        Val::Str(s)
    }
}
