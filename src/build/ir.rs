//! The ir module holds the definitions of our ucg Intermediate Representation before it is converted
//! to an output artifact.
use std::convert::From;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::string::ToString;

use crate::error;

/// Represents the bounds of a numeric range constraint.
#[derive(PartialEq, Debug, Clone)]
pub enum ConstraintBound {
    Int(Option<i64>, Option<i64>),
    Float(Option<f64>, Option<f64>),
}

/// A single arm of a constraint value.
#[derive(PartialEq, Debug, Clone)]
pub enum ConstraintValArm {
    Range(ConstraintBound),
    Exact(Rc<Val>),
}

/// A first-class constraint value that can check if a Val satisfies it.
#[derive(PartialEq, Debug, Clone)]
pub struct ConstraintVal {
    pub arms: Vec<ConstraintValArm>,
}

impl ConstraintVal {
    /// Check if a value satisfies this constraint.
    /// Returns true if the value matches any arm.
    /// Returns true if this constraint contains a self-referential placeholder
    /// (an embedded empty ConstraintVal). Such constraints are validated
    /// statically by the typechecker and should be skipped at runtime.
    pub fn contains_self_ref(&self) -> bool {
        self.arms.iter().any(|arm| match arm {
            ConstraintValArm::Exact(val) => val.contains_empty_constraint(),
            _ => false,
        })
    }

    pub fn check(&self, val: &Val) -> bool {
        // An empty constraint (e.g., a self-referential placeholder) matches
        // anything — the static typechecker has already validated the shape.
        if self.arms.is_empty() {
            return true;
        }
        self.arms.iter().any(|arm| match arm {
            ConstraintValArm::Range(ConstraintBound::Int(min, max)) => {
                if let Val::Int(v) = val {
                    min.is_none_or(|lo| *v >= lo) && max.is_none_or(|hi| *v <= hi)
                } else {
                    false
                }
            }
            ConstraintValArm::Range(ConstraintBound::Float(min, max)) => {
                if let Val::Float(v) = val {
                    min.is_none_or(|lo| *v >= lo) && max.is_none_or(|hi| *v <= hi)
                } else {
                    false
                }
            }
            ConstraintValArm::Exact(expected) => val.equal(expected).unwrap_or(false),
        })
    }
}

/// A tuple's runtime field list: name-value pairs.
pub type TupleFields = Vec<(Rc<str>, Rc<Val>)>;

/// The Intermediate representation of a compiled UCG AST.
#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Empty,
    Boolean(bool),
    Int(i64),
    Float(f64),
    Str(Rc<str>),
    List(Vec<Rc<Val>>),
    Tuple(Vec<(Rc<str>, Rc<Val>)>),
    Env(Vec<(Rc<str>, Rc<str>)>),
    Constraint(ConstraintVal),
}

impl Val {
    /// Returns the Type of a Val as a string.
    pub fn type_name(&self) -> String {
        match *self {
            Val::Empty => "EmptyValue".to_string(),
            Val::Boolean(_) => "Boolean".to_string(),
            Val::Int(_) => "Integer".to_string(),
            Val::Float(_) => "Float".to_string(),
            Val::Str(_) => "String".to_string(),
            Val::List(_) => "List".to_string(),
            Val::Tuple(_) => "Tuple".to_string(),
            Val::Env(_) => "Env".to_string(),
            Val::Constraint(_) => "Constraint".to_string(),
        }
    }

    /// Returns true if this value contains an empty ConstraintVal anywhere
    /// in its structure (indicating a self-referential constraint placeholder).
    pub fn contains_empty_constraint(&self) -> bool {
        match self {
            Val::Constraint(cv) => cv.arms.is_empty(),
            Val::List(items) => items.iter().any(|v| v.contains_empty_constraint()),
            Val::Tuple(fields) => fields.iter().any(|(_, v)| v.contains_empty_constraint()),
            _ => false,
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
            &Val::Constraint(_)
        )
    }

    pub fn equal(&self, target: &Self) -> Result<bool, error::BuildError> {
        // first we do a type equality comparison
        match (self, target) {
            // Empty values are always equal.
            (&Val::Empty, &Val::Empty) => Ok(true),
            (Val::Int(i), Val::Int(ii)) => Ok(i == ii),
            (Val::Float(f), Val::Float(ff)) => Ok(f == ff),
            (Val::Boolean(b), Val::Boolean(bb)) => Ok(b == bb),
            (Val::Str(s), Val::Str(ss)) => Ok(s == ss),
            (Val::List(ldef), Val::List(rdef)) => {
                if ldef.len() != rdef.len() {
                    Ok(false)
                } else {
                    for (i, lv) in ldef.iter().enumerate() {
                        if !lv.equal(rdef[i].as_ref())? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                }
            }
            (Val::Tuple(ldef), Val::Tuple(rdef)) => {
                if ldef.len() != rdef.len() {
                    Ok(false)
                } else {
                    for (i, lv) in ldef.iter().enumerate() {
                        let field_target = &rdef[i];
                        if lv.0 != field_target.0 {
                            // field name equality
                            return Ok(false);
                        } else {
                            // field value equality.
                            if !lv.1.equal(field_target.1.as_ref())? {
                                return Ok(false);
                            }
                        }
                    }
                    Ok(true)
                }
            }
            (Val::Constraint(a), Val::Constraint(b)) => Ok(a == b),
            // EMPTY is always comparable for equality.
            (&Val::Empty, _) => Ok(false),
            (_, &Val::Empty) => Ok(false),
            (me, tgt) => Err(error::BuildError::new(
                format!("Expected {} but got ({})", me.type_name(), tgt),
                error::ErrorType::TypeFail,
            )),
        }
    }

    /// Returns the fields if this Val is a tuple. None otherwise.
    pub fn get_fields(&self) -> Option<&Vec<(Rc<str>, Rc<Val>)>> {
        if let Val::Tuple(fs) = self {
            Some(fs)
        } else {
            None
        }
    }

    pub fn is_int(&self) -> bool {
        if let &Val::Int(_) = self {
            return true;
        }
        false
    }

    pub fn is_empty(&self) -> bool {
        if let &Val::Empty = self {
            return true;
        }
        false
    }

    pub fn is_float(&self) -> bool {
        if let &Val::Float(_) = self {
            return true;
        }
        false
    }

    pub fn is_string(&self) -> bool {
        if let &Val::Str(_) = self {
            return true;
        }
        false
    }

    pub fn is_tuple(&self) -> bool {
        if let &Val::Tuple(_) = self {
            return true;
        }
        false
    }

    pub fn is_env(&self) -> bool {
        if let &Val::Env(_) = self {
            return true;
        }
        false
    }

    pub fn is_list(&self) -> bool {
        if let &Val::List(_) = self {
            return true;
        }
        false
    }

    pub fn is_bool(&self) -> bool {
        if let &Val::Boolean(_) = self {
            return true;
        }
        false
    }

    pub fn is_str(&self) -> bool {
        if let &Val::Str(_) = self {
            return true;
        }
        false
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Val::Boolean(b) => write!(f, "{}", b),
            &Val::Empty => write!(f, "NULL"),
            Val::Float(ff) => write!(f, "{}", ff),
            Val::Int(i) => write!(f, "{}", i),
            Val::Str(s) => write!(f, "\"{}\"", s.replace("\"", "\\\"")),
            Val::List(def) => {
                write!(f, "[")?;
                for v in def.iter() {
                    write!(f, "{}, ", v)?;
                }
                write!(f, "]")
            }
            Val::Tuple(def) => {
                writeln!(f, "{{")?;
                for v in def.iter() {
                    writeln!(f, "\t{} = {},", v.0, v.1)?;
                }
                write!(f, "}}")
            }
            Val::Env(def) => {
                writeln!(f, "{{")?;
                for v in def.iter() {
                    writeln!(f, "\t{}=\"{}\"", v.0, v.1)?;
                }
                write!(f, "}}")
            }
            Val::Constraint(cv) => {
                for (i, arm) in cv.arms.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    match arm {
                        ConstraintValArm::Range(ConstraintBound::Int(min, max)) => {
                            write!(f, "in ")?;
                            if let Some(lo) = min {
                                write!(f, "{}", lo)?;
                            }
                            write!(f, "..")?;
                            if let Some(hi) = max {
                                write!(f, "{}", hi)?;
                            }
                        }
                        ConstraintValArm::Range(ConstraintBound::Float(min, max)) => {
                            write!(f, "in ")?;
                            if let Some(lo) = min {
                                write!(f, "{}", lo)?;
                            }
                            write!(f, "..")?;
                            if let Some(hi) = max {
                                write!(f, "{}", hi)?;
                            }
                        }
                        ConstraintValArm::Exact(val) => {
                            write!(f, "{}", val)?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}

impl From<Val> for Rc<str> {
    fn from(v: Val) -> Self {
        match v {
            Val::Int(ref i) => format!("{}", i),
            Val::Float(ref f) => format!("{}", f),
            Val::Str(ref s) => s.to_string(),
            Val::Boolean(ref b) => format!("{}", b),
            Val::Empty => "NULL".to_string(),
            val => format!("{}", val),
        }
        .into()
    }
}

impl From<Rc<str>> for Val {
    fn from(s: Rc<str>) -> Val {
        Val::Str(s)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::rc::Rc;

    fn str_val(s: &str) -> Val {
        Val::Str(Rc::from(s))
    }

    fn int_val(i: i64) -> Val {
        Val::Int(i)
    }

    fn tuple_val(fields: Vec<(&str, Val)>) -> Val {
        Val::Tuple(
            fields
                .into_iter()
                .map(|(k, v)| (Rc::from(k), Rc::new(v)))
                .collect(),
        )
    }

    fn list_val(items: Vec<Val>) -> Val {
        Val::List(items.into_iter().map(Rc::new).collect())
    }

    // type_name tests

    #[test]
    fn type_name_all_variants() {
        assert_eq!(Val::Empty.type_name(), "EmptyValue");
        assert_eq!(Val::Boolean(true).type_name(), "Boolean");
        assert_eq!(int_val(1).type_name(), "Integer");
        assert_eq!(Val::Float(1.0).type_name(), "Float");
        assert_eq!(str_val("hi").type_name(), "String");
        assert_eq!(list_val(vec![]).type_name(), "List");
        assert_eq!(tuple_val(vec![]).type_name(), "Tuple");
        assert_eq!(Val::Env(vec![]).type_name(), "Env");
    }

    // type_equal tests

    #[test]
    fn type_equal_same_types() {
        assert!(Val::Empty.type_equal(&Val::Empty));
        assert!(Val::Boolean(true).type_equal(&Val::Boolean(false)));
        assert!(int_val(1).type_equal(&int_val(2)));
        assert!(Val::Float(1.0).type_equal(&Val::Float(2.0)));
        assert!(str_val("a").type_equal(&str_val("b")));
        assert!(list_val(vec![]).type_equal(&list_val(vec![int_val(1)])));
        assert!(tuple_val(vec![]).type_equal(&tuple_val(vec![("x", int_val(1))])));
        assert!(Val::Env(vec![]).type_equal(&Val::Env(vec![(Rc::from("k"), Rc::from("v"))])));
    }

    #[test]
    fn type_equal_different_types() {
        assert!(!Val::Empty.type_equal(&int_val(0)));
        assert!(!Val::Boolean(true).type_equal(&int_val(1)));
        assert!(!int_val(1).type_equal(&Val::Float(1.0)));
        assert!(!str_val("a").type_equal(&list_val(vec![])));
        assert!(!tuple_val(vec![]).type_equal(&Val::Env(vec![])));
    }

    // equal tests

    #[test]
    fn equal_empty() {
        assert!(Val::Empty.equal(&Val::Empty).unwrap());
    }

    #[test]
    fn equal_empty_vs_other_is_false() {
        assert!(!Val::Empty.equal(&int_val(0)).unwrap());
        assert!(!int_val(0).equal(&Val::Empty).unwrap());
    }

    #[test]
    fn equal_primitives() {
        assert!(int_val(42).equal(&int_val(42)).unwrap());
        assert!(!int_val(1).equal(&int_val(2)).unwrap());

        assert!(Val::Float(3.14).equal(&Val::Float(3.14)).unwrap());
        assert!(!Val::Float(1.0).equal(&Val::Float(2.0)).unwrap());

        assert!(Val::Boolean(true).equal(&Val::Boolean(true)).unwrap());
        assert!(!Val::Boolean(true).equal(&Val::Boolean(false)).unwrap());

        assert!(str_val("hello").equal(&str_val("hello")).unwrap());
        assert!(!str_val("a").equal(&str_val("b")).unwrap());
    }

    #[test]
    fn equal_lists() {
        let a = list_val(vec![int_val(1), int_val(2)]);
        let b = list_val(vec![int_val(1), int_val(2)]);
        assert!(a.equal(&b).unwrap());

        let c = list_val(vec![int_val(1), int_val(3)]);
        assert!(!a.equal(&c).unwrap());

        let d = list_val(vec![int_val(1)]);
        assert!(!a.equal(&d).unwrap());
    }

    #[test]
    fn equal_tuples() {
        let a = tuple_val(vec![("x", int_val(1)), ("y", str_val("hi"))]);
        let b = tuple_val(vec![("x", int_val(1)), ("y", str_val("hi"))]);
        assert!(a.equal(&b).unwrap());

        let c = tuple_val(vec![("x", int_val(1)), ("z", str_val("hi"))]);
        assert!(!a.equal(&c).unwrap()); // different field name

        let d = tuple_val(vec![("x", int_val(1)), ("y", str_val("bye"))]);
        assert!(!a.equal(&d).unwrap()); // different field value
    }

    #[test]
    fn equal_type_mismatch_is_error() {
        let result = int_val(1).equal(&str_val("1"));
        assert!(result.is_err());
    }

    // is_* predicate tests

    #[test]
    fn is_predicates() {
        assert!(Val::Empty.is_empty());
        assert!(!Val::Empty.is_int());

        assert!(int_val(1).is_int());
        assert!(!int_val(1).is_float());

        assert!(Val::Float(1.0).is_float());
        assert!(str_val("x").is_string());
        assert!(str_val("x").is_str());
        assert!(Val::Boolean(true).is_bool());
        assert!(list_val(vec![]).is_list());
        assert!(tuple_val(vec![]).is_tuple());
        assert!(Val::Env(vec![]).is_env());
    }

    // get_fields tests

    #[test]
    fn get_fields_tuple() {
        let t = tuple_val(vec![("a", int_val(1))]);
        assert!(t.get_fields().is_some());
        assert_eq!(t.get_fields().unwrap().len(), 1);
    }

    #[test]
    fn get_fields_non_tuple() {
        assert!(int_val(1).get_fields().is_none());
    }

    // Display tests

    #[test]
    fn display_primitives() {
        assert_eq!(format!("{}", Val::Empty), "NULL");
        assert_eq!(format!("{}", Val::Boolean(true)), "true");
        assert_eq!(format!("{}", int_val(42)), "42");
        assert_eq!(format!("{}", Val::Float(3.14)), "3.14");
        assert_eq!(format!("{}", str_val("hello")), "\"hello\"");
    }

    #[test]
    fn display_str_escapes_quotes() {
        assert_eq!(format!("{}", str_val("say \"hi\"")), "\"say \\\"hi\\\"\"");
    }

    #[test]
    fn display_list() {
        let l = list_val(vec![int_val(1), int_val(2)]);
        assert_eq!(format!("{}", l), "[1, 2, ]");
    }

    #[test]
    fn display_tuple() {
        let t = tuple_val(vec![("x", int_val(1))]);
        assert_eq!(format!("{}", t), "{\n\tx = 1,\n}");
    }

    // From conversion tests

    #[test]
    fn from_val_to_rc_str() {
        let s: Rc<str> = int_val(42).into();
        assert_eq!(&*s, "42");

        let s: Rc<str> = Val::Float(3.14).into();
        assert_eq!(&*s, "3.14");

        let s: Rc<str> = str_val("hello").into();
        assert_eq!(&*s, "hello");

        let s: Rc<str> = Val::Boolean(true).into();
        assert_eq!(&*s, "true");

        let s: Rc<str> = Val::Empty.into();
        assert_eq!(&*s, "NULL");
    }

    #[test]
    fn from_rc_str_to_val() {
        let s: Rc<str> = Rc::from("test");
        let v: Val = s.into();
        assert!(v.is_str());
        assert!(v.equal(&str_val("test")).unwrap());
    }

    // ConstraintVal tests

    #[test]
    fn constraint_type_name() {
        let cv = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Int(
                Some(1),
                Some(10),
            ))],
        };
        assert_eq!(Val::Constraint(cv).type_name(), "Constraint");
    }

    #[test]
    fn constraint_int_range_check() {
        let cv = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Int(
                Some(1),
                Some(100),
            ))],
        };
        assert!(cv.check(&int_val(1)));
        assert!(cv.check(&int_val(50)));
        assert!(cv.check(&int_val(100)));
        assert!(!cv.check(&int_val(0)));
        assert!(!cv.check(&int_val(101)));
    }

    #[test]
    fn constraint_int_range_open_end() {
        let cv = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Int(Some(0), None))],
        };
        assert!(cv.check(&int_val(0)));
        assert!(cv.check(&int_val(999999)));
        assert!(!cv.check(&int_val(-1)));
    }

    #[test]
    fn constraint_int_range_open_start() {
        let cv = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Int(
                None,
                Some(100),
            ))],
        };
        assert!(cv.check(&int_val(100)));
        assert!(cv.check(&int_val(-999)));
        assert!(!cv.check(&int_val(101)));
    }

    #[test]
    fn constraint_float_range_check() {
        let cv = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Float(
                Some(0.0),
                Some(1.0),
            ))],
        };
        assert!(cv.check(&Val::Float(0.0)));
        assert!(cv.check(&Val::Float(0.5)));
        assert!(cv.check(&Val::Float(1.0)));
        assert!(!cv.check(&Val::Float(-0.1)));
        assert!(!cv.check(&Val::Float(1.1)));
    }

    #[test]
    fn constraint_range_rejects_wrong_type() {
        let cv = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Int(
                Some(1),
                Some(10),
            ))],
        };
        assert!(!cv.check(&str_val("hello")));
        assert!(!cv.check(&Val::Boolean(true)));
        assert!(!cv.check(&Val::Float(5.0)));
    }

    #[test]
    fn constraint_exact_check() {
        let cv = ConstraintVal {
            arms: vec![
                ConstraintValArm::Exact(Rc::new(str_val("active"))),
                ConstraintValArm::Exact(Rc::new(str_val("inactive"))),
            ],
        };
        assert!(cv.check(&str_val("active")));
        assert!(cv.check(&str_val("inactive")));
        assert!(!cv.check(&str_val("unknown")));
    }

    #[test]
    fn constraint_combined_range_and_exact() {
        let cv = ConstraintVal {
            arms: vec![
                ConstraintValArm::Range(ConstraintBound::Int(Some(1), Some(1024))),
                ConstraintValArm::Exact(Rc::new(int_val(8080))),
                ConstraintValArm::Exact(Rc::new(int_val(8443))),
            ],
        };
        assert!(cv.check(&int_val(80)));
        assert!(cv.check(&int_val(8080)));
        assert!(cv.check(&int_val(8443)));
        assert!(!cv.check(&int_val(2000)));
        assert!(!cv.check(&int_val(9999)));
    }

    #[test]
    fn constraint_equality() {
        let cv1 = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Int(
                Some(1),
                Some(10),
            ))],
        };
        let cv2 = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Int(
                Some(1),
                Some(10),
            ))],
        };
        assert!(Val::Constraint(cv1.clone())
            .equal(&Val::Constraint(cv2))
            .unwrap());

        let cv3 = ConstraintVal {
            arms: vec![ConstraintValArm::Range(ConstraintBound::Int(
                Some(1),
                Some(20),
            ))],
        };
        assert!(!Val::Constraint(cv1).equal(&Val::Constraint(cv3)).unwrap());
    }

    #[test]
    fn constraint_display() {
        let cv = ConstraintVal {
            arms: vec![
                ConstraintValArm::Range(ConstraintBound::Int(Some(1), Some(1024))),
                ConstraintValArm::Exact(Rc::new(int_val(8080))),
            ],
        };
        let s = format!("{}", Val::Constraint(cv));
        assert!(s.contains("in 1..1024"));
        assert!(s.contains("| 8080"));
    }
}
