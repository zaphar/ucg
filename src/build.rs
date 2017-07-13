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
use parse::{parse, Statement, Expression, Value, FieldList, SelectorList};

use std::error::Error;
use std::collections::{HashMap, VecDeque};
use std::collections::hash_map::Entry;
use std::rc::Rc;
use std::fmt;
use std::fmt::{Display,Formatter};

quick_error! {
    #[derive(Debug,PartialEq)]
    pub enum BuildError {
        TypeFail(msg: String) {
            description("Type Error")
            display("Type Error {}", msg)
        }
        Unsupported(msg: String) {
            description("Unsupported Operation")
            display("Unsupported Operation {}", msg)
        }
        NoSuchSymbol(msg: String) {
            description("Eval Error")
            display("No Such Variable {}", msg)
        }
        TODO(msg: String) {
            description("TODO Error")
            display("TODO Error {}", msg)
        }
    }
}

/// BuildResult is the result of a build.
type BuildResult = Result<(), Box<Error>>;

/// Val is the type of a value for a field in a Tuple.
#[derive(PartialEq,Debug,Clone)]
pub enum Val<'a> {
    Int(i64),
    Float(f64),
    String(String),
    Tuple(Vec<(&'a str, Rc<Val<'a>>)>),
}

impl<'a> Val<'a> {

    pub fn type_name(&self) -> String {
        match self {
            &Val::Int(_) => "Integer".to_string(),
            &Val::Float(_) => "Float".to_string(),
            &Val::String(_) => "String".to_string(),
            &Val::Tuple(_) => "Tuple".to_string(),
        }
    }

    pub fn type_equal(&self, target: &Self) -> bool {
        match self {
            &Val::Int(_) => if let &Val::Int(_) = target { true } else { false },
            &Val::Float(_) => if let &Val::Float(_) = target { true } else { false },
            &Val::String(_) => if let &Val::String(_) = target { true } else { false },
            &Val::Tuple(_) => if let &Val::Tuple(_) = target { true } else { false },
        }
    }

    pub fn get_fields(&self) -> Option<&Vec<(&'a str, Rc<Val<'a>>)>> {
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

    pub fn is_float(&self) -> bool {
        if let &Val::Float(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_string(&self) -> bool {
        if let &Val::String(_) = self {
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
}

impl<'a> Display for Val<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // TODO(jwall): These should render better than this.
        write!(f, "{}", self.type_name())
    }
}

/// ValueMap defines a set of values in a parsed file.
#[derive(PartialEq,Debug)]
pub struct ValueMap<'a>(HashMap<&'a str, Rc<Val<'a>>>);

/// Builder parses one or more statements into a out Tuple.
pub struct Builder<'a> {
    /// env is the immutable set of key value pairs provided at build time.
    env: HashMap<&'a str, &'a str>,
    /// assets are other parsed files from import statements. They
    /// are keyed by the normalized import path. This acts as a cache
    /// so multiple imports of the same file don't have to be parsed
    /// multiple times.
    assets: HashMap<&'a str, ValueMap<'a>>,
    /// out is our built output.
    out: HashMap<&'a str, Rc<Val<'a>>>,
}

macro_rules! eval_binary_expr {
    ($case:pat, $rside:ident, $result:expr, $msg:expr) => {
        match $rside.as_ref() {
            $case => {
                return Ok(Rc::new($result))
            },
            val => {
                return Err(Box::new(
                    BuildError::TypeFail(
                        format!("Expected {} but got {}", $msg, val))))
            }
        }
    }
}

impl<'a> Builder<'a> {
    /// new_builder constructs Builder with initialized fields ready to parse.
    fn from(&self, v: Value<'a>) -> Result<Rc<Val>, Box<Error>> {
        match v {
            Value::Int(i) => Ok(Rc::new(Val::Int(i))),
            Value::Float(f) => Ok(Rc::new(Val::Float(f))),
            Value::String(s) => Ok(Rc::new(Val::String(s.to_string()))),
            Value::Symbol(s) => {
                self.lookup_sym(s).ok_or(Box::new(
                    BuildError::NoSuchSymbol(s.to_string())))
            },
            Value::Tuple(fields) => {
                // TODO(jwall): We need to resolve the expressions here.
                // TODO(jwall): Don't forget we want a stable order for the fields
                //   in a tuple to make Vec comparisons easier later on.
                Err(Box::new(
                    BuildError::TODO(
                        "Value::Tuple to Val::Tuple is not yet impolemented.".to_string())))
            },
            Value::Selector(selector_list) => {
                self.lookup_selector(selector_list)
            },
        }
    }

    pub fn new() -> Self {
        Builder {
            env: HashMap::new(),
            assets: HashMap::new(),
            out: HashMap::new(),
        }
    }

    pub fn build_dir(&mut self, name: &str) -> BuildResult {
        Ok(())
    }

    pub fn build_file(&mut self, name: &str) -> BuildResult {
        Ok(())
    }

    pub fn build(&mut self, ast: Vec<Statement>) -> BuildResult {
        // TODO(jwall):
        for stmt in ast.iter() {
            self.build_stmt(stmt);
        }
        Ok(())
    }

    fn lookup_sym(&'a self, sym: &str) -> Option<Rc<Val>> {
        if self.out.contains_key(sym) {
            Some(self.out[sym].clone())
        } else {
            None
        }
    }

    fn find_in_fieldlist(target: &str, fs: &Vec<(&str, Rc<Val<'a>>)>) -> Option<Rc<Val<'a>>> {
        for (key, val) in fs.iter().cloned() {
            if target == key {
                return Some(val.clone())
            }
        }
        return None
    }

    fn lookup_selector(&'a self, sl: SelectorList) -> Result<Rc<Val>, Box<Error>> {
        let len = sl.len();
        if len > 0 {
            println!("Looking up symbol {}", sl[0]);
            if let Some(v) = self.lookup_sym(sl[0]) {
                let mut it = sl.iter().skip(1).peekable();
                if it.peek().is_none() {
                    return Ok(v.clone());
                }
                if let &Val::Tuple(ref fs) = v.as_ref() {
                    let mut stack = VecDeque::new();
                    stack.push_back(v.clone());
                    loop {
                        let vref = stack.pop_front().unwrap();
                        if it.peek().is_none() {
                            return Ok(vref.clone());
                        }
                        // This unwrap is safe because we already checked for
                        // None above.
                        let k = it.next().unwrap();
                        if !vref.is_tuple() {
                            return Err(Box::new(BuildError::NoSuchSymbol(
                                format!("Attempted to dereference non-tuple {:?} at field {}.", sl, k))));
                        }
                        // This unwrap is safe because we already checked for
                        // Tuple above.
                        let fs = vref.get_fields().unwrap();
                        if let Some(vv) = Self::find_in_fieldlist(k, fs) {
                            if vv.is_tuple() {
                                stack.push_back(vv.clone());
                                continue;
                            }
                            if it.peek().is_some() {
                                return Err(Box::new(BuildError::NoSuchSymbol(
                                    format!("Unable to match selector path {:?}", sl))));
                            } else {
                                return Ok(vv.clone());
                            }
                        } else {
                            // TODO(jwall): A better error for this would be nice.
                            return Err(Box::new(BuildError::NoSuchSymbol(
                                format!("Unable to match selector path {:?}", sl))));
                        }
                    };
                }
                return Err(Box::new(BuildError::TypeFail(
                    format!("{} is not a Tuple", sl[0]))));
            }
            return Err(Box::new(BuildError::NoSuchSymbol(
                format!("Unable to find Symbol {}", sl[0]))));
        }
        return Err(Box::new(BuildError::NoSuchSymbol(
            "Attempted to lookup an empty selector".to_string())));
    }

    // eval_expr evals a single Expression in the context of a running Builder.
    // It does not mutate the builders collected state at all.
    pub fn eval_expr(&'a self, expr: Expression<'a>) -> Result<Rc<Val>, Box<Error>> {
        match expr {
            Expression::Simple(val) => {
                self.from(val)
            },
            Expression::Add(v, expr) => {
                let expr_result = try!(self.eval_expr(*expr));
                let v = try!(self.from(*v));
                match *v {
                    Val::Int(i) => {
                        eval_binary_expr!(&Val::Int(ii), expr_result,
                                          Val::Int(i + ii), "Integer")
                    },
                    Val::Float(f) => {
                        eval_binary_expr!(&Val::Float(ff), expr_result,
                                          Val::Float(f + ff), "Float")
                    },
                    Val::String(ref s) => {
                        match expr_result.as_ref() {
                            &Val::String(ref ss) => {
                                return Ok(Rc::new(
                                    Val::String([s.to_string(), ss.clone()].concat())))
                            },
                            val => {
                                return Err(Box::new(
                                    BuildError::TypeFail(
                                        format!("Expected String but got {:?}", val))))
                            }
                        }
                    },
                    ref expr => {
                        return Err(Box::new(
                            BuildError::Unsupported(
                                format!("{} does not support the '+' operation", expr.type_name()))))
                    }
                }
            },
            Expression::Sub(v, expr) => {
                let expr_result = try!(self.eval_expr(*expr));
                let v = try!(self.from(*v));
                match *v {
                    Val::Int(i) => {
                        eval_binary_expr!(&Val::Int(ii), expr_result,
                                          Val::Int(i - ii), "Integer")
                    },
                    Val::Float(f) => {
                        eval_binary_expr!(&Val::Float(ff), expr_result,
                                          Val::Float(f - ff), "Float")
                    },
                    ref expr => {
                        return Err(Box::new(
                            BuildError::Unsupported(
                                format!("{} does not support the '-' operation", expr.type_name()))))
                    }
                }
            },
            Expression::Mul(v, expr) => {
                let expr_result = try!(self.eval_expr(*expr));
                let v = try!(self.from(*v));
                match *v {
                    Val::Int(i) => {
                        eval_binary_expr!(&Val::Int(ii), expr_result,
                                          Val::Int(i * ii), "Integer")
                    },
                    Val::Float(f) => {
                        eval_binary_expr!(&Val::Float(ff), expr_result,
                                          Val::Float(f * ff), "Float")
                    },
                    ref expr => {
                        return Err(Box::new(
                            BuildError::Unsupported(
                                format!("{} does not support the '*' operation", expr.type_name()))))
                    }
                }
            },
            Expression::Div(v, expr) => {
                let expr_result = try!(self.eval_expr(*expr));
                let v = try!(self.from(*v));
                match *v {
                    Val::Int(i) => {
                        eval_binary_expr!(&Val::Int(ii), expr_result,
                                          Val::Int(i / ii), "Integer")
                    },
                    Val::Float(f) => {
                        eval_binary_expr!(&Val::Float(ff), expr_result,
                                          Val::Float(f / ff), "Float")
                    },
                    ref expr => {
                        return Err(Box::new(
                            BuildError::Unsupported(
                                format!("{} does not support the '*' operation", expr.type_name()))))
                    }
                }
            },
            Expression::Copy(sel, mut fields) => {
                let v = try!(self.lookup_selector(sel));
                if let Val::Tuple(ref src_fields) = *v {
                    let mut m = HashMap::<&str, Rc<Val>>::new();
                    // loop through fields and build  up a hasmap
                    for &(ref key, ref val) in src_fields.iter() {
                        if let Entry::Vacant(v) = m.entry(*key) {
                            v.insert(val.clone());
                        } else {
                            // TODO(jwall): Is this an error?
                            return Err(Box::new(
                                BuildError::TypeFail(
                                    format!("Duplicate field: {} in tuple", *key))));
                        }
                    }
                    for (key, val) in fields.drain(0..) {
                        let expr_result = try!(self.eval_expr(val));
                        match m.entry(key) {
                            Entry::Vacant(mut v) => {
                                v.insert(expr_result);
                            },
                            Entry::Occupied(mut v) => {
                                // Ensure that the new type matches the old type.
                                // TODO(jwall): This copy is ugly but I don't think it's possible
                                // to both compare and replace this at the same time.
                                let src_val = v.get().clone();
                                if src_val.type_equal(&expr_result) {
                                    v.insert(expr_result);
                                } else {
                                    return Err(Box::new(
                                        BuildError::TypeFail(
                                            format!("Expected type {} for field {} but got {}",
                                                    src_val.type_name(), key, expr_result.type_name()))));
                                }
                            },
                        };
                    }
                    let mut new_fields: Vec<(&str, Rc<Val>)> = m.drain().collect();
                    // We want a stable order for the fields to make comparing tuples
                    // easier in later code. So we sort by the field name before constructing a new tuple.
                    new_fields.sort_by(|a, b| a.0.cmp(b.0));
                    return Ok(Rc::new(Val::Tuple(new_fields)));
                }
                Err(Box::new(
                    BuildError::TypeFail(
                        format!("Expected Tuple got {}", v))))
            },
            Expression::Grouped(expr) => {
                return self.eval_expr(*expr);
            },
            Expression::Call{lambda: sel, arglist: args} => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
            Expression::Lambda{arglist: args, tuple: fields} => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
            Expression::Select{val: target, default: def_expr, tuple: fields} => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
        }
    }

    fn build_stmt(&self, stmt: &Statement) -> BuildResult {
        match stmt {
            &Statement::Let { name: ref sym, value: ref expr } => {
                // TODO
            }
            &Statement::Import { path: ref val, name: ref sym } => {
                // TODO
            }
            &Statement::Expression(ref expr) => {
                // TODO
            }
        };
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::{Builder,Val};
    use parse::{Expression, Value};
    use std::rc::Rc;

    fn test_expr_to_val(mut cases: Vec<(Expression,Val)>, b: Builder) {
        for tpl in cases.drain(0..) {
            assert_eq!(b.eval_expr(tpl.0).unwrap(), Rc::new(tpl.1));
        }
    }

    #[test]
    fn test_eval_div_expr() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Div(Box::new(Value::Int(2)), Box::new(Expression::Simple(Value::Int(2)))),
             Val::Int(1)),
            (Expression::Div(Box::new(Value::Float(2.0)), Box::new(Expression::Simple(Value::Float(2.0)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_div_expr_fail() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Div(Box::new(Value::Float(2.0)), Box::new(Expression::Simple(Value::Int(2)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    fn test_eval_mul_expr() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Mul(Box::new(Value::Int(2)), Box::new(Expression::Simple(Value::Int(2)))),
             Val::Int(4)),
            (Expression::Mul(Box::new(Value::Float(2.0)), Box::new(Expression::Simple(Value::Float(2.0)))),
             Val::Float(4.0)),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_mul_expr_fail() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Mul(Box::new(Value::Float(2.0)), Box::new(Expression::Simple(Value::Int(2)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    fn test_eval_subtract_expr() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Sub(Box::new(Value::Int(2)), Box::new(Expression::Simple(Value::Int(1)))),
             Val::Int(1)),
            (Expression::Sub(Box::new(Value::Float(2.0)), Box::new(Expression::Simple(Value::Float(1.0)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_subtract_expr_fail() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Sub(Box::new(Value::Float(2.0)), Box::new(Expression::Simple(Value::Int(2)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    fn test_eval_add_expr() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Add(Box::new(Value::Int(1)), Box::new(Expression::Simple(Value::Int(1)))),
             Val::Int(2)),
            (Expression::Add(Box::new(Value::Float(1.0)), Box::new(Expression::Simple(Value::Float(1.0)))),
             Val::Float(2.0)),
            (Expression::Add(Box::new(Value::String("foo")), Box::new(Expression::Simple(Value::String("bar")))),
             Val::String("foobar".to_string())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_add_expr_fail() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Add(Box::new(Value::Float(2.0)), Box::new(Expression::Simple(Value::Int(2)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    fn test_eval_simple_expr() {
        test_expr_to_val(vec![
            (Expression::Simple(Value::Int(1)), Val::Int(1)),
            (Expression::Simple(Value::Float(2.0)), Val::Float(2.0)),
            (Expression::Simple(Value::String("foo")), Val::String("foo".to_string())),
        ], Builder::new());
    }

    #[test]
    fn test_eval_simple_lookup_expr() {
        let mut b = Builder::new();
        b.out.entry("var1").or_insert(Rc::new(Val::Int(1)));
        test_expr_to_val(vec![
            (Expression::Simple(Value::Symbol("var1")), Val::Int(1)),
        ], b);
    }

    #[test]
    fn test_eval_simple_lookup_error() {
        let mut b = Builder::new();
        b.out.entry("var1").or_insert(Rc::new(Val::Int(1)));
        assert!(b.eval_expr(Expression::Simple(Value::Symbol("var"))).is_err());
    }

    #[test]
    fn test_eval_selector_expr() {
        // TODO(jwall): Tests for this expression.
        let mut b = Builder::new();
        b.out.entry("var1").or_insert(Rc::new(Val::Tuple(vec![
            ("lvl1", Rc::new(Val::Tuple(
                vec![
                    ("lvl2", Rc::new(Val::Int(3))),
                ]
            ))),
        ])));
        b.out.entry("var2").or_insert(Rc::new(Val::Int(2)));
        b.out.entry("var3").or_insert(Rc::new(Val::Tuple(vec![
            ("lvl1", Rc::new(Val::Int(4)))
        ])));
        test_expr_to_val(vec![
            (Expression::Simple(Value::Selector(vec!["var1"])), Val::Tuple(
                vec![
                    ("lvl1", Rc::new(Val::Tuple(
                        vec![
                            ("lvl2", Rc::new(Val::Int(3))),
                        ]
                    ))),
                ]
            )),
            (Expression::Simple(Value::Selector(vec!["var1","lvl1"])), Val::Tuple(
                vec![
                    ("lvl2", Rc::new(Val::Int(3))),
                ]
            )),
            (Expression::Simple(Value::Selector(vec!["var1","lvl1", "lvl2"])), Val::Int(3)),
            (Expression::Simple(Value::Selector(vec!["var2"])), Val::Int(2)),
            (Expression::Simple(Value::Selector(vec!["var3", "lvl1"])), Val::Int(4)),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Unable to find Symbol tpl1")]
    fn test_expr_copy_no_such_tuple() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Copy(vec!["tpl1"], Vec::new()),
             Val::Tuple(Vec::new())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Tuple got Integer")]
    fn test_expr_copy_not_a_tuple() {
        let mut b = Builder::new();
        b.out.entry("tpl1").or_insert(Rc::new(Val::Int(1)));
        test_expr_to_val(vec![
            (Expression::Copy(vec!["tpl1"], Vec::new()),
             Val::Tuple(Vec::new())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected type Integer for field fld1 but got String")]
    fn test_expr_copy_field_type_error() {
        let mut b = Builder::new();
        b.out.entry("tpl1").or_insert(Rc::new(Val::Tuple(vec![
            ("fld1", Rc::new(Val::Int(1))),
        ])));
        test_expr_to_val(vec![
            (Expression::Copy(vec!["tpl1"],
                              vec![("fld1", Expression::Simple(Value::String("2")))]),
             Val::Tuple(
                vec![
                    ("fld1", Rc::new(Val::String("2".to_string()))),
                ],
            )),
        ], b);
    }

    // TODO(jwall): What about the duplicate field error?

    #[test]
    fn test_expr_copy() {
        // TODO(jwall): Tests for this expression.
        let mut b = Builder::new();
        b.out.entry("tpl1").or_insert(Rc::new(Val::Tuple(vec![
            ("fld1", Rc::new(Val::Int(1))),
        ])));
        test_expr_to_val(vec![
            (Expression::Copy(vec!["tpl1"],
                              vec![("fld2", Expression::Simple(Value::String("2")))]),
             // Add a new field to the copy
             Val::Tuple(
                 // NOTE(jwall): The order of these is important in order to ensure
                 // that the compare assertion is correct. The ordering has no
                 // semantics though so at some point we should probably be less restrictive.
                vec![
                    ("fld1", Rc::new(Val::Int(1))),
                    ("fld2", Rc::new(Val::String("2".to_string()))),
                ],
            )),
             // Overwrite a field in the copy
            (Expression::Copy(vec!["tpl1"],
                              vec![
                                  ("fld1", Expression::Simple(Value::Int(3))),
                                  ("fld2", Expression::Simple(Value::String("2"))),
                              ]),
             Val::Tuple(
                vec![
                    ("fld1", Rc::new(Val::Int(3))),
                    ("fld2", Rc::new(Val::String("2".to_string()))),
                ],
             )),
            // The source tuple is still unmodified.
            (Expression::Simple(Value::Selector(vec!["tpl1"])),
             Val::Tuple(
                vec![
                    ("fld1", Rc::new(Val::Int(1))),
                ],
            )),
        ], b);
    }
}
