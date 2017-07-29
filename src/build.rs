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
use parse::{parse,Statement,Expression,Value,FieldList,SelectorList};

use std::fs::File;
use std::io::Read;
use std::error::Error;
use std::collections::{HashSet,HashMap, VecDeque};
use std::collections::hash_map::Entry;
use std::fmt;
use std::fmt::{Display,Formatter};
use std::ops::Deref;
use std::rc::Rc;

use nom;

quick_error! {
    #[derive(Debug,PartialEq)]
    pub enum BuildError {
        TypeFail(msg: String) {
            description("Type Error")
            display("Type Error {}", msg)
        }
        DuplicateBinding(msg: String) {
            description("Atttempt to add duplicate binding in file")
            display("Atttempt to add duplicate binding in file {}", msg)
        }
        IncompleteParse(msg: String) {
            description("Incomplete Parse of file")
            display("Incomplete Parse of file {}", msg)
        }
        Unsupported(msg: String) {
            description("Unsupported Operation")
            display("Unsupported Operation {}", msg)
        }
        NoSuchSymbol(msg: String) {
            description("Eval Error")
            display("No Such Variable {}", msg)
        }
        BadArgLen(msg: String) {
            description("Eval Error")
            display("Bad Argument Length {}", msg)
        }
        TODO(msg: String) {
            description("TODO Error")
            display("TODO Error {}", msg)
        }
    }
}

/// BuildResult is the result of a build.
type BuildResult = Result<(), Box<Error>>;

/// MacroDef is a pure function that always returns a Tuple.
///
/// MacroDef's are not closures. They can not reference
/// any values except what is defined in their arguments.
#[derive(PartialEq,Debug,Clone)]
pub struct MacroDef {
    argdefs: Vec<String>,
    fields: FieldList,
}

impl MacroDef {
    fn eval(&self, mut args: Vec<Rc<Val>>) -> Result<Vec<(String, Rc<Val>)>, Box<Error>> {
        // Error conditions. If the args don't match the length and types of the argdefs then this is
        // macro call error.
        if args.len() > self.argdefs.len() {
            return Err(Box::new(
                BuildError::BadArgLen(
                    "Macro called with too many args".to_string())));
        }
        // If the args don't match the types required by the expressions then that is a TypeFail.
        // If the expressions reference Symbols not defined in the MacroDef that is also an error.
        // TODO(jwall): We should probably enforce that the Expression Symbols must be in argdefs rules
        // at Macro definition time not evaluation time.
        let mut scope = HashMap::new();
        for (i, arg) in args.drain(0..).enumerate() {
            scope.entry(self.argdefs[i].clone()).or_insert(arg.clone());
        }
        let b = Builder::new_with_scope(scope);
        let mut result: Vec<(String, Rc<Val>)> = Vec::new();
        for &(ref key, ref expr) in self.fields.iter() {
            // We clone the expressions here because this macro may be consumed
            // multiple times in the future.
            let val = try!(b.eval_expr(expr.clone()));
            result.push((key.clone(), val.clone()));
        }
        Ok(result)
    }
}

/// Val is the type of a value for a field in a Tuple.
#[derive(PartialEq,Debug,Clone)]
pub enum Val {
    Int(i64),
    Float(f64),
    String(String),
    Tuple(Vec<(String, Rc<Val>)>),
    Macro(MacroDef),
}

impl Val {

    pub fn type_name(&self) -> String {
        match self {
            &Val::Int(_) => "Integer".to_string(),
            &Val::Float(_) => "Float".to_string(),
            &Val::String(_) => "String".to_string(),
            &Val::Tuple(_) => "Tuple".to_string(),
            &Val::Macro(_) => "Macro".to_string(),
        }
    }

    pub fn type_equal(&self, target: &Self) -> bool {
        match self {
            &Val::Int(_) => if let &Val::Int(_) = target { true } else { false },
            &Val::Float(_) => if let &Val::Float(_) = target { true } else { false },
            &Val::String(_) => if let &Val::String(_) = target { true } else { false },
            &Val::Tuple(_) => if let &Val::Tuple(_) = target { true } else { false },
            &Val::Macro(_) => if let &Val::Macro(_) = target { true } else { false },
        }
    }

    pub fn get_fields(&self) -> Option<&Vec<(String, Rc<Val>)>> {
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

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // TODO(jwall): These should render better than this.
        write!(f, "{}", self.type_name())
    }
}

/// ValueMap defines a set of values in a parsed file.
type ValueMap = HashMap<String, Rc<Val>>;

/// Builder parses one or more statements into a out Tuple.
pub struct Builder {
    /// assets are other parsed files from import statements. They
    /// are keyed by the normalized import path. This acts as a cache
    /// so multiple imports of the same file don't have to be parsed
    /// multiple times.
    assets: ValueMap,
    // List of file paths we have already parsed.
    files: HashSet<String>,
    /// out is our built output.
    out: ValueMap,
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

impl Builder {
    /// new_builder constructs Builder with initialized fields ready to parse.
    fn value_to_val(&self, v: Value) -> Result<Rc<Val>, Box<Error>> {
        match v {
            Value::Int(i) => Ok(Rc::new(Val::Int(i))),
            Value::Float(f) => Ok(Rc::new(Val::Float(f))),
            Value::String(s) => Ok(Rc::new(Val::String(s.to_string()))),
            Value::Symbol(s) => {
                self.lookup_sym(&s).ok_or(Box::new(
                    BuildError::NoSuchSymbol(format!("Unable to find {}", s))))
            },
            Value::Tuple(mut fields) => {
                let mut new_fields = Vec::new();
                for (name, expr) in fields.drain(0..) {
                    let val = try!(self.eval_expr(expr));
                    new_fields.push((name, val));
                }
                new_fields.sort_by(|a, b| a.0.cmp(&b.0));
                Ok(Rc::new(Val::Tuple(new_fields)))
            },
            Value::Selector(selector_list) => {
                self.lookup_selector(selector_list)
            },
        }
    }

    pub fn new() -> Self {
        Builder {
            assets: HashMap::new(),
            files: HashSet::new(),
            out: HashMap::new(),
        }
    }

    pub fn new_with_scope(scope: HashMap<String, Rc<Val>>) -> Self {
        Builder {
            assets: HashMap::new(),
            files: HashSet::new(),
            out: scope,
        }
    }

    pub fn build(&mut self, mut ast: Vec<Statement>) -> BuildResult {
        for stmt in ast.drain(0..) {
            try!(self.build_stmt(stmt));
        }
        Ok(())
    }

    fn lookup_sym(&self, sym: &str) -> Option<Rc<Val>> {
        if self.out.contains_key(sym) {
            return Some(self.out[sym].clone());
        } if self.assets.contains_key(sym) {
            // TODO(jwall): Unit tests for this.
            return Some(self.assets[sym].clone());
        }
        None
    }

    fn find_in_fieldlist(target: &str, fs: &Vec<(String, Rc<Val>)>) -> Option<Rc<Val>> {
        for (key, val) in fs.iter().cloned() {
            if target == key {
                return Some(val.clone())
            }
        }
        return None
    }

    fn lookup_selector(&self, sl: SelectorList) -> Result<Rc<Val>, Box<Error>> {
        // TODO(jwall): This should also check the assets for imported files?
        let len = sl.len();
        if len > 0 {
            if let Some(v) = self.lookup_sym(&sl[0]) {
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
    pub fn eval_expr(&self, expr: Expression) -> Result<Rc<Val>, Box<Error>> {
        // TODO(jwall): We probably don't want to consume these expressions.
        //   Take a reference instead?
        match expr {
            Expression::Simple(val) => {
                self.value_to_val(val)
            },
            Expression::Add(v, expr) => {
                let expr_result = try!(self.eval_expr(*expr));
                let v = try!(self.value_to_val(*v));
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
                let v = try!(self.value_to_val(*v));
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
                let v = try!(self.value_to_val(*v));
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
                let v = try!(self.value_to_val(*v));
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
                    let mut m = HashMap::<String, Rc<Val>>::new();
                    // loop through fields and build  up a hasmap
                    for &(ref key, ref val) in src_fields.iter() {
                        if let Entry::Vacant(v) = m.entry(key.to_string()) {
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
                        match m.entry(key.clone()) {
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
                    let mut new_fields: Vec<(String, Rc<Val>)> = m.drain()
                        .map(|(s, v)| (s.to_string(), v))
                        .collect();
                    // We want a stable order for the fields to make comparing tuples
                    // easier in later code. So we sort by the field name before constructing a new tuple.
                    new_fields.sort_by(|a, b| a.0.cmp(&b.0));
                    return Ok(Rc::new(Val::Tuple(new_fields)));
                }
                Err(Box::new(
                    BuildError::TypeFail(
                        format!("Expected Tuple got {}", v))))
            },
            Expression::Grouped(expr) => {
                return self.eval_expr(*expr);
            },
            Expression::Call{macroref: sel, arglist: mut args} => {
                let v = try!(self.lookup_selector(sel));
                if let &Val::Macro(ref m) = v.deref() {
                    // Congratulations this is actually a macro.
                    let mut argvals: Vec<Rc<Val>> = Vec::new();
                    for arg in args.drain(0..) {
                        argvals.push(try!(self.eval_expr(arg)));
                    }
                    let fields = try!(m.eval(argvals));
                    return Ok(Rc::new(Val::Tuple(fields)));
                }
                Err(Box::new(
                    BuildError::TypeFail(
                        // We should pretty print the selectors here.
                        format!("{} is not a Macro", v))))
            },
            Expression::Macro{arglist: args, tuple: fields} => {
                // TODO(jwall): Walk the AST and verify that the symbols all
                // exist as names in the arglist.
                let md = MacroDef{
                    argdefs: args,
                    fields: fields,
                };
                Ok(Rc::new(Val::Macro(md)))
            },
            Expression::Select{val: target, default: def_expr, tuple: mut fields} => {
                // First resolve the target expression.
                let v = try!(self.eval_expr(*target));
                // Second ensure that the expression resolves to a string.
                if let &Val::String(ref name) = v.deref() {
                    // Third find the field with that name in the tuple.
                    for (fname, val_expr) in fields.drain(0..) {
                        if &fname == name {
                            // Fourth return the result of evaluating that field.
                            return self.eval_expr(val_expr);
                        }
                    }
                    // Otherwise return the default
                    return self.eval_expr(*def_expr);
                } else {
                    return Err(Box::new(
                        BuildError::TypeFail(
                            format!("Expected String but got {} in Select expression",
                                    v.type_name()))));
                }
            },
        }
    }

    pub fn build_file_string(&mut self, name: &str, input: String) -> BuildResult {
        match parse((&input[..]).as_bytes()) {
            nom::IResult::Done(_, mut stmts) => {
                for stmt in stmts.drain(0..) {
                    try!(self.build_stmt(stmt));
                }
                Ok(())
            },
            nom::IResult::Error(err) => Err(Box::new(err)),
            nom::IResult::Incomplete(_) => Err(Box::new(
                BuildError::IncompleteParse(format!("Could not parse input from file: {}", name)))),
        }
    }

    pub fn build_file(&mut self, name: &str) -> BuildResult {
        let mut f = try!(File::open(name));
        let mut s = String::new();
        // TODO(jwall): It would be nice to be able to do this with streaming
        try!(f.read_to_string(&mut s));
        self.build_file_string(name, s)
    }

    fn build_stmt(&mut self, stmt: Statement) -> BuildResult {
        match stmt {
            Statement::Let { name: sym, value: expr } => {
                let val = try!(self.eval_expr(expr));
                match self.out.entry(sym) {
                    Entry::Occupied(e) => {
                        return Err(Box::new(
                            BuildError::DuplicateBinding(
                                format!("Let binding for {} already exists", e.key()))));
                    },
                    Entry::Vacant(e) => {
                        e.insert(val);
                    },
                }
            }
            Statement::Import { path: val, name: sym } => {
                // TODO(jwall): Unit Tests for this.
                if !self.files.contains(&val) { // Only parse the file once on import.
                    if self.assets.get(&sym).is_none() {
                        let mut b = Self::new();
                        b.build_file(&val);
                        let fields: Vec<(String, Rc<Val>)> = b.out.drain().collect();
                        self.assets.entry(sym).or_insert(Rc::new(Val::Tuple(fields)));
                        self.files.insert(val);
                    }
                }
            }
            Statement::Expression(ref expr) => {
                // TODO(jwall): Is this just a noop? Maybe it's completely unnecessary?
            }
        };
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::{Builder,Val,MacroDef};
    use parse::{Statement,Expression,Value};
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
            (Expression::Mul(Box::new(Value::Float(2.0)),
                             Box::new(Expression::Simple(Value::Int(2)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    fn test_eval_subtract_expr() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Sub(Box::new(Value::Int(2)),
                             Box::new(Expression::Simple(Value::Int(1)))),
             Val::Int(1)),
            (Expression::Sub(Box::new(Value::Float(2.0)),
                             Box::new(Expression::Simple(Value::Float(1.0)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_subtract_expr_fail() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Sub(Box::new(Value::Float(2.0)),
                             Box::new(Expression::Simple(Value::Int(2)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    fn test_eval_add_expr() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Add(Box::new(Value::Int(1)),
                             Box::new(Expression::Simple(Value::Int(1)))),
             Val::Int(2)),
            (Expression::Add(Box::new(Value::Float(1.0)),
                             Box::new(Expression::Simple(Value::Float(1.0)))),
             Val::Float(2.0)),
            (Expression::Add(Box::new(Value::String("foo".to_string())),
                             Box::new(Expression::Simple(Value::String("bar".to_string())))),
             Val::String("foobar".to_string())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_add_expr_fail() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Add(Box::new(Value::Float(2.0)),
                             Box::new(Expression::Simple(Value::Int(2)))),
             Val::Float(1.0)),
        ], b);
    }

    #[test]
    fn test_eval_simple_expr() {
        test_expr_to_val(vec![
            (Expression::Simple(Value::Int(1)), Val::Int(1)),
            (Expression::Simple(Value::Float(2.0)), Val::Float(2.0)),
            (Expression::Simple(Value::String("foo".to_string())),
             Val::String("foo".to_string())),
            (Expression::Simple(Value::Tuple(vec![("bar".to_string(),
                                                   Expression::Simple(Value::Int(1)))])),
             Val::Tuple(vec![("bar".to_string(), Rc::new(Val::Int(1)))])),
        ], Builder::new());
    }

    #[test]
    fn test_eval_simple_lookup_expr() {
        let mut b = Builder::new();
        b.out.entry("var1".to_string()).or_insert(Rc::new(Val::Int(1)));
        test_expr_to_val(vec![
            (Expression::Simple(Value::Symbol("var1".to_string())), Val::Int(1)),
        ], b);
    }

    #[test]
    fn test_eval_simple_lookup_error() {
        let mut b = Builder::new();
        b.out.entry("var1".to_string()).or_insert(Rc::new(Val::Int(1)));
        assert!(b.eval_expr(Expression::Simple(Value::Symbol("var".to_string()))).is_err());
    }

    #[test]
    fn test_eval_selector_expr() {
        // TODO(jwall): Tests for this expression.
        let mut b = Builder::new();
        b.out.entry("var1".to_string()).or_insert(Rc::new(Val::Tuple(vec![
            ("lvl1".to_string(), Rc::new(Val::Tuple(
                vec![
                    ("lvl2".to_string(), Rc::new(Val::Int(3))),
                ]
            ))),
        ])));
        b.out.entry("var2".to_string()).or_insert(Rc::new(Val::Int(2)));
        b.out.entry("var3".to_string()).or_insert(Rc::new(Val::Tuple(vec![
            ("lvl1".to_string(), Rc::new(Val::Int(4)))
        ])));
        test_expr_to_val(vec![
            (Expression::Simple(Value::Selector(vec!["var1".to_string()])), Val::Tuple(
                vec![
                    ("lvl1".to_string(), Rc::new(Val::Tuple(
                        vec![
                            ("lvl2".to_string(), Rc::new(Val::Int(3))),
                        ]
                    ))),
                ]
            )),
            (Expression::Simple(Value::Selector(vec!["var1".to_string(),
                                                     "lvl1".to_string()])),
             Val::Tuple(
                vec![
                    ("lvl2".to_string(), Rc::new(Val::Int(3))),
                ]
            )),
            (Expression::Simple(Value::Selector(vec!["var1".to_string(),
                                                     "lvl1".to_string(),
                                                     "lvl2".to_string()])),
             Val::Int(3)),
            (Expression::Simple(Value::Selector(vec!["var2".to_string()])),
             Val::Int(2)),
            (Expression::Simple(Value::Selector(vec!["var3".to_string(),
                                                     "lvl1".to_string()])),
             Val::Int(4)),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Unable to find Symbol tpl1")]
    fn test_expr_copy_no_such_tuple() {
        let mut b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Copy(vec!["tpl1".to_string()], Vec::new()),
             Val::Tuple(Vec::new())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Tuple got Integer")]
    fn test_expr_copy_not_a_tuple() {
        let mut b = Builder::new();
        b.out.entry("tpl1".to_string()).or_insert(Rc::new(Val::Int(1)));
        test_expr_to_val(vec![
            (Expression::Copy(vec!["tpl1".to_string()], Vec::new()),
             Val::Tuple(Vec::new())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected type Integer for field fld1 but got String")]
    fn test_expr_copy_field_type_error() {
        let mut b = Builder::new();
        b.out.entry("tpl1".to_string()).or_insert(Rc::new(Val::Tuple(vec![
            ("fld1".to_string(), Rc::new(Val::Int(1))),
        ])));
        test_expr_to_val(vec![
            (Expression::Copy(vec!["tpl1".to_string()],
                              vec![("fld1".to_string(),
                                    Expression::Simple(Value::String("2".to_string())))]),
             Val::Tuple(
                vec![
                    ("fld1".to_string(), Rc::new(Val::String("2".to_string()))),
                ],
            )),
        ], b);
    }

    // TODO(jwall): What about the duplicate field error?

    #[test]
    fn test_expr_copy() {
        // TODO(jwall): Tests for this expression.
        let mut b = Builder::new();
        b.out.entry("tpl1".to_string()).or_insert(Rc::new(Val::Tuple(vec![
            ("fld1".to_string(), Rc::new(Val::Int(1))),
        ])));
        test_expr_to_val(vec![
            (Expression::Copy(vec!["tpl1".to_string()],
                              vec![("fld2".to_string(),
                                    Expression::Simple(Value::String("2".to_string())))]),
             // Add a new field to the copy
             Val::Tuple(
                 // NOTE(jwall): The order of these is important in order to ensure
                 // that the compare assertion is correct. The ordering has no
                 // semantics though so at some point we should probably be less restrictive.
                vec![
                    ("fld1".to_string(), Rc::new(Val::Int(1))),
                    ("fld2".to_string(), Rc::new(Val::String("2".to_string()))),
                ],
            )),
             // Overwrite a field in the copy
            (Expression::Copy(vec!["tpl1".to_string()],
                              vec![
                                  ("fld1".to_string(),
                                   Expression::Simple(Value::Int(3))),
                                  ("fld2".to_string(),
                                   Expression::Simple(Value::String("2".to_string()))),
                              ]),
             Val::Tuple(
                vec![
                    ("fld1".to_string(), Rc::new(Val::Int(3))),
                    ("fld2".to_string(), Rc::new(Val::String("2".to_string()))),
                ],
             )),
            // The source tuple is still unmodified.
            (Expression::Simple(Value::Selector(vec!["tpl1".to_string()])),
             Val::Tuple(
                vec![
                    ("fld1".to_string(), Rc::new(Val::Int(1))),
                ],
            )),
        ], b);
    }

    #[test]
    fn test_macro_call() {
        let mut b = Builder::new();
        b.out.entry("tstmac".to_string()).or_insert(Rc::new(Val::Macro(MacroDef{
            argdefs: vec!["arg1".to_string()],
            fields: vec![
                ("foo".to_string(), Expression::Simple(Value::Symbol("arg1".to_string()))),
            ],
        })));
        test_expr_to_val(vec![
            (Expression::Call{
                macroref: vec!["tstmac".to_string()],
                arglist: vec![Expression::Simple(Value::String("bar".to_string()))],
            },
             Val::Tuple(vec![
                 ("foo".to_string(), Rc::new(Val::String("bar".to_string()))),
             ])),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Unable to find arg1")]
    fn test_macro_hermetic() {
        let mut b = Builder::new();
        b.out.entry("arg1".to_string()).or_insert(Rc::new(Val::String("bar".to_string())));
        b.out.entry("tstmac".to_string()).or_insert(Rc::new(Val::Macro(MacroDef{
            argdefs: vec!["arg2".to_string()],
            fields: vec![
                ("foo".to_string(), Expression::Simple(Value::Symbol("arg1".to_string()))),
            ],
        })));
        test_expr_to_val(vec![
            (Expression::Call{
                macroref: vec!["tstmac".to_string()],
                arglist: vec![Expression::Simple(Value::String("bar".to_string()))],
            },
             Val::Tuple(vec![
                 ("foo".to_string(), Rc::new(Val::String("bar".to_string()))),
             ])),
        ], b);
    }

    #[test]
    fn test_select_expr() {
        let mut b = Builder::new();
        b.out.entry("foo".to_string()).or_insert(Rc::new(Val::String("bar".to_string())));
        b.out.entry("baz".to_string()).or_insert(Rc::new(Val::String("boo".to_string())));
        test_expr_to_val(vec![
            (Expression::Select{
                val: Box::new(Expression::Simple(Value::Symbol("foo".to_string()))),
                default: Box::new(Expression::Simple(Value::Int(1))),
                tuple: vec![
                    ("bar".to_string(), Expression::Simple(Value::Int(2))),
                    ("quux".to_string(), Expression::Simple(Value::String("2".to_string()))),
                ],
             },
             Val::Int(2)),
            (Expression::Select{
                val: Box::new(Expression::Simple(Value::Symbol("baz".to_string()))),
                default: Box::new(Expression::Simple(Value::Int(1))),
                tuple: vec![
                    ("bar".to_string(), Expression::Simple(Value::Int(2))),
                    ("quux".to_string(), Expression::Simple(Value::String("2".to_string()))),
                ],
            },
             // If the field doesn't exist then we get the default.
             Val::Int(1)),
        ], b);
    }

    #[test]
    #[should_panic(expected ="Expected String but got Integer in Select expression")]
    fn test_select_expr_not_a_string() {
        let mut b = Builder::new();
        b.out.entry("foo".to_string()).or_insert(Rc::new(Val::Int(4)));
        test_expr_to_val(vec![
            (Expression::Select{
                val: Box::new(Expression::Simple(Value::Symbol("foo".to_string()))),
                default: Box::new(Expression::Simple(Value::Int(1))),
                tuple: vec![
                    ("bar".to_string(), Expression::Simple(Value::Int(2))),
                    ("quux".to_string(), Expression::Simple(Value::String("2".to_string()))),
                ],
             },
             Val::Int(2)),
        ], b);
    }

    #[test]
    fn test_let_statement() {
        let mut b = Builder::new();
        b.build_stmt(Statement::Let{
            name:"foo".to_string(),
            value: Expression::Simple(Value::String("bar".to_string())),
        });
        test_expr_to_val(vec![
            (Expression::Simple(Value::Symbol("foo".to_string())),
             Val::String("bar".to_string())),
        ], b);
    }

    #[test]
    fn test_build_file_string() {
        let mut b = Builder::new();
        b.build_file_string("foo.ucg", "let foo = 1;".to_string());
        assert!(b.out.contains_key("foo"));
    }
}
