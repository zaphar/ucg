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
use std::fs::File;
use std::io::Read;
use std::error::Error;
use std::collections::{HashSet, HashMap, VecDeque};
use std::collections::hash_map::Entry;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use std::convert::From;

use nom;

use tokenizer::Span;
use ast::*;
use format;
use parse::parse;

impl MacroDef {
    pub fn eval(&self,
                mut args: Vec<Rc<Val>>)
                -> Result<Vec<(Positioned<String>, Rc<Val>)>, Box<Error>> {
        // Error conditions. If the args don't match the length and types of the argdefs then this is
        // macro call error.
        if args.len() > self.argdefs.len() {
            return Err(Box::new(BuildError::BadArgLen("Macro called with too many args"
                .to_string())));
        }
        // If the args don't match the types required by the expressions then that is a TypeFail.
        // If the expressions reference Symbols not defined in the MacroDef that is also an error.
        // TODO(jwall): We should probably enforce that the Expression Symbols must be in argdefs rules
        // at Macro definition time not evaluation time.
        let mut scope = HashMap::<Positioned<String>, Rc<Val>>::new();
        for (i, arg) in args.drain(0..).enumerate() {
            scope.entry(self.argdefs[i].clone()).or_insert(arg.clone());
        }
        let b = Builder::new_with_scope(scope);
        let mut result: Vec<(Positioned<String>, Rc<Val>)> = Vec::new();
        for &(ref key, ref expr) in self.fields.iter() {
            // We clone the expressions here because this macro may be consumed
            // multiple times in the future.
            let val = try!(b.eval_expr(expr));
            result.push((key.into(), val.clone()));
        }
        Ok(result)
    }
}

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
        FormatError(msg: String) {
            description("String Format Error")
            display("String format Error {}", msg)
        }
        TODO(msg: String) {
            description("TODO Error")
            display("TODO Error {}", msg)
        }
    }
}

/// BuildResult is the result of a build.
type BuildResult = Result<(), Box<Error>>;

/// Val is the Intermediate representation of a compiled UCG AST.
#[derive(PartialEq,Debug,Clone)]
pub enum Val {
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Rc<Val>>),
    Tuple(Vec<(Positioned<String>, Rc<Val>)>),
    Macro(MacroDef),
}

impl Val {
    pub fn type_name(&self) -> String {
        match self {
            &Val::Int(_) => "Integer".to_string(),
            &Val::Float(_) => "Float".to_string(),
            &Val::String(_) => "String".to_string(),
            &Val::List(_) => "List".to_string(),
            &Val::Tuple(_) => "Tuple".to_string(),
            &Val::Macro(_) => "Macro".to_string(),
        }
    }

    pub fn type_equal(&self, target: &Self) -> bool {
        match self {
            &Val::Int(_) => {
                if let &Val::Int(_) = target {
                    true
                } else {
                    false
                }
            }
            &Val::Float(_) => {
                if let &Val::Float(_) = target {
                    true
                } else {
                    false
                }
            }
            &Val::String(_) => {
                if let &Val::String(_) = target {
                    true
                } else {
                    false
                }
            }
            &Val::List(_) => {
                if let &Val::List(_) = target {
                    true
                } else {
                    false
                }
            }
            &Val::Tuple(_) => {
                if let &Val::Tuple(_) = target {
                    true
                } else {
                    false
                }
            }
            &Val::Macro(_) => {
                if let &Val::Macro(_) = target {
                    true
                } else {
                    false
                }
            }
        }
    }

    pub fn get_fields(&self) -> Option<&Vec<(Positioned<String>, Rc<Val>)>> {
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

    pub fn is_list(&self) -> bool {
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

impl From<Val> for String {
    fn from(v: Val) -> String {
        match v {
            Val::Int(ref i) => format!("{}", i),
            Val::Float(ref f) => format!("{}", f),
            Val::String(ref s) => s.to_string(),
            val => format!("<{}>", val),
        }
    }
}

/// ValueMap defines a set of values in a parsed file.
type ValueMap = HashMap<Positioned<String>, Rc<Val>>;

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
    /// last is the result of the last statement.
    pub last: Option<Rc<Val>>,
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
    fn value_to_val(&self, v: &Value) -> Result<Rc<Val>, Box<Error>> {
        match v {
            &Value::Int(ref i) => Ok(Rc::new(Val::Int(i.val))),
            &Value::Float(ref f) => Ok(Rc::new(Val::Float(f.val))),
            &Value::String(ref s) => Ok(Rc::new(Val::String(s.val.to_string()))),
            &Value::Symbol(ref s) => {
                self.lookup_sym(&(s.into()))
                    .ok_or(Box::new(BuildError::NoSuchSymbol(format!("Unable to find {}", s.val))))
            }
            &Value::Tuple(ref tuple_node) => {
                let fields = tuple_node.val();
                let mut new_fields = Vec::<(Positioned<String>, Rc<Val>)>::new();
                for &(ref name, ref expr) in fields.iter() {
                    let val = try!(self.eval_expr(expr));
                    new_fields.push((name.into(), val));
                }
                new_fields.sort_by(|a, b| a.0.cmp(&b.0));
                Ok(Rc::new(Val::Tuple(new_fields)))
            }
            &Value::Selector(ref selector_list_node) => {
                self.lookup_selector(&selector_list_node.val)
            }
        }
    }

    pub fn new() -> Self {
        Builder {
            assets: HashMap::new(),
            files: HashSet::new(),
            out: HashMap::new(),
            last: None,
        }
    }

    pub fn new_with_scope(scope: ValueMap) -> Self {
        Builder {
            assets: HashMap::new(),
            files: HashSet::new(),
            out: scope,
            last: None,
        }
    }

    pub fn get_out_by_name(&self, name: &str) -> Option<Rc<Val>> {
        let key = Positioned {
            pos: Position {
                line: 0,
                column: 0,
            },
            val: name.to_string(),
        };
        self.lookup_sym(&key)
    }

    pub fn build(&mut self, ast: &Vec<Statement>) -> BuildResult {
        for stmt in ast.iter() {
            try!(self.build_stmt(stmt));
        }
        Ok(())
    }

    pub fn build_file_string(&mut self, name: &str, input: String) -> BuildResult {
        match parse(Span::new(&input)) {
            nom::IResult::Done(_span, stmts) => {
                for stmt in stmts.iter() {
                    try!(self.build_stmt(stmt));
                }
                Ok(())
            }
            nom::IResult::Error(err) => Err(Box::new(err)),
            nom::IResult::Incomplete(_) => {
                Err(Box::new(BuildError::IncompleteParse(format!("Could not parse input from \
                                                                  file: {}",
                                                                 name))))
            }
        }
    }

    pub fn build_file(&mut self, name: &str) -> BuildResult {
        let mut f = try!(File::open(name));
        let mut s = String::new();
        // TODO(jwall): It would be nice to be able to do this while streaming
        try!(f.read_to_string(&mut s));
        self.build_file_string(name, s)
    }

    fn build_stmt(&mut self, stmt: &Statement) -> BuildResult {
        match stmt {
            &Statement::Let(LetDef{ name: ref sym, value: ref expr }) => {
                let val = try!(self.eval_expr(expr));
                self.last = Some(val.clone());
                match self.out.entry(sym.into()) {
                    Entry::Occupied(e) => {
                        return Err(Box::new(BuildError::DuplicateBinding(format!("Let binding \
                                                                                  for {:?} already \
                                                                                  exists",
                                                                                 e.key()))));
                    }
                    Entry::Vacant(e) => {
                        e.insert(val);
                    }
                }
            }
            &Statement::Import(ImportDef{ path: ref val, name: ref sym }) => {
                if !self.files.contains(&val.fragment) {
                    // Only parse the file once on import.
                    let positioned_sym = sym.into();
                    if self.assets.get(&positioned_sym).is_none() {
                        let mut b = Self::new();
                        try!(b.build_file(&val.fragment));
                        let fields: Vec<(Positioned<String>, Rc<Val>)> = b.out.drain().collect();
                        let result = Rc::new(Val::Tuple(fields));
                        self.assets.entry(positioned_sym).or_insert(result.clone());
                        self.files.insert(val.fragment.clone());
                        self.last = Some(result);
                    }
                }
            }
            &Statement::Expression(ref expr) => {
                self.last = Some(try!(self.eval_expr(expr)));
            }
        };
        Ok(())
    }

    fn lookup_sym(&self, sym: &Positioned<String>) -> Option<Rc<Val>> {
        if self.out.contains_key(sym) {
            return Some(self.out[sym].clone());
        }
        if self.assets.contains_key(sym) {
            return Some(self.assets[sym].clone());
        }
        None
    }

    fn find_in_fieldlist(target: &str, fs: &Vec<(Positioned<String>, Rc<Val>)>) -> Option<Rc<Val>> {
        for (key, val) in fs.iter().cloned() {
            if target == &key.val {
                return Some(val.clone());
            }
        }
        return None;
    }

    fn lookup_selector(&self, sl: &SelectorList) -> Result<Rc<Val>, Box<Error>> {
        let len = sl.len();
        if len > 0 {
            let pos_sl = (&sl[0]).into();
            if let Some(v) = self.lookup_sym(&pos_sl) {
                let mut it = sl.iter().skip(1).peekable();
                let mut stack = VecDeque::new();
                stack.push_back(v.clone());
                loop {
                    let vref = stack.pop_front().unwrap();
                    if it.peek().is_none() {
                        return Ok(vref.clone());
                    }
                    // This unwrap is safe because we already checked for
                    // None above.
                    let next = it.next().unwrap();
                    match vref.as_ref() {
                        &Val::Tuple(_) => {
                            // This unwrap is safe because we already checked for
                            // Tuple in the pattern match.
                            let fs = vref.get_fields().unwrap();
                            if let Some(vv) = Self::find_in_fieldlist(&next.fragment, fs) {
                                stack.push_back(vv.clone());
                                continue;
                            } else {
                                // TODO(jwall): A better error for this would be nice.
                                return Err(Box::new(BuildError::NoSuchSymbol(format!("Unable to \
                                                                                          match selector \
                                                                                          path {:?}",
                                                                                         sl))));
                            }
                        }
                        &Val::List(ref elems) => {
                            // TODO(jwall): better error reporting here would probably be good.
                            let idx = try!(next.fragment.parse::<usize>());
                            if idx < elems.len() {
                                stack.push_back(elems[idx].clone());
                                continue;
                            } else {
                                // TODO(jwall): A better error for this would be nice.
                                return Err(Box::new(BuildError::NoSuchSymbol(format!("Unable to \
                                                                                      match selector \
                                                                                      path {:?}",
                                                                                     sl))));

                            }
                        }
                        _ => {
                            return Err(Box::new(BuildError::TypeFail(format!("{} is not a Tuple or List",
                                sl[0].fragment))));
                        }
                    }
                }
            }
            return Err(Box::new(BuildError::NoSuchSymbol(format!("Unable to find Symbol {}",
                                                                 sl[0].fragment))));
        }
        return Err(Box::new(BuildError::NoSuchSymbol("Attempted to lookup an empty selector"
            .to_string())));
    }

    // eval_expr evals a single Expression in the context of a running Builder.
    // It does not mutate the builders collected state at all.
    pub fn eval_expr(&self, expr: &Expression) -> Result<Rc<Val>, Box<Error>> {
        // TODO(jwall): We probably don't want to consume these expressions.
        //   Take a reference instead?
        match expr {
            &Expression::Simple(ref val) => self.value_to_val(val),
            &Expression::Binary(ref def) => {
                let kind = &def.kind;
                let v = &def.left;
                let expr = &def.right;
                let expr_result = try!(self.eval_expr(expr));
                let v = try!(self.value_to_val(v));
                match kind {
                    &BinaryExprType::Add => {
                        match *v {
                            Val::Int(i) => {
                                eval_binary_expr!(&Val::Int(ii),
                                                  expr_result,
                                                  Val::Int(i + ii),
                                                  "Integer")
                            }
                            Val::Float(f) => {
                                eval_binary_expr!(&Val::Float(ff),
                                                  expr_result,
                                                  Val::Float(f + ff),
                                                  "Float")
                            }
                            Val::String(ref s) => {
                                match expr_result.as_ref() {
                                    &Val::String(ref ss) => {
                                        return Ok(Rc::new(Val::String([s.to_string(), ss.clone()]
                                            .concat())))
                                    }
                                    val => {
                                        return Err(Box::new(BuildError::TypeFail(format!("Expected \
                                                                                          String \
                                                                                          but got \
                                                                                          {:?}",
                                                                                         val))))
                                    }
                                }
                            }
                            ref expr => {
                                return Err(Box::new(
                                    BuildError::Unsupported(
                                        format!("{} does not support the '+' operation", expr.type_name()))))
                            }
                        }
                    }
                    &BinaryExprType::Sub => {
                        match *v {
                            Val::Int(i) => {
                                eval_binary_expr!(&Val::Int(ii),
                                                  expr_result,
                                                  Val::Int(i - ii),
                                                  "Integer")
                            }
                            Val::Float(f) => {
                                eval_binary_expr!(&Val::Float(ff),
                                                  expr_result,
                                                  Val::Float(f - ff),
                                                  "Float")
                            }
                            ref expr => {
                                return Err(Box::new(
                                    BuildError::Unsupported(
                                        format!("{} does not support the '-' operation", expr.type_name()))))
                            }
                        }
                    }
                    &BinaryExprType::Mul => {
                        match *v {
                            Val::Int(i) => {
                                eval_binary_expr!(&Val::Int(ii),
                                                  expr_result,
                                                  Val::Int(i * ii),
                                                  "Integer")
                            }
                            Val::Float(f) => {
                                eval_binary_expr!(&Val::Float(ff),
                                                  expr_result,
                                                  Val::Float(f * ff),
                                                  "Float")
                            }
                            ref expr => {
                                return Err(Box::new(
                                    BuildError::Unsupported(
                                        format!("{} does not support the '*' operation", expr.type_name()))))
                            }
                        }
                    }
                    &BinaryExprType::Div => {
                        match *v {
                            Val::Int(i) => {
                                eval_binary_expr!(&Val::Int(ii),
                                                  expr_result,
                                                  Val::Int(i / ii),
                                                  "Integer")
                            }
                            Val::Float(f) => {
                                eval_binary_expr!(&Val::Float(ff),
                                                  expr_result,
                                                  Val::Float(f / ff),
                                                  "Float")
                            }
                            ref expr => {
                                return Err(Box::new(
                                    BuildError::Unsupported(
                                        format!("{} does not support the '*' operation", expr.type_name()))))
                            }
                        }
                    }
                }
            }
            &Expression::Copy(ref def) => {
                let v = try!(self.lookup_selector(&def.selector));
                if let Val::Tuple(ref src_fields) = *v {
                    let mut m = HashMap::<Positioned<String>, Rc<Val>>::new();
                    // loop through fields and build  up a hahsmap
                    for &(ref key, ref val) in src_fields.iter() {
                        if let Entry::Vacant(v) = m.entry(key.clone()) {
                            v.insert(val.clone());
                        } else {
                            return Err(Box::new(BuildError::TypeFail(format!("Duplicate \
                                                                              field: {} in \
                                                                              tuple",
                                                                             key.val))));
                        }
                    }
                    for &(ref key, ref val) in def.fields.iter() {
                        let expr_result = try!(self.eval_expr(val));
                        match m.entry(key.into()) {
                            Entry::Vacant(v) => {
                                v.insert(expr_result);
                            }
                            Entry::Occupied(mut v) => {
                                // Ensure that the new type matches the old type.
                                let src_val = v.get().clone();
                                if src_val.type_equal(&expr_result) {
                                    v.insert(expr_result);
                                } else {
                                    return Err(Box::new(
                                        BuildError::TypeFail(
                                            format!("Expected type {} for field {} but got {}",
                                                    src_val.type_name(), key.fragment, expr_result.type_name()))));
                                }
                            }
                        };
                    }
                    let mut new_fields: Vec<(Positioned<String>, Rc<Val>)> = m.drain().collect();
                    // We want a stable order for the fields to make comparing tuples
                    // easier in later code. So we sort by the field name before constructing a new tuple.
                    new_fields.sort_by(|a, b| a.0.cmp(&b.0));
                    return Ok(Rc::new(Val::Tuple(new_fields)));
                }
                Err(Box::new(BuildError::TypeFail(format!("Expected Tuple got {}", v))))
            }
            &Expression::Grouped(ref expr) => {
                return self.eval_expr(expr);
            }
            &Expression::List(ref def) => {
                let mut vals = Vec::new();
                for expr in def.elems.iter() {
                    vals.push(try!(self.eval_expr(expr)));
                }
                return Ok(Rc::new(Val::List(vals)));
            }
            &Expression::Format(ref def) => {
                let tmpl = &def.template;
                let args = &def.args;
                let mut vals = Vec::new();
                for v in args.iter() {
                    let rcv = try!(self.eval_expr(v));
                    vals.push(rcv.deref().clone());
                }
                let formatter = format::Formatter::new(tmpl.clone(), vals);
                Ok(Rc::new(Val::String(try!(formatter.render()))))
            }
            &Expression::Call(ref def) => {
                let sel = &def.macroref;
                let args = &def.arglist;
                let v = try!(self.lookup_selector(sel));
                if let &Val::Macro(ref m) = v.deref() {
                    // Congratulations this is actually a macro.
                    let mut argvals: Vec<Rc<Val>> = Vec::new();
                    for arg in args.iter() {
                        argvals.push(try!(self.eval_expr(arg)));
                    }
                    let fields = try!(m.eval(argvals));
                    return Ok(Rc::new(Val::Tuple(fields)));
                }
                Err(Box::new(BuildError::TypeFail(// We should pretty print the selectors here.
                                                  format!("{} is not a Macro", v))))
            }
            &Expression::Macro(ref def) => {
                match def.validate_symbols() {
                    Ok(()) => Ok(Rc::new(Val::Macro(def.clone()))),
                    Err(set) => {
                        Err(Box::new(BuildError::NoSuchSymbol(format!("Macro has the following \
                                                                       undefined symbols: {:?}",
                                                                      set))))
                    }
                }
            }
            &Expression::Select(ref def) => {
                let target = &def.val;
                let def_expr = &def.default;
                let fields = &def.tuple;
                // First resolve the target expression.
                let v = try!(self.eval_expr(target));
                // Second ensure that the expression resolves to a string.
                if let &Val::String(ref name) = v.deref() {
                    // Third find the field with that name in the tuple.
                    for &(ref fname, ref val_expr) in fields.iter() {
                        if &fname.fragment == name {
                            // Fourth return the result of evaluating that field.
                            return self.eval_expr(val_expr);
                        }
                    }
                    // Otherwise return the default
                    return self.eval_expr(def_expr);
                } else {
                    return Err(Box::new(BuildError::TypeFail(format!("Expected String but got \
                                                                      {} in Select expression",
                                                                     v.type_name()))));
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Builder, Val, MacroDef, SelectDef, CallDef};
    use ast::*;
    use std::rc::Rc;

    fn test_expr_to_val(mut cases: Vec<(Expression, Val)>, b: Builder) {
        for tpl in cases.drain(0..) {
            assert_eq!(b.eval_expr(&tpl.0).unwrap(), Rc::new(tpl.1));
        }
    }

    #[test]
    fn test_eval_div_expr() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Div,
                    left: Value::Int(make_value_node(2, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(2, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Int(1)),
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Div,
                    left: Value::Float(make_value_node(2.0, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Float(make_value_node(2.0, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Float(1.0)),
        ],
                         b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_div_expr_fail() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Div,
                    left: Value::Float(make_value_node(2.0, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(2, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Float(1.0)),
        ],
                         b);
    }

    #[test]
    fn test_eval_mul_expr() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Mul,
                    left: Value::Int(make_value_node(2, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(2, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Int(4)),
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Mul,
                    left: Value::Float(make_value_node(2.0, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Float(make_value_node(2.0, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Float(4.0)),
        ],
                         b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_mul_expr_fail() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Mul,
                    left: Value::Float(make_value_node(2.0, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(20, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Float(1.0)),
        ],
                         b);
    }

    #[test]
    fn test_eval_subtract_expr() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Sub,
                    left: Value::Int(make_value_node(2, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Int(1)),
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Sub,
                    left: Value::Float(make_value_node(2.0, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Float(make_value_node(1.0, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Float(1.0)),
        ],
                         b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_subtract_expr_fail() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Sub,
                    left: Value::Float(make_value_node(2.0, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(2, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Float(1.0)),
        ],
                         b);
    }

    #[test]
    fn test_eval_add_expr() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Int(make_value_node(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Int(2)),
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Float(make_value_node(1.0, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Float(make_value_node(1.0, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Float(2.0)),
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::String(make_value_node("foo".to_string(), 1, 1)),
                    right: Box::new(Expression::Simple(Value::String(make_value_node("bar".to_string(), 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::String("foobar".to_string())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_add_expr_fail() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Binary(
                BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Float(make_value_node(2.0, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(2, 1, 1)))),
            pos: Position{line: 1, column: 0},
                }),
             Val::Float(1.0)),
        ],
                         b);
    }

    #[test]
    fn test_eval_simple_expr() {
        test_expr_to_val(vec![
            (Expression::Simple(Value::Int(make_value_node(1, 1, 1))), Val::Int(1)),
            (Expression::Simple(Value::Float(make_value_node(2.0, 1, 1))), Val::Float(2.0)),
            (Expression::Simple(Value::String(make_value_node("foo".to_string(), 1, 1))),
             Val::String("foo".to_string())),
            (Expression::Simple(Value::Tuple(make_value_node(vec![
                (Token::new("bar", Position{line: 1, column: 1}), Expression::Simple(Value::Int(make_value_node(1, 1, 1))))
                ], 1, 1))),
             Val::Tuple(vec![(Positioned::new("bar".to_string(), Position{line: 1, column: 1}),
                              Rc::new(Val::Int(1)))])),
        ],
                         Builder::new());
    }

    #[test]
    fn test_eval_simple_lookup_expr() {
        let mut b = Builder::new();
        b.out
            .entry(Positioned::new("var1".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::Int(1)));
        test_expr_to_val(vec![
            (Expression::Simple(Value::Symbol(make_value_node("var1".to_string(), 1, 1))), Val::Int(1)),
        ],
                         b);
    }

    #[test]
    fn test_eval_simple_lookup_error() {
        let mut b = Builder::new();
        b.out
            .entry(Positioned::new("var1".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::Int(1)));
        let expr = Expression::Simple(Value::Symbol(make_value_node("var".to_string(), 1, 1)));
        assert!(b.eval_expr(&expr).is_err());
    }

    #[test]
    fn test_eval_selector_expr() {
        let mut b = Builder::new();
        b.out.entry(Positioned::new("var1".to_string(), Position{line: 1, column: 0})).or_insert(Rc::new(Val::Tuple(vec![
            (Positioned::new("lvl1".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Tuple(
                vec![
                    (Positioned::new("lvl2".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(3))),
                ]
            ))),
        ])));
        b.out
            .entry(Positioned::new("var2".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::Int(2)));
        b.out
            .entry(Positioned::new("var3".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::Tuple(vec![(Positioned::new("lvl1".to_string(),
                                      Position {line: 1, column: 0}),
                      Rc::new(Val::Int(4)))])));

        test_expr_to_val(vec![
            (Expression::Simple(Value::Selector(make_value_node(vec![Token::new("var1", Position{line: 1, column: 1})], 1, 1))), Val::Tuple(
                vec![
                    (Positioned::new("lvl1".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Tuple(
                        vec![
                            (Positioned::new("lvl2".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(3))),
                        ]
                    ))),
                ]
            )),
            (Expression::Simple(Value::Selector(make_value_node(vec![Token::new("var1", Position{line: 1, column: 1}),
                                                     Token::new("lvl1", Position{line: 1, column: 1})], 1, 1))),
             Val::Tuple(
                vec![
                    (Positioned::new("lvl2".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(3))),
                ]
            )),
            (Expression::Simple(Value::Selector(make_value_node(vec![Token::new("var1", Position{line: 1, column: 1}),
                                                     Token::new("lvl1", Position{line: 1, column: 1}),
                                                     Token::new("lvl2", Position{line: 1, column: 1})], 1, 1))),
             Val::Int(3)),
            (Expression::Simple(Value::Selector(make_value_node(vec![Token::new("var2", Position{line: 1, column: 1})], 1, 1))),
             Val::Int(2)),
            (Expression::Simple(Value::Selector(make_value_node(vec![Token::new("var3", Position{line: 1, column: 1}),
                                                     Token::new("lvl1", Position{line: 1, column: 1})], 1, 1))),
             Val::Int(4)),
        ], b);
    }

    #[test]
    fn test_eval_selector_list_expr() {
        let mut b = Builder::new();
        b.out
            .entry(Positioned::new("var1".to_string(),
                                   Position {
                                       line: 1,
                                       column: 1,
                                   }))
            .or_insert(Rc::new(Val::List(vec![
                Rc::new(Val::String("val1".to_string())),
                Rc::new(Val::Tuple(vec![
                            (Positioned::new("var2".to_string(), Position{line: 1, column: 1}),
                             Rc::new(Val::Int(1))),
                        ])),
                ])));
        // TODO(jwall): Assert that we can index into lists using dot syntax.

        test_expr_to_val(vec![
            (Expression::Simple(Value::Selector(make_value_node(vec![
                Token::new("var1", Position{line: 1, column: 1}),
                Token::new("0", Position{line: 1, column: 1})
             ], 1, 1))),
             Val::String("val1".to_string()))
        ],
                         b);
    }

    #[test]
    #[should_panic(expected = "Unable to find Symbol tpl1")]
    fn test_expr_copy_no_such_tuple() {
        let b = Builder::new();
        test_expr_to_val(vec![
            (Expression::Copy(CopyDef{selector: vec![Token::new("tpl1", Position{line: 1, column: 1})], fields: Vec::new(), pos: Position{line: 1, column: 0}}),
             Val::Tuple(Vec::new())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected Tuple got Integer")]
    fn test_expr_copy_not_a_tuple() {
        let mut b = Builder::new();
        b.out
            .entry(Positioned::new("tpl1".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::Int(1)));
        test_expr_to_val(vec![
            (Expression::Copy(CopyDef{selector: vec![Token::new("tpl1", Position{line: 1, column: 1})], fields: Vec::new(), pos: Position{line: 1, column: 0}}),
             Val::Tuple(Vec::new())),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Expected type Integer for field fld1 but got String")]
    fn test_expr_copy_field_type_error() {
        let mut b = Builder::new();
        b.out.entry(Positioned::new("tpl1".to_string(), Position{line: 1, column: 0})).or_insert(Rc::new(Val::Tuple(vec![
            (Positioned::new("fld1".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(1))),
        ])));
        test_expr_to_val(vec![
            (Expression::Copy(
                CopyDef{
                    selector: vec![Token::new("tpl1", Position{line: 1, column: 1})],
                    fields: vec![(Token::new("fld1", Position{line: 1, column: 1}),
                                  Expression::Simple(Value::String(make_value_node("2".to_string(), 1, 1))))],
                    pos: Position{line: 1, column: 0}}),
             Val::Tuple(
                vec![
                    (Positioned::new("fld1".to_string(), Position{line: 1, column: 1}), Rc::new(Val::String("2".to_string()))),
                ],
            )),
        ], b);
    }

    #[test]
    fn test_expr_copy() {
        let mut b = Builder::new();
        b.out.entry(Positioned::new("tpl1".to_string(), Position{line: 1, column: 0})).or_insert(Rc::new(Val::Tuple(vec![
            (Positioned::new("fld1".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(1))),
        ])));
        test_expr_to_val(vec![
            (Expression::Copy(
                CopyDef{
                    selector: vec![Token::new("tpl1", Position{line: 1, column: 1})],
                    fields: vec![(Token::new("fld2", Position{line: 1, column: 1}),
                                  Expression::Simple(Value::String(make_value_node("2".to_string(), 1, 1))))],
                    pos: Position{line: 1, column: 0},
                }),
             // Add a new field to the copy
             Val::Tuple(
                 // NOTE(jwall): The order of these is important in order to ensure
                 // that the compare assertion is correct. The ordering has no
                 // semantics though so at some point we should probably be less restrictive.
                vec![
                    (Positioned::new("fld1".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(1))),
                    (Positioned::new("fld2".to_string(), Position{line: 1, column: 1}), Rc::new(Val::String("2".to_string()))),
                ],
            )),
             // Overwrite a field in the copy
            (Expression::Copy(
                CopyDef{
                    selector: vec![Token::new("tpl1", Position{line: 1, column: 1})],
                    fields: vec![
                        (Token::new("fld1", Position{line: 1, column: 1}),
                         Expression::Simple(Value::Int(make_value_node(3, 1, 1)))),
                        (Token::new("fld2", Position{line: 1, column: 1}),
                         Expression::Simple(Value::String(make_value_node("2".to_string(), 1, 1)))),
                    ],
                    pos: Position{line: 1, column: 0},
                }),
             Val::Tuple(
                vec![
                    (Positioned::new("fld1".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(3))),
                    (Positioned::new("fld2".to_string(), Position{line: 1, column: 0}), Rc::new(Val::String("2".to_string()))),
                ],
             )),
            // The source tuple is still unmodified.
            (Expression::Simple(Value::Selector(make_value_node(vec![Token::new("tpl1", Position{line: 1, column: 1})], 1, 1))),
             Val::Tuple(
                vec![
                    (Positioned::new("fld1".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(1))),
                ],
            )),
        ], b);
    }

    #[test]
    fn test_macro_call() {
        let mut b = Builder::new();
        b.out.entry(Positioned::new("tstmac".to_string(), Position{line: 1, column: 0})).or_insert(Rc::new(Val::Macro(MacroDef{
            argdefs: vec![Positioned::new("arg1".to_string(), Position{line: 1, column: 0})],
            fields: vec![
                (Token::new("foo", Position{line: 1, column: 1}), Expression::Simple(Value::Symbol(make_value_node("arg1".to_string(), 1, 1)))),
            ],
            pos: Position{line: 1, column: 0},
        })));
        test_expr_to_val(vec![
            (Expression::Call(CallDef{
                macroref: vec![Token::new("tstmac", Position{line: 1, column: 1})],
                arglist: vec![Expression::Simple(Value::String(make_value_node("bar".to_string(), 1, 1)))],
                pos: Position{line: 1, column: 0},
            }),
             Val::Tuple(vec![
                 (Positioned::new("foo".to_string(), Position{line: 1, column: 1}),
                  Rc::new(Val::String("bar".to_string()))),
             ])),
        ], b);
    }

    #[test]
    #[should_panic(expected = "Unable to find arg1")]
    fn test_macro_hermetic() {
        let mut b = Builder::new();
        b.out
            .entry(Positioned::new("arg1".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::String("bar".to_string())));
        b.out.entry(Positioned::new("tstmac".to_string(), Position{line: 1, column: 0})).or_insert(Rc::new(Val::Macro(MacroDef{
            argdefs: vec![Positioned::new("arg2".to_string(), Position{line: 1, column: 0})],
            fields: vec![
                (Token::new("foo", Position{line: 1, column: 1}), Expression::Simple(Value::Symbol(make_value_node("arg1".to_string(), 1, 1)))),
            ],
            pos: Position{line: 1, column: 0},
        })));
        test_expr_to_val(vec![
            (Expression::Call(CallDef{
                macroref: vec![Token::new("tstmac", Position{line: 1, column: 1})],
                arglist: vec![Expression::Simple(Value::String(make_value_node("bar".to_string(), 1, 1)))],
                pos: Position{line: 1, column: 1},
            }),
             Val::Tuple(vec![
                 (Positioned::new("foo".to_string(), Position{line: 1, column: 0}), Rc::new(Val::String("bar".to_string()))),
             ])),
        ], b);
    }

    #[test]
    fn test_select_expr() {
        let mut b = Builder::new();
        b.out
            .entry(Positioned::new("foo".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::String("bar".to_string())));
        b.out
            .entry(Positioned::new("baz".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::String("boo".to_string())));
        test_expr_to_val(vec![
            (Expression::Select(SelectDef{
                val: Box::new(Expression::Simple(Value::Symbol(make_value_node("foo".to_string(), 1, 1)))),
                default: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
                tuple: vec![
                    (Token::new("foo", Position{line: 1, column: 1}), Expression::Simple(Value::String(make_value_node("2".to_string(), 1, 1)))),
                    (Token::new("bar", Position{line: 1, column: 1}), Expression::Simple(Value::Int(make_value_node(2, 1, 1)))),
                ],
                pos: Position{line: 1, column: 0},
            }),
             Val::Int(2)),
            (Expression::Select(SelectDef{
                val: Box::new(Expression::Simple(Value::Symbol(make_value_node("baz".to_string(), 1, 1)))),
                default: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
                tuple: vec![
                    (Token::new("bar", Position{line: 1, column: 1}), Expression::Simple(Value::Int(make_value_node(2, 1, 1)))),
                    (Token::new("quux", Position{line: 1, column: 1}), Expression::Simple(Value::String(make_value_node("2".to_string(), 1, 1)))),
                ],
                pos: Position{line: 1, column: 0},
            }),
             // If the field doesn't exist then we get the default.
             Val::Int(1)),
        ], b);
    }

    #[test]
    #[should_panic(expected ="Expected String but got Integer in Select expression")]
    fn test_select_expr_not_a_string() {
        let mut b = Builder::new();
        b.out
            .entry(Positioned::new("foo".to_string(),
                                   Position {
                                       line: 1,
                                       column: 0,
                                   }))
            .or_insert(Rc::new(Val::Int(4)));
        test_expr_to_val(vec![
            (Expression::Select(SelectDef{
                val: Box::new(Expression::Simple(Value::Symbol(make_value_node("foo".to_string(), 1, 1)))),
                default: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
                tuple: vec![
                    (Token::new("bar", Position{line: 1, column: 1}), Expression::Simple(Value::Int(make_value_node(2, 1, 1)))),
                    (Token::new("quux", Position{line: 1, column: 1}), Expression::Simple(Value::String(make_value_node("2".to_string(), 1, 1)))),
                ],
                pos: Position{line: 1, column: 0},
             }),
             Val::Int(2)),
        ], b);
    }

    #[test]
    fn test_let_statement() {
        let mut b = Builder::new();
        let stmt = Statement::Let(LetDef {
            name: Token::new("foo",
                             Position {
                                 line: 1,
                                 column: 1,
                             }),
            value: Expression::Simple(Value::String(make_value_node("bar".to_string(), 1, 1))),
        });
        b.build_stmt(&stmt).unwrap();
        test_expr_to_val(vec![
            (Expression::Simple(Value::Symbol(make_value_node("foo".to_string(), 1, 1))),
             Val::String("bar".to_string())),
        ],
                         b);
    }

    #[test]
    fn test_build_file_string() {
        let mut b = Builder::new();
        b.build_file_string("foo.ucg", "let foo = 1;".to_string()).unwrap();
        let key = Positioned::new("foo".to_string(),
                                  Position {
                                      line: 1,
                                      column: 0,
                                  });
        assert!(b.out.contains_key(&key));
    }

    #[test]
    fn test_asset_symbol_lookups() {
        let mut b = Builder::new();
        b.assets.entry(Positioned::new("foo".to_string(), Position{line: 1, column: 0})).or_insert(Rc::new(Val::Tuple(vec![
                (Positioned::new("bar".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Tuple(vec![
                    (Positioned::new("quux".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(1))),
                ]))),
            ])));
        test_expr_to_val(vec![
            (Expression::Simple(Value::Symbol(make_value_node("foo".to_string(), 1, 1))),
             Val::Tuple(vec![
                (Positioned::new("bar".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Tuple(vec![
                    (Positioned::new("quux".to_string(), Position{line: 1, column: 0}), Rc::new(Val::Int(1))),
                ]))),
            ])),
        ],
                         b);
    }
}
