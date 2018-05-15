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

//! The build stage of the ucg compiler.
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::From;
use std::env;
use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Read;
use std::ops::Deref;
use std::rc::Rc;

use ast::tree::*;
use error;
use format;
use parse::parse;
use tokenizer::Span;

impl MacroDef {
    /// Expands a ucg Macro using the given arguments into a new Tuple.
    pub fn eval(
        &self,
        mut args: Vec<Rc<Val>>,
    ) -> Result<Vec<(Positioned<String>, Rc<Val>)>, Box<Error>> {
        // Error conditions. If the args don't match the length and types of the argdefs then this is
        // macro call error.
        if args.len() > self.argdefs.len() {
            return Err(Box::new(error::Error::new(
                "Macro called with too many args",
                error::ErrorType::BadArgLen,
                self.pos.clone(),
            )));
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

/// The result of a build.
type BuildResult = Result<(), Box<Error>>;

/// The Intermediate representation of a compiled UCG AST.
#[derive(PartialEq, Debug, Clone)]
pub enum Val {
    Empty,
    Boolean(bool),
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Rc<Val>>),
    Tuple(Vec<(Positioned<String>, Rc<Val>)>),
    Macro(MacroDef),
}

impl Val {
    /// Returns the Type of a Val as a string.
    pub fn type_name(&self) -> String {
        match self {
            &Val::Empty => "EmptyValue".to_string(),
            &Val::Boolean(_) => "Boolean".to_string(),
            &Val::Int(_) => "Integer".to_string(),
            &Val::Float(_) => "Float".to_string(),
            &Val::String(_) => "String".to_string(),
            &Val::List(_) => "List".to_string(),
            &Val::Tuple(_) => "Tuple".to_string(),
            &Val::Macro(_) => "Macro".to_string(),
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
            &Val::String(_),
            &Val::List(_),
            &Val::Tuple(_),
            &Val::Macro(_)
        )
    }

    // TODO(jwall): Unit Tests for this.
    pub fn equal(&self, target: &Self, pos: Position) -> Result<bool, error::Error> {
        // first we do a type equality comparison
        match (self, target) {
            // Empty values are always equal.
            (&Val::Empty, &Val::Empty) => Ok(true),
            (&Val::Int(ref i), &Val::Int(ref ii)) => Ok(i == ii),
            (&Val::Float(ref f), &Val::Float(ref ff)) => Ok(f == ff),
            (&Val::Boolean(ref b), &Val::Boolean(ref bb)) => Ok(b == bb),
            (&Val::String(ref s), &Val::String(ref ss)) => Ok(s == ss),
            (&Val::List(ref ldef), &Val::List(ref lldef)) => {
                if ldef.len() != lldef.len() {
                    Ok(false)
                } else {
                    for (i, v) in ldef.iter().enumerate() {
                        // TODO(jwall): We should probably do a slightly better error message here.
                        try!(v.equal(lldef[i].as_ref(), pos.clone()));
                    }
                    Ok(true)
                }
            }
            (&Val::Tuple(ref ldef), &Val::Tuple(ref lldef)) => {
                if ldef.len() != lldef.len() {
                    Ok(false)
                } else {
                    for (i, v) in ldef.iter().enumerate() {
                        let field_target = &lldef[i];
                        eprintln!(
                            "left field: '{}', right field: '{}'",
                            v.0.val, field_target.0.val
                        );
                        if v.0.val != field_target.0.val {
                            // field name equality
                            eprintln!("Field Not equal!!!");
                            return Ok(false);
                        } else {
                            eprintln!("Field Equal!!!");
                            // field value equality.
                            if !try!(v.1.equal(field_target.1.as_ref(), v.0.pos.clone())) {
                                return Ok(false);
                            }
                        }
                    }
                    Ok(true)
                }
            }
            (&Val::Macro(_), &Val::Macro(_)) => Err(error::Error::new(
                "Macros are not comparable",
                error::ErrorType::TypeFail,
                pos,
            )),
            (me, tgt) => Err(error::Error::new(
                format!("Types differ for {}, {}", me, tgt),
                error::ErrorType::TypeFail,
                pos,
            )),
        }
    }

    /// Returns the fields if this Val is a tuple. None otherwise.
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
        if let &Val::List(_) = self {
            return true;
        }
        return false;
    }

    pub fn is_macro(&self) -> bool {
        if let &Val::Macro(_) = self {
            return true;
        }
        return false;
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Val::Boolean(b) => write!(f, "Boolean({})", b),
            &Val::Empty => write!(f, "EmptyValue"),
            &Val::Float(ref ff) => write!(f, "Float({})", ff),
            &Val::Int(ref i) => write!(f, "Int({})", i),
            &Val::String(ref s) => write!(f, "String({})", s),
            &Val::List(ref def) => {
                try!(write!(f, "[\n"));
                for v in def.iter() {
                    try!(write!(f, "\t{},\n", v));
                }
                write!(f, "]")
            }
            &Val::Macro(_) => write!(f, "Macro(..)"),
            &Val::Tuple(ref def) => {
                try!(write!(f, "Tuple(\n"));
                for v in def.iter() {
                    try!(write!(f, "\t{} = {},\n", v.0.val, v.1));
                }
                write!(f, ")")
            }
        }
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

impl From<String> for Val {
    fn from(s: String) -> Val {
        Val::String(s)
    }
}

/// Defines a set of values in a parsed file.
type ValueMap = HashMap<Positioned<String>, Rc<Val>>;

/// Handles building ucg code.
pub struct Builder {
    env: Rc<Val>,
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
    ($case:pat, $pos:ident, $rside:ident, $result:expr, $msg:expr) => {
        match $rside.as_ref() {
            $case => {
                return Ok(Rc::new($result));
            }
            val => {
                return Err(Box::new(error::Error::new(
                    format!("Expected {} but got {}", $msg, val),
                    error::ErrorType::TypeFail,
                    $pos.clone(),
                )));
            }
        }
    };
}

impl Builder {
    fn tuple_to_val(&self, fields: &Vec<(Token, Expression)>) -> Result<Rc<Val>, Box<Error>> {
        let mut new_fields = Vec::<(Positioned<String>, Rc<Val>)>::new();
        for &(ref name, ref expr) in fields.iter() {
            let val = try!(self.eval_expr(expr));
            new_fields.push((name.into(), val));
        }
        Ok(Rc::new(Val::Tuple(new_fields)))
    }

    fn list_to_val(&self, def: &ListDef) -> Result<Rc<Val>, Box<Error>> {
        let mut vals = Vec::new();
        for expr in def.elems.iter() {
            vals.push(try!(self.eval_expr(expr)));
        }
        Ok(Rc::new(Val::List(vals)))
    }

    fn value_to_val(&self, v: &Value) -> Result<Rc<Val>, Box<Error>> {
        match v {
            &Value::Empty(_) => Ok(Rc::new(Val::Empty)),
            &Value::Boolean(ref b) => Ok(Rc::new(Val::Boolean(b.val))),
            &Value::Int(ref i) => Ok(Rc::new(Val::Int(i.val))),
            &Value::Float(ref f) => Ok(Rc::new(Val::Float(f.val))),
            &Value::String(ref s) => Ok(Rc::new(Val::String(s.val.to_string()))),
            &Value::Symbol(ref s) => self.lookup_sym(&(s.into())).ok_or(Box::new(
                error::Error::new(
                    format!("Unable to find {}", s.val),
                    error::ErrorType::NoSuchSymbol,
                    v.pos().clone(),
                ),
            )),
            &Value::List(ref def) => self.list_to_val(def),
            &Value::Tuple(ref tuple) => self.tuple_to_val(&tuple.val),
            &Value::Selector(ref selector_list_node) => {
                self.lookup_selector(&selector_list_node.sel)
            }
        }
    }

    /// Constructs a new Builder.
    pub fn new() -> Self {
        // TODO(jwall): Construct a map with the environment variables in it.
        Self::new_with_scope(HashMap::new())
    }

    /// Constructs a new Builder with a provided scope.
    pub fn new_with_scope(scope: ValueMap) -> Self {
        let env_vars: Vec<(Positioned<String>, Rc<Val>)> = env::vars()
            .map(|t| (Positioned::new(t.0, 0, 0), Rc::new(t.1.into())))
            .collect();
        Self::new_with_env_and_scope(scope, Val::Tuple(env_vars))
    }

    pub fn new_with_env_and_scope(scope: ValueMap, env: Val) -> Self {
        Builder {
            env: Rc::new(env),
            assets: HashMap::new(),
            files: HashSet::new(),
            out: scope,
            last: None,
        }
    }

    /// Returns a Val by name from previously built UCG.
    pub fn get_out_by_name(&self, name: &str) -> Option<Rc<Val>> {
        let key = Positioned {
            pos: Position::new(0, 0),
            val: name.to_string(),
        };
        self.lookup_sym(&key)
    }

    /// Builds a list of parsed UCG Statements.
    pub fn build(&mut self, ast: &Vec<Statement>) -> BuildResult {
        for stmt in ast.iter() {
            try!(self.build_stmt(stmt));
        }
        Ok(())
    }

    pub fn eval_string(&mut self, input: &str) -> Result<Rc<Val>, Box<Error>> {
        match parse(Span::new(input)) {
            Ok(stmts) => {
                let mut out: Option<Rc<Val>> = None;
                for stmt in stmts.iter() {
                    out = Some(try!(self.build_stmt(stmt)));
                }
                match out {
                    None => return Ok(Rc::new(Val::Empty)),
                    Some(val) => Ok(val),
                }
            }
            Err(err) => Err(Box::new(err)),
        }
    }

    /// Builds a string of ucg syntax.
    pub fn build_file_string(&mut self, input: String) -> BuildResult {
        self.last = Some(try!(self.eval_string(&input)));
        Ok(())
    }

    /// Builds a ucg file at the named path.
    pub fn build_file(&mut self, name: &str) -> BuildResult {
        let mut f = try!(File::open(name));
        let mut s = String::new();
        // TODO(jwall): It would be nice to be able to do this while streaming
        try!(f.read_to_string(&mut s));
        self.build_file_string(s)
    }

    fn build_import(&mut self, def: &ImportDef) -> Result<Rc<Val>, Box<Error>> {
        let sym = &def.name;
        let positioned_sym = sym.into();
        if !self.files.contains(&def.path.fragment) {
            // Only parse the file once on import.
            if self.assets.get(&positioned_sym).is_none() {
                let mut b = Self::new();
                try!(b.build_file(&def.path.fragment));
                let fields: Vec<(Positioned<String>, Rc<Val>)> = b.out.drain().collect();
                let result = Rc::new(Val::Tuple(fields));
                self.assets.entry(positioned_sym).or_insert(result.clone());
                self.files.insert(def.path.fragment.clone());
                return Ok(result);
            } else {
                return Ok(self.assets.get(&positioned_sym).unwrap().clone());
            }
        } else {
            return match self.assets.get(&positioned_sym) {
                None => {
                    // some kind of error here I think.
                    Err(Box::new(error::Error::new(
                        "Unknown Error processing import",
                        error::ErrorType::Unsupported,
                        def.name.pos.clone(),
                    )))
                }
                Some(val) => Ok(val.clone()),
            };
        }
    }

    fn build_let(&mut self, def: &LetDef) -> Result<Rc<Val>, Box<Error>> {
        let val = try!(self.eval_expr(&def.value));
        let name = &def.name;
        match self.out.entry(name.into()) {
            Entry::Occupied(e) => {
                return Err(Box::new(error::Error::new(
                    format!(
                        "Let binding \
                         for {:?} already \
                         exists",
                        e.key()
                    ),
                    error::ErrorType::DuplicateBinding,
                    def.name.pos.clone(),
                )));
            }
            Entry::Vacant(e) => {
                e.insert(val.clone());
            }
        }
        Ok(val)
    }

    fn build_stmt(&mut self, stmt: &Statement) -> Result<Rc<Val>, Box<Error>> {
        match stmt {
            &Statement::Let(ref def) => self.build_let(def),
            &Statement::Import(ref def) => self.build_import(def),
            &Statement::Expression(ref expr) => self.eval_expr(expr),
        }
    }

    fn lookup_sym(&self, sym: &Positioned<String>) -> Option<Rc<Val>> {
        if &sym.val == "env" {
            return Some(self.env.clone());
        }
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

    fn lookup_in_tuple(
        &self,
        stack: &mut VecDeque<Rc<Val>>,
        sl: &SelectorList,
        next: (&Position, &str),
        fs: &Vec<(Positioned<String>, Rc<Val>)>,
    ) -> Result<(), Box<Error>> {
        // This unwrap is safe because we already checked for
        // Tuple in the pattern match.
        if let Some(vv) = Self::find_in_fieldlist(next.1, fs) {
            stack.push_back(vv.clone());
        } else {
            // TODO(jwall): A better error for this would be nice.
            return Err(Box::new(error::Error::new(
                format!(
                    "Unable to \
                     match selector \
                     path {:?}",
                    sl
                ),
                error::ErrorType::NoSuchSymbol,
                next.0.clone(),
            )));
        }
        Ok(())
    }

    fn lookup_in_list(
        &self,
        stack: &mut VecDeque<Rc<Val>>,
        sl: &SelectorList,
        next: (&Position, &str),
        elems: &Vec<Rc<Val>>,
    ) -> Result<(), Box<Error>> {
        // TODO(jwall): better error reporting here would probably be good.
        let idx = try!(next.1.parse::<usize>());
        if idx < elems.len() {
            stack.push_back(elems[idx].clone());
        } else {
            // TODO(jwall): A better error for this would be nice.
            return Err(Box::new(error::Error::new(
                format!(
                    "Unable to \
                     match selector \
                     path {:?}",
                    sl
                ),
                error::ErrorType::NoSuchSymbol,
                next.0.clone(),
            )));
        }
        Ok(())
    }

    fn lookup_selector(&self, sl: &SelectorList) -> Result<Rc<Val>, Box<Error>> {
        let first = try!(self.eval_expr(&sl.head));
        // TODO(jwall): Handle environment lookups.
        // First we ensure that the result is a tuple or a list.
        let mut stack = VecDeque::new();
        match first.as_ref() {
            &Val::Tuple(_) => {
                stack.push_back(first.clone());
            }
            &Val::List(_) => {
                stack.push_back(first.clone());
            }
            val => {
                eprintln!("Not a tuple or list! {:?}", val)
                // noop
            }
        }

        if let &Some(ref tail) = &sl.tail {
            if tail.len() == 0 {
                return Ok(first);
            }
            let mut it = tail.iter().peekable();
            loop {
                let vref = stack.pop_front().unwrap();
                if it.peek().is_none() {
                    return Ok(vref.clone());
                }
                // This unwrap is safe because we already checked for
                // None above.
                let next = it.next().unwrap();
                match vref.as_ref() {
                    &Val::Tuple(ref fs) => {
                        try!(self.lookup_in_tuple(&mut stack, sl, (&next.pos, &next.fragment), fs));
                        continue;
                    }
                    &Val::List(ref elems) => {
                        try!(self.lookup_in_list(
                            &mut stack,
                            sl,
                            (&next.pos, &next.fragment),
                            elems
                        ));
                        continue;
                    }
                    _ => {
                        return Err(Box::new(error::Error::new(
                            format!("{} is not a Tuple or List", vref),
                            error::ErrorType::TypeFail,
                            next.pos.clone(),
                        )));
                    }
                }
            }
        } else {
            return Ok(first);
        }
    }

    fn add_vals(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        match *left {
            Val::Int(i) => {
                eval_binary_expr!(&Val::Int(ii), pos, right, Val::Int(i + ii), "Integer")
            }
            Val::Float(f) => {
                eval_binary_expr!(&Val::Float(ff), pos, right, Val::Float(f + ff), "Float")
            }
            Val::String(ref s) => match right.as_ref() {
                &Val::String(ref ss) => {
                    return Ok(Rc::new(Val::String([s.to_string(), ss.clone()].concat())))
                }
                val => {
                    return Err(Box::new(error::Error::new(
                        format!(
                            "Expected \
                             String \
                             but got \
                             {:?}",
                            val
                        ),
                        error::ErrorType::TypeFail,
                        pos.clone(),
                    )))
                }
            },
            Val::List(ref l) => match right.as_ref() {
                &Val::List(ref r) => {
                    let mut new_vec = Vec::new();
                    new_vec.extend(l.iter().cloned());
                    new_vec.extend(r.iter().cloned());
                    return Ok(Rc::new(Val::List(new_vec)));
                }
                val => {
                    return Err(Box::new(error::Error::new(
                        format!(
                            "Expected \
                             List \
                             but got \
                             {:?}",
                            val
                        ),
                        error::ErrorType::TypeFail,
                        pos.clone(),
                    )))
                }
            },
            ref expr => {
                return Err(Box::new(error::Error::new(
                    format!("{} does not support the '+' operation", expr.type_name()),
                    error::ErrorType::Unsupported,
                    pos.clone(),
                )))
            }
        }
    }

    fn subtract_vals(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        match *left {
            Val::Int(i) => {
                eval_binary_expr!(&Val::Int(ii), pos, right, Val::Int(i - ii), "Integer")
            }
            Val::Float(f) => {
                eval_binary_expr!(&Val::Float(ff), pos, right, Val::Float(f - ff), "Float")
            }
            ref expr => {
                return Err(Box::new(error::Error::new(
                    format!("{} does not support the '-' operation", expr.type_name()),
                    error::ErrorType::Unsupported,
                    pos.clone(),
                )))
            }
        }
    }

    fn multiply_vals(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        match *left {
            Val::Int(i) => {
                eval_binary_expr!(&Val::Int(ii), pos, right, Val::Int(i * ii), "Integer")
            }
            Val::Float(f) => {
                eval_binary_expr!(&Val::Float(ff), pos, right, Val::Float(f * ff), "Float")
            }
            ref expr => {
                return Err(Box::new(error::Error::new(
                    format!("{} does not support the '*' operation", expr.type_name()),
                    error::ErrorType::Unsupported,
                    pos.clone(),
                )))
            }
        }
    }

    fn divide_vals(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        match *left {
            Val::Int(i) => {
                eval_binary_expr!(&Val::Int(ii), pos, right, Val::Int(i / ii), "Integer")
            }
            Val::Float(f) => {
                eval_binary_expr!(&Val::Float(ff), pos, right, Val::Float(f / ff), "Float")
            }
            ref expr => {
                return Err(Box::new(error::Error::new(
                    format!("{} does not support the '*' operation", expr.type_name()),
                    error::ErrorType::Unsupported,
                    pos.clone(),
                )))
            }
        }
    }

    fn do_deep_equal(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        Ok(Rc::new(Val::Boolean(try!(
            left.equal(right.as_ref(), pos.clone())
        ))))
    }

    fn do_not_deep_equal(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        Ok(Rc::new(Val::Boolean(!try!(
            left.equal(right.as_ref(), pos.clone())
        ))))
    }

    fn do_gt(&self, pos: &Position, left: Rc<Val>, right: Rc<Val>) -> Result<Rc<Val>, Box<Error>> {
        // first ensure that left and right are numeric vals of the same type.
        if let &Val::Int(ref l) = left.as_ref() {
            if let &Val::Int(ref r) = right.as_ref() {
                return Ok(Rc::new(Val::Boolean(l > r)));
            }
        }
        if let &Val::Float(ref l) = left.as_ref() {
            if let &Val::Float(ref r) = right.as_ref() {
                return Ok(Rc::new(Val::Boolean(l > r)));
            }
        }
        Err(Box::new(error::Error::new(
            format!(
                "Incompatible types for numeric comparison {} with {}",
                left.type_name(),
                right.type_name()
            ),
            error::ErrorType::TypeFail,
            pos.clone(),
        )))
    }

    fn do_lt(&self, pos: &Position, left: Rc<Val>, right: Rc<Val>) -> Result<Rc<Val>, Box<Error>> {
        // first ensure that left and right are numeric vals of the same type.
        if let &Val::Int(ref l) = left.as_ref() {
            if let &Val::Int(ref r) = right.as_ref() {
                return Ok(Rc::new(Val::Boolean(l < r)));
            }
        }
        if let &Val::Float(ref l) = left.as_ref() {
            if let &Val::Float(ref r) = right.as_ref() {
                return Ok(Rc::new(Val::Boolean(l < r)));
            }
        }
        Err(Box::new(error::Error::new(
            format!(
                "Incompatible types for numeric comparison {} with {}",
                left.type_name(),
                right.type_name()
            ),
            error::ErrorType::TypeFail,
            pos.clone(),
        )))
    }

    fn do_ltequal(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        if let &Val::Int(ref l) = left.as_ref() {
            if let &Val::Int(ref r) = right.as_ref() {
                return Ok(Rc::new(Val::Boolean(l <= r)));
            }
        }
        if let &Val::Float(ref l) = left.as_ref() {
            if let &Val::Float(ref r) = right.as_ref() {
                return Ok(Rc::new(Val::Boolean(l <= r)));
            }
        }
        Err(Box::new(error::Error::new(
            format!(
                "Incompatible types for numeric comparison {} with {}",
                left.type_name(),
                right.type_name()
            ),
            error::ErrorType::TypeFail,
            pos.clone(),
        )))
    }

    fn do_gtequal(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        if let &Val::Int(ref l) = left.as_ref() {
            if let &Val::Int(ref r) = right.as_ref() {
                return Ok(Rc::new(Val::Boolean(l >= r)));
            }
        }
        if let &Val::Float(ref l) = left.as_ref() {
            if let &Val::Float(ref r) = right.as_ref() {
                return Ok(Rc::new(Val::Boolean(l >= r)));
            }
        }
        Err(Box::new(error::Error::new(
            format!(
                "Incompatible types for numeric comparison {} with {}",
                left.type_name(),
                right.type_name()
            ),
            error::ErrorType::TypeFail,
            pos.clone(),
        )))
    }

    fn eval_binary(&self, def: &BinaryOpDef) -> Result<Rc<Val>, Box<Error>> {
        let kind = &def.kind;
        let left = try!(self.eval_expr(&def.left));
        let right = try!(self.eval_expr(&def.right));
        match kind {
            &BinaryExprType::Add => self.add_vals(&def.pos, left, right),
            &BinaryExprType::Sub => self.subtract_vals(&def.pos, left, right),
            &BinaryExprType::Mul => self.multiply_vals(&def.pos, left, right),
            &BinaryExprType::Div => self.divide_vals(&def.pos, left, right),
        }
    }

    fn eval_compare(&self, def: &ComparisonDef) -> Result<Rc<Val>, Box<Error>> {
        let kind = &def.kind;
        let left = try!(self.eval_expr(&def.left));
        let right = try!(self.eval_expr(&def.right));
        match kind {
            &CompareType::Equal => self.do_deep_equal(&def.pos, left, right),
            &CompareType::GT => self.do_gt(&def.pos, left, right),
            &CompareType::LT => self.do_lt(&def.pos, left, right),
            &CompareType::GTEqual => self.do_gtequal(&def.pos, left, right),
            &CompareType::LTEqual => self.do_ltequal(&def.pos, left, right),
            &CompareType::NotEqual => self.do_not_deep_equal(&def.pos, left, right),
        }
    }

    fn eval_copy(&self, def: &CopyDef) -> Result<Rc<Val>, Box<Error>> {
        let v = try!(self.lookup_selector(&def.selector.sel));
        if let Val::Tuple(ref src_fields) = *v {
            let mut m = HashMap::<Positioned<String>, (i32, Rc<Val>)>::new();
            // loop through fields and build  up a hashmap
            // TODO(jwall): Maintain field order here.
            let mut count = 0;
            for &(ref key, ref val) in src_fields.iter() {
                if let Entry::Vacant(v) = m.entry(key.clone()) {
                    v.insert((count, val.clone()));
                    count += 1;
                } else {
                    return Err(Box::new(error::Error::new(
                        format!(
                            "Duplicate \
                             field: {} in \
                             tuple",
                            key.val
                        ),
                        error::ErrorType::TypeFail,
                        key.pos.clone(),
                    )));
                }
            }
            for &(ref key, ref val) in def.fields.iter() {
                let expr_result = try!(self.eval_expr(val));
                // TODO(jwall): Maintain field order here.
                match m.entry(key.into()) {
                    // brand new field here.
                    Entry::Vacant(v) => {
                        v.insert((count, expr_result));
                        count += 1;
                    }
                    Entry::Occupied(mut v) => {
                        // overriding field here.
                        // Ensure that the new type matches the old type.
                        let src_val = v.get().clone();
                        if src_val.1.type_equal(&expr_result) {
                            v.insert((src_val.0, expr_result));
                        } else {
                            return Err(Box::new(error::Error::new(
                                format!(
                                    "Expected type {} for field {} but got {}",
                                    src_val.1.type_name(),
                                    key.fragment,
                                    expr_result.type_name()
                                ),
                                error::ErrorType::TypeFail,
                                key.pos.clone(),
                            )));
                        }
                    }
                };
            }
            let mut new_fields: Vec<(Positioned<String>, (i32, Rc<Val>))> = m.drain().collect();
            // We want to maintain our order for the fields to make comparing tuples
            // easier in later code. So we sort by the field order before constructing a new tuple.
            new_fields.sort_by(|a, b| {
                let ta = a.1.clone();
                let tb = b.1.clone();
                ta.0.cmp(&tb.0)
            });
            return Ok(Rc::new(Val::Tuple(
                new_fields
                    .iter()
                    .map(|a| {
                        let first = a.0.clone();
                        let t = a.1.clone();
                        (first, t.1)
                    })
                    .collect(),
            )));
        }
        Err(Box::new(error::Error::new(
            format!("Expected Tuple got {}", v),
            error::ErrorType::TypeFail,
            def.selector.pos.clone(),
        )))
    }

    fn eval_format(&self, def: &FormatDef) -> Result<Rc<Val>, Box<Error>> {
        let tmpl = &def.template;
        let args = &def.args;
        let mut vals = Vec::new();
        for v in args.iter() {
            let rcv = try!(self.eval_expr(v));
            vals.push(rcv.deref().clone());
        }
        let formatter = format::Formatter::new(tmpl.clone(), vals);
        Ok(Rc::new(Val::String(try!(formatter.render(&def.pos)))))
    }

    fn eval_call(&self, def: &CallDef) -> Result<Rc<Val>, Box<Error>> {
        let sel = &def.macroref;
        let args = &def.arglist;
        let v = try!(self.lookup_selector(&sel.sel));
        if let &Val::Macro(ref m) = v.deref() {
            // Congratulations this is actually a macro.
            let mut argvals: Vec<Rc<Val>> = Vec::new();
            for arg in args.iter() {
                argvals.push(try!(self.eval_expr(arg)));
            }
            let fields = try!(m.eval(argvals));
            return Ok(Rc::new(Val::Tuple(fields)));
        }
        Err(Box::new(error::Error::new(
            // We should pretty print the selectors here.
            format!("{} is not a Macro", v),
            error::ErrorType::TypeFail,
            def.pos.clone(),
        )))
    }

    fn eval_macro_def(&self, def: &MacroDef) -> Result<Rc<Val>, Box<Error>> {
        match def.validate_symbols() {
            Ok(()) => Ok(Rc::new(Val::Macro(def.clone()))),
            Err(set) => Err(Box::new(error::Error::new(
                format!(
                    "Macro has the following \
                     undefined symbols: {:?}",
                    set
                ),
                error::ErrorType::NoSuchSymbol,
                def.pos.clone(),
            ))),
        }
    }

    fn eval_select(&self, def: &SelectDef) -> Result<Rc<Val>, Box<Error>> {
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
            // Otherwise return the default.
            return self.eval_expr(def_expr);
        } else {
            return Err(Box::new(error::Error::new(
                format!(
                    "Expected String but got \
                     {} in Select expression",
                    v.type_name()
                ),
                error::ErrorType::TypeFail,
                def.pos.clone(),
            )));
        }
    }

    // FIXME(jwall): We still need to write unit tests for these.
    fn eval_list_op(&self, def: &ListOpDef) -> Result<Rc<Val>, Box<Error>> {
        let l = &def.target.elems;
        let mac = &def.mac;
        if let &Val::Macro(ref macdef) = try!(self.lookup_selector(&mac.sel)).as_ref() {
            let mut out = Vec::new();
            for expr in l.iter() {
                let argvals = vec![try!(self.eval_expr(expr))];
                let fields = try!(macdef.eval(argvals));
                if let Some(v) = Self::find_in_fieldlist(&def.field, &fields) {
                    match def.typ {
                        ListOpType::Map => {
                            out.push(v.clone());
                        }
                        ListOpType::Filter => {
                            if let &Val::Empty = v.as_ref() {
                                // noop
                                continue;
                            }
                            out.push(v.clone());
                        }
                    }
                }
            }
            return Ok(Rc::new(Val::List(out)));
        }
        return Err(Box::new(error::Error::new(
            format!("Expected macro but got {:?}", mac),
            error::ErrorType::TypeFail,
            def.pos.clone(),
        )));
    }

    // Evals a single Expression in the context of a running Builder.
    // It does not mutate the builders collected state at all.
    pub fn eval_expr(&self, expr: &Expression) -> Result<Rc<Val>, Box<Error>> {
        // TODO(jwall): We need a rewrite step to handle operator precendence order.
        match expr {
            &Expression::Simple(ref val) => self.value_to_val(val),
            &Expression::Binary(ref def) => self.eval_binary(def),
            &Expression::Compare(ref def) => self.eval_compare(def),
            &Expression::Copy(ref def) => self.eval_copy(def),
            &Expression::Grouped(ref expr) => self.eval_expr(expr),
            &Expression::Format(ref def) => self.eval_format(def),
            &Expression::Call(ref def) => self.eval_call(def),
            &Expression::Macro(ref def) => self.eval_macro_def(def),
            &Expression::Select(ref def) => self.eval_select(def),
            &Expression::ListOp(ref def) => self.eval_list_op(def),
        }
    }
}

#[cfg(test)]
mod compile_test {
    use super::{Builder, Val};

    fn assert_build<S: Into<String>>(input: S, assert: &str) {
        let mut b = Builder::new();
        b.build_file_string(input.into()).unwrap();
        let result = b.eval_string(assert).unwrap();
        if let &Val::Boolean(ok) = result.as_ref() {
            assert!(ok, format!("'{}' is not true", assert));
        } else {
            assert!(
                false,
                format!("'{}' does not evaluate to a boolean: {:?}", assert, result)
            );
        }
    }

    #[test]
    fn test_comparisons() {
        let input = "
        let one = 1;
        let two = 2;
        let foo = \"foo\";
        let bar = \"bar\";
        let tpl1 = {
            foo = \"bar\",
            one = 1
        };
        let tpl2 = tpl1{};
        let tpl3 = {
            bar = \"foo\",
            two = 1
        };
        let list = [1, 2, 3];
        let list2 = list;
        let list3 = [1, 2];
        ";
        assert_build(input, "one == one;");
        assert_build(input, "one >= one;");
        assert_build(input, "two > one;");
        assert_build(input, "two >= two;");
        assert_build(input, "tpl1 == tpl2;");
        assert_build(input, "tpl1 != tpl3;");
        assert_build(input, "list == list2;");
        assert_build(input, "list != list3;");
    }

    #[test]
    fn test_deep_comparison() {
        let input = "
        let tpl1 = {
            foo = \"bar\",
            lst = [1, 2, 3],
            inner = {
                fld = \"value\"
            }
        };
        let copy = tpl1;
        let extra = tpl1{one = 1};
        let less = {
            foo = \"bar\"
        };
        ";

        assert_build(input, "tpl1.inner == copy.inner;");
        assert_build(input, "tpl1.inner.fld == copy.inner.fld;");
        assert_build(input, "tpl1.lst == copy.lst;");
        assert_build(input, "tpl1.foo == copy.foo;");
        assert_build(input, "tpl1 == copy;");
        assert_build(input, "tpl1 != extra;");
        assert_build(input, "tpl1 != less;");
    }

    #[test]
    fn test_expression_comparisons() {
        assert_build("", "2 == 1+1;");
        assert_build("", "(1+1) == 2;");
        assert_build("", "(1+1) == (1+1);");
        assert_build("", "(\"foo\" + \"bar\") == \"foobar\";");
    }

    #[test]
    fn test_binary_operator_precedence() {
        assert_build("let result = 2 * 2 + 1;", "result == 6;");
        assert_build("let result = (2 * 2) + 1;", "result == 5;");
    }
}

#[cfg(test)]
mod test {
    use super::{Builder, CallDef, MacroDef, SelectDef, Val};
    use ast::tree::*;
    use std::rc::Rc;

    fn test_expr_to_val(mut cases: Vec<(Expression, Val)>, b: Builder) {
        for tpl in cases.drain(0..) {
            assert_eq!(b.eval_expr(&tpl.0).unwrap(), Rc::new(tpl.1));
        }
    }

    #[test]
    fn test_eval_div_expr() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Div,
                        left: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Int(1),
                ),
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Div,
                        left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Float(1.0),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_div_expr_fail() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Div,
                        left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Float(1.0),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_eval_mul_expr() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Mul,
                        left: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Int(4),
                ),
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Mul,
                        left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Float(4.0),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_mul_expr_fail() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Mul,
                        left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(20, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Float(1.0),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_eval_subtract_expr() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Sub,
                        left: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Int(1),
                ),
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Sub,
                        left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Float(1.0),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_subtract_expr_fail() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Sub,
                        left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Float(1.0),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_eval_add_expr() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Add,
                        left: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Int(2),
                ),
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Add,
                        left: Box::new(Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Float(2.0),
                ),
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Add,
                        left: Box::new(Expression::Simple(Value::String(value_node!(
                            "foo".to_string(),
                            1,
                            1
                        )))),
                        right: Box::new(Expression::Simple(Value::String(value_node!(
                            "bar".to_string(),
                            1,
                            1
                        )))),
                        pos: Position::new(1, 0),
                    }),
                    Val::String("foobar".to_string()),
                ),
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Add,
                        left: Box::new(Expression::Simple(Value::List(ListDef {
                            elems: vec![
                                Expression::Simple(Value::String(value_node!(
                                    "foo".to_string(),
                                    1,
                                    1
                                ))),
                            ],
                            pos: Position::new(1, 1),
                        }))),
                        right: Box::new(Expression::Simple(Value::List(ListDef {
                            elems: vec![
                                Expression::Simple(Value::String(value_node!(
                                    "bar".to_string(),
                                    1,
                                    1
                                ))),
                            ],
                            pos: Position::new(1, 1),
                        }))),
                        pos: Position::new(1, 0),
                    }),
                    Val::List(vec![
                        Rc::new(Val::String("foo".to_string())),
                        Rc::new(Val::String("bar".to_string())),
                    ]),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Expected Float")]
    fn test_eval_add_expr_fail() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Binary(BinaryOpDef {
                        kind: BinaryExprType::Add,
                        left: Box::new(Expression::Simple(Value::Float(value_node!(2.0, 1, 1)))),
                        right: Box::new(Expression::Simple(Value::Int(value_node!(2, 1, 1)))),
                        pos: Position::new(1, 0),
                    }),
                    Val::Float(1.0),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_eval_simple_expr() {
        test_expr_to_val(
            vec![
                (
                    Expression::Simple(Value::Int(value_node!(1, 1, 1))),
                    Val::Int(1),
                ),
                (
                    Expression::Simple(Value::Float(value_node!(2.0, 1, 1))),
                    Val::Float(2.0),
                ),
                (
                    Expression::Simple(Value::String(value_node!("foo".to_string(), 1, 1))),
                    Val::String("foo".to_string()),
                ),
                (
                    Expression::Simple(Value::Tuple(value_node!(
                        vec![
                            (
                                make_tok!("bar", 1, 1),
                                Expression::Simple(Value::Int(value_node!(1, 1, 1))),
                            ),
                        ],
                        1,
                        1
                    ))),
                    Val::Tuple(vec![
                        (value_node!("bar".to_string(), 1, 1), Rc::new(Val::Int(1))),
                    ]),
                ),
            ],
            Builder::new(),
        );
    }

    #[test]
    fn test_eval_simple_lookup_expr() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("var1".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Int(1)));
        test_expr_to_val(
            vec![
                (
                    Expression::Simple(Value::Symbol(value_node!("var1".to_string(), 1, 1))),
                    Val::Int(1),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_eval_simple_lookup_error() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("var1".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Int(1)));
        let expr = Expression::Simple(Value::Symbol(value_node!("var".to_string(), 1, 1)));
        assert!(b.eval_expr(&expr).is_err());
    }

    #[test]
    fn test_eval_selector_expr() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("var1".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Tuple(vec![
                (
                    value_node!("lvl1".to_string(), 1, 0),
                    Rc::new(Val::Tuple(vec![
                        (value_node!("lvl2".to_string(), 1, 0), Rc::new(Val::Int(3))),
                    ])),
                ),
            ])));
        b.out
            .entry(value_node!("var2".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Int(2)));
        b.out
            .entry(value_node!("var3".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Tuple(vec![
                (value_node!("lvl1".to_string(), 1, 0), Rc::new(Val::Int(4))),
            ])));

        test_expr_to_val(
            vec![
                (
                    Expression::Simple(Value::Selector(make_selector!(make_expr!("var1")))),
                    Val::Tuple(vec![
                        (
                            value_node!("lvl1".to_string(), 1, 0),
                            Rc::new(Val::Tuple(vec![
                                (value_node!("lvl2".to_string(), 1, 0), Rc::new(Val::Int(3))),
                            ])),
                        ),
                    ]),
                ),
                (
                    Expression::Simple(Value::Selector(
                        make_selector!(make_expr!("var1") => "lvl1"),
                    )),
                    Val::Tuple(vec![
                        (value_node!("lvl2".to_string(), 1, 0), Rc::new(Val::Int(3))),
                    ]),
                ),
                (
                    Expression::Simple(Value::Selector(
                        make_selector!(make_expr!("var1") => "lvl1", "lvl2"),
                    )),
                    Val::Int(3),
                ),
                (
                    Expression::Simple(Value::Selector(make_selector!(make_expr!("var2")))),
                    Val::Int(2),
                ),
                (
                    Expression::Simple(Value::Selector(
                        make_selector!(make_expr!("var3") => "lvl1"),
                    )),
                    Val::Int(4),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_eval_selector_list_expr() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("var1".to_string(), 1, 1))
            .or_insert(Rc::new(Val::List(vec![
                Rc::new(Val::String("val1".to_string())),
                Rc::new(Val::Tuple(vec![
                    (value_node!("var2".to_string(), 1, 1), Rc::new(Val::Int(1))),
                ])),
            ])));
        // TODO(jwall): Assert that we can index into lists using dot syntax.

        test_expr_to_val(
            vec![
                (
                    Expression::Simple(Value::Selector(
                        make_selector!(make_expr!("var1") =>  "0" => 1, 1),
                    )),
                    Val::String("val1".to_string()),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Unable to find tpl1")]
    fn test_expr_copy_no_such_tuple() {
        let b = Builder::new();
        test_expr_to_val(
            vec![
                (
                    Expression::Copy(CopyDef {
                        selector: make_selector!(make_expr!("tpl1")),
                        fields: Vec::new(),
                        pos: Position::new(1, 0),
                    }),
                    Val::Tuple(Vec::new()),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Expected Tuple got Int(1)")]
    fn test_expr_copy_not_a_tuple() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("tpl1".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Int(1)));
        test_expr_to_val(
            vec![
                (
                    Expression::Copy(CopyDef {
                        selector: make_selector!(make_expr!("tpl1")),
                        fields: Vec::new(),
                        pos: Position::new(1, 0),
                    }),
                    Val::Tuple(Vec::new()),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Expected type Integer for field fld1 but got String")]
    fn test_expr_copy_field_type_error() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("tpl1".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Tuple(vec![
                (value_node!("fld1".to_string(), 1, 0), Rc::new(Val::Int(1))),
            ])));
        test_expr_to_val(
            vec![
                (
                    Expression::Copy(CopyDef {
                        selector: make_selector!(make_expr!("tpl1")),
                        fields: vec![
                            (
                                make_tok!("fld1", 1, 1),
                                Expression::Simple(Value::String(value_node!(
                                    "2".to_string(),
                                    1,
                                    1
                                ))),
                            ),
                        ],
                        pos: Position::new(1, 0),
                    }),
                    Val::Tuple(vec![
                        (
                            value_node!("fld1".to_string(), 1, 1),
                            Rc::new(Val::String("2".to_string())),
                        ),
                    ]),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_expr_copy() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("tpl1".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Tuple(vec![
                (value_node!("fld1".to_string(), 1, 0), Rc::new(Val::Int(1))),
            ])));
        test_expr_to_val(
            vec![
                (
                    Expression::Copy(CopyDef {
                        selector: make_selector!(make_expr!("tpl1")),
                        fields: vec![
                            (
                                make_tok!("fld2", 1, 1),
                                Expression::Simple(Value::String(value_node!(
                                    "2".to_string(),
                                    1,
                                    1
                                ))),
                            ),
                        ],
                        pos: Position::new(1, 0),
                    }),
                    // Add a new field to the copy
                    Val::Tuple(
                        // NOTE(jwall): The order of these is important in order to ensure
                        // that the compare assertion is correct. The ordering has no
                        // semantics though so at some point we should probably be less restrictive.
                        vec![
                            (value_node!("fld1".to_string(), 1, 0), Rc::new(Val::Int(1))),
                            (
                                value_node!("fld2".to_string(), 1, 1),
                                Rc::new(Val::String("2".to_string())),
                            ),
                        ],
                    ),
                ),
                // Overwrite a field in the copy
                (
                    Expression::Copy(CopyDef {
                        selector: make_selector!(make_expr!("tpl1")),
                        fields: vec![
                            (
                                make_tok!("fld1", 1, 1),
                                Expression::Simple(Value::Int(value_node!(3, 1, 1))),
                            ),
                            (
                                make_tok!("fld2", 1, 1),
                                Expression::Simple(Value::String(value_node!(
                                    "2".to_string(),
                                    1,
                                    1
                                ))),
                            ),
                        ],
                        pos: Position::new(1, 0),
                    }),
                    Val::Tuple(vec![
                        (value_node!("fld1".to_string(), 1, 0), Rc::new(Val::Int(3))),
                        (
                            value_node!("fld2".to_string(), 1, 0),
                            Rc::new(Val::String("2".to_string())),
                        ),
                    ]),
                ),
                // The source tuple is still unmodified.
                (
                    Expression::Simple(Value::Selector(make_selector!(make_expr!["tpl1"]))),
                    Val::Tuple(vec![
                        (value_node!("fld1".to_string(), 1, 0), Rc::new(Val::Int(1))),
                    ]),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_macro_call() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("tstmac".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Macro(MacroDef {
                argdefs: vec![value_node!("arg1".to_string(), 1, 0)],
                fields: vec![
                    (
                        make_tok!("foo", 1, 1),
                        Expression::Simple(Value::Symbol(value_node!("arg1".to_string(), 1, 1))),
                    ),
                ],
                pos: Position::new(1, 0),
            })));
        test_expr_to_val(
            vec![
                (
                    Expression::Call(CallDef {
                        macroref: make_selector!(make_expr!("tstmac")),
                        arglist: vec![
                            Expression::Simple(Value::String(value_node!("bar".to_string(), 1, 1))),
                        ],
                        pos: Position::new(1, 0),
                    }),
                    Val::Tuple(vec![
                        (
                            value_node!("foo".to_string(), 1, 1),
                            Rc::new(Val::String("bar".to_string())),
                        ),
                    ]),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Unable to find arg1")]
    fn test_macro_hermetic() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("arg1".to_string(), 1, 0))
            .or_insert(Rc::new(Val::String("bar".to_string())));
        b.out
            .entry(value_node!("tstmac".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Macro(MacroDef {
                argdefs: vec![value_node!("arg2".to_string(), 1, 0)],
                fields: vec![
                    (
                        make_tok!("foo", 1, 1),
                        Expression::Simple(Value::Symbol(value_node!("arg1".to_string(), 1, 1))),
                    ),
                ],
                pos: Position::new(1, 0),
            })));
        test_expr_to_val(
            vec![
                (
                    Expression::Call(CallDef {
                        macroref: make_selector!(make_expr!("tstmac")),
                        arglist: vec![
                            Expression::Simple(Value::String(value_node!("bar".to_string(), 1, 1))),
                        ],
                        pos: Position::new(1, 1),
                    }),
                    Val::Tuple(vec![
                        (
                            value_node!("foo".to_string(), 1, 0),
                            Rc::new(Val::String("bar".to_string())),
                        ),
                    ]),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_select_expr() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("foo".to_string(), 1, 0))
            .or_insert(Rc::new(Val::String("bar".to_string())));
        b.out
            .entry(value_node!("baz".to_string(), 1, 0))
            .or_insert(Rc::new(Val::String("boo".to_string())));
        test_expr_to_val(
            vec![
                (
                    Expression::Select(SelectDef {
                        val: Box::new(Expression::Simple(Value::Symbol(value_node!(
                            "foo".to_string(),
                            1,
                            1
                        )))),
                        default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                        tuple: vec![
                            (
                                make_tok!("foo", 1, 1),
                                Expression::Simple(Value::String(value_node!(
                                    "2".to_string(),
                                    1,
                                    1
                                ))),
                            ),
                            (
                                make_tok!("bar", 1, 1),
                                Expression::Simple(Value::Int(value_node!(2, 1, 1))),
                            ),
                        ],
                        pos: Position::new(1, 0),
                    }),
                    Val::Int(2),
                ),
                (
                    Expression::Select(SelectDef {
                        val: Box::new(Expression::Simple(Value::Symbol(value_node!(
                            "baz".to_string(),
                            1,
                            1
                        )))),
                        default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                        tuple: vec![
                            (
                                make_tok!("bar", 1, 1),
                                Expression::Simple(Value::Int(value_node!(2, 1, 1))),
                            ),
                            (
                                make_tok!("quux", 1, 1),
                                Expression::Simple(Value::String(value_node!(
                                    "2".to_string(),
                                    1,
                                    1
                                ))),
                            ),
                        ],
                        pos: Position::new(1, 0),
                    }),
                    // If the field doesn't exist then we get the default.
                    Val::Int(1),
                ),
            ],
            b,
        );
    }

    #[test]
    #[should_panic(expected = "Expected String but got Integer in Select expression")]
    fn test_select_expr_not_a_string() {
        let mut b = Builder::new();
        b.out
            .entry(value_node!("foo".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Int(4)));
        test_expr_to_val(
            vec![
                (
                    Expression::Select(SelectDef {
                        val: Box::new(Expression::Simple(Value::Symbol(value_node!(
                            "foo".to_string(),
                            1,
                            1
                        )))),
                        default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 1)))),
                        tuple: vec![
                            (
                                make_tok!("bar", 1, 1),
                                Expression::Simple(Value::Int(value_node!(2, 1, 1))),
                            ),
                            (
                                make_tok!("quux", 1, 1),
                                Expression::Simple(Value::String(value_node!(
                                    "2".to_string(),
                                    1,
                                    1
                                ))),
                            ),
                        ],
                        pos: Position::new(1, 0),
                    }),
                    Val::Int(2),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_let_statement() {
        let mut b = Builder::new();
        let stmt = Statement::Let(LetDef {
            name: make_tok!("foo", 1, 1),
            value: Expression::Simple(Value::String(value_node!("bar".to_string(), 1, 1))),
        });
        b.build_stmt(&stmt).unwrap();
        test_expr_to_val(
            vec![
                (
                    Expression::Simple(Value::Symbol(value_node!("foo".to_string(), 1, 1))),
                    Val::String("bar".to_string()),
                ),
            ],
            b,
        );
    }

    #[test]
    fn test_build_file_string() {
        let mut b = Builder::new();
        b.build_file_string("let foo = 1;".to_string()).unwrap();
        let key = value_node!("foo".to_string(), 1, 0);
        assert!(b.out.contains_key(&key));
    }

    #[test]
    fn test_asset_symbol_lookups() {
        let mut b = Builder::new();
        b.assets
            .entry(value_node!("foo".to_string(), 1, 0))
            .or_insert(Rc::new(Val::Tuple(vec![
                (
                    value_node!("bar".to_string(), 1, 0),
                    Rc::new(Val::Tuple(vec![
                        (value_node!("quux".to_string(), 1, 0), Rc::new(Val::Int(1))),
                    ])),
                ),
            ])));
        test_expr_to_val(
            vec![
                (
                    Expression::Simple(Value::Symbol(value_node!("foo".to_string(), 1, 1))),
                    Val::Tuple(vec![
                        (
                            value_node!("bar".to_string(), 1, 0),
                            Rc::new(Val::Tuple(vec![
                                (value_node!("quux".to_string(), 1, 0), Rc::new(Val::Int(1))),
                            ])),
                        ),
                    ]),
                ),
            ],
            b,
        );
    }
}
