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
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, VecDeque};
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::ops::Deref;
use std::path::PathBuf;
use std::rc::Rc;
use std::string::ToString;

use simple_error;

use crate::ast::*;
use crate::build::scope::{Scope, ValueMap};
use crate::error;
use crate::format;
use crate::iter::OffsetStrIter;
use crate::parse::parse;

pub mod assets;
pub mod ir;
pub mod scope;

pub use self::ir::Val;

impl MacroDef {
    /// Expands a ucg Macro using the given arguments into a new Tuple.
    pub fn eval(
        &self,
        root: PathBuf,
        parent_builder: &FileBuilder,
        mut args: Vec<Rc<Val>>,
    ) -> Result<Vec<(PositionedItem<String>, Rc<Val>)>, Box<Error>> {
        // Error conditions. If the args don't match the length and types of the argdefs then this is
        // macro call error.
        if args.len() > self.argdefs.len() {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Macro called with too many args in file: {}",
                    root.to_string_lossy()
                ),
                error::ErrorType::BadArgLen,
                self.pos.clone(),
            )));
        }
        // If the args don't match the types required by the expressions then that is a TypeFail.
        // If the expressions reference Symbols not defined in the MacroDef that is also an error.
        // TODO(jwall): We should probably enforce that the Expression Symbols must be in argdefs rules
        // at Macro definition time not evaluation time.
        let mut scope = HashMap::<PositionedItem<String>, Rc<Val>>::new();
        for (i, arg) in args.drain(0..).enumerate() {
            scope.entry(self.argdefs[i].clone()).or_insert(arg.clone());
        }
        let mut b = parent_builder.clone_builder(root);
        b.set_build_output(scope);
        let mut result: Vec<(PositionedItem<String>, Rc<Val>)> = Vec::new();
        for &(ref key, ref expr) in self.fields.iter() {
            // We clone the expressions here because this macro may be consumed
            // multiple times in the future.
            let scope = b.scope.spawn_child();
            let val = b.eval_expr(expr, &scope)?;
            result.push((key.into(), val.clone()));
        }
        Ok(result)
    }
}

/// The result of a build.
type BuildResult = Result<(), Box<Error>>;

/// AssertCollector collects the results of assertions in the UCG AST.
pub struct AssertCollector {
    pub success: bool,
    pub summary: String,
    pub failures: String,
}

/// Builder handles building ucg code for a single file.
pub struct FileBuilder<'a> {
    file: PathBuf,
    import_path: &'a Vec<PathBuf>,
    validate_mode: bool,
    pub assert_collector: AssertCollector,
    strict: bool,
    scope: Scope,
    // NOTE(jwall): We use interior mutability here because we need
    // our asset cache to be shared by multiple different sub-builders.
    // We use Rc to handle the reference counting for us and we use
    // RefCell to give us interior mutability. This sacrifices our
    // compile time memory safety for runtime checks. However it's
    // acceptable in this case since I can't figure out a better way to
    // handle it.
    // The assets are other parsed files from import statements. They
    // are keyed by the canonicalized import path. This acts as a cache
    // so multiple imports of the same file don't have to be parsed
    // multiple times.
    assets: Rc<RefCell<assets::Cache>>,
    pub is_module: bool,
    pub last: Option<Rc<Val>>,
    pub out_lock: Option<(String, Rc<Val>)>,
}

macro_rules! eval_binary_expr {
    ($case:pat, $pos:ident, $rside:ident, $result:expr, $msg:expr) => {
        match $rside.as_ref() {
            $case => {
                return Ok(Rc::new($result));
            }
            val => {
                return Err(Box::new(error::BuildError::new(
                    format!("Expected {} but got {}", $msg, val),
                    error::ErrorType::TypeFail,
                    $pos.clone(),
                )));
            }
        }
    };
}

impl<'a> FileBuilder<'a> {
    /// Constructs a new Builder.
    pub fn new<P: Into<PathBuf>>(
        file: P,
        import_paths: &'a Vec<PathBuf>,
        cache: Rc<RefCell<assets::Cache>>,
    ) -> Self {
        let env_vars: Vec<(String, String)> = env::vars().collect();
        let scope = scope::Scope::new(Rc::new(Val::Env(env_vars)));
        Self::new_with_scope(file, import_paths, cache, scope)
    }

    /// Constructs a new Builder with a provided scope.
    pub fn new_with_scope<P: Into<PathBuf>>(
        file: P,
        import_paths: &'a Vec<PathBuf>,
        cache: Rc<RefCell<assets::Cache>>,
        scope: Scope,
    ) -> Self {
        let file = file.into();
        FileBuilder {
            // Our import stack is initialized with ourself.
            file: file,
            import_path: import_paths,
            validate_mode: false,
            assert_collector: AssertCollector {
                success: true,
                summary: String::new(),
                failures: String::new(),
            },
            scope: scope,
            strict: true,
            assets: cache,
            out_lock: None,
            is_module: false,
            last: None,
        }
    }

    pub fn clone_builder<P: Into<PathBuf>>(&self, file: P) -> Self {
        FileBuilder {
            file: file.into(),
            import_path: self.import_path,
            validate_mode: false,
            assert_collector: AssertCollector {
                success: true,
                summary: String::new(),
                failures: String::new(),
            },
            strict: true,
            assets: self.assets.clone(),
            scope: self.scope.spawn_clean(),
            out_lock: None,
            is_module: false,
            last: None,
        }
    }

    pub fn set_build_output(&mut self, scope: ValueMap) {
        self.scope.build_output = scope;
    }

    pub fn set_strict(&mut self, to: bool) {
        self.strict = to;
    }

    fn eval_tuple(
        &mut self,
        fields: &Vec<(Token, Expression)>,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<Error>> {
        let mut new_fields = Vec::<(PositionedItem<String>, Rc<Val>)>::new();
        for &(ref name, ref expr) in fields.iter() {
            let val = self.eval_expr(expr, scope)?;
            new_fields.push((name.into(), val));
        }
        Ok(Rc::new(Val::Tuple(new_fields)))
    }

    fn eval_list(&mut self, def: &ListDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let mut vals = Vec::new();
        for expr in def.elems.iter() {
            vals.push(self.eval_expr(expr, scope)?);
        }
        Ok(Rc::new(Val::List(vals)))
    }

    fn eval_value(&mut self, v: &Value, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        match v {
            &Value::Empty(_) => Ok(Rc::new(Val::Empty)),
            &Value::Boolean(ref b) => Ok(Rc::new(Val::Boolean(b.val))),
            &Value::Int(ref i) => Ok(Rc::new(Val::Int(i.val))),
            &Value::Float(ref f) => Ok(Rc::new(Val::Float(f.val))),
            &Value::Str(ref s) => Ok(Rc::new(Val::Str(s.val.to_string()))),
            &Value::Symbol(ref s) => {
                scope
                    .lookup_sym(&(s.into()))
                    .ok_or(Box::new(error::BuildError::new(
                        format!("Unable to find binding {}", s.val,),
                        error::ErrorType::NoSuchSymbol,
                        v.pos().clone(),
                    )))
            }
            &Value::List(ref def) => self.eval_list(def, scope),
            &Value::Tuple(ref tuple) => self.eval_tuple(&tuple.val, scope),
            &Value::Selector(ref selector_list_node) => {
                self.lookup_selector(&selector_list_node.sel, scope)
            }
        }
    }

    /// Returns a Val by name from previously built UCG.
    pub fn get_out_by_name(&self, name: &str) -> Option<Rc<Val>> {
        let key = PositionedItem {
            pos: Position::new(0, 0, 0),
            val: name.to_string(),
        };
        self.scope.lookup_sym(&key)
    }

    /// Puts the builder in validation mode.
    ///
    /// Among other things this means that assertions will be evaluated and their results
    /// will be saved in a report for later output.
    pub fn enable_validate_mode(&mut self) {
        self.validate_mode = true;
    }

    /// Builds a list of parsed UCG Statements.
    pub fn eval_stmts(&mut self, ast: &Vec<Statement>) -> BuildResult {
        for stmt in ast.iter() {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }

    fn eval_input(&mut self, input: OffsetStrIter) -> Result<Rc<Val>, Box<Error>> {
        match parse(input.clone()) {
            Ok(stmts) => {
                //panic!("Successfully parsed {}", input);
                let mut out: Option<Rc<Val>> = None;
                for stmt in stmts.iter() {
                    out = Some(self.eval_stmt(stmt)?);
                }
                match out {
                    None => return Ok(Rc::new(Val::Empty)),
                    Some(val) => Ok(val),
                }
            }
            Err(err) => Err(Box::new(error::BuildError::new(
                format!("{}", err,),
                error::ErrorType::ParseError,
                (&input).into(),
            ))),
        }
    }

    /// Evaluate an input string as UCG.
    pub fn eval_string(&mut self, input: &str) -> Result<Rc<Val>, Box<Error>> {
        self.eval_input(OffsetStrIter::new(input))
    }

    /// Builds a ucg file at the named path.
    pub fn build(&mut self) -> BuildResult {
        let mut f = File::open(&self.file)?;
        let mut s = String::new();
        f.read_to_string(&mut s)?;
        let eval_result = self.eval_string(&s);
        match eval_result {
            Ok(v) => {
                self.last = Some(v);
                Ok(())
            }
            Err(e) => {
                let err = simple_error::SimpleError::new(
                    format!(
                        "Error building file: {}\n{}",
                        self.file.to_string_lossy(),
                        e.as_ref()
                    )
                    .as_ref(),
                );
                Err(Box::new(err))
            }
        }
    }

    fn check_reserved_word(name: &str) -> bool {
        match name {
            "self" | "assert" | "true" | "false" | "let" | "import" | "as" | "select" | "macro"
            | "module" | "env" | "map" | "filter" | "NULL" | "out" => true,
            _ => false,
        }
    }

    fn detect_import_cycle(&self, path: &str) -> bool {
        self.scope
            .import_stack
            .iter()
            .find(|p| *p == path)
            .is_some()
    }

    fn eval_import(&mut self, def: &ImportDef) -> Result<Rc<Val>, Box<Error>> {
        let sym = &def.name;
        if Self::check_reserved_word(&sym.fragment) {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Import {} binding collides with reserved word",
                    sym.fragment
                ),
                error::ErrorType::ReservedWordError,
                sym.pos.clone(),
            )));
        }
        // Try a relative path first.
        let mut normalized = self.file.parent().unwrap().to_path_buf();
        let import_path = PathBuf::from(&def.path.fragment);
        if import_path.is_relative() {
            normalized.push(&import_path);
            // First see if the normalized file exists or not.
            if !normalized.exists() {
                // If it does not then look for it in the list of import_paths
                for mut p in self.import_path.iter().cloned() {
                    p.push(&import_path);
                    if p.exists() {
                        normalized = p;
                        break;
                    }
                }
            }
        } else {
            normalized = import_path;
        }
        normalized = normalized.canonicalize()?;
        if self.detect_import_cycle(normalized.to_string_lossy().as_ref()) {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Import Cycle Detected!!!! {} is already in import stack: {:?}",
                    normalized.to_string_lossy(),
                    self.scope.import_stack,
                ),
                error::ErrorType::Unsupported,
                sym.pos.clone(),
            )));
        }
        // Introduce a scope so the above borrow is dropped before we modify
        // the cache below.
        // Only parse the file once on import.
        let maybe_asset = self.assets.borrow().get(&normalized)?;
        let result = match maybe_asset {
            Some(v) => v.clone(),
            None => {
                let mut b = self.clone_builder(normalized.clone());
                b.build()?;
                b.get_outputs_as_val()
            }
        };
        let key = sym.into();
        if self.scope.build_output.contains_key(&key) {
            return Err(Box::new(error::BuildError::new(
                format!("Binding for import name {} already exists", sym.fragment),
                error::ErrorType::DuplicateBinding,
                def.path.pos.clone(),
            )));
        }
        self.scope.build_output.insert(key, result.clone());
        let mut mut_assets_cache = self.assets.borrow_mut();
        mut_assets_cache.stash(normalized.clone(), result.clone())?;
        return Ok(result);
    }

    fn eval_let(&mut self, def: &LetDef) -> Result<Rc<Val>, Box<Error>> {
        let child_scope = self.scope.clone();
        let val = self.eval_expr(&def.value, &child_scope)?;
        let name = &def.name;
        if Self::check_reserved_word(&name.fragment) {
            return Err(Box::new(error::BuildError::new(
                format!("Let {} binding collides with reserved word", name.fragment),
                error::ErrorType::ReservedWordError,
                name.pos.clone(),
            )));
        }
        match self.scope.build_output.entry(name.into()) {
            Entry::Occupied(e) => {
                return Err(Box::new(error::BuildError::new(
                    format!(
                        "Binding \
                         for {:?} already \
                         exists",
                        e.key(),
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

    fn eval_stmt(&mut self, stmt: &Statement) -> Result<Rc<Val>, Box<Error>> {
        let child_scope = self.scope.clone();
        match stmt {
            &Statement::Assert(ref expr) => self.build_assert(&expr),
            &Statement::Let(ref def) => self.eval_let(def),
            &Statement::Import(ref def) => self.eval_import(def),
            &Statement::Expression(ref expr) => self.eval_expr(expr, &child_scope),
            // Only one output can be used per file. Right now we enforce this by
            // having a single builder per file.
            &Statement::Output(ref typ, ref expr) => {
                if let None = self.out_lock {
                    let val = self.eval_expr(expr, &child_scope)?;
                    self.out_lock = Some((typ.fragment.to_string(), val.clone()));
                    Ok(val)
                } else {
                    Err(Box::new(error::BuildError::new(
                        format!("You can only have one output per file."),
                        error::ErrorType::DuplicateBinding,
                        typ.pos.clone(),
                    )))
                }
            }
        }
    }

    fn find_in_fieldlist(
        target: &str,
        fs: &Vec<(PositionedItem<String>, Rc<Val>)>,
    ) -> Option<Rc<Val>> {
        for (key, val) in fs.iter().cloned() {
            if target == &key.val {
                return Some(val.clone());
            }
        }
        return None;
    }

    fn lookup_in_env(
        &self,
        search: &Token,
        stack: &mut VecDeque<Rc<Val>>,
        fs: &Vec<(String, String)>,
    ) -> Result<(), Box<Error>> {
        for &(ref name, ref val) in fs.iter() {
            if &search.fragment == name {
                stack.push_back(Rc::new(Val::Str(val.clone())));
                return Ok(());
            } else if !self.strict {
                stack.push_back(Rc::new(Val::Empty));
                return Ok(());
            }
        }
        return Err(Box::new(error::BuildError::new(
            format!("Environment Variable {} not set", search.fragment),
            error::ErrorType::NoSuchSymbol,
            search.pos.clone(),
        )));
    }

    fn lookup_in_tuple(
        &self,
        stack: &mut VecDeque<Rc<Val>>,
        sl: &SelectorList,
        next: (&Position, &str),
        fs: &Vec<(PositionedItem<String>, Rc<Val>)>,
    ) -> Result<(), Box<Error>> {
        if let Some(vv) = Self::find_in_fieldlist(next.1, fs) {
            stack.push_back(vv.clone());
        } else {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Unable to \
                     match element {} in selector \
                     path [{}]",
                    next.1, sl,
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
        let idx = next.1.parse::<usize>()?;
        if idx < elems.len() {
            stack.push_back(elems[idx].clone());
        } else {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Unable to \
                     match element {} in selector \
                     path [{}]",
                    next.1, sl,
                ),
                error::ErrorType::NoSuchSymbol,
                next.0.clone(),
            )));
        }
        Ok(())
    }

    fn lookup_selector(&mut self, sl: &SelectorList, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let first = self.eval_expr(&sl.head, scope)?;
        // First we ensure that the result is a tuple or a list.
        let mut stack = VecDeque::new();
        match first.as_ref() {
            &Val::Tuple(_) => {
                stack.push_back(first.clone());
            }
            &Val::List(_) => {
                stack.push_back(first.clone());
            }
            &Val::Env(_) => {
                stack.push_back(first.clone());
            }
            _ => {
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
                        self.lookup_in_tuple(&mut stack, sl, (&next.pos, &next.fragment), fs)?;
                        continue;
                    }
                    &Val::Env(ref fs) => {
                        self.lookup_in_env(&next, &mut stack, fs)?;
                        continue;
                    }
                    &Val::List(ref elems) => {
                        self.lookup_in_list(&mut stack, sl, (&next.pos, &next.fragment), elems)?;
                        continue;
                    }
                    _ => {
                        return Err(Box::new(error::BuildError::new(
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
            Val::Str(ref s) => match right.as_ref() {
                &Val::Str(ref ss) => {
                    return Ok(Rc::new(Val::Str([s.to_string(), ss.clone()].concat())))
                }
                val => {
                    return Err(Box::new(error::BuildError::new(
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
                    return Err(Box::new(error::BuildError::new(
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
                return Err(Box::new(error::BuildError::new(
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
                return Err(Box::new(error::BuildError::new(
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
                return Err(Box::new(error::BuildError::new(
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
                return Err(Box::new(error::BuildError::new(
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
        Ok(Rc::new(Val::Boolean(
            left.equal(right.as_ref(), pos.clone())?,
        )))
    }

    fn do_not_deep_equal(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<Error>> {
        Ok(Rc::new(Val::Boolean(
            !left.equal(right.as_ref(), pos.clone())?,
        )))
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
        Err(Box::new(error::BuildError::new(
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
        Err(Box::new(error::BuildError::new(
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
        Err(Box::new(error::BuildError::new(
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
        Err(Box::new(error::BuildError::new(
            format!(
                "Incompatible types for numeric comparison {} with {}",
                left.type_name(),
                right.type_name()
            ),
            error::ErrorType::TypeFail,
            pos.clone(),
        )))
    }

    fn eval_binary(&mut self, def: &BinaryOpDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let kind = &def.kind;
        let left = self.eval_expr(&def.left, scope)?;
        let right = self.eval_expr(&def.right, scope)?;
        match kind {
            &BinaryExprType::Add => self.add_vals(&def.pos, left, right),
            &BinaryExprType::Sub => self.subtract_vals(&def.pos, left, right),
            &BinaryExprType::Mul => self.multiply_vals(&def.pos, left, right),
            &BinaryExprType::Div => self.divide_vals(&def.pos, left, right),
        }
    }

    fn eval_compare(&mut self, def: &ComparisonDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let kind = &def.kind;
        let left = self.eval_expr(&def.left, scope)?;
        let right = self.eval_expr(&def.right, scope)?;
        match kind {
            &CompareType::Equal => self.do_deep_equal(&def.pos, left, right),
            &CompareType::GT => self.do_gt(&def.pos, left, right),
            &CompareType::LT => self.do_lt(&def.pos, left, right),
            &CompareType::GTEqual => self.do_gtequal(&def.pos, left, right),
            &CompareType::LTEqual => self.do_ltequal(&def.pos, left, right),
            &CompareType::NotEqual => self.do_not_deep_equal(&def.pos, left, right),
        }
    }

    fn get_outputs_as_val(&mut self) -> Rc<Val> {
        let fields: Vec<(PositionedItem<String>, Rc<Val>)> =
            self.scope.build_output.drain().collect();
        Rc::new(Val::Tuple(fields))
    }

    fn copy_from_base(
        &mut self,
        src_fields: &Vec<(PositionedItem<String>, Rc<Val>)>,
        overrides: &Vec<(Token, Expression)>,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<Error>> {
        let mut m = HashMap::<PositionedItem<String>, (i32, Rc<Val>)>::new();
        // loop through fields and build  up a hashmap
        let mut count = 0;
        for &(ref key, ref val) in src_fields.iter() {
            if let Entry::Vacant(v) = m.entry(key.clone()) {
                v.insert((count, val.clone()));
                count += 1;
            } else {
                return Err(Box::new(error::BuildError::new(
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
        for &(ref key, ref val) in overrides.iter() {
            let expr_result = self.eval_expr(val, scope)?;
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
                    if src_val.1.type_equal(&expr_result)
                        || src_val.1.is_empty()
                        || expr_result.is_empty()
                    {
                        v.insert((src_val.0, expr_result));
                    } else {
                        return Err(Box::new(error::BuildError::new(
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
        let mut new_fields: Vec<(PositionedItem<String>, (i32, Rc<Val>))> = m.drain().collect();
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

    fn eval_copy(&mut self, def: &CopyDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let v = self.lookup_selector(&def.selector.sel, scope)?;
        if let &Val::Tuple(ref src_fields) = v.as_ref() {
            let mut child_scope = scope.spawn_child();
            child_scope.set_curr_val(v.clone());
            return self.copy_from_base(&src_fields, &def.fields, &child_scope);
        }
        if let &Val::Module(ref mod_def) = v.as_ref() {
            let maybe_tpl = mod_def.clone().arg_tuple.unwrap().clone();
            if let &Val::Tuple(ref src_fields) = maybe_tpl.as_ref() {
                // 1. First we create a builder.
                let mut b = self.clone_builder(self.file.clone());
                b.is_module = true;
                // 2. We construct an argument tuple by copying from the defs
                //    argset.
                // Push our base tuple on the stack so the copy can use
                // self to reference it.
                let mut child_scope = scope.spawn_child();
                child_scope.set_curr_val(maybe_tpl.clone());
                let mod_args = self.copy_from_base(src_fields, &def.fields, &child_scope)?;
                // put our copied parameters tuple in our builder under the mod key.
                let mod_key =
                    PositionedItem::new_with_pos(String::from("mod"), Position::new(0, 0, 0));
                match b.scope.build_output.entry(mod_key) {
                    Entry::Occupied(e) => {
                        return Err(Box::new(error::BuildError::new(
                            format!(
                                "Binding \
                                 for {:?} already \
                                 exists in module",
                                e.key(),
                            ),
                            error::ErrorType::DuplicateBinding,
                            mod_def.pos.clone(),
                        )));
                    }
                    Entry::Vacant(e) => {
                        e.insert(mod_args.clone());
                    }
                }
                // 4. Evaluate all the statements using the builder.
                b.eval_stmts(&mod_def.statements)?;
                // 5. Take all of the bindings in the module and construct a new
                //    tuple using them.
                return Ok(b.get_outputs_as_val());
            } else {
                return Err(Box::new(error::BuildError::new(
                    format!(
                        "Weird value stored in our module parameters slot {:?}",
                        mod_def.arg_tuple
                    ),
                    error::ErrorType::TypeFail,
                    def.selector.pos.clone(),
                )));
            }
        }
        Err(Box::new(error::BuildError::new(
            format!("Expected Tuple or Module got {}", v),
            error::ErrorType::TypeFail,
            def.selector.pos.clone(),
        )))
    }

    fn eval_format(&mut self, def: &FormatDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let tmpl = &def.template;
        let args = &def.args;
        let mut vals = Vec::new();
        for v in args.iter() {
            let rcv = self.eval_expr(v, scope)?;
            vals.push(rcv.deref().clone());
        }
        let formatter = format::Formatter::new(tmpl.clone(), vals);
        Ok(Rc::new(Val::Str(formatter.render(&def.pos)?)))
    }

    fn eval_call(&mut self, def: &CallDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let sel = &def.macroref;
        let args = &def.arglist;
        let v = self.lookup_selector(&sel.sel, scope)?;
        if let &Val::Macro(ref m) = v.deref() {
            // Congratulations this is actually a macro.
            let mut argvals: Vec<Rc<Val>> = Vec::new();
            for arg in args.iter() {
                argvals.push(self.eval_expr(arg, scope)?);
            }
            let fields = m.eval(self.file.clone(), self, argvals)?;
            return Ok(Rc::new(Val::Tuple(fields)));
        }
        Err(Box::new(error::BuildError::new(
            // We should pretty print the selectors here.
            format!("{} is not a Macro", v),
            error::ErrorType::TypeFail,
            def.pos.clone(),
        )))
    }

    fn eval_macro_def(&self, def: &MacroDef) -> Result<Rc<Val>, Box<Error>> {
        match def.validate_symbols() {
            Ok(()) => Ok(Rc::new(Val::Macro(def.clone()))),
            Err(set) => Err(Box::new(error::BuildError::new(
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

    fn file_dir(&self) -> PathBuf {
        return if self.file.is_file() {
            // Only use the dirname portion if the root is a file.
            self.file.parent().unwrap().to_path_buf()
        } else {
            // otherwise use clone of the root..
            self.file.clone()
        };
    }

    fn eval_module_def(&mut self, def: &ModuleDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let root = self.file_dir();
        // Always work on a copy. The original should not be modified.
        let mut def = def.clone();
        // First we rewrite the imports to be absolute paths.
        def.imports_to_absolute(root);
        // Then we create our tuple default.
        def.arg_tuple = Some(self.eval_tuple(&def.arg_set, scope)?);
        // Then we construct a new Val::Module
        Ok(Rc::new(Val::Module(def)))
    }

    fn eval_select(&mut self, def: &SelectDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let target = &def.val;
        let def_expr = &def.default;
        let fields = &def.tuple;
        // First resolve the target expression.
        let v = self.eval_expr(target, scope)?;
        // Second ensure that the expression resolves to a string.
        if let &Val::Str(ref name) = v.deref() {
            // Third find the field with that name in the tuple.
            for &(ref fname, ref val_expr) in fields.iter() {
                if &fname.fragment == name {
                    // Fourth return the result of evaluating that field.
                    return self.eval_expr(val_expr, scope);
                }
            }
            // Otherwise return the default.
            return self.eval_expr(def_expr, scope);
        } else if let &Val::Boolean(b) = v.deref() {
            for &(ref fname, ref val_expr) in fields.iter() {
                if &fname.fragment == "true" && b {
                    // Fourth return the result of evaluating that field.
                    return self.eval_expr(val_expr, scope);
                } else if &fname.fragment == "false" && !b {
                    return self.eval_expr(val_expr, scope);
                }
            }
            // Otherwise return the default.
            return self.eval_expr(def_expr, scope);
        } else {
            return Err(Box::new(error::BuildError::new(
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

    fn eval_list_op(&mut self, def: &ListOpDef, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        let maybe_list = self.eval_expr(&def.target, scope)?;
        let l = match maybe_list.as_ref() {
            &Val::List(ref elems) => elems,
            other => {
                return Err(Box::new(error::BuildError::new(
                    format!("Expected List as target but got {:?}", other.type_name()),
                    error::ErrorType::TypeFail,
                    def.target.pos().clone(),
                )));
            }
        };
        let mac = &def.mac;
        if let &Val::Macro(ref macdef) = self.lookup_selector(&mac.sel, scope)?.as_ref() {
            let mut out = Vec::new();
            for item in l.iter() {
                let argvals = vec![item.clone()];
                let fields = macdef.eval(self.file.clone(), self, argvals)?;
                if let Some(v) = Self::find_in_fieldlist(&def.field, &fields) {
                    match def.typ {
                        ListOpType::Map => {
                            out.push(v.clone());
                        }
                        ListOpType::Filter => {
                            if let &Val::Empty = v.as_ref() {
                                // noop
                                continue;
                            } else if let &Val::Boolean(false) = v.as_ref() {
                                // noop
                                continue;
                            }
                            out.push(item.clone());
                        }
                    }
                }
            }
            return Ok(Rc::new(Val::List(out)));
        }
        return Err(Box::new(error::BuildError::new(
            format!("Expected macro but got {:?}", mac),
            error::ErrorType::TypeFail,
            def.pos.clone(),
        )));
    }

    fn build_assert(&mut self, tok: &Token) -> Result<Rc<Val>, Box<Error>> {
        if !self.validate_mode {
            // we are not in validate_mode then build_asserts are noops.
            return Ok(Rc::new(Val::Empty));
        }
        let expr = &tok.fragment;
        let assert_input =
            OffsetStrIter::new_with_offsets(expr, tok.pos.line - 1, tok.pos.column - 1);
        let ok = match self.eval_input(assert_input) {
            Ok(v) => v,
            Err(e) => {
                // failure!
                let msg = format!(
                    "NOT OK - '{}' at line: {} column: {}\n\tCompileError: {}\n",
                    expr, tok.pos.line, tok.pos.column, e
                );
                self.assert_collector.summary.push_str(&msg);
                self.assert_collector.failures.push_str(&msg);
                self.assert_collector.success = false;
                return Ok(Rc::new(Val::Empty));
            }
        };

        if let &Val::Boolean(b) = ok.as_ref() {
            // record the assertion result.
            if b {
                // success!
                let msg = format!(
                    "OK - '{}' at line: {} column: {}\n",
                    expr, tok.pos.line, tok.pos.column
                );
                self.assert_collector.summary.push_str(&msg);
            } else {
                // failure!
                let msg = format!(
                    "NOT OK - '{}' at line: {} column: {}\n",
                    expr, tok.pos.line, tok.pos.column
                );
                self.assert_collector.summary.push_str(&msg);
                self.assert_collector.failures.push_str(&msg);
                self.assert_collector.success = false;
            }
        } else {
            // record an assertion type-failure result.
            let msg = format!(
                "TYPE FAIL - '{}' Expected Boolean got {} at line: {} column: {}\n",
                expr, ok, tok.pos.line, tok.pos.column
            );
            self.assert_collector.failures.push_str(&msg);
            self.assert_collector.success = false;
            self.assert_collector.summary.push_str(&msg);
        }
        Ok(ok)
    }

    // Evals a single Expression in the context of a running Builder.
    // It does not mutate the builders collected state at all.
    pub fn eval_expr(&mut self, expr: &Expression, scope: &Scope) -> Result<Rc<Val>, Box<Error>> {
        match expr {
            &Expression::Simple(ref val) => self.eval_value(val, scope),
            &Expression::Binary(ref def) => self.eval_binary(def, scope),
            &Expression::Compare(ref def) => self.eval_compare(def, scope),
            &Expression::Copy(ref def) => self.eval_copy(def, scope),
            &Expression::Grouped(ref expr) => self.eval_expr(expr, scope),
            &Expression::Format(ref def) => self.eval_format(def, scope),
            &Expression::Call(ref def) => self.eval_call(def, scope),
            &Expression::Macro(ref def) => self.eval_macro_def(def),
            &Expression::Module(ref def) => self.eval_module_def(def, scope),
            &Expression::Select(ref def) => self.eval_select(def, scope),
            &Expression::ListOp(ref def) => self.eval_list_op(def, scope),
        }
    }
}

#[cfg(test)]
mod compile_test;

#[cfg(test)]
mod test;
