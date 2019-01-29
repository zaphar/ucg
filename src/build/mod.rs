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
use std::collections::HashMap;
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::ops::Deref;
use std::path::PathBuf;
use std::rc::Rc;
use std::string::ToString;

use regex;
use simple_error;
use unicode_segmentation::UnicodeSegmentation;

use crate::ast::*;
use crate::build::scope::{find_in_fieldlist, Scope, ValueMap};
use crate::convert::ImporterRegistry;
use crate::error;
use crate::format::{ExpressionFormatter, FormatRenderer, SimpleFormatter};
use crate::iter::OffsetStrIter;
use crate::parse::parse;

pub mod assets;
pub mod ir;
pub mod scope;
mod stdlib;

pub use self::ir::Val;

enum ProcessingOpType {
    Map,
    Filter,
}

impl FuncDef {
    /// Expands a ucg function using the given arguments into a new Tuple.
    pub fn eval(
        &self,
        // TODO(jwall): This should come from the FuncDef instead.
        root: PathBuf,
        parent_builder: &FileBuilder,
        mut args: Vec<Rc<Val>>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        // Error conditions. If the args don't match the length and types of the argdefs then this is
        // func call error.
        if args.len() > self.argdefs.len() {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Func called with too many args in file: {}",
                    root.to_string_lossy()
                ),
                error::ErrorType::BadArgLen,
                self.pos.clone(),
            )));
        }
        // If the args don't match the types required by the expressions then that is a TypeFail.
        // If the expressions reference Symbols not defined in the FuncDef that is also an error.
        let mut build_output = HashMap::<PositionedItem<String>, Rc<Val>>::new();
        for (i, arg) in args.drain(0..).enumerate() {
            build_output
                .entry(self.argdefs[i].clone())
                .or_insert(arg.clone());
        }
        let mut b = parent_builder.clone_builder();
        if let Some(ref scope) = self.scope {
            b.scope = scope.spawn_child();
        }
        // We clobber anything that used to be in the scope with the arguments.
        b.merge_build_output(build_output, true);
        Ok(b.eval_expr(self.fields.as_ref(), &b.scope.spawn_child())?)
    }
}

/// The result of a build.
type BuildResult = Result<(), Box<dyn Error>>;

/// AssertCollector collects the results of assertions in the UCG AST.
pub struct AssertCollector {
    pub counter: i32,
    pub success: bool,
    pub summary: String,
    pub failures: String,
}

/// Builder handles building ucg code for a single file.
pub struct FileBuilder<'a> {
    working_dir: PathBuf,
    std: Rc<HashMap<String, &'static str>>,
    import_path: &'a Vec<PathBuf>,
    validate_mode: bool,
    pub assert_collector: AssertCollector,
    strict: bool,
    scope: Scope,
    import_registry: ImporterRegistry,
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

// TODO(jwall): Use the builder patter here. Just like AstWalker.
impl<'a> FileBuilder<'a> {
    /// Constructs a new Builder.
    pub fn new<P: Into<PathBuf>>(
        working_dir: P,
        import_paths: &'a Vec<PathBuf>,
        cache: Rc<RefCell<assets::Cache>>,
    ) -> Self {
        let env_vars: Vec<(String, String)> = env::vars().collect();
        let scope = scope::Scope::new(Rc::new(Val::Env(env_vars)));
        Self::new_with_scope(working_dir, import_paths, cache, scope)
    }

    /// Constructs a new Builder with a provided scope.
    pub fn new_with_scope<P: Into<PathBuf>>(
        working_dir: P,
        import_paths: &'a Vec<PathBuf>,
        cache: Rc<RefCell<assets::Cache>>,
        scope: Scope,
    ) -> Self {
        FileBuilder {
            // Our import stack is initialized with ourself.
            working_dir: working_dir.into(),
            std: Rc::new(stdlib::get_libs()),
            import_path: import_paths,
            validate_mode: false,
            assert_collector: AssertCollector {
                counter: 0,
                success: true,
                summary: String::new(),
                failures: String::new(),
            },
            scope: scope,
            strict: true,
            import_registry: ImporterRegistry::make_registry(),
            assets: cache,
            out_lock: None,
            is_module: false,
            last: None,
        }
    }

    pub fn clone_builder(&self) -> Self {
        FileBuilder {
            working_dir: self.working_dir.clone(),
            std: self.std.clone(),
            import_path: self.import_path,
            validate_mode: false,
            assert_collector: AssertCollector {
                counter: 0,
                success: true,
                summary: String::new(),
                failures: String::new(),
            },
            strict: true,
            assets: self.assets.clone(),
            // This is admittedly a little wasteful but we can live with it for now.
            import_registry: ImporterRegistry::make_registry(),
            scope: self.scope.spawn_clean(),
            out_lock: None,
            is_module: false,
            last: None,
        }
    }

    // TODO(jwall): With builder pattern
    pub fn set_build_output(&mut self, scope: ValueMap) {
        self.scope.build_output = scope;
    }

    /// Builds a ucg file at the named path.
    pub fn build<P: Into<PathBuf>>(&mut self, file: P) -> BuildResult {
        let file = file.into();
        self.working_dir = file.parent().unwrap().to_path_buf();
        let mut f = File::open(&file)?;
        let mut s = String::new();
        f.read_to_string(&mut s)?;
        let input = OffsetStrIter::new(&s).with_src_file(file.clone());
        let eval_result = self.eval_input(input);
        match eval_result {
            Ok(v) => {
                self.last = Some(v);
                Ok(())
            }
            Err(e) => {
                let err = simple_error::SimpleError::new(
                    format!(
                        "Error building file: {}\n{}",
                        file.to_string_lossy(),
                        e.as_ref()
                    )
                    .as_ref(),
                );
                Err(Box::new(err))
            }
        }
    }

    pub fn merge_build_output(&mut self, scope: ValueMap, clobber: bool) {
        for (name, value) in scope.iter() {
            if !clobber && !self.scope.build_output.contains_key(name) {
                self.scope.build_output.insert(name.clone(), value.clone());
            } else {
                self.scope.build_output.insert(name.clone(), value.clone());
            }
        }
    }

    pub fn set_strict(&mut self, to: bool) {
        self.strict = to;
    }

    fn eval_tuple(
        &self,
        fields: &Vec<(Token, Expression)>,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let mut new_fields = Vec::<(PositionedItem<String>, Rc<Val>)>::new();
        for &(ref name, ref expr) in fields.iter() {
            let val = self.eval_expr(expr, scope)?;
            new_fields.push((name.into(), val));
        }
        Ok(Rc::new(Val::Tuple(new_fields)))
    }

    fn eval_list(&self, def: &ListDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        let mut vals = Vec::new();
        for expr in def.elems.iter() {
            vals.push(self.eval_expr(expr, scope)?);
        }
        Ok(Rc::new(Val::List(vals)))
    }

    fn eval_value(&self, v: &Value, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        match v {
            &Value::Empty(_) => Ok(Rc::new(Val::Empty)),
            &Value::Boolean(ref b) => Ok(Rc::new(Val::Boolean(b.val))),
            &Value::Int(ref i) => Ok(Rc::new(Val::Int(i.val))),
            &Value::Float(ref f) => Ok(Rc::new(Val::Float(f.val))),
            &Value::Str(ref s) => Ok(Rc::new(Val::Str(s.val.to_string()))),
            &Value::Symbol(ref s) => {
                scope
                    .lookup_sym(&(s.into()), true)
                    .ok_or(Box::new(error::BuildError::new(
                        format!("Unable to find binding {}", s.val,),
                        error::ErrorType::NoSuchSymbol,
                        v.pos().clone(),
                    )))
            }
            &Value::List(ref def) => self.eval_list(def, scope),
            &Value::Tuple(ref tuple) => self.eval_tuple(&tuple.val, scope),
        }
    }

    /// Returns a Val by name from previously built UCG.
    pub fn get_out_by_name(&self, name: &str) -> Option<Rc<Val>> {
        let key = PositionedItem {
            pos: Position::new(0, 0, 0),
            val: name.to_string(),
        };
        self.scope.lookup_sym(&key, true)
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

    fn eval_input(&mut self, input: OffsetStrIter) -> Result<Rc<Val>, Box<dyn Error>> {
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

    // TODO Non file builder specific.
    /// Evaluate an input string as UCG.
    pub fn eval_string(&mut self, input: &str) -> Result<Rc<Val>, Box<dyn Error>> {
        self.eval_input(OffsetStrIter::new(input))
    }

    fn check_reserved_word(name: &str) -> bool {
        match name {
            "self" | "assert" | "true" | "false" | "let" | "import" | "as" | "select" | "func"
            | "module" | "env" | "map" | "filter" | "NULL" | "out" | "in" | "is" | "not" => true,
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

    fn find_file<P: Into<PathBuf>>(
        &self,
        path: P,
        use_import_path: bool,
    ) -> Result<PathBuf, Box<dyn Error>> {
        // Try a relative path first.
        let path = path.into();
        // TODO(jwall): Change this to take a root directory.
        let mut normalized = self.working_dir.clone();
        if path.is_relative() {
            normalized.push(&path);
            // First see if the normalized file exists or not.
            if !normalized.exists() && use_import_path {
                // If it does not then look for it in the list of import_paths
                for mut p in self.import_path.iter().cloned() {
                    p.push(&path);
                    if p.exists() {
                        normalized = p;
                        break;
                    }
                }
            }
        } else {
            normalized = path;
        }
        Ok(normalized.canonicalize()?)
    }

    fn eval_import(&self, def: &ImportDef) -> Result<Rc<Val>, Box<dyn Error>> {
        // Look for a std file first.
        if def.path.fragment.starts_with("std/") {
            if self.std.contains_key(&def.path.fragment) {
                // Okay then this is a stdlib and it's special.
                // Introduce a scope so the above borrow is dropped before we modify
                // the cache below.
                // Only parse the file once on import.
                let path = PathBuf::from(&def.path.fragment);
                let maybe_asset = self.assets.borrow().get(&path)?;
                let result = match maybe_asset {
                    Some(v) => v.clone(),
                    None => {
                        // TODO(jwall): This does not need to be a FileBuilder specifically
                        let mut b = self.clone_builder();
                        b.eval_string(self.std.get(&def.path.fragment).unwrap())?;
                        b.get_outputs_as_val()
                    }
                };
                let mut mut_assets_cache = self.assets.borrow_mut();
                mut_assets_cache.stash(path, result.clone())?;
                return Ok(result);
            } else {
                return Err(Box::new(error::BuildError::new(
                    format!("No such import {} in the std library.", def.path.fragment),
                    error::ErrorType::Unsupported,
                    def.pos.clone(),
                )));
            }
        }
        // Try a relative path first.
        let normalized = self.find_file(&def.path.fragment, true)?;
        if self.detect_import_cycle(normalized.to_string_lossy().as_ref()) {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Import Cycle Detected!!!! {} is already in import stack: {:?}",
                    normalized.to_string_lossy(),
                    self.scope.import_stack,
                ),
                error::ErrorType::Unsupported,
                def.pos.clone(),
            )));
        }
        // Introduce a scope so the above borrow is dropped before we modify
        // the cache below.
        // Only parse the file once on import.
        let maybe_asset = self.assets.borrow().get(&normalized)?;
        let result = match maybe_asset {
            Some(v) => v.clone(),
            None => {
                let mut b = self.clone_builder();
                b.build(&normalized)?;
                b.get_outputs_as_val()
            }
        };
        let mut mut_assets_cache = self.assets.borrow_mut();
        mut_assets_cache.stash(normalized.clone(), result.clone())?;
        return Ok(result);
    }

    fn eval_let(&mut self, def: &LetDef) -> Result<Rc<Val>, Box<dyn Error>> {
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

    fn eval_stmt(&mut self, stmt: &Statement) -> Result<Rc<Val>, Box<dyn Error>> {
        let child_scope = self.scope.clone();
        match stmt {
            &Statement::Assert(ref expr) => self.build_assert(&expr, &child_scope),
            &Statement::Let(ref def) => self.eval_let(def),
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

    fn add_vals(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        match *left {
            Val::Int(i) => {
                eval_binary_expr!(&Val::Int(ii), pos, right, Val::Int(i + ii), "Integer")
            }
            Val::Float(f) => {
                eval_binary_expr!(&Val::Float(ff), pos, right, Val::Float(f + ff), "Float")
            }
            Val::Str(ref s) => match right.as_ref() {
                &Val::Str(ref ss) => {
                    return Ok(Rc::new(Val::Str([s.to_string(), ss.clone()].concat())));
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
                    )));
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
                    )));
                }
            },
            ref expr => {
                return Err(Box::new(error::BuildError::new(
                    format!("{} does not support the '+' operation", expr.type_name()),
                    error::ErrorType::Unsupported,
                    pos.clone(),
                )));
            }
        }
    }

    fn subtract_vals(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
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
                )));
            }
        }
    }

    fn multiply_vals(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
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
                )));
            }
        }
    }

    fn divide_vals(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
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
                )));
            }
        }
    }

    fn do_deep_equal(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        Ok(Rc::new(Val::Boolean(
            left.equal(right.as_ref(), pos.clone())?,
        )))
    }

    fn do_not_deep_equal(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        Ok(Rc::new(Val::Boolean(
            !left.equal(right.as_ref(), pos.clone())?,
        )))
    }

    fn do_gt(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
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

    fn do_lt(
        &self,
        pos: &Position,
        left: Rc<Val>,
        right: Rc<Val>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
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
    ) -> Result<Rc<Val>, Box<dyn Error>> {
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
    ) -> Result<Rc<Val>, Box<dyn Error>> {
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

    fn do_dot_lookup(&self, right: &Expression, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        let pos = right.pos().clone();
        let scope = scope.clone().use_curr_val();
        match right {
            Expression::Copy(_) => return self.eval_expr(right, &scope),
            Expression::Call(_) => return self.eval_expr(right, &scope),
            Expression::Simple(Value::Symbol(ref s)) => {
                scope
                    .lookup_sym(s, true)
                    .ok_or(Box::new(error::BuildError::new(
                        format!("Unable to find binding {}", s.val,),
                        error::ErrorType::NoSuchSymbol,
                        pos,
                    )))
            }
            Expression::Simple(Value::Str(ref s)) => {
                scope
                    .lookup_sym(s, false)
                    .ok_or(Box::new(error::BuildError::new(
                        format!("Unable to find binding {}", s.val,),
                        error::ErrorType::NoSuchSymbol,
                        pos,
                    )))
            }
            Expression::Simple(Value::Int(ref i)) => {
                scope.lookup_idx(right.pos(), &Val::Int(i.val))
            }
            _ => {
                let val = self.eval_expr(right, &scope)?;
                match val.as_ref() {
                    Val::Int(i) => scope.lookup_idx(right.pos(), &Val::Int(*i)),
                    Val::Str(ref s) => scope
                        .lookup_sym(&PositionedItem::new(s.clone(), pos.clone()), false)
                        .ok_or(Box::new(error::BuildError::new(
                            format!("Unable to find binding {}", s,),
                            error::ErrorType::NoSuchSymbol,
                            pos,
                        ))),
                    _ => Err(Box::new(error::BuildError::new(
                        format!("Invalid selector lookup {}", val.type_name(),),
                        error::ErrorType::NoSuchSymbol,
                        pos,
                    ))),
                }
            }
        }
    }

    fn do_bool_operator(
        &self,
        kind: &BinaryExprType,
        left: &Expression,
        right: &Expression,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let left_pos = left.pos();
        let left = self.eval_expr(left, scope)?;
        if let Val::Boolean(b) = left.as_ref() {
            let right_pos = right.pos();
            let b = *b;
            if kind == &BinaryExprType::AND {
                if !b {
                    // short circuit
                    return Ok(Rc::new(Val::Boolean(b)));
                }
                let right = self.eval_expr(right, scope)?;
                if right.is_bool() {
                    return Ok(right);
                }
            } else {
                if b {
                    // short circuit
                    return Ok(Rc::new(Val::Boolean(b)));
                }
                let right = self.eval_expr(right, scope)?;
                if right.is_bool() {
                    return Ok(right);
                }
            }
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Expected boolean value for operator but got {}",
                    left.type_name()
                ),
                error::ErrorType::TypeFail,
                right_pos.clone(),
            )));
        } else {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Expected boolean value for operator but got {}",
                    left.type_name()
                ),
                error::ErrorType::TypeFail,
                left_pos.clone(),
            )));
        }
    }

    fn do_element_check(
        &self,
        left: &Expression,
        right: &Expression,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        // First we evaluate our right hand side so we have a something to search
        // inside for our left hand expression.
        let right_pos = right.pos().clone();
        let right = self.eval_expr(right, scope)?;
        // presence checks are only valid for tuples and lists.
        if !(right.is_tuple() || right.is_list()) {
            return Err(Box::new(error::BuildError::new(
                format!(
                    "Invalid righthand type for in operator {}",
                    right.type_name()
                ),
                error::ErrorType::TypeFail,
                right_pos,
            )));
        }
        if let &Val::List(ref els) = right.as_ref() {
            let left_pos = left.pos().clone();
            let left = self.eval_expr(left, scope)?;
            for val in els.iter() {
                if let Ok(b) = self.do_deep_equal(&left_pos, left.clone(), val.clone()) {
                    if let &Val::Boolean(b) = b.as_ref() {
                        if b {
                            // We found a match
                            return Ok(Rc::new(Val::Boolean(true)));
                        }
                    }
                }
            }
            // We didn't find a match anywhere so return false.
            return Ok(Rc::new(Val::Boolean(false)));
        } else {
            // Handle our tuple case since this isn't a list.
            let child_scope = scope.spawn_child().set_curr_val(right.clone());
            // Search for the field in our tuple or list.
            let maybe_val = self.do_dot_lookup(left, &child_scope);
            // Return the result of the search.
            return Ok(Rc::new(Val::Boolean(maybe_val.is_ok())));
        }
    }

    fn eval_re_match(
        &self,
        left: Rc<Val>,
        left_pos: &Position,
        right: Rc<Val>,
        right_pos: &Position,
        negate: bool,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let re = if let Val::Str(ref s) = right.as_ref() {
            regex::Regex::new(s.as_ref())?
        } else {
            return Err(Box::new(error::BuildError::new(
                format!("Expected string for regex but got {}", right.type_name()),
                error::ErrorType::TypeFail,
                right_pos.clone(),
            )));
        };
        let tgt = if let Val::Str(ref s) = left.as_ref() {
            s.as_ref()
        } else {
            return Err(Box::new(error::BuildError::new(
                format!("Expected string but got {}", left.type_name()),
                error::ErrorType::TypeFail,
                left_pos.clone(),
            )));
        };
        return if negate {
            Ok(Rc::new(Val::Boolean(!re.is_match(tgt))))
        } else {
            Ok(Rc::new(Val::Boolean(re.is_match(tgt))))
        };
    }

    fn eval_binary(&self, def: &BinaryOpDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        let kind = &def.kind;
        if let &BinaryExprType::IN = kind {
            // TODO Should we support this operation on strings too?
            return self.do_element_check(&def.left, &def.right, scope);
        }
        if let &BinaryExprType::IS = kind {
            return self.eval_is_check(def, scope);
        }
        match kind {
            // We special case the boolean operators because we want them to short circuit.
            &BinaryExprType::AND | &BinaryExprType::OR => {
                return self.do_bool_operator(kind, &def.left, &def.right, scope);
            }
            _ => {
                // noop
            }
        }
        let left = self.eval_expr(&def.left, scope)?;
        let child_scope = scope.spawn_child().set_curr_val(left.clone());
        if let &BinaryExprType::DOT = kind {
            return self.do_dot_lookup(&def.right, &child_scope);
        };
        // TODO(jwall): We need to handle call and copy expressions specially.
        let right = match self.eval_expr(&def.right, scope) {
            Ok(v) => v,
            Err(e) => return Err(e),
        };
        match kind {
            // Handle math and concatenation operators here
            &BinaryExprType::Add => self.add_vals(&def.pos, left, right),
            &BinaryExprType::Sub => self.subtract_vals(&def.pos, left, right),
            &BinaryExprType::Mul => self.multiply_vals(&def.pos, left, right),
            &BinaryExprType::Div => self.divide_vals(&def.pos, left, right),
            // Handle Comparison operators here
            &BinaryExprType::Equal => self.do_deep_equal(&def.pos, left, right),
            &BinaryExprType::GT => self.do_gt(&def.pos, left, right),
            &BinaryExprType::LT => self.do_lt(&def.pos, left, right),
            &BinaryExprType::GTEqual => self.do_gtequal(&def.pos, left, right),
            &BinaryExprType::LTEqual => self.do_ltequal(&def.pos, left, right),
            &BinaryExprType::NotEqual => self.do_not_deep_equal(&def.pos, left, right),
            &BinaryExprType::REMatch => {
                self.eval_re_match(left, def.left.pos(), right, def.right.pos(), false)
            }
            &BinaryExprType::NotREMatch => {
                self.eval_re_match(left, def.left.pos(), right, def.right.pos(), true)
            }
            &BinaryExprType::IN
            | &BinaryExprType::IS
            | &BinaryExprType::DOT
            | &BinaryExprType::AND
            | &BinaryExprType::OR => panic!("Unreachable"),
        }
    }

    fn get_outputs_as_val(&mut self) -> Rc<Val> {
        let fields: Vec<(PositionedItem<String>, Rc<Val>)> =
            self.scope.build_output.drain().collect();
        Rc::new(Val::Tuple(fields))
    }

    fn copy_from_base(
        &self,
        src_fields: &Vec<(PositionedItem<String>, Rc<Val>)>,
        overrides: &Vec<(Token, Expression)>,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
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

    fn eval_copy(&self, def: &CopyDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        let v = self.eval_value(&def.selector, scope)?;
        if let &Val::Tuple(ref src_fields) = v.as_ref() {
            let child_scope = scope.spawn_child().set_curr_val(v.clone());
            return self.copy_from_base(&src_fields, &def.fields, &child_scope);
        }
        if let &Val::Module(ref mod_def) = v.as_ref() {
            let maybe_tpl = mod_def.clone().arg_tuple.unwrap().clone();
            if let &Val::Tuple(ref src_fields) = maybe_tpl.as_ref() {
                // 1. First we create a builder.
                // TODO(jwall): This file should optionally come from the module def itself.
                let mut b = self.clone_builder();
                b.is_module = true;
                // 2. We construct an argument tuple by copying from the defs
                //    argset.
                // Push our base tuple on the stack so the copy can use
                // self to reference it.
                let child_scope = scope.spawn_child().set_curr_val(maybe_tpl.clone());
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
                    def.selector.pos().clone(),
                )));
            }
        }
        Err(Box::new(error::BuildError::new(
            format!("Expected Tuple or Module got {}", v),
            error::ErrorType::TypeFail,
            def.selector.pos().clone(),
        )))
    }

    fn eval_format(&self, def: &FormatDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        let tmpl = &def.template;
        return match &def.args {
            FormatArgs::List(ref args) => {
                let mut vals = Vec::new();
                for v in args.iter() {
                    let rcv = self.eval_expr(v, scope)?;
                    vals.push(rcv.deref().clone());
                }
                let formatter = SimpleFormatter::new(tmpl.clone(), vals);
                Ok(Rc::new(Val::Str(formatter.render(&def.pos)?)))
            }
            FormatArgs::Single(ref expr) => {
                let val = self.eval_expr(expr, scope)?;
                let mut builder = self.clone_builder();
                builder.scope.build_output.insert(
                    PositionedItem::new("item".to_string(), expr.pos().clone()),
                    val,
                );
                let formatter = ExpressionFormatter::new(tmpl.clone(), builder);
                Ok(Rc::new(Val::Str(formatter.render(&def.pos)?)))
            }
        };
    }

    fn eval_call(&self, def: &CallDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        let args = &def.arglist;
        let v = self.eval_value(&def.funcref, scope)?;
        if let &Val::Func(ref def) = v.deref() {
            // Congratulations this is actually a function.
            let mut argvals: Vec<Rc<Val>> = Vec::new();
            for arg in args.iter() {
                argvals.push(self.eval_expr(arg, scope)?);
            }
            return Ok(def.eval(self.working_dir.clone(), self, argvals)?);
        }
        Err(Box::new(error::BuildError::new(
            // We should pretty print the selectors here.
            format!("{} is not a Function", v),
            error::ErrorType::TypeFail,
            def.pos.clone(),
        )))
    }

    fn eval_func_def(&self, def: &mut FuncDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        def.scope = Some(scope.spawn_child());
        Ok(Rc::new(Val::Func(def.clone())))
    }

    // TODO(jwall): This stays with the FileBuilder specifically.
    fn file_dir(&self) -> PathBuf {
        return if self.working_dir.is_file() {
            // Only use the dirname portion if the root is a file.
            self.working_dir.parent().unwrap().to_path_buf()
        } else {
            // otherwise use clone of the root.
            self.working_dir.clone()
        };
    }

    fn eval_module_def(&self, def: &ModuleDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        // TODO(jwall): This should actually be passed in to here.
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

    fn eval_select(&self, def: &SelectDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
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

    fn eval_functional_list_processing(
        &self,
        elems: &Vec<Rc<Val>>,
        def: &FuncDef,
        typ: ProcessingOpType,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let mut out = Vec::new();
        for item in elems.iter() {
            let argvals = vec![item.clone()];
            let val = def.eval(self.working_dir.clone(), self, argvals)?;
            match typ {
                ProcessingOpType::Map => {
                    out.push(val.clone());
                }
                ProcessingOpType::Filter => {
                    if let &Val::Empty = val.as_ref() {
                        // noop
                        continue;
                    } else if let &Val::Boolean(false) = val.as_ref() {
                        // noop
                        continue;
                    }
                    out.push(item.clone());
                }
            }
        }
        return Ok(Rc::new(Val::List(out)));
    }

    fn eval_functional_tuple_processing(
        &self,
        fs: &Vec<(PositionedItem<String>, Rc<Val>)>,
        def: &FuncDef,
        typ: ProcessingOpType,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let mut out = Vec::new();
        for &(ref name, ref val) in fs {
            let argvals = vec![Rc::new(Val::Str(name.val.clone())), val.clone()];
            let result = def.eval(self.working_dir.clone(), self, argvals)?;
            match typ {
                ProcessingOpType::Map => {
                    if let &Val::List(ref fs) = result.as_ref() {
                        if fs.len() == 2 {
                            // index 0 should be a string for the new field name.
                            // index 1 should be the val.
                            let new_name = if let &Val::Str(ref s) = fs[0].as_ref() {
                                s.clone()
                            } else {
                                return Err(Box::new(error::BuildError::new(
                                    format!(
                                        "map on tuple expects the first item out list to be a string but got size {}",
                                        fs[0].type_name()
                                    ),
                                    error::ErrorType::TypeFail,
                                    def.pos.clone(),
                                )));
                            };
                            out.push((
                                PositionedItem::new(new_name, name.pos.clone()),
                                fs[1].clone(),
                            ));
                        } else {
                            return Err(Box::new(error::BuildError::new(
                                format!(
                                    "map on a tuple field expects a list of size 2 as output but got size {}",
                                    fs.len()
                                ),
                                error::ErrorType::TypeFail,
                                def.pos.clone(),
                            )));
                        }
                    } else {
                        return Err(Box::new(error::BuildError::new(
                            format!(
                                "map on a tuple field expects a list as output but got {:?}",
                                result.type_name()
                            ),
                            error::ErrorType::TypeFail,
                            def.pos.clone(),
                        )));
                    }
                }
                ProcessingOpType::Filter => {
                    if let &Val::Empty = result.as_ref() {
                        // noop
                        continue;
                    } else if let &Val::Boolean(false) = result.as_ref() {
                        // noop
                        continue;
                    }
                    out.push((name.clone(), val.clone()));
                }
            }
        }
        Ok(Rc::new(Val::Tuple(out)))
    }

    fn eval_reduce_op(&self, def: &ReduceOpDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        let maybe_target = self.eval_expr(&def.target, scope)?;
        let mut acc = self.eval_expr(&def.acc, scope)?;
        let maybe_mac = self.eval_value(&Value::Symbol(def.func.clone()), &self.scope.clone())?;
        let funcdef = match maybe_mac.as_ref() {
            &Val::Func(ref funcdef) => funcdef,
            _ => {
                return Err(Box::new(error::BuildError::new(
                    format!("Expected func but got {:?}", def.func),
                    error::ErrorType::TypeFail,
                    def.pos.clone(),
                )));
            }
        };
        match maybe_target.as_ref() {
            &Val::List(ref elems) => {
                for item in elems.iter() {
                    let argvals = vec![acc.clone(), item.clone()];
                    let result = funcdef.eval(self.working_dir.clone(), self, argvals)?;
                    acc = result;
                }
            }
            &Val::Tuple(ref fs) => {
                for &(ref name, ref val) in fs.iter() {
                    let argvals = vec![
                        acc.clone(),
                        Rc::new(Val::Str(name.val.clone())),
                        val.clone(),
                    ];
                    let result = funcdef.eval(self.working_dir.clone(), self, argvals)?;
                    acc = result;
                }
            }
            &Val::Str(ref s) => {
                for gc in s.graphemes(true) {
                    let argvals = vec![acc.clone(), Rc::new(Val::Str(gc.to_string()))];
                    let result = funcdef.eval(self.working_dir.clone(), self, argvals)?;
                    acc = result;
                }
            }
            other => {
                return Err(Box::new(error::BuildError::new(
                    format!(
                        "Expected List Str, or Tuple as target but got {:?}",
                        other.type_name()
                    ),
                    error::ErrorType::TypeFail,
                    def.target.pos().clone(),
                )));
            }
        }
        Ok(acc)
    }

    fn eval_functional_string_processing(
        &self,
        s: &str,
        def: &FuncDef,
        typ: ProcessingOpType,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let mut result = String::new();
        for gc in s.graphemes(true) {
            let arg = Rc::new(Val::Str(gc.to_string()));
            let out = def.eval(self.working_dir.clone(), self, vec![arg])?;
            match typ {
                ProcessingOpType::Filter => {
                    match out.as_ref() {
                        Val::Boolean(b) => {
                            if *b {
                                result.push_str(gc);
                            }
                        }
                        Val::Empty => {
                            // noop
                        }
                        _ => {
                            return Err(Box::new(error::BuildError::new(
                                format!(
                                    "Expected boolean or NULL for filter return but got {}",
                                    out.type_name()
                                ),
                                error::ErrorType::TypeFail,
                                def.pos.clone(),
                            )));
                        }
                    }
                }
                ProcessingOpType::Map => match out.as_ref() {
                    Val::Str(s) => {
                        result.push_str(&s);
                    }
                    _ => {
                        return Err(Box::new(error::BuildError::new(
                            format!("Expected string map return but got {}", out.type_name()),
                            error::ErrorType::TypeFail,
                            def.pos.clone(),
                        )));
                    }
                },
            }
        }
        Ok(Rc::new(Val::Str(result)))
    }

    fn eval_functional_processing(
        &self,
        def: &MapFilterOpDef,
        typ: ProcessingOpType,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let maybe_target = self.eval_expr(&def.target, scope)?;
        let maybe_mac = self.eval_value(&Value::Symbol(def.func.clone()), &self.scope.clone())?;
        let macdef = match maybe_mac.as_ref() {
            &Val::Func(ref macdef) => macdef,
            _ => {
                return Err(Box::new(error::BuildError::new(
                    format!("Expected func but got {:?}", def.func),
                    error::ErrorType::TypeFail,
                    def.pos.clone(),
                )));
            }
        };
        return match maybe_target.as_ref() {
            &Val::List(ref elems) => self.eval_functional_list_processing(elems, macdef, typ),
            &Val::Tuple(ref fs) => self.eval_functional_tuple_processing(fs, macdef, typ),
            // TODO(jwall): Strings?
            &Val::Str(ref s) => self.eval_functional_string_processing(s, macdef, typ),
            other => Err(Box::new(error::BuildError::new(
                format!(
                    "Expected List or Tuple as target but got {:?}",
                    other.type_name()
                ),
                error::ErrorType::TypeFail,
                def.target.pos().clone(),
            ))),
        };
    }

    fn record_assert_result(&mut self, msg: &str, is_success: bool) {
        if !is_success {
            let msg = format!("{} - NOT OK: {}\n", self.assert_collector.counter, msg);
            self.assert_collector.summary.push_str(&msg);
            self.assert_collector.failures.push_str(&msg);
            self.assert_collector.success = false;
        } else {
            let msg = format!("{} - OK: {}\n", self.assert_collector.counter, msg);
            self.assert_collector.summary.push_str(&msg);
        }
        self.assert_collector.counter += 1;
    }

    fn build_assert(
        &mut self,
        expr: &Expression,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        if !self.validate_mode {
            // we are not in validate_mode so build_asserts are noops.
            return Ok(Rc::new(Val::Empty));
        }
        let ok = match self.eval_expr(expr, scope) {
            Ok(v) => v,
            Err(e) => {
                // failure!
                let msg = format!("CompileError: {}\n", e);
                self.record_assert_result(&msg, false);
                return Ok(Rc::new(Val::Empty));
            }
        };

        match ok.as_ref() {
            &Val::Tuple(ref fs) => {
                let ok_field = match find_in_fieldlist("ok", fs) {
                    Some(ref val) => match val.as_ref() {
                        &Val::Boolean(b) => b,
                        _ => {
                            let msg = format!(
                                    "TYPE FAIL - Expected Boolean field ok in tuple {}, line: {} column: {}",
                                    ok.as_ref(), expr.pos().line, expr.pos().column
                                );
                            self.record_assert_result(&msg, false);
                            return Ok(Rc::new(Val::Empty));
                        }
                    },
                    None => {
                        let msg = format!(
                            "TYPE FAIL - Expected Boolean field ok in tuple {}, line: {} column: {}",
                            ok.as_ref(), expr.pos().line, expr.pos().column
                        );
                        self.record_assert_result(&msg, false);
                        return Ok(Rc::new(Val::Empty));
                    }
                };
                let desc = match find_in_fieldlist("desc", fs) {
                    Some(ref val) => match val.as_ref() {
                        Val::Str(ref s) => s.clone(),
                        _ => {
                            let msg = format!(
                                    "TYPE FAIL - Expected Boolean field desc in tuple {} line: {} column: {}",
                                    ok, expr.pos().line, expr.pos().column
                                );
                            self.record_assert_result(&msg, false);
                            return Ok(Rc::new(Val::Empty));
                        }
                    },
                    None => {
                        let msg = format!(
                            "TYPE FAIL - Expected Boolean field desc in tuple {} line: {} column: {}\n",
                            ok, expr.pos().line, expr.pos().column
                        );
                        self.record_assert_result(&msg, false);
                        return Ok(Rc::new(Val::Empty));
                    }
                };
                self.record_assert_result(&desc, ok_field);
            }
            &Val::Empty
            | &Val::Boolean(_)
            | &Val::Env(_)
            | &Val::Float(_)
            | &Val::Int(_)
            | &Val::Str(_)
            | &Val::List(_)
            | &Val::Func(_)
            | &Val::Module(_) => {
                // record an assertion type-failure result.
                let msg = format!(
                    "TYPE FAIL - Expected tuple with ok and desc fields got {} at line: {} column: {}\n",
                    ok, expr.pos().line, expr.pos().column
                );
                self.record_assert_result(&msg, false);
                return Ok(Rc::new(Val::Empty));
            }
        }
        Ok(ok)
    }

    fn get_file_as_string(&self, pos: &Position, path: &str) -> Result<String, Box<dyn Error>> {
        let normalized = match self.find_file(path, false) {
            Ok(p) => p,
            Err(e) => {
                return Err(Box::new(error::BuildError::new(
                    format!("Error finding file {} {}", path, e),
                    error::ErrorType::TypeFail,
                    pos.clone(),
                )));
            }
        };
        let mut f = match File::open(&normalized) {
            Ok(f) => f,
            Err(e) => {
                return Err(Box::new(error::BuildError::new(
                    format!("Error opening file {} {}", normalized.to_string_lossy(), e),
                    error::ErrorType::TypeFail,
                    pos.clone(),
                )));
            }
        };
        let mut contents = String::new();
        f.read_to_string(&mut contents)?;
        Ok(contents)
    }

    pub fn eval_include(&self, def: &IncludeDef) -> Result<Rc<Val>, Box<dyn Error>> {
        return if def.typ.fragment == "str" {
            Ok(Rc::new(Val::Str(
                self.get_file_as_string(&def.path.pos, &def.path.fragment)?,
            )))
        } else {
            let maybe_importer = self.import_registry.get_importer(&def.typ.fragment);
            match maybe_importer {
                Some(importer) => {
                    let file_contents =
                        self.get_file_as_string(&def.path.pos, &def.path.fragment)?;
                    let val = importer.import(file_contents.as_bytes())?;
                    Ok(val)
                }
                None => Err(Box::new(error::BuildError::new(
                    format!("Unknown include conversion type {}", def.typ.fragment),
                    error::ErrorType::Unsupported,
                    def.typ.pos.clone(),
                ))),
            }
        };
    }

    fn eval_func_op(&self, def: &FuncOpDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        match def {
            FuncOpDef::Filter(ref def) => {
                self.eval_functional_processing(def, ProcessingOpType::Filter, scope)
            }
            FuncOpDef::Map(ref def) => {
                self.eval_functional_processing(def, ProcessingOpType::Map, scope)
            }
            FuncOpDef::Reduce(ref def) => self.eval_reduce_op(def, scope),
        }
    }

    pub fn eval_range(&self, def: &RangeDef, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        let start = self.eval_expr(&def.start, scope)?;
        let start = match start.as_ref() {
            &Val::Int(i) => i,
            _ => {
                return Err(Box::new(error::BuildError::new(
                    format!(
                        "Expected an integer for range start but got {}",
                        start.type_name()
                    ),
                    error::ErrorType::TypeFail,
                    def.start.pos().clone(),
                )));
            }
        };
        // See if there was a step.
        let step = match &def.step {
            Some(step) => {
                let step = self.eval_expr(&step, scope)?;
                match step.as_ref() {
                    &Val::Int(i) => i,
                    _ => {
                        return Err(Box::new(error::BuildError::new(
                            format!(
                                "Expected an integer for range step but got {}",
                                step.type_name()
                            ),
                            error::ErrorType::TypeFail,
                            def.start.pos().clone(),
                        )));
                    }
                }
            }
            None => 1,
        };

        // Get the end.
        let end = self.eval_expr(&def.end, scope)?;
        let end = match end.as_ref() {
            &Val::Int(i) => i,
            _ => {
                return Err(Box::new(error::BuildError::new(
                    format!(
                        "Expected an integer for range start but got {}",
                        end.type_name()
                    ),
                    error::ErrorType::TypeFail,
                    def.start.pos().clone(),
                )));
            }
        };

        let vec = (start..end + 1)
            .step_by(step as usize)
            .map(|i| Rc::new(Val::Int(i)))
            .collect();
        Ok(Rc::new(Val::List(vec)))
    }

    pub fn eval_is_check(
        &self,
        def: &BinaryOpDef,
        scope: &Scope,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let tval = self.eval_expr(def.right.as_ref(), scope)?;
        let typ = match tval.as_ref() {
            Val::Str(ref s) => s.clone(),
            _ => {
                return Err(Box::new(error::BuildError::new(
                    format!("Expected string expression but got {}", tval),
                    error::ErrorType::TypeFail,
                    def.right.pos().clone(),
                )));
            }
        };
        let val = self.eval_expr(def.left.as_ref(), scope)?;
        let result = match typ.as_str() {
            "str" => val.is_str(),
            "bool" => val.is_bool(),
            "null" => val.is_empty(),
            "int" => val.is_int(),
            "float" => val.is_float(),
            "tuple" => val.is_tuple(),
            "list" => val.is_list(),
            "func" => val.is_func(),
            "module" => val.is_module(),
            other => {
                return Err(Box::new(error::BuildError::new(
                    format!("Expected valid type name but got {}", other),
                    error::ErrorType::TypeFail,
                    def.right.pos().clone(),
                )));
            }
        };
        Ok(Rc::new(Val::Boolean(result)))
    }

    // Evals a single Expression in the context of a running Builder.
    // It does not mutate the builders collected state at all.
    pub fn eval_expr(&self, expr: &Expression, scope: &Scope) -> Result<Rc<Val>, Box<dyn Error>> {
        match expr {
            &Expression::Simple(ref val) => self.eval_value(val, scope),
            &Expression::Binary(ref def) => self.eval_binary(def, scope),
            &Expression::Copy(ref def) => self.eval_copy(def, scope),
            &Expression::Range(ref def) => self.eval_range(def, scope),
            &Expression::Grouped(ref expr) => self.eval_expr(expr, scope),
            &Expression::Format(ref def) => self.eval_format(def, scope),
            &Expression::Call(ref def) => self.eval_call(def, scope),
            &Expression::Func(ref def) => {
                let mut def_clone = def.clone();
                self.eval_func_def(&mut def_clone, scope)
            }
            &Expression::Module(ref def) => {
                let mut def_clone = def.clone();
                self.eval_module_def(&mut def_clone, scope)
            }
            &Expression::Select(ref def) => self.eval_select(def, scope),
            &Expression::FuncOp(ref def) => self.eval_func_op(def, scope),
            &Expression::Include(ref def) => self.eval_include(def),
            &Expression::Import(ref def) => self.eval_import(def),
            &Expression::Fail(ref def) => {
                let err = self.eval_expr(&def.message, scope)?;
                return if let Val::Str(ref s) = err.as_ref() {
                    Err(Box::new(error::BuildError::new(
                        s.clone(),
                        error::ErrorType::UserDefined,
                        def.pos.clone(),
                    )))
                } else {
                    Err(Box::new(error::BuildError::new(
                        format!(
                            "Expected string for message but got {}",
                            def.message.as_ref()
                        ),
                        error::ErrorType::TypeFail,
                        def.message.pos().clone(),
                    )))
                };
            }
            &Expression::Not(ref def) => {
                let val = self.eval_expr(&def.expr, scope)?;
                return if let Val::Boolean(b) = val.as_ref() {
                    Ok(Rc::new(Val::Boolean(!b)))
                } else {
                    Err(Box::new(error::BuildError::new(
                        format!(
                            "Expected boolean for expression but got {}",
                            def.expr.as_ref()
                        ),
                        error::ErrorType::TypeFail,
                        def.expr.pos().clone(),
                    )))
                };
            }
        }
    }
}

#[cfg(test)]
mod compile_test;

#[cfg(test)]
mod test;
