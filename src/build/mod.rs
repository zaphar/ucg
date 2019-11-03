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
use std::collections::HashMap;
use std::convert::TryInto;
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::rc::Rc;

use rustyline;
use simple_error;

use crate::ast::*;
use crate::error;
use crate::iter::OffsetStrIter;
use crate::parse::parse;

use crate::build::opcode::pointer::OpPointer;
use crate::build::opcode::translate;
use crate::build::opcode::translate::PositionMap;
use crate::build::opcode::Environment;
use crate::build::opcode::VM;

pub mod assets;
pub mod format;
pub mod ir;
pub mod opcode;
pub mod scope;

mod stdlib;

pub use self::ir::Val;

/// The result of a build.
type BuildResult = Result<(), Box<dyn Error>>;

/// AssertCollector collects the results of assertions in the UCG AST.
pub struct AssertCollector {
    pub counter: i32,
    pub success: bool,
    pub summary: String,
    pub failures: String,
}

impl AssertCollector {
    pub fn new() -> Self {
        Self {
            counter: 0,
            success: true,
            summary: String::new(),
            failures: String::new(),
        }
    }

    fn record_assert_result(&mut self, msg: &str, is_success: bool) {
        if !is_success {
            let msg = format!("{} - NOT OK: {}\n", self.counter, msg);
            self.summary.push_str(&msg);
            self.failures.push_str(&msg);
            self.success = false;
        } else {
            let msg = format!("{} - OK: {}\n", self.counter, msg);
            self.summary.push_str(&msg);
        }
        self.counter += 1;
    }
}

// TODO(jwall): I think the Rc<Val> is no longer necessary.
/// Builder handles building ucg code for a single file.
pub struct FileBuilder<'a, Stdout, Stderr>
where
    Stdout: std::io::Write + Clone,
    Stderr: std::io::Write + Clone,
{
    pub environment: Rc<RefCell<Environment<Stdout, Stderr>>>,
    working_dir: PathBuf,
    // FIXME(jwall): These need to be compiled and added to the op cache.
    // specifically in the environment.
    std: Rc<HashMap<String, &'static str>>,
    import_path: &'a Vec<PathBuf>,
    pub last: Option<Rc<Val>>,
    pub out: Option<Rc<Val>>,
    validate_mode: bool,
}

impl<'a, Stdout, Stderr> FileBuilder<'a, Stdout, Stderr>
where
    Stdout: std::io::Write + Clone,
    Stderr: std::io::Write + Clone,
{
    /// Constructs a new Builder.
    pub fn new<P: Into<PathBuf>>(
        working_dir: P,
        import_paths: &'a Vec<PathBuf>,
        stdout: Stdout,
        stderr: Stderr,
    ) -> Self {
        let environment = Environment::new_with_vars(stdout, stderr, env::vars().collect());
        FileBuilder {
            environment: Rc::new(RefCell::new(environment)),
            // Our import stack is initialized with ourself.
            working_dir: working_dir.into(),
            std: Rc::new(stdlib::get_libs()),
            import_path: import_paths,
            out: None,
            last: None,
            validate_mode: false,
        }
    }

    pub fn clone_builder(&self) -> Self {
        FileBuilder {
            environment: self.environment.clone(),
            working_dir: self.working_dir.clone(),
            std: self.std.clone(),
            import_path: self.import_path,
            out: None,
            last: None,
            validate_mode: self.validate_mode,
        }
    }

    /// Builds a ucg file at the named path.
    pub fn build<P: Into<PathBuf>>(&mut self, file: P) -> BuildResult {
        let file = file.into();
        self.working_dir = file.parent().unwrap().to_path_buf();
        let mut f = self.open_file(&Position::new(0, 0, 0), &file)?;
        let mut s = String::new();
        f.read_to_string(&mut s)?;
        let input = OffsetStrIter::new(&s).with_src_file(file.clone());
        // TODO(jwall): Pass in the file name?
        let eval_result = self.eval_input(input, Some(file.clone()));
        match eval_result {
            Ok(v) => {
                self.last = Some(v);
                Ok(())
            }
            Err(e) => {
                let err = simple_error::SimpleError::new(&format!(
                    "Error building file: {}\n{}",
                    file.to_string_lossy(),
                    e.as_ref()
                ));
                Err(Box::new(err))
            }
        }
    }

    fn open_file<P: Into<PathBuf>>(&self, pos: &Position, path: P) -> Result<File, Box<dyn Error>> {
        let path = path.into();
        match File::open(&path) {
            Ok(f) => Ok(f),
            Err(e) => Err(error::BuildError::with_pos(
                format!("Error opening file {} {}", path.to_string_lossy(), e),
                error::ErrorType::TypeFail,
                pos.clone(),
            )
            .to_boxed()),
        }
    }

    /// Puts the builder in validation mode.
    ///
    /// Among other things this means that assertions will be evaluated and their results
    /// will be saved in a report for later output.
    pub fn enable_validate_mode(&mut self) {
        self.validate_mode = true;
    }

    /// Builds a list of parsed UCG Statements.
    pub fn eval_stmts(&mut self, ast: Vec<Statement>, path: Option<PathBuf>) -> BuildResult {
        // We should probably stash this in an op_cache somewhere?
        let ops = translate::AST::translate(ast, &self.working_dir);
        let mut vm = VM::new(Rc::new(ops), self.environment.clone(), &self.working_dir);
        if path.is_some() {
            vm.set_path(path.unwrap());
        }
        if self.validate_mode {
            vm.enable_validate_mode();
        }
        vm.run()?;
        self.out = Some(Rc::new(vm.symbols_to_tuple(false).into()));
        Ok(())
    }

    pub fn repl(&mut self, mut editor: rustyline::Editor<()>, config_home: PathBuf) -> BuildResult {
        // loop
        let mut lines = crate::io::StatementAccumulator::new();
        println!("Welcome to the UCG repl. Ctrl-D to exit");
        println!("Type '#help' for help.");
        println!("");
        // Initialize VM with an empty OpPointer
        let mut vm = VM::new(
            Rc::new(PositionMap::new()),
            self.environment.clone(),
            &self.working_dir,
        );
        loop {
            // print prompt
            let line = editor.readline(&format!("{}> ", lines.next_line()))?;
            // repl commands are only valid while not accumulating a statement;
            let trimmed = line.trim();
            if trimmed.starts_with("#") {
                // handle the various commands.
                if trimmed.starts_with("#help") {
                    println!(include_str!("../help/repl.txt"));
                } else if trimmed.starts_with("#del") {
                    // remove a named binding from the builder output.
                    let args: Vec<&str> = trimmed.split(" ").skip(1).collect();
                    if args.len() != 1 {
                        // print usage of the #del command
                        eprintln!("The '#del' command expects a single argument specifying \nthe binding to delete.");
                    } else {
                        let key = args[0].to_string();
                        if let None = vm.remove_symbol(&key) {
                            eprintln!("No such binding {}", key);
                        }
                    }
                } else {
                    eprintln!("Invalid repl command...");
                    eprintln!("");
                    println!(include_str!("../help/repl.txt"));
                }
                continue;
            }
            lines.push(line);
            // check to see if that line is a statement
            loop {
                // read a statement
                if let Some(stmt) = lines.get_statement() {
                    // if it is then
                    // eval statement
                    let stmts = parse(OffsetStrIter::new(&stmt), None)?;
                    let ops = translate::AST::translate(stmts, &self.working_dir);
                    vm = vm.to_new_pointer(OpPointer::new(Rc::new(ops)));
                    match vm.run() {
                        // print the result
                        Err(e) => eprintln!("{}", e),
                        Ok(_) => {
                            match vm.last {
                                Some((ref val, _)) => {
                                    println!("{}", val);
                                    vm.last = None;
                                }
                                None => {}
                            }
                            editor.history_mut().add(stmt);
                            editor.save_history(&config_home)?;
                        }
                    }
                    // start loop over at prompt.
                    break;
                }
                // if not then keep accumulating lines without a prompt
                lines.push(editor.readline(&format!("{}> ", lines.next_line()))?);
            }
        }
    }

    // TODO(jwall): The repl is going to have to be in here.
    pub fn eval_input(
        &mut self,
        input: OffsetStrIter,
        path: Option<PathBuf>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        match parse(input.clone(), None) {
            Ok(stmts) => {
                self.eval_stmts(stmts, path)?;
                if let Some(v) = self.out.clone() {
                    return Ok(v);
                }
                unreachable!();
            }
            Err(err) => Err(Box::new(err)),
        }
    }

    /// Evaluate an input string as UCG.
    pub fn eval_string(&mut self, input: &str) -> Result<Rc<Val>, Box<dyn Error>> {
        self.eval_input(OffsetStrIter::new(input), None)
    }

    pub fn eval_expr(&mut self, expr: Expression) -> Result<Rc<Val>, Box<dyn Error>> {
        let mut ops_map = translate::PositionMap {
            ops: Vec::new(),
            pos: Vec::new(),
        };
        translate::AST::translate_stmt(
            Statement::Expression(expr),
            &mut ops_map,
            &self.working_dir,
        );
        let mut vm = VM::new(
            Rc::new(ops_map),
            self.environment.clone(),
            &self.working_dir,
        );
        if self.validate_mode {
            vm.enable_validate_mode();
        }
        vm.run()?;
        if let Some((val, _)) = vm.last.clone() {
            return Ok(Rc::new(val.try_into()?));
        }
        unreachable!();
    }

    pub fn get_out_by_name(&self, name: &str) -> Option<Rc<Val>> {
        if let Some(val) = self.out.clone() {
            if let &Val::Tuple(ref flds) = val.as_ref() {
                for (k, v) in flds.iter() {
                    if k == name {
                        return Some(v.clone());
                    }
                }
            }
        }
        return None;
    }

    pub fn assert_results(&self) -> bool {
        self.environment.borrow().assert_results.success
    }

    pub fn assert_summary(&self) -> String {
        self.environment.borrow().assert_results.summary.clone()
    }
}

#[cfg(test)]
mod compile_test;

#[cfg(test)]
mod test;
