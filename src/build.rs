use parse::{parse, Statement, Expression, Value, FieldList};

use std::error::Error;
use std::collections::HashMap;
use std::convert::From;

quick_error! {
    #[derive(Debug,PartialEq)]
    pub enum BuildError {
        TypeFail(msg: String) {
            description("Eval Error")
            display("Eval Error {}", msg)
        }
        NoSuchSymbol(msg: String) {
            description("Eval Error")
            display("Eval Error {}", msg)
        }
        TODO(msg: String) {
            description("Eval Error")
            display("Eval Error {}", msg)
        }
    }
}

/// BuildResult is the result of a build.
type BuildResult = Result<(), Box<Error>>;

/// Val is the type of a value for a field in a Tuple.
#[derive(PartialEq,Debug)]
pub enum Val<'a> {
    Int(i64),
    Float(f64),
    String(&'a str),
    Symbol(&'a str),
    Tuple(FieldList<'a>),
}

impl<'a> From<Value<'a>> for Val<'a> {
    fn from(v: Value<'a>) -> Self {
        match v {
            Value::Int(i) => Val::Int(i),
            Value::Float(f) => Val::Float(f),
            Value::String(s) => Val::String(s),
            Value::Symbol(s) => Val::Symbol(s),
            Value::Tuple(ref fields) => {
                Val::Tuple(Vec::new())
            }
        }
    }
}

/// Tuple defines a set of fields.
#[derive(PartialEq,Debug)]
pub struct Tuple<'a>(HashMap<&'a str, Val<'a>>);

/// Builder parses one or more statements into a out Tuple.
pub struct Builder<'a> {
    /// env is the set of key value pairs provided at build time.
    env: HashMap<&'a str, &'a str>,
    /// assets are other parsed files from import statements. They
    /// are keyed by the normalized import path. This acts as a cache
    /// so multiple imports of the same file don't have to be parsed
    /// multiple times.
    assets: HashMap<&'a str, Tuple<'a>>,
    /// out is our built output.
    out: Tuple<'a>,
}

impl<'a> Builder<'a> {
    /// new_builder constructs Builder with initialized fields ready to parse.
    pub fn new_builder() -> Self {
        Builder {
            env: HashMap::new(),
            assets: HashMap::new(),
            out: Tuple(HashMap::new()),
        }
    }

    pub fn build_dir(&self, name: &str) -> BuildResult {
        Ok(())
    }

    pub fn build_file(&self, name: &str) -> BuildResult {
        Ok(())
    }

    pub fn build(&self, ast: Vec<Statement>) -> BuildResult {
        // TODO(jwall):
        for stmt in ast.iter() {
            self.build_stmt(stmt);
        }
        Ok(())
    }

    fn eval_expr(&'a self, expr: Expression<'a>) -> Result<Val, Box<Error>> {
        match expr {
            Expression::Simple(val) => {
                Ok(Val::from(val))
            },
            Expression::Add(v, expr) => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
            Expression::Sub(v, expr) => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
            Expression::Mul(v, expr) => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
            Expression::Div(v, expr) => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
            Expression::Copy(sel, fields) => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
            Expression::Selector(sel) => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
            },
            Expression::Grouped(expr) => {
                Err(Box::new(
                    BuildError::TODO(
                        "TODO(jwall): Unimplemented Expression".to_string())))
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
