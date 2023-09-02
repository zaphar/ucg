// Copyright 2020 Jeremy Wall
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! Implements typechecking for the parsed ucg AST.
// FIXME(jwall): This probably just needs to disappear now.
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::ast::walk::Visitor;
use crate::ast::{Expression, FailDef, ImportDef, IncludeDef, Position, Shape, Statement, Value};
use crate::error::{BuildError, ErrorType};

use Expression::{
    Binary, Call, Cast, Copy, Debug, Fail, Format, Func, FuncOp, Grouped, Import, Include, Module,
    Not, Range, Select, Simple,
};
use Statement::Let;
use Value::{Boolean, Empty, Float, Int, List, Str, Symbol, Tuple};

pub struct Checker {
    symbol_table: BTreeMap<Rc<str>, Shape>,
    err_stack: Vec<BuildError>,
    shape_stack: Vec<Shape>,
}

impl Checker {
    pub fn new() -> Self {
        return Self {
            symbol_table: BTreeMap::new(),
            err_stack: Vec::new(),
            shape_stack: Vec::new(),
        };
    }

    pub fn pop_shape(&mut self) -> Option<Shape> {
        self.shape_stack.pop()
    }

    pub fn result(mut self) -> Result<BTreeMap<Rc<str>, Shape>, BuildError> {
        if let Some(err) = self.err_stack.pop() {
            Err(err)
        } else {
            Ok(self.symbol_table)
        }
    }
}

impl Visitor for Checker {
    fn visit_import(&mut self, _i: &mut ImportDef) {
        // noop by default;
    }

    fn leave_import(&mut self) {
        // noop by default
    }

    fn visit_include(&mut self, _i: &mut IncludeDef) {
        // noop by default;
    }

    fn leave_include(&mut self) {
        // noop by default
    }

    fn visit_fail(&mut self, _f: &mut FailDef) {
        // noop by default;
    }

    fn leave_fail(&mut self) {
        // noop by default
    }

    fn visit_value(&mut self, val: &mut Value) {
        // TODO(jwall): Some values can contain expressions. Handle those here.
        match val {
            Value::Empty(p) => self.shape_stack.push(Shape::Empty(p.clone())),
            Value::Boolean(p) => self.shape_stack.push(Shape::Boolean(p.clone())),
            Value::Int(p) => self.shape_stack.push(Shape::Int(p.clone())),
            Value::Float(p) => self.shape_stack.push(Shape::Float(p.clone())),
            Value::Str(p) => self.shape_stack.push(Shape::Str(p.clone())),
            // Symbols in a shape are placeholders. They allow a form of genericity
            // in the shape. They can be any type and are only refined down.
            // by their presence in an expression.
            Value::Symbol(p) => self.shape_stack.push(Shape::Hole(p.clone())),
            Value::List(_) => {
                // noop
            }
            Value::Tuple(_) => {
                // noop
            }
        }
    }

    fn leave_value(&mut self, _val: &Value) {
        // noop by default
    }

    fn visit_expression(&mut self, _expr: &mut Expression) {
        // noop by default
    }

    fn leave_expression(&mut self, expr: &Expression) {
        let shape = expr.derive_shape(&mut self.symbol_table);
        if let Shape::TypeErr(pos, msg) = &shape {
            self.err_stack.push(BuildError::with_pos(
                msg.clone(),
                ErrorType::TypeFail,
                pos.clone(),
            ));
        } else {
            self.shape_stack.push(shape);
        }
    }

    fn visit_statement(&mut self, _stmt: &mut Statement) {
        // noop by default
    }

    fn leave_statement(&mut self, stmt: &Statement) {
        // noop by default
    }
}

#[cfg(test)]
mod test;
