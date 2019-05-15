// Copyright 2019 Jeremy Wall
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
use std::borrow::BorrowMut;
use std::error::Error;
use std::io::Write;

use crate::ast::walk::Walker;
use crate::ast::*;

// TODO(jwall): We really need a way to preserve comments for these.
// Perhaps for code formatting we actually want to work on the token stream instead?

pub struct Printer {
    indent: u8,
    curr_indent: u8,
    w: Box<dyn Write>,
    pub errs: Vec<Box<dyn Error>>,
}

impl Printer {
    pub fn new(indent: u8, w: Box<dyn Write>) -> Self {
        Printer {
            indent: indent,
            curr_indent: 0,
            w: w,
            errs: Vec::new(),
        }
    }

    pub fn render_list_def(&mut self, def: &ListDef) -> std::io::Result<()> {
        panic!("Unimplemented");
        Ok(())
    }

    pub fn render_tuple_def(&mut self, def: &Vec<(Token, Expression)>) -> std::io::Result<()> {
        panic!("Unimplemented");
        Ok(())
    }

    pub fn render_value(&mut self, v: &Value) {
        // TODO
        let w: &mut Write = self.w.borrow_mut();
        let result = match v {
            Value::Boolean(b) => write!(w, "{}", b),
            Value::Empty(_) => write!(w, "NULL"),
            // TODO(jwall): Should we maintain precision for floats?
            Value::Float(f) => write!(w, "{}", f),
            Value::Int(i) => write!(w, "{}", i),
            // TODO(jwall): Make sure that we properly escape quotes here when rendering this?
            Value::Str(s) => write!(w, "\"{}\"", s),
            Value::Symbol(s) => write!(w, "{}", s),
            Value::List(l) => self.render_list_def(l),
            Value::Tuple(tpl) => self.render_tuple_def(&tpl.val),
        };
        if let Err(e) = result {
            self.errs.push(Box::new(e));
        }
    }

    fn render_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Binary(_def) => {}
            Expression::Call(_def) => {}
            Expression::Copy(_def) => {}
            Expression::Debug(_def) => {}
            Expression::Fail(_def) => {}
            Expression::Format(_def) => {}
            Expression::Func(_def) => {}
            Expression::FuncOp(_def) => {}
            Expression::Grouped(_expr, _) => {}
            Expression::Import(_def) => {}
            Expression::Include(_def) => {}
            Expression::Module(_def) => {}
            Expression::Not(_def) => {}
            Expression::Range(_def) => {}
            Expression::Select(_def) => {}
            Expression::Simple(_def) => {}
        }
    }

    fn render_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Let(_def) => {}
            Statement::Expression(_expr) => {}
            Statement::Assert(_def) => {}
            Statement::Output(_, _tok, _expr) => {}
        }
    }

    pub fn render(&mut self, stmts: Vec<&mut Statement>) {
        self.walk_statement_list(stmts);
    }
}

impl Walker for Printer {
    fn visit_value(&mut self, val: &mut Value) {
        self.render_value(val);
    }
    fn visit_expression(&mut self, expr: &mut Expression) {
        self.render_expr(expr);
    }
    fn visit_statement(&mut self, stmt: &mut Statement) {
        self.render_stmt(stmt);
    }
}
