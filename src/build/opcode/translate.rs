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
use crate::ast::{Expression, Statement, Value};
use crate::build::opcode::Op;
use crate::build::opcode::Primitive;
use crate::build::opcode::Value::{C, F, M, P, T};

pub struct AST();

impl AST {
    pub fn translate(stmts: Vec<Statement>) -> Vec<Op> {
        let mut ops = Vec::new();
        for stmt in stmts {
            match stmt {
                Statement::Expression(expr) => Self::translate_expr(expr, &mut ops),
                Statement::Assert(_, _) => {
                    unimplemented!("Assert statements are not implmented yet")
                }
                Statement::Let(_) => unimplemented!("Let statements are not implmented yet"),
                Statement::Output(_, _, _) => {
                    unimplemented!("Out statements are not implmented yet")
                }
                Statement::Print(_, _, _) => {
                    unimplemented!("Print statements are not implmented yet")
                }
            }
        }
        return ops;
    }

    fn translate_expr(expr: Expression, mut ops: &mut Vec<Op>) {
        match expr {
            Expression::Simple(v) => {
                Self::translate_value(v, &mut ops);
            }
            Expression::Fail(_) => unimplemented!("Fail expressions are not implmented yet"),
            Expression::Format(_) => unimplemented!("Format expressions are not implmented yet"),
            Expression::Func(_) => unimplemented!("Func expressions are not implmented yet"),
            Expression::FuncOp(_) => unimplemented!("FuncOp expressions are not implmented yet"),
            Expression::Grouped(_, _) => {
                unimplemented!("Grouped expressions are not implmented yet")
            }
            Expression::Import(_) => unimplemented!("Import expressions are not implmented yet"),
            Expression::Include(_) => unimplemented!("Include expressions are not implmented yet"),
            Expression::Module(_) => unimplemented!("Module expressions are not implmented yet"),
            Expression::Not(_) => unimplemented!("Not expressions are not implmented yet"),
            Expression::Range(_) => unimplemented!("Range expressions are not implmented yet"),
            Expression::Select(_) => unimplemented!("Select expressions are not implmented yet"),
            Expression::Binary(_) => unimplemented!("Binary expressions are not implmented yet"),
            Expression::Call(_) => unimplemented!("Call expressions are not implmented yet"),
            Expression::Copy(_) => unimplemented!("Copy expressions are not implmented yet"),
            Expression::Debug(_) => unimplemented!("Debug expressions are not implmented yet"),
        }
    }

    fn translate_value(value: Value, ops: &mut Vec<Op>) {
        match value {
            Value::Int(i) => ops.push(Op::Val(Primitive::Int(i.val))),
            Value::Float(f) => ops.push(Op::Val(Primitive::Float(f.val))),
            Value::Str(s) => ops.push(Op::Val(Primitive::Str(s.val))),
            Value::Empty(_pos) => ops.push(Op::Val(Primitive::Empty)),
            Value::Boolean(b) => ops.push(Op::Val(Primitive::Bool(b.val))),
            Value::Symbol(s) => ops.push(Op::Sym(s.val)),
            Value::Tuple(_flds) => unimplemented!("Select expression are not implmented yet"),
            Value::List(_els) => unimplemented!("Select expression are not implmented yet"),
        }
    }
}
