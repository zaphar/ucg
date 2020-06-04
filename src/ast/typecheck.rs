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

use std::collections::BTreeMap;

use crate::ast::walk::Visitor;
use crate::ast::{Expression, Shape, Statement, Value};

use Expression::{
    Binary, Call, Cast, Copy, Debug, Fail, Format, Func, FuncOp, Grouped, Import, Include, Module,
    Not, Range, Select, Simple,
};
use Statement::Let;
use Value::{Boolean, Empty, Float, Int, List, Str, Symbol, Tuple};

pub struct Checker {
    symbol_table: BTreeMap<String, Shape>,
}

impl Visitor for Checker {
    fn visit_import(&mut self, _i: &mut super::ImportDef) {
        // noop by default;
    }
    fn visit_include(&mut self, _i: &mut super::IncludeDef) {
        // noop by default;
    }
    fn visit_fail(&mut self, _f: &mut super::FailDef) {
        // noop by default;
    }
    fn visit_value(&mut self, _val: &mut Value) {
        // noop by default
    }
    fn visit_expression(&mut self, _expr: &mut Expression) {
        // noop by default
    }
    fn visit_statement(&mut self, _stmt: &mut Statement) {
        // noop by default
    }
}
