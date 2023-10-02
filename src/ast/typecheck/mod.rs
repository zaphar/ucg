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
use std::rc::Rc;

use crate::ast::walk::Visitor;
use crate::ast::{
    Expression, FailDef, FuncShapeDef, ImportDef, IncludeDef, Shape, Statement, Value,
};
use crate::error::{BuildError, ErrorType};

use super::{
    CastType, CopyDef, FuncDef, ImportShape, ModuleShape, NarrowedShape, NotDef, PositionedItem,
};

/// Trait for shape derivation.
pub trait DeriveShape {
    /// Derive a shape using a provided symbol table.
    fn derive_shape(&self, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape;
}

impl DeriveShape for FuncDef {
    fn derive_shape(&self, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
        // 1. First set up our symbols.
        let mut sym_table = self
            .argdefs
            .iter()
            .map(|sym| (sym.val.clone(), dbg!(Shape::Hole(sym.clone()))))
            .collect::<BTreeMap<Rc<str>, Shape>>();
        sym_table.append(&mut symbol_table.clone());
        // 2.Then determine the shapes of those symbols in our expression.
        let shape = self.fields.derive_shape(&mut sym_table);
        // 3. Finally determine what the return shape can be.
        // only include the closed over shapes.
        let table = self
            .argdefs
            .iter()
            .map(|sym| {
                (
                    sym.val.clone(),
                    dbg!(sym_table
                        .get(&sym.val)
                        .unwrap()
                        .clone()
                        .with_pos(sym.pos.clone())),
                )
            })
            .collect::<BTreeMap<Rc<str>, Shape>>();
        Shape::Func(FuncShapeDef {
            args: table,
            ret: shape.with_pos(self.pos.clone()).into(),
        })
    }
}

fn derive_include_shape(
    IncludeDef {
        pos,
        path: _path,
        typ: _typ,
    }: &IncludeDef,
) -> Shape {
    Shape::Narrowed(NarrowedShape::new_with_pos(
        vec![
            Shape::Tuple(PositionedItem::new(vec![], pos.clone())),
            Shape::List(NarrowedShape::new_with_pos(vec![], pos.clone())),
        ],
        pos.clone(),
    ))
}

fn derive_not_shape(def: &NotDef, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
    let shape = def.expr.as_ref().derive_shape(symbol_table);
    if let Shape::Boolean(_) = &shape {
        return Shape::Boolean(def.pos.clone());
    } else if let Shape::Hole(_) = &shape {
        return Shape::Boolean(def.pos.clone());
    } else if let Shape::Narrowed(shape_list) = &shape {
        for s in shape_list.types.iter() {
            if let Shape::Boolean(_) = s {
                return Shape::Boolean(def.pos.clone());
            }
        }
    };
    Shape::TypeErr(
        def.pos.clone(),
        format!(
            "Expected Boolean value in Not expression but got: {:?}",
            shape
        ),
    )
}

fn derive_copy_shape(def: &CopyDef, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
    let base_shape = def.selector.derive_shape(symbol_table);
    match &base_shape {
        // TODO(jwall): Should we allow a stack of these?
        Shape::TypeErr(_, _) => base_shape,
        Shape::Boolean(_)
        | Shape::Int(_)
        | Shape::Float(_)
        | Shape::Str(_)
        | Shape::List(_)
        | Shape::Func(_) => Shape::TypeErr(
            def.pos.clone(),
            format!("Not a Copyable type {}", base_shape.type_name()),
        ),
        // This is an interesting one. Do we assume tuple or module here?
        Shape::Hole(pi) => Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::Tuple(PositionedItem::new(vec![], pi.pos.clone())),
                Shape::Module(ModuleShape {
                    items: vec![],
                    ret: Box::new(Shape::Narrowed(NarrowedShape {
                        pos: pi.pos.clone(),
                        types: vec![],
                    })),
                }),
                Shape::Import(ImportShape::Unresolved(pi.clone())),
            ],
            pi.pos.clone(),
        )),
        Shape::Narrowed(potentials) => {
            // 1. Do the possible shapes include tuple, module, or import?
            let filtered = potentials
                .types
                .iter()
                .filter_map(|v| match v {
                    Shape::Tuple(_) | Shape::Module(_) | Shape::Import(_) | Shape::Hole(_) => {
                        Some(v.clone())
                    }
                    _ => None,
                })
                .collect::<Vec<Shape>>();
            if !filtered.is_empty() {
                //  1.1 Then return those and strip the others.
                Shape::Narrowed(NarrowedShape::new_with_pos(filtered, def.pos.clone()))
            } else {
                // 2. Else return a type error
                Shape::TypeErr(
                    def.pos.clone(),
                    format!("Not a Copyable type {}", base_shape.type_name()),
                )
            }
        }
        // These have understandable ways to resolve the type.
        Shape::Module(mdef) => {
            let arg_fields = def
                .fields
                .iter()
                .map(|(tok, expr)| (tok.fragment.clone(), expr.derive_shape(symbol_table)))
                .collect::<BTreeMap<Rc<str>, Shape>>();
            // 1. Do our copyable fields have the right names and shapes based on mdef.items.
            for (tok, shape) in mdef.items.iter() {
                if let Some(s) = arg_fields.get(&tok.fragment) {
                    if let Shape::TypeErr(pos, msg) = shape.narrow(s, symbol_table) {
                        return Shape::TypeErr(pos, msg);
                    }
                }
            }
            //  1.1 If so then return the ret as our shape.
            mdef.ret.as_ref().clone()
        }
        Shape::Tuple(t_def) => {
            let mut base_fields = t_def.clone();
            base_fields.val.extend(
                def.fields
                    .iter()
                    .map(|(tok, expr)| (tok.clone(), expr.derive_shape(symbol_table))),
            );
            Shape::Tuple(base_fields).with_pos(def.pos.clone())
        }
        Shape::Import(ImportShape::Unresolved(_)) => Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![Shape::Tuple(PositionedItem::new(vec![], def.pos.clone()))],
            def.pos.clone(),
        )),
        Shape::Import(ImportShape::Resolved(_, tuple_shape)) => {
            let mut base_fields = tuple_shape.clone();
            base_fields.extend(
                def.fields
                    .iter()
                    .map(|(tok, expr)| (tok.clone(), expr.derive_shape(symbol_table))),
            );
            Shape::Tuple(PositionedItem::new(base_fields, def.pos.clone()))
        }
    }
}

impl DeriveShape for Expression {
    fn derive_shape(&self, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
        match self {
            Expression::Simple(v) => v.derive_shape(symbol_table),
            Expression::Format(def) => Shape::Str(def.pos.clone()),
            Expression::Not(def) => derive_not_shape(def, symbol_table),
            Expression::Grouped(v, _pos) => v.as_ref().derive_shape(symbol_table),
            Expression::Range(def) => Shape::List(NarrowedShape::new_with_pos(
                vec![Shape::Int(def.start.pos().clone())],
                def.pos.clone(),
            )),
            Expression::Cast(def) => match def.cast_type {
                CastType::Int => Shape::Int(def.pos.clone()),
                CastType::Str => Shape::Str(def.pos.clone()),
                CastType::Float => Shape::Float(def.pos.clone()),
                CastType::Bool => Shape::Boolean(def.pos.clone()),
            },
            Expression::Import(def) => Shape::Import(ImportShape::Unresolved(PositionedItem::new(
                def.path.fragment.clone(),
                def.path.pos.clone(),
            ))),
            Expression::Binary(def) => {
                let left_shape = def.left.derive_shape(symbol_table);
                let right_shape = def.right.derive_shape(symbol_table);
                left_shape.narrow(&right_shape, symbol_table)
            }
            Expression::Copy(def) => derive_copy_shape(def, symbol_table),
            Expression::Include(def) => derive_include_shape(def),
            Expression::Call(_) => todo!(),
            Expression::Func(def) => def.derive_shape(symbol_table),
            Expression::Select(_) => todo!(),
            Expression::FuncOp(_) => todo!(),
            Expression::Module(_) => todo!(),
            Expression::Fail(_) => todo!(),
            Expression::Debug(_) => todo!(),
        }
    }
}

impl DeriveShape for Value {
    fn derive_shape(&self, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
        match self {
            Value::Empty(p) => Shape::Narrowed(NarrowedShape::new_with_pos(vec![], p.clone())),
            Value::Boolean(p) => Shape::Boolean(p.pos.clone()),
            Value::Int(p) => Shape::Int(p.pos.clone()),
            Value::Float(p) => Shape::Float(p.pos.clone()),
            Value::Str(p) => Shape::Str(p.pos.clone()),
            Value::Symbol(p) => {
                if let Some(s) = symbol_table.get(&p.val) {
                    s.clone()
                } else {
                    Shape::Hole(p.clone())
                }
            }
            Value::Tuple(flds) => {
                let mut field_shapes = Vec::new();
                for &(ref tok, ref expr) in &flds.val {
                    field_shapes.push((tok.clone(), expr.derive_shape(symbol_table)));
                }
                Shape::Tuple(PositionedItem::new(field_shapes, flds.pos.clone()))
            }
            Value::List(flds) => {
                let mut field_shapes = Vec::new();
                for f in &flds.elems {
                    field_shapes.push(f.derive_shape(symbol_table));
                }
                Shape::List(NarrowedShape::new_with_pos(field_shapes, flds.pos.clone()))
            }
        }
    }
}

pub struct Checker {
    symbol_table: BTreeMap<Rc<str>, Shape>,
    err_stack: Vec<BuildError>,
    shape_stack: Vec<Shape>,
}

// TODO(jwall): I am beginning to suspect that derive_shape should be a Trait.
// It would allow me to specify the contract a little more specifically now that
// I'm basically implementing the method all over the place.

// TODO(jwall): The symbol table contract also needs to be fleshed out a little better.
// I need to acccount for scopes syntactic scopes a bit. packages, functions and modules all are a
// factor.

impl Checker {
    pub fn new() -> Self {
        return Self {
            symbol_table: BTreeMap::new(),
            err_stack: Vec::new(),
            shape_stack: Vec::new(),
        };
    }

    pub fn with_symbol_table(mut self, symbol_table: BTreeMap<Rc<str>, Shape>) -> Self {
        self.symbol_table = symbol_table;
        self
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
        match val {
            Value::Empty(p) => self
                .shape_stack
                .push(Shape::Narrowed(NarrowedShape::new_with_pos(
                    vec![],
                    p.clone(),
                ))),
            Value::Boolean(p) => self.shape_stack.push(Shape::Boolean(p.pos.clone())),
            Value::Int(p) => self.shape_stack.push(Shape::Int(p.pos.clone())),
            Value::Float(p) => self.shape_stack.push(Shape::Float(p.pos.clone())),
            Value::Str(p) => self.shape_stack.push(Shape::Str(p.pos.clone())),
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
