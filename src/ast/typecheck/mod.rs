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
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::rc::Rc;

use crate::ast::walk::{Visitor, Walker};
use crate::ast::{
    Expression, FailDef, FuncShapeDef, ImportDef, IncludeDef, Shape, Statement, Value,
};
use crate::error::{BuildError, ErrorType};
use crate::iter::OffsetStrIter;
use crate::parse::parse;

use super::{
    BinaryExprType, BinaryOpDef, CallDef, CastType, CopyDef, FuncDef, FuncOpDef, ImportShape,
    MapFilterOpDef, ModuleDef, ModuleShape, NarrowedShape, NarrowingShape, NotDef, Position,
    PositionedItem, ReduceOpDef, SelectDef, TupleShape,
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
            .map(|(sym, constraint)| {
                let shape = if let Some(c) = constraint {
                    c.derive_shape(symbol_table)
                } else {
                    Shape::Hole(sym.clone())
                };
                (sym.val.clone(), shape)
            })
            .collect::<BTreeMap<Rc<str>, Shape>>();
        sym_table.append(&mut symbol_table.clone());
        // 2. Then determine the shapes of those symbols in our expression.
        let shape = self.fields.derive_shape(&mut sym_table);
        // 3. Finally determine what the return shape can be.
        // only include the closed over shapes.
        // Capture declaration order before collecting into BTreeMap (which sorts alphabetically).
        let arg_order: Vec<Rc<str>> = self
            .argdefs
            .iter()
            .map(|(sym, _)| sym.val.clone())
            .collect();
        let table = self
            .argdefs
            .iter()
            .map(|(sym, _constraint)| {
                (
                    sym.val.clone(),
                    sym_table
                        .get(&sym.val)
                        .unwrap()
                        .clone()
                        .with_pos(sym.pos.clone()),
                )
            })
            .collect::<BTreeMap<Rc<str>, Shape>>();
        Shape::Func(FuncShapeDef {
            args: table,
            arg_order,
            ret: shape.with_pos(self.pos.clone()).into(),
        })
    }
}

impl DeriveShape for ModuleDef {
    fn derive_shape(&self, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
        let mut sym_table: BTreeMap<Rc<str>, Shape> = BTreeMap::new();
        let mod_key: Rc<str> = "mod".into();
        let mut mod_shape: TupleShape = Vec::new();
        if let Some(pos) = self.arg_set.first().map(|(t, _, _)| t.pos.clone()) {
            mod_shape = self
                .arg_set
                .iter()
                .map(|(tok, _constraint, expr)| {
                    let default_shape = expr.derive_shape(symbol_table);
                    let shape = match &default_shape {
                        // Empty tuple default means "any tuple" - treat as unconstrained
                        Shape::Tuple(pi) if pi.val.is_empty() => Shape::Narrowed(NarrowedShape {
                            pos: tok.pos.clone(),
                            types: NarrowingShape::Any,
                        }),
                        _ => default_shape,
                    };
                    (tok.into(), shape)
                })
                .collect();
            sym_table.insert(
                mod_key.clone(),
                Shape::Tuple(PositionedItem::new(mod_shape.clone(), pos)),
            );
        }
        // TODO(jwall): We should modify the shape_list when we can continue narrowing a type.
        let mut checker = Checker::new().with_symbol_table(sym_table);
        checker.walk_statement_list(self.statements.clone().iter_mut().collect());
        // Derive the out expression shape directly instead of using walk_expression,
        // since DeriveShape already recurses through expression children.
        let mut ret = if let Some(out_expr) = &self.out_expr {
            Box::new(out_expr.derive_shape(&mut checker.symbol_table))
        } else {
            // No explicit out expression: the module implicitly returns all its let
            // bindings as a tuple (excluding the "mod" arg parameter).
            let pos = self.pos.clone();
            let tuple_fields: TupleShape = checker
                .symbol_table
                .iter()
                .filter(|(k, _)| k.as_ref() != "mod")
                .map(|(k, v)| (PositionedItem::new(k.clone(), pos.clone()), v.clone()))
                .collect();
            if tuple_fields.is_empty() {
                Box::new(
                    checker
                        .pop_shape()
                        .unwrap_or(Shape::Narrowed(NarrowedShape {
                            pos: self.pos.clone(),
                            types: NarrowingShape::Any,
                        })),
                )
            } else {
                Box::new(Shape::Tuple(PositionedItem::new(tuple_fields, pos)))
            }
        };
        // Enforce output constraint if present.
        // If narrowing fails, return the TypeErr directly so it surfaces to the caller.
        if let Some(ref constraint_expr) = self.out_constraint {
            let constraint_shape = constraint_expr.derive_shape(symbol_table);
            let narrowed = ret.narrow(&constraint_shape, symbol_table);
            if let Shape::TypeErr(_, _) = &narrowed {
                return narrowed;
            }
            ret = Box::new(narrowed);
        }
        // Read back narrowed arg shapes from the checker's symbol table
        if let Some(Shape::Tuple(mod_tuple)) = checker.symbol_table.get(&mod_key) {
            mod_shape = mod_tuple.val.clone();
        }
        Shape::Module(ModuleShape {
            items: mod_shape,
            ret,
        })
    }
}

impl DeriveShape for SelectDef {
    fn derive_shape(&self, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
        let SelectDef {
            val: _,
            default: _,
            tuple,
            pos: _,
        } = self;
        let mut narrowed_shape =
            NarrowedShape::new_with_pos(Vec::with_capacity(tuple.len()), self.pos.clone());
        for (_, _constraint, expr) in tuple {
            let shape = expr.derive_shape(symbol_table);
            narrowed_shape.merge_in_shape(shape, symbol_table);
        }
        Shape::Narrowed(narrowed_shape)
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
    match &shape {
        Shape::Boolean(_) => {
            return Shape::Boolean(def.pos.clone());
        }
        Shape::Hole(_) => {
            return Shape::Boolean(def.pos.clone());
        }
        Shape::Narrowed(NarrowedShape {
            pos: _,
            types: NarrowingShape::Any,
        }) => {
            return Shape::Boolean(def.pos.clone());
        }
        Shape::Narrowed(NarrowedShape {
            pos: _,
            types: NarrowingShape::Narrowed(shape_list),
        }) => {
            for s in shape_list.iter() {
                if let Shape::Boolean(_) = s {
                    return Shape::Boolean(def.pos.clone());
                }
            }
        }
        _ => {
            // noop
        }
    }
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
        | Shape::Func(_)
        | Shape::ConstraintRef(_) => Shape::TypeErr(
            def.pos.clone(),
            format!("Not a Copyable type {}", base_shape.type_name()),
        ),
        // This is an interesting one. Do we assume tuple or module here?
        Shape::Hole(pi) => Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::Tuple(PositionedItem::new(vec![], pi.pos.clone())),
                Shape::Module(ModuleShape {
                    items: vec![],
                    ret: Box::new(Shape::Narrowed(NarrowedShape::new_with_pos(
                        vec![],
                        pi.pos.clone(),
                    ))),
                }),
                Shape::Import(ImportShape::Unresolved(pi.clone())),
            ],
            pi.pos.clone(),
        )),
        Shape::Narrowed(NarrowedShape {
            pos: _,
            types: NarrowingShape::Any,
        }) => Shape::Narrowed(NarrowedShape::new_with_pos(
            vec![
                Shape::Tuple(PositionedItem {
                    pos: def.pos.clone(),
                    val: Vec::new(),
                }),
                Shape::Module(ModuleShape {
                    items: vec![],
                    ret: Box::new(Shape::Narrowed(NarrowedShape {
                        pos: def.pos.clone(),
                        types: NarrowingShape::Any,
                    })),
                }),
            ],
            def.pos.clone(),
        )),
        Shape::Narrowed(NarrowedShape {
            pos: _,
            types: NarrowingShape::Narrowed(potentials),
        }) => {
            // 1. Do the possible shapes include tuple, module, or import?
            let filtered = potentials
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
                .map(|(tok, _constraint, expr)| {
                    (tok.fragment.clone(), expr.derive_shape(symbol_table))
                })
                .collect::<BTreeMap<Rc<str>, Shape>>();
            // 1. Do our copyable fields have the right names and shapes based on mdef.items.
            for (sym, shape) in mdef.items.iter() {
                if let Some(s) = arg_fields.get(&sym.val) {
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
                    .map(|(tok, _constraint, expr)| (tok.into(), expr.derive_shape(symbol_table))),
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
                    .map(|(tok, _constraint, expr)| (tok.into(), expr.derive_shape(symbol_table))),
            );
            Shape::Tuple(PositionedItem::new(base_fields, def.pos.clone()))
        }
    }
}

fn derive_call_shape(def: &CallDef, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
    let func_shape = def.funcref.derive_shape(symbol_table);
    match &func_shape {
        Shape::Func(fdef) => {
            // Check arg count
            if fdef.args.len() != def.arglist.len() {
                return Shape::TypeErr(
                    def.pos.clone(),
                    format!(
                        "Function expects {} arguments but got {}",
                        fdef.args.len(),
                        def.arglist.len()
                    ),
                );
            }
            // Check each positional argument against its declared type.
            // arg_order preserves declaration order so we can zip with the positional arglist.
            for (arg_name, arg_expr) in fdef.arg_order.iter().zip(def.arglist.iter()) {
                let actual_shape = arg_expr.derive_shape(symbol_table);
                if let Some(declared_shape) = fdef.args.get(arg_name) {
                    if let Shape::TypeErr(pos, msg) =
                        declared_shape.narrow(&actual_shape, symbol_table)
                    {
                        return Shape::TypeErr(pos, msg);
                    }
                }
            }
            // Return the function's return type
            fdef.ret.as_ref().clone()
        }
        Shape::Hole(_) => {
            // Unknown function, derive arg shapes but return Any
            for arg_expr in def.arglist.iter() {
                arg_expr.derive_shape(symbol_table);
            }
            Shape::Narrowed(NarrowedShape {
                pos: def.pos.clone(),
                types: NarrowingShape::Any,
            })
        }
        Shape::Narrowed(nshape) => {
            match &nshape.types {
                NarrowingShape::Any => {
                    // Could be a function, derive arg shapes but return Any
                    for arg_expr in def.arglist.iter() {
                        arg_expr.derive_shape(symbol_table);
                    }
                    Shape::Narrowed(NarrowedShape {
                        pos: def.pos.clone(),
                        types: NarrowingShape::Any,
                    })
                }
                NarrowingShape::Narrowed(types) => {
                    // Filter to Func types and check each
                    let func_types: Vec<&FuncShapeDef> = types
                        .iter()
                        .filter_map(|t| {
                            if let Shape::Func(fdef) = t {
                                Some(fdef)
                            } else {
                                None
                            }
                        })
                        .collect();
                    if func_types.is_empty() {
                        return Shape::TypeErr(def.pos.clone(), "Not a callable type".to_owned());
                    }
                    // Derive arg shapes
                    let arg_shapes: Vec<Shape> = def
                        .arglist
                        .iter()
                        .map(|e| e.derive_shape(symbol_table))
                        .collect();
                    // Collect possible return types
                    let mut ret_shapes = Vec::new();
                    for fdef in func_types {
                        if fdef.args.len() == arg_shapes.len() {
                            ret_shapes.push(fdef.ret.as_ref().clone());
                        }
                    }
                    if ret_shapes.is_empty() {
                        Shape::TypeErr(
                            def.pos.clone(),
                            "No callable candidate matches argument count".to_owned(),
                        )
                    } else if ret_shapes.len() == 1 {
                        ret_shapes.pop().unwrap()
                    } else {
                        Shape::Narrowed(NarrowedShape::new_with_pos(ret_shapes, def.pos.clone()))
                    }
                }
            }
        }
        _ => Shape::TypeErr(
            def.pos.clone(),
            format!("Not a callable type: {}", func_shape.type_name()),
        ),
    }
}

fn derive_func_op_shape(def: &FuncOpDef, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
    match def {
        FuncOpDef::Map(MapFilterOpDef { func, target, pos }) => {
            let target_shape = target.derive_shape(symbol_table);
            let func_shape = func.derive_shape(symbol_table);
            // target must be a list
            match &target_shape {
                Shape::List(_) | Shape::Hole(_) => {}
                Shape::Narrowed(NarrowedShape {
                    types: NarrowingShape::Any,
                    ..
                }) => {}
                _ => {
                    return Shape::TypeErr(
                        pos.clone(),
                        format!(
                            "map target must be a list, got {}",
                            target_shape.type_name()
                        ),
                    );
                }
            }
            // Return type is List(func.ret)
            match &func_shape {
                Shape::Func(fdef) => Shape::List(NarrowedShape::new_with_pos(
                    vec![fdef.ret.as_ref().clone()],
                    pos.clone(),
                )),
                _ => Shape::List(NarrowedShape {
                    pos: pos.clone(),
                    types: NarrowingShape::Any,
                }),
            }
        }
        FuncOpDef::Filter(MapFilterOpDef { func, target, pos }) => {
            let target_shape = target.derive_shape(symbol_table);
            let _func_shape = func.derive_shape(symbol_table);
            // target must be a list, return type is same list type
            match &target_shape {
                Shape::List(_) => target_shape,
                Shape::Hole(_) => Shape::List(NarrowedShape {
                    pos: pos.clone(),
                    types: NarrowingShape::Any,
                }),
                Shape::Narrowed(NarrowedShape {
                    types: NarrowingShape::Any,
                    ..
                }) => Shape::List(NarrowedShape {
                    pos: pos.clone(),
                    types: NarrowingShape::Any,
                }),
                _ => Shape::TypeErr(
                    pos.clone(),
                    format!(
                        "filter target must be a list, got {}",
                        target_shape.type_name()
                    ),
                ),
            }
        }
        FuncOpDef::Reduce(ReduceOpDef {
            func,
            acc,
            target,
            pos,
        }) => {
            let target_shape = target.derive_shape(symbol_table);
            let acc_shape = acc.derive_shape(symbol_table);
            let func_shape = func.derive_shape(symbol_table);
            // target must be a list
            match &target_shape {
                Shape::List(_) | Shape::Hole(_) => {}
                Shape::Narrowed(NarrowedShape {
                    types: NarrowingShape::Any,
                    ..
                }) => {}
                _ => {
                    return Shape::TypeErr(
                        pos.clone(),
                        format!(
                            "reduce target must be a list, got {}",
                            target_shape.type_name()
                        ),
                    );
                }
            }
            // Return type is acc's shape narrowed against func.ret
            match &func_shape {
                Shape::Func(fdef) => {
                    let narrowed = acc_shape.narrow(&fdef.ret, symbol_table);
                    match narrowed {
                        Shape::TypeErr(_, _) => acc_shape,
                        other => other,
                    }
                }
                _ => acc_shape,
            }
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
                if def.kind == BinaryExprType::DOT {
                    let shape =
                        derive_dot_expression(&def.pos, &left_shape, &def.right, symbol_table);
                    // Update the symbol table with the inferred left shape
                    if let Expression::Simple(Value::Symbol(pi)) = def.left.as_ref() {
                        if let Shape::TypeErr(_, _) = &shape {
                            // Don't update symbol table on type errors
                        } else {
                            if let Shape::Hole(_) = &left_shape {
                                let inferred = infer_container_shape_from_dot(
                                    &left_shape,
                                    &def.right,
                                    &shape,
                                    &def.pos,
                                    symbol_table,
                                );
                                symbol_table.insert(pi.val.clone(), inferred);
                            }
                        }
                    }
                    shape
                } else {
                    let right_shape = def.right.derive_shape(symbol_table);
                    match &def.kind {
                        // Comparison operators return Boolean
                        BinaryExprType::Equal
                        | BinaryExprType::NotEqual
                        | BinaryExprType::GT
                        | BinaryExprType::LT
                        | BinaryExprType::GTEqual
                        | BinaryExprType::LTEqual
                        | BinaryExprType::REMatch
                        | BinaryExprType::NotREMatch
                        | BinaryExprType::IN
                        | BinaryExprType::IS => {
                            // Check operands are compatible (narrow checks this)
                            // but always return Boolean
                            Shape::Boolean(def.pos.clone())
                        }
                        // Boolean operators require boolean operands
                        BinaryExprType::AND | BinaryExprType::OR => {
                            // Narrow to check compatibility
                            let narrowed = left_shape.narrow(&right_shape, symbol_table);
                            if let Shape::TypeErr(_, _) = &narrowed {
                                narrowed
                            } else {
                                Shape::Boolean(def.pos.clone())
                            }
                        }
                        // Math operators narrow types
                        _ => left_shape.narrow(&right_shape, symbol_table),
                    }
                }
            }
            Expression::Copy(def) => derive_copy_shape(def, symbol_table),
            Expression::Include(def) => derive_include_shape(def),
            Expression::Call(def) => derive_call_shape(def, symbol_table),
            Expression::Func(def) => def.derive_shape(symbol_table),
            Expression::Select(def) => def.derive_shape(symbol_table),
            Expression::FuncOp(def) => derive_func_op_shape(def, symbol_table),
            Expression::Module(def) => def.derive_shape(symbol_table),
            Expression::Fail(def) => {
                let msg_shape = def.message.derive_shape(symbol_table);
                match &msg_shape {
                    Shape::Str(_) | Shape::Hole(_) => {}
                    Shape::Narrowed(NarrowedShape {
                        types: NarrowingShape::Any,
                        ..
                    }) => {}
                    _ => {
                        return Shape::TypeErr(
                            def.pos.clone(),
                            format!(
                                "fail message must be a string, got {}",
                                msg_shape.type_name()
                            ),
                        );
                    }
                }
                // fail never produces a value - any type is compatible
                Shape::Narrowed(NarrowedShape {
                    pos: def.pos.clone(),
                    types: NarrowingShape::Any,
                })
            }
            Expression::Debug(def) => {
                // Debug is transparent - return the shape of the inner expression
                def.expr.derive_shape(symbol_table)
            }
            Expression::Convert(def) => {
                // Convert always produces a string
                Shape::Str(def.pos.clone())
            }
            Expression::Constraint(def) => {
                let mut shapes = Vec::new();
                for arm in &def.arms {
                    match arm {
                        crate::ast::ConstraintArm::Range(rdef) => {
                            // Derive shape from whichever bound is present
                            let bound_shape = if let Some(ref start) = rdef.start {
                                start.derive_shape(symbol_table)
                            } else if let Some(ref end) = rdef.end {
                                end.derive_shape(symbol_table)
                            } else {
                                return Shape::TypeErr(
                                    rdef.pos.clone(),
                                    "Range constraint must have at least one bound".to_string(),
                                );
                            };
                            match &bound_shape {
                                Shape::Int(_) | Shape::Float(_) => shapes.push(bound_shape),
                                _ => {
                                    return Shape::TypeErr(
                                        rdef.pos.clone(),
                                        "Range constraint bounds must be numeric".to_string(),
                                    )
                                }
                            }
                        }
                        crate::ast::ConstraintArm::Shape(expr) => {
                            shapes.push(expr.derive_shape(symbol_table));
                        }
                    }
                }
                if shapes.len() == 1 {
                    shapes.pop().unwrap()
                } else {
                    Shape::Narrowed(NarrowedShape::new_with_pos(shapes, def.pos.clone()))
                }
            }
        }
    }
}

/// Infer the container shape from a dot expression.
/// When we have `hole.field`, we need to infer what the hole must be
/// (a tuple with that field).
fn infer_container_shape_from_dot(
    left_shape: &Shape,
    right_expr: &Expression,
    _field_shape: &Shape,
    _pos: &Position,
    _symbol_table: &mut BTreeMap<Rc<str>, Shape>,
) -> Shape {
    match (left_shape, right_expr) {
        (Shape::Hole(hole_pi), Expression::Simple(Value::Symbol(pi)))
        | (Shape::Hole(hole_pi), Expression::Simple(Value::Str(pi))) => {
            Shape::Tuple(PositionedItem::new(
                vec![(
                    PositionedItem::new(pi.val.clone(), pi.pos.clone()),
                    Shape::Narrowed(NarrowedShape {
                        pos: hole_pi.pos.clone(),
                        types: NarrowingShape::Any,
                    }),
                )],
                hole_pi.pos.clone(),
            ))
        }
        (Shape::Hole(hole_pi), Expression::Simple(Value::Int(_))) => Shape::List(NarrowedShape {
            pos: hole_pi.pos.clone(),
            types: NarrowingShape::Any,
        }),
        _ => left_shape.clone(),
    }
}

fn derive_dot_expression(
    pos: &Position,
    left_shape: &Shape,
    right_expr: &Expression,
    symbol_table: &mut BTreeMap<Rc<str>, Shape>,
) -> Shape {
    // left shape is symbol or tuple or array.
    // right shape is symbol, str, number, grouped expression
    match (left_shape, right_expr) {
        // ===== Recursive cases: right side is itself a DOT chain =====
        (
            Shape::Tuple(tshape),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::DOT,
                left,
                right,
                pos,
            }),
        ) => {
            // left is the next accessor in the chain
            let accessor_shape = left.derive_shape(symbol_table);
            // Resolve what field the accessor refers to in this tuple
            let resolved = resolve_tuple_field(tshape, &accessor_shape, left, pos);
            match resolved {
                Shape::TypeErr(_, _) => resolved,
                field_shape => {
                    // Recurse with the field's shape as the new left
                    derive_dot_expression(pos, &field_shape, right, symbol_table)
                }
            }
        }
        (
            Shape::List(lshape),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::DOT,
                left,
                right,
                pos,
            }),
        ) => {
            let accessor_shape = left.derive_shape(symbol_table);
            match &accessor_shape {
                Shape::Int(_) => {
                    // Indexing a list gives us the element type
                    let elem_shape = Shape::Narrowed(lshape.clone());
                    derive_dot_expression(pos, &elem_shape, right, symbol_table)
                }
                Shape::Hole(_pi) => {
                    // Unknown accessor on a list - could be int index
                    let elem_shape = Shape::Narrowed(lshape.clone());
                    derive_dot_expression(pos, &elem_shape, right, symbol_table)
                }
                _ => Shape::TypeErr(
                    pos.clone(),
                    format!(
                        "Lists can only be indexed by integer, got {}",
                        accessor_shape.type_name()
                    ),
                ),
            }
        }
        (
            Shape::Narrowed(narrowed_shape),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::DOT,
                left: _,
                right: _,
                pos,
            }),
        ) => {
            // For narrowed types with DOT chains, try each candidate
            match &narrowed_shape.types {
                NarrowingShape::Any => {
                    // Unconstrained - result is also unconstrained
                    Shape::Narrowed(NarrowedShape {
                        pos: pos.clone(),
                        types: NarrowingShape::Any,
                    })
                }
                NarrowingShape::Narrowed(types) => {
                    // Try each candidate type
                    let mut results = Vec::new();
                    for t in types {
                        let inner_result = derive_dot_expression(pos, t, right_expr, symbol_table);
                        if let Shape::TypeErr(_, _) = &inner_result {
                            // Skip incompatible candidates
                        } else {
                            results.push(inner_result);
                        }
                    }
                    if results.is_empty() {
                        Shape::TypeErr(
                            pos.clone(),
                            "No candidate type is compatible with field access".to_owned(),
                        )
                    } else if results.len() == 1 {
                        results.pop().unwrap()
                    } else {
                        Shape::Narrowed(NarrowedShape::new_with_pos(results, pos.clone()))
                    }
                }
            }
        }
        (
            Shape::Hole(_hole_pi),
            Expression::Binary(BinaryOpDef {
                kind: BinaryExprType::DOT,
                left,
                right,
                pos,
            }),
        ) => {
            // We don't know what the hole is, but it's being accessed with a dot chain.
            // The accessor tells us something about the hole's type.
            let accessor_shape = left.derive_shape(symbol_table);
            match &accessor_shape {
                // Symbol/Str accessor means it could be a tuple
                Shape::Hole(_) | Shape::Str(_) => {
                    // Infer as tuple with unknown field, then continue chain
                    let field_shape = Shape::Narrowed(NarrowedShape {
                        pos: pos.clone(),
                        types: NarrowingShape::Any,
                    });
                    derive_dot_expression(pos, &field_shape, right, symbol_table)
                }
                Shape::Int(_) => {
                    // Infer as list, element is unknown
                    let elem_shape = Shape::Narrowed(NarrowedShape {
                        pos: pos.clone(),
                        types: NarrowingShape::Any,
                    });
                    derive_dot_expression(pos, &elem_shape, right, symbol_table)
                }
                _ => Shape::Narrowed(NarrowedShape {
                    pos: pos.clone(),
                    types: NarrowingShape::Any,
                }),
            }
        }

        // ===== Terminal cases: right side is a simple value =====

        // Tuple field access by name
        (Shape::Tuple(tshape), Expression::Simple(Value::Str(pi)))
        | (Shape::Tuple(tshape), Expression::Simple(Value::Symbol(pi))) => {
            for (field_name, field_shape) in tshape.val.iter() {
                if field_name.val == pi.val {
                    return field_shape.clone();
                }
            }
            Shape::TypeErr(
                pi.pos.clone(),
                format!("Field '{}' not found in tuple", pi.val),
            )
        }

        // Tuple indexed by int - type error
        (Shape::Tuple(_), Expression::Simple(Value::Int(pi))) => Shape::TypeErr(
            pi.pos.clone(),
            "Tuples cannot be indexed by integer".to_owned(),
        ),

        // List indexed by int - return element shape
        (Shape::List(lshape), Expression::Simple(Value::Int(_pi))) => {
            Shape::Narrowed(lshape.clone())
        }

        // List accessed by name - type error
        (Shape::List(_), Expression::Simple(Value::Symbol(pi)))
        | (Shape::List(_), Expression::Simple(Value::Str(pi))) => Shape::TypeErr(
            pi.pos.clone(),
            "Lists cannot be accessed by field name".to_owned(),
        ),

        // Hole accessed by name - infer as tuple with that field
        (Shape::Hole(hole_pi), Expression::Simple(Value::Symbol(_pi)))
        | (Shape::Hole(hole_pi), Expression::Simple(Value::Str(_pi))) => {
            Shape::Narrowed(NarrowedShape {
                pos: hole_pi.pos.clone(),
                types: NarrowingShape::Any,
            })
        }

        // Hole accessed by int - infer as list
        (Shape::Hole(hole_pi), Expression::Simple(Value::Int(_pi))) => {
            Shape::Narrowed(NarrowedShape {
                pos: hole_pi.pos.clone(),
                types: NarrowingShape::Any,
            })
        }

        // Narrowed accessed by name - filter candidates
        (Shape::Narrowed(nshape), Expression::Simple(Value::Symbol(pi)))
        | (Shape::Narrowed(nshape), Expression::Simple(Value::Str(pi))) => {
            match &nshape.types {
                NarrowingShape::Any => Shape::Narrowed(NarrowedShape {
                    pos: pi.pos.clone(),
                    types: NarrowingShape::Any,
                }),
                NarrowingShape::Narrowed(types) if types.is_empty() => {
                    // Empty candidates = unconstrained, field access is allowed
                    Shape::Narrowed(NarrowedShape {
                        pos: pi.pos.clone(),
                        types: NarrowingShape::Any,
                    })
                }
                NarrowingShape::Narrowed(types) => {
                    let mut results = Vec::new();
                    for t in types {
                        match t {
                            Shape::Tuple(tshape) => {
                                for (field_name, field_shape) in tshape.val.iter() {
                                    if field_name.val == pi.val {
                                        results.push(field_shape.clone());
                                    }
                                }
                            }
                            Shape::Hole(_) => {
                                results.push(Shape::Narrowed(NarrowedShape {
                                    pos: pi.pos.clone(),
                                    types: NarrowingShape::Any,
                                }));
                            }
                            _ => { /* not field-accessible, skip */ }
                        }
                    }
                    if results.is_empty() {
                        Shape::TypeErr(
                            pi.pos.clone(),
                            format!("No candidate type has field '{}'", pi.val),
                        )
                    } else if results.len() == 1 {
                        results.pop().unwrap()
                    } else {
                        Shape::Narrowed(NarrowedShape::new_with_pos(results, pi.pos.clone()))
                    }
                }
            }
        }

        // Narrowed accessed by int - filter to list-like candidates
        (Shape::Narrowed(nshape), Expression::Simple(Value::Int(pi))) => {
            match &nshape.types {
                NarrowingShape::Any => Shape::Narrowed(NarrowedShape {
                    pos: pi.pos.clone(),
                    types: NarrowingShape::Any,
                }),
                NarrowingShape::Narrowed(types) if types.is_empty() => {
                    // Empty candidates = unconstrained
                    Shape::Narrowed(NarrowedShape {
                        pos: pi.pos.clone(),
                        types: NarrowingShape::Any,
                    })
                }
                NarrowingShape::Narrowed(types) => {
                    let mut results = Vec::new();
                    for t in types {
                        match t {
                            Shape::List(lshape) => {
                                results.push(Shape::Narrowed(lshape.clone()));
                            }
                            Shape::Hole(_) => {
                                results.push(Shape::Narrowed(NarrowedShape {
                                    pos: pi.pos.clone(),
                                    types: NarrowingShape::Any,
                                }));
                            }
                            _ => { /* not int-indexable, skip */ }
                        }
                    }
                    if results.is_empty() {
                        Shape::TypeErr(
                            pi.pos.clone(),
                            "No candidate type supports integer indexing".to_owned(),
                        )
                    } else if results.len() == 1 {
                        results.pop().unwrap()
                    } else {
                        Shape::Narrowed(NarrowedShape::new_with_pos(results, pi.pos.clone()))
                    }
                }
            }
        }

        // Grouped expression - unwrap and recurse
        (_, Expression::Grouped(expr, _)) => {
            derive_dot_expression(pos, left_shape, expr.as_ref(), symbol_table)
        }

        // Resolved import - treat as a tuple of exported bindings
        (Shape::Import(ImportShape::Resolved(_, tuple_fields)), _) => {
            let tshape = PositionedItem::new(tuple_fields.clone(), pos.clone());
            derive_dot_expression(pos, &Shape::Tuple(tshape), right_expr, symbol_table)
        }

        // Unresolved import - allow any field access
        (Shape::Import(ImportShape::Unresolved(_)), _) => Shape::Narrowed(NarrowedShape {
            pos: pos.clone(),
            types: NarrowingShape::Any,
        }),

        // TypeErr propagation
        (Shape::TypeErr(_, _), _) => left_shape.clone(),

        // Everything else is invalid
        (_, _) => Shape::TypeErr(pos.clone(), "Invalid field selector".to_owned()),
    }
}

/// Helper to resolve a field in a tuple shape given an accessor
fn resolve_tuple_field(
    tshape: &PositionedItem<TupleShape>,
    _accessor_shape: &Shape,
    accessor_expr: &Expression,
    pos: &Position,
) -> Shape {
    match accessor_expr {
        Expression::Simple(Value::Symbol(pi)) | Expression::Simple(Value::Str(pi)) => {
            for (field_name, field_shape) in tshape.val.iter() {
                if field_name.val == pi.val {
                    return field_shape.clone();
                }
            }
            Shape::TypeErr(
                pos.clone(),
                format!("Field '{}' not found in tuple", pi.val),
            )
        }
        _ => {
            // For computed field access, we can't know the field at type-check time
            Shape::Narrowed(NarrowedShape {
                pos: pos.clone(),
                types: NarrowingShape::Any,
            })
        }
    }
}

impl DeriveShape for Value {
    fn derive_shape(&self, symbol_table: &mut BTreeMap<Rc<str>, Shape>) -> Shape {
        match self {
            Value::Empty(p) => Shape::Narrowed(NarrowedShape {
                pos: p.clone(),
                types: NarrowingShape::Any,
            }),
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
            Value::Tuple(flds) => derive_field_list_shape(&flds.val, &flds.pos, symbol_table),
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

fn derive_field_list_shape(
    flds: &Vec<(super::Token, Option<Expression>, Expression)>,
    pos: &Position,
    symbol_table: &mut BTreeMap<Rc<str>, Shape>,
) -> Shape {
    let mut field_shapes = Vec::new();
    for (tok, constraint, expr) in flds {
        let value_shape = expr.derive_shape(symbol_table);
        let shape = if let Some(c) = constraint {
            let constraint_shape = c.derive_shape(symbol_table);
            let narrowed = value_shape.narrow(&constraint_shape, symbol_table);
            if let Shape::TypeErr(_, _) = &narrowed {
                return narrowed;
            }
            narrowed
        } else {
            value_shape
        };
        field_shapes.push((
            PositionedItem::new(tok.fragment.clone(), tok.pos.clone()),
            shape,
        ));
    }
    Shape::Tuple(PositionedItem::new(field_shapes, pos.clone()))
}

pub struct Checker {
    symbol_table: BTreeMap<Rc<str>, Shape>,
    err_stack: Vec<BuildError>,
    shape_stack: Vec<Shape>,
    // Tracks nesting into Module expressions. When > 0, visit_statement
    // is skipped because inner statements are handled by ModuleDef::derive_shape
    // with its own Checker.
    nested_depth: usize,
    // When false, type errors involving Empty/NULL values are suppressed.
    strict: bool,
    // Working directory for resolving import paths.
    working_dir: Option<PathBuf>,
    // Cache of already-resolved import shapes, shared across Checker instances.
    shape_cache: Rc<RefCell<BTreeMap<PathBuf, Shape>>>,
    // Stack of currently-being-resolved imports for cycle detection.
    import_stack: Vec<PathBuf>,
}

impl Default for Checker {
    fn default() -> Self {
        Self::new()
    }
}

impl Checker {
    pub fn new() -> Self {
        Self {
            symbol_table: BTreeMap::new(),
            err_stack: Vec::new(),
            shape_stack: Vec::new(),
            nested_depth: 0,
            strict: true,
            working_dir: None,
            shape_cache: Rc::new(RefCell::new(BTreeMap::new())),
            import_stack: Vec::new(),
        }
    }

    pub fn with_strict(mut self, strict: bool) -> Self {
        self.strict = strict;
        self
    }

    pub fn with_working_dir<P: Into<PathBuf>>(mut self, dir: P) -> Self {
        self.working_dir = Some(dir.into());
        self
    }

    pub fn with_shape_cache(mut self, cache: Rc<RefCell<BTreeMap<PathBuf, Shape>>>) -> Self {
        self.shape_cache = cache;
        self
    }

    pub fn with_import_stack(mut self, stack: Vec<PathBuf>) -> Self {
        self.import_stack = stack;
        self
    }

    pub fn with_symbol_table(mut self, symbol_table: BTreeMap<Rc<str>, Shape>) -> Self {
        self.symbol_table = symbol_table;
        self
    }

    pub fn pop_shape(&mut self) -> Option<Shape> {
        self.shape_stack.pop()
    }

    /// Check that a recursive constraint is constructible.
    /// A ConstraintRef to `name` is valid only inside a list or as one arm of
    /// an alternation that has at least one non-self-referential arm.
    /// Returns Some(Position) of the offending ref if invalid, None if ok.
    fn validate_recursive_constraint(name: &Rc<str>, shape: &Shape) -> Option<Position> {
        Self::check_constraint_ref(name, shape, false)
    }

    /// Recursively walk a shape checking ConstraintRef positions.
    /// `guarded` is true when we're inside a list or alternation with alternatives.
    fn check_constraint_ref(name: &Rc<str>, shape: &Shape, guarded: bool) -> Option<Position> {
        match shape {
            Shape::ConstraintRef(pi) if pi.val == *name => {
                if guarded {
                    None // Valid: inside a list or alternation
                } else {
                    Some(pi.pos.clone()) // Invalid: no base case
                }
            }
            Shape::ConstraintRef(_) => None, // Different constraint, not our concern
            Shape::Tuple(pi) => {
                // Check each field — fields are NOT guarded by the tuple
                for (_field_name, field_shape) in pi.val.iter() {
                    if let Some(pos) = Self::check_constraint_ref(name, field_shape, guarded) {
                        return Some(pos);
                    }
                }
                None
            }
            Shape::List(ns) => {
                // List elements are guarded — a list can be empty
                match &ns.types {
                    NarrowingShape::Narrowed(types) => {
                        for t in types {
                            if let Some(pos) = Self::check_constraint_ref(name, t, true) {
                                return Some(pos);
                            }
                        }
                    }
                    NarrowingShape::Any => {}
                }
                None
            }
            Shape::Narrowed(ns) => {
                // An alternation guards its arms IF there's at least one
                // arm that doesn't reference the constraint (a base case).
                match &ns.types {
                    NarrowingShape::Narrowed(types) => {
                        let has_base_case =
                            types.iter().any(|t| !Self::shape_contains_ref(name, t));
                        for t in types {
                            if let Some(pos) =
                                Self::check_constraint_ref(name, t, guarded || has_base_case)
                            {
                                return Some(pos);
                            }
                        }
                    }
                    NarrowingShape::Any => {}
                }
                None
            }
            // Primitive shapes and others — no recursion concerns
            _ => None,
        }
    }

    /// Returns true if a shape contains any ConstraintRef to `name`.
    fn shape_contains_ref(name: &Rc<str>, shape: &Shape) -> bool {
        match shape {
            Shape::ConstraintRef(pi) => pi.val == *name,
            Shape::Tuple(pi) => pi
                .val
                .iter()
                .any(|(_, s)| Self::shape_contains_ref(name, s)),
            Shape::List(ns) => match &ns.types {
                NarrowingShape::Narrowed(types) => {
                    types.iter().any(|t| Self::shape_contains_ref(name, t))
                }
                NarrowingShape::Any => false,
            },
            Shape::Narrowed(ns) => match &ns.types {
                NarrowingShape::Narrowed(types) => {
                    types.iter().any(|t| Self::shape_contains_ref(name, t))
                }
                NarrowingShape::Any => false,
            },
            _ => false,
        }
    }

    fn push_shape_or_err(&mut self, shape: Shape) {
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

    /// Resolve an import path to a Shape by reading, parsing, and type-checking
    /// the imported file. Returns `ImportShape::Resolved` with the exported tuple
    /// shape, or `ImportShape::Unresolved` if no working directory is set.
    fn resolve_import(&mut self, path: &str, pos: &Position) -> Shape {
        let working_dir = match &self.working_dir {
            Some(dir) => dir.clone(),
            None => {
                return Shape::Import(ImportShape::Unresolved(PositionedItem::new(
                    path.into(),
                    pos.clone(),
                )));
            }
        };

        let resolved_path = working_dir.join(path);

        // Check the cache first
        if let Some(cached) = self.shape_cache.borrow().get(&resolved_path) {
            return cached.clone();
        }

        // Check for import cycles
        if self.import_stack.contains(&resolved_path) {
            return Shape::TypeErr(
                pos.clone(),
                format!("Import cycle detected: {}", resolved_path.display()),
            );
        }

        // Read the file
        let contents = match std::fs::read_to_string(&resolved_path) {
            Ok(c) => c,
            Err(_) => {
                // File not found or unreadable — return unresolved rather than error,
                // since the file might be generated or available at runtime.
                return Shape::Import(ImportShape::Unresolved(PositionedItem::new(
                    path.into(),
                    pos.clone(),
                )));
            }
        };

        // Parse the file
        let iter = OffsetStrIter::new(&contents).with_src_file(&resolved_path);
        let mut stmts = match parse(iter, None) {
            Ok(stmts) => stmts,
            Err(_) => {
                return Shape::TypeErr(
                    pos.clone(),
                    format!("Failed to parse imported file: {}", resolved_path.display()),
                );
            }
        };

        // Type-check the imported file with a new Checker sharing the cache
        let import_dir = resolved_path.parent().map(|p| p.to_path_buf());
        let mut import_stack = self.import_stack.clone();
        import_stack.push(resolved_path.clone());

        let mut child_checker = Checker::new()
            .with_shape_cache(self.shape_cache.clone())
            .with_import_stack(import_stack)
            .with_strict(self.strict);
        if let Some(dir) = import_dir {
            child_checker = child_checker.with_working_dir(dir);
        }

        child_checker.walk_statement_list(stmts.iter_mut().collect());

        // Check for type errors in the imported file
        let symbol_table = match child_checker.result() {
            Ok(syms) => syms,
            Err(err) => {
                // Don't cache type errors — the file may be fixed and re-imported.
                return Shape::TypeErr(
                    err.pos.unwrap_or_else(|| pos.clone()),
                    format!(
                        "Type error in imported file {}: {}",
                        resolved_path.display(),
                        err.msg
                    ),
                );
            }
        };

        // Collect exported symbols as a tuple shape
        let tuple_fields: TupleShape = symbol_table
            .iter()
            .map(|(name, shape)| {
                (
                    PositionedItem::new(name.clone(), pos.clone()),
                    shape.clone(),
                )
            })
            .collect();

        let resolved = Shape::Import(ImportShape::Resolved(pos.clone(), tuple_fields));

        // Cache only successful results
        self.shape_cache
            .borrow_mut()
            .insert(resolved_path, resolved.clone());

        resolved
    }

    /// Returns the accumulated symbol table if type checking succeeded,
    /// or the first type error encountered (which is typically the root cause).
    pub fn result(self) -> Result<BTreeMap<Rc<str>, Shape>, BuildError> {
        if self.err_stack.is_empty() {
            Ok(self.symbol_table)
        } else {
            // Return the first error — it's usually the root cause.
            // Subsequent errors are often cascading failures.
            Err(self.err_stack.into_iter().next().unwrap())
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

    // visit_value is intentionally a noop — DeriveShape handles value shapes.

    fn visit_expression(&mut self, expr: &mut Expression) {
        // Track entry into Module expressions so we skip their inner statements
        // (they're handled by ModuleDef::derive_shape with a separate Checker).
        if matches!(expr, Expression::Module(_)) {
            self.nested_depth += 1;
        }
    }

    fn leave_expression(&mut self, expr: &Expression) {
        if matches!(expr, Expression::Module(_)) {
            self.nested_depth -= 1;
        }
    }

    fn visit_statement(&mut self, stmt: &mut Statement) {
        // Skip statements inside nested Module expressions — they're handled
        // by ModuleDef::derive_shape with its own Checker.
        if self.nested_depth > 0 {
            return;
        }
        match stmt {
            Statement::Let(def) => {
                let name = def.name.fragment.clone();
                let mut shape = def.value.derive_shape(&mut self.symbol_table);
                // Resolve unresolved imports if we have a working directory
                if let Shape::Import(ImportShape::Unresolved(pi)) = &shape {
                    shape = self.resolve_import(&pi.val, &pi.pos);
                }
                // Enforce constraint if present
                if let Some(ref constraint_expr) = def.constraint {
                    let constraint_shape = constraint_expr.derive_shape(&mut self.symbol_table);
                    let narrowed = shape.narrow(&constraint_shape, &mut self.symbol_table);
                    if let Shape::TypeErr(pos, msg) = &narrowed {
                        self.err_stack.push(BuildError::with_pos(
                            msg.clone(),
                            ErrorType::TypeFail,
                            pos.clone(),
                        ));
                        return;
                    }
                    shape = narrowed;
                }
                if let Shape::TypeErr(pos, msg) = &shape {
                    self.err_stack.push(BuildError::with_pos(
                        msg.clone(),
                        ErrorType::TypeFail,
                        pos.clone(),
                    ));
                } else {
                    self.symbol_table.insert(name.clone(), shape.clone());
                    self.shape_stack.push(shape);
                }
            }
            Statement::Constraint(def) => {
                let name = def.name.fragment.clone();
                // Pre-seed symbol table with a ConstraintRef so self-references
                // in the RHS resolve (enabling recursive constraints).
                self.symbol_table.insert(
                    name.clone(),
                    Shape::ConstraintRef(PositionedItem::new(name.clone(), def.name.pos.clone())),
                );
                let shape = def.value.derive_shape(&mut self.symbol_table);
                if let Shape::TypeErr(pos, msg) = &shape {
                    self.err_stack.push(BuildError::with_pos(
                        msg.clone(),
                        ErrorType::TypeFail,
                        pos.clone(),
                    ));
                } else if let Some(pos) = Self::validate_recursive_constraint(&name, &shape) {
                    self.err_stack.push(BuildError::with_pos(
                        format!(
                            "Recursive constraint '{}' is unconstructible: self-reference must appear inside a list or alternation with a base case",
                            name
                        ),
                        ErrorType::TypeFail,
                        pos,
                    ));
                } else {
                    self.symbol_table.insert(name.clone(), shape.clone());
                    self.shape_stack.push(shape);
                }
            }
            &mut Statement::Assert(ref mut _pos, ref expr) => {
                let shape = expr.derive_shape(&mut self.symbol_table);
                // Assert statements require a tuple with shape {ok=<bool>, desc=<str>}
                let expected = Shape::Tuple(PositionedItem::new(
                    vec![
                        (
                            PositionedItem::new(Rc::from("ok"), _pos.clone()),
                            Shape::Boolean(_pos.clone()),
                        ),
                        (
                            PositionedItem::new(Rc::from("desc"), _pos.clone()),
                            Shape::Str(_pos.clone()),
                        ),
                    ],
                    _pos.clone(),
                ));
                let narrowed = shape.narrow(&expected, &mut self.symbol_table);
                self.push_shape_or_err(narrowed);
            }
            &mut Statement::Output(ref mut _pos, ref mut _tok, ref expr) => {
                let shape = expr.derive_shape(&mut self.symbol_table);
                self.push_shape_or_err(shape);
            }
            &mut Statement::Expression(ref expr) => {
                let shape = expr.derive_shape(&mut self.symbol_table);
                self.push_shape_or_err(shape);
            }
        }
    }

    fn leave_statement(&mut self, _stmt: &Statement) {
        // noop by default
    }
}

#[cfg(test)]
mod test;
