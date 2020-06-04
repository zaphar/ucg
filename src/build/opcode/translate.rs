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
use std::collections::BTreeMap;
use std::path::Path;

use crate::ast::rewrite::Rewriter;
use crate::ast::walk::Walker;
use crate::ast::{
    BinaryExprType, BinaryOpDef, Expression, FormatArgs, FuncOpDef, Position, PositionedItem,
    SelectDef, Statement, TemplatePart, Token, TokenType, Value,
};
use crate::build::format::{ExpressionTemplate, SimpleTemplate, TemplateParser};
use crate::build::opcode::Primitive;
use crate::build::opcode::{Hook, Op};

pub struct AST();

#[derive(Debug, PartialEq)]
pub struct OpsMap {
    pub ops: Vec<Op>,
    pub pos: Vec<Position>,
    pub links: BTreeMap<String, Position>,
}

impl OpsMap {
    pub fn new() -> Self {
        OpsMap {
            ops: Vec::new(),
            pos: Vec::new(),
            links: BTreeMap::new(),
        }
    }

    pub fn with_ops(mut self, ops: Vec<Op>, pos: Vec<Position>) -> Self {
        self.ops = ops;
        self.pos = pos;
        self
    }

    pub fn add_link(&mut self, path: String, pos: Position) {
        self.links.insert(path, pos);
    }

    pub fn len(&self) -> usize {
        self.ops.len()
    }

    pub fn push(&mut self, op: Op, pos: Position) {
        self.ops.push(op);
        self.pos.push(pos);
    }

    pub fn replace(&mut self, idx: usize, op: Op) {
        self.ops[idx] = op;
    }
}

impl AST {
    pub fn translate<P: AsRef<Path>>(mut stmts: Vec<Statement>, root: &P) -> OpsMap {
        let mut rewriter = Rewriter::new(root.as_ref());
        let mut_stmts = stmts.iter_mut().collect();
        rewriter.walk_statement_list(mut_stmts);
        let mut ops = OpsMap::new();
        Self::translate_stmts(stmts, &mut ops, root.as_ref());
        return ops;
    }

    fn translate_stmt(stmt: Statement, mut ops: &mut OpsMap, root: &Path) {
        match stmt {
            Statement::Expression(expr) => {
                let expr_pos = expr.pos().clone();
                Self::translate_expr(expr, &mut ops, root);
                ops.push(Op::Pop, expr_pos);
            }
            Statement::Assert(pos, expr) => {
                Self::translate_expr(expr, &mut ops, root);
                ops.push(Op::Runtime(Hook::Assert), pos);
            }
            Statement::Let(def) => {
                let binding = def.name.fragment;
                ops.push(Op::Sym(binding), def.name.pos);
                Self::translate_expr(def.value, &mut ops, root);
                ops.push(Op::Bind, def.pos);
            }
            Statement::Output(pos, tok, expr) => {
                ops.push(Op::Val(Primitive::Str(tok.fragment)), tok.pos);
                Self::translate_expr(expr, &mut ops, root);
                ops.push(Op::Runtime(Hook::Out), pos);
            }
            Statement::Print(pos, tok, expr) => {
                ops.push(Op::Val(Primitive::Str(tok.fragment)), tok.pos);
                Self::translate_expr(expr, &mut ops, root);
                ops.push(Op::Runtime(Hook::Convert), pos.clone());
                ops.push(Op::Pop, pos);
            }
        }
    }

    fn translate_stmts(stmts: Vec<Statement>, mut ops: &mut OpsMap, root: &Path) {
        for stmt in stmts {
            Self::translate_stmt(stmt, &mut ops, root);
        }
    }

    fn translate_expr(expr: Expression, mut ops: &mut OpsMap, root: &Path) {
        match expr {
            Expression::Simple(v) => {
                Self::translate_value(v, &mut ops, root);
            }
            Expression::Binary(def) => {
                match def.kind {
                    BinaryExprType::Add => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Add, def.pos);
                    }
                    BinaryExprType::Sub => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Sub, def.pos);
                    }
                    BinaryExprType::Div => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Div, def.pos);
                    }
                    BinaryExprType::Mul => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Mul, def.pos);
                    }
                    BinaryExprType::Equal => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Equal, def.pos);
                    }
                    BinaryExprType::GT => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Gt, def.pos);
                    }
                    BinaryExprType::LT => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Lt, def.pos);
                    }
                    BinaryExprType::GTEqual => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::GtEq, def.pos);
                    }
                    BinaryExprType::LTEqual => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::LtEq, def.pos);
                    }
                    BinaryExprType::NotEqual => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Equal, def.pos.clone());
                        ops.push(Op::Not, def.pos);
                    }
                    BinaryExprType::REMatch => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Runtime(Hook::Regex), def.pos);
                    }
                    BinaryExprType::NotREMatch => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Runtime(Hook::Regex), def.pos.clone());
                        ops.push(Op::Not, def.pos);
                    }
                    BinaryExprType::IS => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Typ, def.pos.clone());
                        ops.push(Op::Equal, def.pos);
                    }
                    BinaryExprType::AND => {
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Noop, def.pos);
                        let idx = ops.len() - 1;
                        Self::translate_expr(*def.right, &mut ops, root);
                        let jptr = (ops.len() - 1 - idx) as i32;
                        ops.replace(idx, Op::And(jptr));
                    }
                    BinaryExprType::OR => {
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Noop, def.pos); // Placeholder
                        let idx = ops.len() - 1;
                        Self::translate_expr(*def.right, &mut ops, root);
                        let jptr = (ops.len() - 1 - idx) as i32;
                        ops.replace(idx, Op::Or(jptr));
                    }
                    BinaryExprType::Mod => {
                        Self::translate_expr(*def.right, &mut ops, root);
                        Self::translate_expr(*def.left, &mut ops, root);
                        ops.push(Op::Mod, def.pos);
                    }
                    BinaryExprType::IN => {
                        // Dot expressions expect the right side to be pushed first
                        Self::translate_expr(*def.right.clone(), &mut ops, root);
                        // Symbols on the left side should be converted to strings to satisfy
                        // the Index operation contract.
                        match *def.left.clone() {
                            Expression::Simple(Value::Symbol(name)) => {
                                // We really just want an expression that turns a symbol
                                // into a name if the subject is a tuple and doesn't
                                // otherwise
                                let new_expr = Expression::Select(SelectDef {
                                    val: Box::new(Expression::Binary(BinaryOpDef {
                                        kind: BinaryExprType::IS,
                                        right: Box::new(Expression::Simple(Value::Str(
                                            PositionedItem::new(
                                                "tuple".to_owned(),
                                                def.left.pos().clone(),
                                            ),
                                        ))),
                                        left: def.right.clone(),
                                        pos: def.left.pos().clone(),
                                    })),
                                    default: Some(Box::new(Expression::Simple(Value::Symbol(
                                        name.clone(),
                                    )))),
                                    tuple: vec![(
                                        Token::new("true", TokenType::BAREWORD, def.right.pos()),
                                        Expression::Simple(Value::Str(name)),
                                    )],
                                    pos: def.left.pos().clone(),
                                });
                                Self::translate_expr(new_expr, &mut ops, root);
                            }
                            expr => {
                                Self::translate_expr(expr, &mut ops, root);
                            }
                        }
                        ops.push(Op::Exist, def.pos.clone());
                    }
                    BinaryExprType::DOT => {
                        // Symbols on the right side should be converted to strings to satisfy
                        // the Index operation contract.
                        match *def.right {
                            Expression::Copy(copy_def) => {
                                // Dot expressions expect the left side to be pushed first
                                Self::translate_expr(*def.left, &mut ops, root);
                                // first handle the selector
                                match copy_def.selector {
                                    Value::Str(sym) | Value::Symbol(sym) => {
                                        ops.push(Op::Val(Primitive::Str(sym.val)), sym.pos);
                                    }
                                    Value::Int(sym) => {
                                        ops.push(Op::Val(Primitive::Int(sym.val)), sym.pos);
                                    }
                                    _ => unreachable!(),
                                }
                                ops.push(Op::Index, copy_def.pos.clone());
                                Self::translate_copy(ops, copy_def.fields, copy_def.pos, root);
                                return;
                            }
                            Expression::Call(call_def) => {
                                // first push our arguments.
                                let count = call_def.arglist.len() as i64;
                                for e in call_def.arglist {
                                    Self::translate_expr(e, &mut ops, root);
                                }
                                ops.push(Op::Val(Primitive::Int(count)), call_def.pos.clone());
                                // Dot expressions expect the left side to be pushed first
                                Self::translate_expr(*def.left, &mut ops, root);
                                // then handle the selector
                                let func_pos = call_def.funcref.pos().clone();
                                match call_def.funcref {
                                    Value::Str(sym) | Value::Symbol(sym) => {
                                        ops.push(Op::Val(Primitive::Str(sym.val)), sym.pos);
                                    }
                                    Value::Int(sym) => {
                                        ops.push(Op::Val(Primitive::Int(sym.val)), sym.pos);
                                    }
                                    _ => unreachable!(),
                                }
                                ops.push(Op::Index, def.pos);
                                ops.push(Op::FCall, func_pos);
                                return;
                            }
                            Expression::Simple(Value::Symbol(name)) => {
                                // Dot expressions expect the left side to be pushed first
                                Self::translate_expr(*def.left, &mut ops, root);
                                Self::translate_expr(
                                    Expression::Simple(Value::Str(name)),
                                    &mut ops,
                                    root,
                                );
                            }
                            expr => {
                                // Dot expressions expect the left side to be pushed first
                                Self::translate_expr(*def.left, &mut ops, root);
                                Self::translate_expr(expr, &mut ops, root);
                            }
                        }
                        ops.push(Op::Index, def.pos);
                    }
                };
            }
            Expression::Grouped(expr, _) => {
                Self::translate_expr(*expr, &mut ops, root);
            }
            Expression::Fail(def) => {
                let msg_pos = def.message.pos().clone();
                Self::translate_expr(*def.message, &mut ops, root);
                ops.push(Op::Val(Primitive::Str("UserDefined: ".to_owned())), msg_pos);
                ops.push(Op::Add, def.pos.clone());
                ops.push(Op::Bang, def.pos);
            }
            Expression::Format(def) => {
                match def.args {
                    FormatArgs::List(mut elems) => {
                        let formatter = SimpleTemplate::new();
                        // TODO(jwall): This really belongs in a preprocess step
                        // before here.
                        let mut parts = formatter.parse(&def.template).unwrap();
                        // We need to push process these in reverse order for the
                        // vm to process things correctly;
                        elems.reverse();
                        parts.reverse();
                        let mut elems_iter = elems.drain(0..);
                        let mut parts_iter = parts.drain(0..);
                        Self::translate_template_part(
                            def.pos.clone(),
                            parts_iter.next().unwrap(),
                            &mut elems_iter,
                            &mut ops,
                            true,
                            root,
                        );
                        for p in parts_iter {
                            Self::translate_template_part(
                                def.pos.clone(),
                                p,
                                &mut elems_iter,
                                &mut ops,
                                true,
                                root,
                            );
                            // TODO(jwall): We could get a little more helpful about where
                            // these positions are.
                            ops.push(Op::Add, def.pos.clone());
                        }
                    }
                    FormatArgs::Single(expr) => {
                        let formatter = ExpressionTemplate::new();
                        // TODO(jwall): This really belongs in a preprocess step
                        // before here.
                        let mut parts = formatter.parse(&def.template).unwrap();
                        parts.reverse();
                        let mut parts_iter = parts.drain(0..);
                        ops.push(Op::Noop, expr.pos().clone());
                        let scope_idx = ops.len() - 1;

                        // Add our item binding shadowing any binding that already
                        // existed.
                        let expr_pos = expr.pos().clone();
                        ops.push(Op::Sym("item".to_owned()), expr.pos().clone());
                        Self::translate_expr(*expr, &mut ops, root);
                        ops.push(Op::BindOver, expr_pos.clone());
                        let mut elems = Vec::new();
                        let mut elems_iter = elems.drain(0..);
                        Self::translate_template_part(
                            def.pos.clone(),
                            parts_iter.next().unwrap(),
                            &mut elems_iter,
                            &mut ops,
                            false,
                            root,
                        );
                        for p in parts_iter {
                            Self::translate_template_part(
                                def.pos.clone(),
                                p,
                                &mut elems_iter,
                                &mut ops,
                                false,
                                root,
                            );
                            ops.push(Op::Add, expr_pos.clone());
                        }
                        ops.push(Op::Return, expr_pos);
                        let jump_idx = (ops.len() - 1 - scope_idx) as i32;
                        ops.replace(scope_idx, Op::NewScope(jump_idx));
                    }
                }
            }
            Expression::Func(def) => {
                ops.push(Op::InitList, def.pos.clone());
                for b in def.argdefs {
                    // FIXME(jwall): if there is a projection
                    // then add the projection check here?
                    ops.push(Op::Sym(b.val), b.pos.clone());
                    ops.push(Op::Element, b.pos);
                }
                ops.push(Op::Noop, def.pos.clone());
                let idx = ops.len() - 1;
                Self::translate_expr(*def.fields, &mut ops, root);
                ops.push(Op::Return, def.pos);
                let jptr = ops.len() - 1 - idx;
                ops.replace(idx, Op::Func(jptr as i32));
            }
            Expression::FuncOp(def) => {
                match def {
                    FuncOpDef::Map(def) => {
                        // push the function on the stack first.
                        Self::translate_expr(*def.func, &mut ops, root);
                        // push the target on the stack third
                        Self::translate_expr(*def.target, &mut ops, root);
                        // finally push the Hook::Map opcode
                        ops.push(Op::Runtime(Hook::Map), def.pos);
                    }
                    FuncOpDef::Filter(def) => {
                        // push the function on the stack first.
                        Self::translate_expr(*def.func, &mut ops, root);
                        // push the target on the stack third
                        Self::translate_expr(*def.target, &mut ops, root);
                        // finally push the Hook::Map opcode
                        ops.push(Op::Runtime(Hook::Filter), def.pos);
                    }
                    FuncOpDef::Reduce(def) => {
                        // push the function on the stack first.
                        Self::translate_expr(*def.func, &mut ops, root);
                        // push the accumulator on the stack third
                        Self::translate_expr(*def.acc, &mut ops, root);
                        // push the target on the stack third
                        Self::translate_expr(*def.target, &mut ops, root);
                        // finally push the Hook::Map opcode
                        ops.push(Op::Runtime(Hook::Reduce), def.pos);
                    }
                }
            }
            Expression::Import(def) => {
                let link_path = def.path.fragment.clone();
                ops.add_link(link_path, def.path.pos.clone());
                ops.push(Op::Val(Primitive::Str(def.path.fragment)), def.path.pos);
                ops.push(Op::Runtime(Hook::Import), def.pos);
            }
            Expression::Include(def) => {
                ops.push(Op::Val(Primitive::Str(def.typ.fragment)), def.typ.pos);
                ops.push(Op::Val(Primitive::Str(def.path.fragment)), def.path.pos);
                ops.push(Op::Runtime(Hook::Include), def.pos);
            }
            Expression::Module(def) => {
                let argset = def.arg_set;
                let out_expr = def.out_expr;
                let stmts = def.statements;
                // Init our module tuple bindings
                ops.push(Op::InitTuple, def.pos.clone());
                for (t, e) in argset {
                    // FIXME(jwall): if there is a projection
                    // then add the projection check here?
                    ops.push(Op::Sym(t.fragment), t.pos.clone());
                    Self::translate_expr(e, &mut ops, root);
                    ops.push(Op::Field, t.pos);
                }
                // If there is one then emit our return expression
                if let Some(expr) = out_expr {
                    // Insert placeholder until we know jptr for this thunk
                    let expr_pos = expr.pos().clone();
                    ops.push(Op::Noop, expr.pos().clone());
                    let idx = ops.len() - 1;
                    Self::translate_expr(*expr, &mut ops, root);
                    ops.push(Op::Return, expr_pos.clone());
                    let jptr = ops.len() - idx - 1;
                    ops.replace(idx, Op::InitThunk(jptr as i32));
                }
                // Insert a placeholder Opcode until we know jptr for the
                // module.
                ops.push(Op::Noop, def.pos.clone());
                let idx = ops.len() - 1;
                // Bind our mod tuple.
                ops.push(Op::Bind, def.pos.clone());
                // emit all of our statements;
                Self::translate_stmts(stmts, &mut ops, root);
                // Return from the module
                ops.push(Op::Return, def.pos);
                let jptr = ops.len() - idx - 1;
                ops.replace(idx, Op::Module(jptr as i32));
            }
            Expression::Not(def) => {
                Self::translate_expr(*def.expr, &mut ops, root);
                ops.push(Op::Not, def.pos);
            }
            Expression::Range(def) => {
                Self::translate_expr(*def.end, &mut ops, root);
                if let Some(expr) = def.step {
                    Self::translate_expr(*expr, &mut ops, root);
                } else {
                    ops.push(Op::Val(Primitive::Empty), def.pos.clone());
                }
                Self::translate_expr(*def.start, &mut ops, root);
                ops.push(Op::Runtime(Hook::Range), def.pos);
            }
            Expression::Select(def) => {
                let default_pos = def.val.pos().clone();
                Self::translate_expr(*def.val, &mut ops, root);
                let mut jumps = Vec::new();
                for (key, val) in def.tuple {
                    ops.push(Op::Sym(key.fragment), key.pos.clone());
                    ops.push(Op::Noop, key.pos);
                    let idx = ops.len() - 1;
                    let expr_pos = val.pos().clone();
                    Self::translate_expr(val, &mut ops, root);
                    ops.push(Op::Noop, expr_pos);
                    jumps.push(ops.len() - 1);
                    let jptr = ops.len() - idx - 1;
                    ops.replace(idx, Op::SelectJump(jptr as i32));
                }
                ops.push(Op::Pop, def.pos.clone());
                if let Some(default) = def.default {
                    Self::translate_expr(*default, &mut ops, root);
                } else {
                    ops.push(
                        Op::Val(Primitive::Str(
                            "Unhandled select case with no default".to_owned(),
                        )),
                        default_pos,
                    );
                    ops.push(Op::Bang, def.pos);
                }
                let end = ops.len() - 1;
                for i in jumps {
                    let idx = end - i;
                    ops.replace(i, Op::Jump(idx as i32));
                }
            }
            Expression::Call(call_def) => {
                let count = call_def.arglist.len() as i64;
                for e in call_def.arglist {
                    Self::translate_expr(e, &mut ops, root);
                }
                ops.push(Op::Val(Primitive::Int(count)), call_def.pos.clone());
                // then push the func reference
                let func_pos = call_def.funcref.pos().clone();
                Self::translate_value(call_def.funcref, &mut ops, root);
                ops.push(Op::FCall, func_pos);
            }
            Expression::Cast(cast_def) => {
                Self::translate_expr(*cast_def.target, &mut ops, root);
                ops.push(Op::Cast(cast_def.cast_type), cast_def.pos);
            }
            Expression::Copy(def) => {
                Self::translate_value(def.selector, &mut ops, root);
                Self::translate_copy(ops, def.fields, def.pos, root);
            }
            Expression::Debug(def) => {
                let mut buffer: Vec<u8> = Vec::new();
                {
                    let mut printer = crate::ast::printer::AstPrinter::new(2, &mut buffer);
                    let _ = printer.render_expr(&def.expr);
                }
                let expr_pretty = String::from_utf8(buffer).unwrap();
                ops.push(Op::Val(Primitive::Str(expr_pretty)), def.pos.clone());
                Self::translate_expr(*def.expr, &mut ops, root);
                ops.push(Op::Runtime(Hook::Trace(def.pos.clone())), def.pos);
            }
        }
    }

    fn translate_template_part<EI: Iterator<Item = Expression>>(
        pos: Position,
        part: TemplatePart,
        elems: &mut EI,
        mut ops: &mut OpsMap,
        place_holder: bool,
        root: &Path,
    ) {
        match part {
            TemplatePart::Str(s) => {
                ops.push(Op::Val(Primitive::Str(s.into_iter().collect())), pos);
            }
            TemplatePart::PlaceHolder(_idx) => {
                if !place_holder {
                    // In theory this should never be reachable
                    unreachable!();
                } else {
                    Self::translate_expr(elems.next().unwrap(), &mut ops, root);
                    ops.push(Op::Render, pos);
                }
            }
            TemplatePart::Expression(expr) => {
                if place_holder {
                    unreachable!();
                } else {
                    Self::translate_expr(expr, &mut ops, root);
                    ops.push(Op::Render, pos);
                }
            }
        }
    }

    fn translate_copy(
        mut ops: &mut OpsMap,
        flds: Vec<(Token, Expression)>,
        pos: Position,
        root: &Path,
    ) {
        ops.push(Op::PushSelf, pos.clone());
        ops.push(Op::InitTuple, pos.clone());
        for (t, e) in flds {
            ops.push(Op::Sym(t.fragment), t.pos.clone());
            Self::translate_expr(e, &mut ops, root);
            ops.push(Op::Field, t.pos.clone());
        }
        ops.push(Op::Cp, pos.clone());
        ops.push(Op::PopSelf, pos);
    }

    fn translate_value(value: Value, mut ops: &mut OpsMap, root: &Path) {
        match value {
            Value::Int(i) => ops.push(Op::Val(Primitive::Int(i.val)), i.pos),
            Value::Float(f) => ops.push(Op::Val(Primitive::Float(f.val)), f.pos),
            Value::Str(s) => ops.push(Op::Val(Primitive::Str(s.val)), s.pos),
            Value::Empty(pos) => ops.push(Op::Val(Primitive::Empty), pos),
            Value::Boolean(b) => ops.push(Op::Val(Primitive::Bool(b.val)), b.pos),
            Value::Symbol(s) => {
                ops.push(Op::DeRef(s.val), s.pos);
            }
            Value::Tuple(flds) => {
                ops.push(Op::InitTuple, flds.pos);
                for (k, v) in flds.val {
                    ops.push(Op::Sym(k.fragment), k.pos.clone());
                    Self::translate_expr(v, &mut ops, root);
                    ops.push(Op::Field, k.pos.clone());
                }
            }
            Value::List(els) => {
                ops.push(Op::InitList, els.pos);
                for el in els.elems {
                    let el_pos = el.pos().clone();
                    Self::translate_expr(el, &mut ops, root);
                    ops.push(Op::Element, el_pos);
                }
            }
        }
    }
}
