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
use std::cell::RefCell;
use std::collections::BTreeSet;
use std::rc::Rc;

use crate::ast::Position;

use super::environment::Environment;
use super::pointer::OpPointer;
use super::runtime;
use super::scope::Stack;
use super::translate::PositionMap;
use super::Composite::{List, Tuple};
use super::Hook;
use super::Primitive::{Bool, Empty, Float, Int, Str};
use super::Value::{C, F, M, P, S, T};
use super::{Error, Op, Primitive, Value};
use super::{Func, Module};

fn construct_reserved_word_set() -> BTreeSet<&'static str> {
    let mut words = BTreeSet::new();
    for word in vec![
        "let", "module", "func", "out", "assert", "self", "import", "include", "as", "map",
        "filter", "convert", "fail", "NULL", "in", "is", "TRACE",
    ] {
        words.insert(word);
    }
    words
}

pub struct VM<O, E>
where
    O: std::io::Write,
    E: std::io::Write,
{
    stack: Vec<(Rc<Value>, Position)>,
    symbols: Stack,
    runtime: runtime::Builtins,
    ops: OpPointer,
    pub env: Rc<RefCell<Environment<O, E>>>,
    pub last: Option<(Rc<Value>, Position)>,
    self_stack: Vec<(Rc<Value>, Position)>,
    reserved_words: BTreeSet<&'static str>,
}

impl<'a, O, E> VM<O, E>
where
    O: std::io::Write,
    E: std::io::Write,
{
    pub fn new(ops: Rc<PositionMap>, env: Rc<RefCell<Environment<O, E>>>) -> Self {
        Self::with_pointer(OpPointer::new(ops), env)
    }

    pub fn with_pointer(ops: OpPointer, env: Rc<RefCell<Environment<O, E>>>) -> Self {
        Self {
            stack: Vec::new(),
            symbols: Stack::new(),
            runtime: runtime::Builtins::new(),
            ops: ops,
            env: env,
            last: None,
            self_stack: Vec::new(),
            reserved_words: construct_reserved_word_set(),
        }
    }

    pub fn enable_validate_mode(&mut self) {
        self.runtime.enable_validate_mode();
    }

    pub fn to_scoped(self, symbols: Stack) -> Self {
        Self {
            stack: Vec::new(),
            symbols: symbols,
            runtime: self.runtime.clone(),
            ops: self.ops.clone(),
            env: self.env.clone(),
            last: self.last,
            self_stack: self.self_stack,
            reserved_words: self.reserved_words,
        }
    }

    pub fn symbols_to_tuple(&self, include_mod: bool) -> Value {
        let mut flds = Vec::new();
        for sym in self.symbols.symbol_list() {
            if include_mod || sym != "mod" {
                let (val, _) = self.symbols.get(sym).unwrap().clone();
                flds.push((sym.clone(), val));
            }
        }
        return C(Tuple(flds));
    }

    pub fn run(&mut self) -> Result<(), Error> {
        loop {
            let op = if let Some(op) = self.ops.next() {
                op.clone()
            } else {
                break;
            };
            let pos = self.ops.pos().unwrap().clone();
            let idx = self.ops.idx()?;
            match op {
                Op::Val(p) => self.push(Rc::new(P(p.clone())), pos)?,
                Op::Sym(s) => self.push(Rc::new(S(s.clone())), pos)?,
                Op::DeRef(s) => self.op_deref(s.clone(), &pos)?,
                Op::Add => self.op_add(pos)?,
                Op::Mod => self.op_mod(pos)?,
                Op::Sub => self.op_sub(pos)?,
                Op::Mul => self.op_mul(pos)?,
                Op::Div => self.op_div(pos)?,
                Op::Bind => self.op_bind(true)?,
                Op::BindOver => self.op_bind(false)?,
                Op::Equal => self.op_equal(pos)?,
                Op::Not => self.op_not(&pos)?,
                Op::Gt => self.op_gt(&pos)?,
                Op::Lt => self.op_lt(&pos)?,
                Op::GtEq => self.op_gteq(pos)?,
                Op::LtEq => self.op_lteq(pos)?,
                // Add a Composite list value to the stack
                Op::InitList => self.push(Rc::new(C(List(Vec::new()))), pos)?,
                // Add a composite tuple value to the stack
                Op::InitTuple => self.push(Rc::new(C(Tuple(Vec::new()))), pos)?,
                Op::Field => self.op_field()?,
                Op::Element => self.op_element()?,
                Op::Index => self.op_index(false, pos)?,
                Op::SafeIndex => self.op_index(true, pos)?,
                Op::Cp => self.op_copy(pos)?,
                //FIXME(jwall): Should this take a user provided message?
                Op::Bang => self.op_bang()?,
                Op::InitThunk(jp) => self.op_thunk(idx, jp, pos)?,
                Op::Noop => {
                    // Do nothing
                }
                Op::Jump(jp) => self.op_jump(jp)?,
                Op::JumpIfTrue(jp) => self.op_jump_if_true(jp)?,
                Op::JumpIfFalse(jp) => self.op_jump_if_false(jp)?,
                Op::SelectJump(jp) => self.op_select_jump(jp)?,
                Op::And(jp) => self.op_and(jp, pos)?,
                Op::Or(jp) => self.op_or(jp, pos)?,
                Op::Module(mptr) => self.op_module(idx, mptr, pos)?,
                Op::Func(jptr) => self.op_func(idx, jptr, pos)?,
                Op::FCall => self.op_fcall(pos)?,
                Op::NewScope(jp) => self.op_new_scope(jp, self.ops.clone())?,
                Op::Return => {
                    &self.stack;
                    return Ok(());
                }
                Op::Pop => {
                    self.pop()?;
                }
                Op::Typ => self.op_typ()?,
                Op::Runtime(h) => self.op_runtime(h, pos)?,
                Op::Render => self.op_render()?,
                Op::PushSelf => self.op_push_self()?,
                Op::PopSelf => self.op_pop_self()?,
            };
        }
        Ok(())
    }

    fn op_typ(&mut self) -> Result<(), Error> {
        let (val, pos) = self.pop()?;
        let typ_name = match val.as_ref() {
            P(Int(_)) => "int",
            P(Float(_)) => "float",
            P(Bool(_)) => "bool",
            P(Str(_)) => "str",
            P(Empty) => "null",
            C(Tuple(_)) => "tuple",
            C(List(_)) => "list",
            F(_) => "func",
            M(_) => "module",
            S(_) => "sym",
            T(_) => "thunk",
        }
        .to_owned();
        self.push(Rc::new(P(Str(typ_name))), pos)?;
        Ok(())
    }

    fn op_deref(&mut self, name: String, pos: &Position) -> Result<(), Error> {
        let (val, _) = self.get_binding(&name, pos)?.clone();
        self.push(val, pos.clone())
    }

    fn op_jump(&mut self, jp: i32) -> Result<(), Error> {
        self.ops.jump(
            self.ops
                .ptr
                .map(|v| (v as i32 + jp) as usize)
                .unwrap_or(jp as usize),
        )?;
        Ok(())
    }

    fn op_and(&mut self, jp: i32, pos: Position) -> Result<(), Error> {
        let (cond, cond_pos) = self.pop()?;
        let cc = cond.clone();
        if let &P(Bool(cond)) = cond.as_ref() {
            if !cond {
                self.push(cc, cond_pos)?;
                self.op_jump(jp)?;
            }
        } else {
            return Err(Error::new(
                format!(
                    "Not a boolean condition {:?} in && expression at {}",
                    cond, pos
                ),
                cond_pos.clone(),
            ));
        }
        Ok(())
    }

    fn op_or(&mut self, jp: i32, pos: Position) -> Result<(), Error> {
        let (cond, cond_pos) = self.pop()?;
        let cc = cond.clone();
        if let &P(Bool(cond)) = cond.as_ref() {
            if cond {
                self.push(cc, cond_pos)?;
                self.op_jump(jp)?;
            }
        } else {
            return Err(Error::new(
                format!(
                    "Not a boolean condition {:?} in || expression at {}!",
                    cond, pos
                ),
                cond_pos.clone(),
            ));
        }
        Ok(())
    }

    fn op_jump_if_true(&mut self, jp: i32) -> Result<(), Error> {
        let (cond, cond_pos) = self.pop()?;
        if let &P(Bool(cond)) = cond.as_ref() {
            if cond {
                self.op_jump(jp)?;
            }
        } else {
            return Err(Error::new(
                format!("Expected boolean but got {:?}!", cond),
                cond_pos.clone(),
            ));
        }
        Ok(())
    }

    fn op_jump_if_false(&mut self, jp: i32) -> Result<(), Error> {
        let (cond, pos) = self.pop()?;
        if let &P(Bool(cond)) = cond.as_ref() {
            if !cond {
                self.op_jump(jp)?;
            }
        } else {
            return Err(Error::new(
                format!("Expected boolean but got {:?}!", cond),
                pos.clone(),
            ));
        }
        Ok(())
    }

    fn op_select_jump(&'a mut self, jp: i32) -> Result<(), Error> {
        // pop field value off
        let (field_name, _) = self.pop()?;
        // pop search value off
        let (search, srch_pos) = self.pop()?;
        // compare them.
        let matched = match (field_name.as_ref(), search.as_ref()) {
            (&S(ref fname), &P(Str(ref sname))) | (&S(ref fname), &S(ref sname)) => fname == sname,
            _ => false,
        };
        if !matched {
            // if they aren't equal then push search value back on and jump
            self.push(search, srch_pos)?;
            self.op_jump(jp)?;
        }
        Ok(())
    }

    fn op_module(&'a mut self, idx: usize, jptr: i32, pos: Position) -> Result<(), Error> {
        let (mod_val, mod_val_pos) = self.pop()?;
        let (result_ptr, flds) = match mod_val.as_ref() {
            &C(Tuple(ref flds)) => (None, flds.clone()),
            &T(ptr) => {
                let (tpl_val, tpl_val_pos) = self.pop()?;
                if let &C(Tuple(ref flds)) = tpl_val.as_ref() {
                    (Some(ptr), flds.clone())
                } else {
                    return Err(Error::new(
                        format!("Expected tuple but got {:?}", tpl_val),
                        tpl_val_pos,
                    ));
                }
            }
            _ => {
                return Err(Error::new(
                    format!("Expected tuple but got {:?}", mod_val),
                    mod_val_pos,
                ));
            }
        };
        let mut ops = self.ops.clone();
        let pkg_pos = self.ops.pos().unwrap().clone();
        ops.jump(idx)?;
        let pkg_ptr = if let Some(ref path) = self.ops.path {
            let pkg_ops = vec![
                Op::InitList,
                Op::Func(3),
                Op::Val(Str(path.to_string_lossy().to_string())),
                Op::Runtime(Hook::Import),
                Op::Return,
            ];
            let pos_list = vec![
                pkg_pos.clone(),
                pkg_pos.clone(),
                pkg_pos.clone(),
                pkg_pos.clone(),
                pkg_pos,
            ];
            Some(OpPointer::new(Rc::new(PositionMap {
                ops: pkg_ops,
                pos: pos_list,
            })))
        } else {
            None
        };
        self.push(
            Rc::new(M(Module {
                ptr: ops,
                result_ptr: result_ptr,
                flds: flds,
                pkg_ptr: pkg_ptr,
            })),
            pos,
        )?;
        self.op_jump(jptr)
    }

    fn op_func(&mut self, idx: usize, jptr: i32, pos: Position) -> Result<(), Error> {
        // get arity from stack
        let scope_snapshot = self.symbols.snapshot();
        let mut bindings = Vec::new();
        // get imported symbols from stack
        let (list_val, args_pos) = self.pop()?;
        if let &C(List(ref elems)) = list_val.as_ref() {
            for e in elems {
                if let &S(ref sym) = e.as_ref() {
                    bindings.push(sym.clone());
                } else {
                    return Err(Error::new(
                        format!("Not an argument name {:?}", e),
                        args_pos,
                    ));
                }
            }
        } else {
            return Err(Error::new(format!("Fault!!! Bad Argument List"), args_pos));
        }
        let mut ops = self.ops.clone();
        ops.jump(idx)?;
        self.push(
            Rc::new(F(Func {
                ptr: ops, // where the function starts.
                bindings: bindings,
                snapshot: scope_snapshot,
            })),
            pos,
        )?;
        self.op_jump(jptr)
    }

    pub fn fcall_impl(
        f: &Func,
        stack: &mut Vec<(Rc<Value>, Position)>,
        env: Rc<RefCell<Environment<O, E>>>,
    ) -> Result<(Rc<Value>, Position), Error> {
        let Func {
            ref ptr,
            ref bindings,
            ref snapshot,
        } = f;
        // use the captured scope snapshot for the function.
        let mut vm = Self::with_pointer(ptr.clone(), env).to_scoped(snapshot.clone());
        for nm in bindings.iter() {
            // now put each argument on our scope stack as a binding.
            // TODO(jwall): This should do a better error if there is
            // nothing on the stack.
            let (val, pos) = stack.pop().unwrap();
            vm.binding_push(nm.clone(), val, false, &pos, &pos)?;
        }
        // proceed to the function body
        vm.run()?;
        return vm.pop();
    }

    fn op_new_scope(&mut self, jp: i32, ptr: OpPointer) -> Result<(), Error> {
        let scope_snapshot = self.symbols.snapshot();
        let mut vm = Self::with_pointer(ptr, self.env.clone()).to_scoped(scope_snapshot);
        vm.run()?;
        let result = vm.pop()?;
        self.push(result.0, result.1)?;
        self.op_jump(jp)?;
        Ok(())
    }

    fn op_fcall(&mut self, pos: Position) -> Result<(), Error> {
        let (f, _) = self.pop()?;
        if let &F(ref f) = f.as_ref() {
            let (val, _) = Self::fcall_impl(f, &mut self.stack, self.env.clone())?;
            self.push(val, pos.clone())?;
        }
        Ok(())
    }

    fn op_thunk(&mut self, idx: usize, jp: i32, pos: Position) -> Result<(), Error> {
        self.push(Rc::new(T(idx)), pos)?;
        self.op_jump(jp)
    }

    fn op_not(&mut self, pos: &Position) -> Result<(), Error> {
        let (operand, operand_pos) = self.pop()?;
        if let P(Bool(val)) = operand.as_ref() {
            self.push(Rc::new(P(Bool(!val))), operand_pos)?;
            return Ok(());
        }
        return Err(Error::new(
            format!(
                "Expected Boolean but got {:?} in expression at {}",
                operand, pos
            ),
            operand_pos,
        ));
    }

    fn op_equal(&mut self, pos: Position) -> Result<(), Error> {
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        // FIXME(jwall): We need to enforce our equality rules here.
        self.push(Rc::new(P(Bool(left == right))), pos)?;
        Ok(())
    }

    fn op_gt(&mut self, pos: &Position) -> Result<(), Error> {
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i > ii))), pos.clone())?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f > ff))), pos.clone())?;
            }
            _ => {
                return Err(Error::new(
                    format!(
                        "Expected Numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                ));
            }
        }
        Ok(())
    }

    fn op_lt(&mut self, pos: &Position) -> Result<(), Error> {
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i < ii))), pos.clone())?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f < ff))), pos.clone())?;
            }
            _ => {
                return Err(Error::new(
                    format!(
                        "Expected Numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos.clone(),
                ));
            }
        }
        Ok(())
    }

    fn op_lteq(&mut self, pos: Position) -> Result<(), Error> {
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i <= ii))), pos)?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f <= ff))), pos)?;
            }
            _ => {
                return Err(Error::new(
                    format!(
                        "Expected Numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos,
                ));
            }
        }
        Ok(())
    }

    fn op_gteq(&mut self, pos: Position) -> Result<(), Error> {
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        match (left.as_ref(), right.as_ref()) {
            (&P(Int(i)), &P(Int(ii))) => {
                self.push(Rc::new(P(Bool(i >= ii))), pos)?;
            }
            (&P(Float(f)), &P(Float(ff))) => {
                self.push(Rc::new(P(Bool(f >= ff))), pos)?;
            }
            _ => {
                return Err(Error::new(
                    format!(
                        "Expected Numeric values of the same type but got {:?} and {:?}",
                        left, right
                    ),
                    pos,
                ));
            }
        }
        Ok(())
    }

    fn op_mod(&mut self, pos: Position) -> Result<(), Error> {
        // Adds the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.modulus(&left, &right, &pos)?)), pos)?;
        Ok(())
    }

    fn op_add(&mut self, pos: Position) -> Result<(), Error> {
        // Adds the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(self.add(&left, &right, &pos)?), pos)?;
        Ok(())
    }

    fn op_sub(&mut self, pos: Position) -> Result<(), Error> {
        // Subtracts the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.sub(&left, &right, &pos)?)), pos)?;
        Ok(())
    }

    fn op_mul(&mut self, pos: Position) -> Result<(), Error> {
        // Multiplies the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.mul(&left, &right, &pos)?)), pos)?;
        Ok(())
    }

    fn op_div(&mut self, pos: Position) -> Result<(), Error> {
        // Divides the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, _) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.div(&left, &right, &pos)?)), pos)?;
        Ok(())
    }

    fn op_push_self(&mut self) -> Result<(), Error> {
        // We'll need a self stack.
        let (val, pos) = self.pop()?;
        self.self_stack.push((val.clone(), pos.clone()));
        self.push(val.clone(), pos)?;
        Ok(())
    }

    fn op_pop_self(&mut self) -> Result<(), Error> {
        // We'll need a self stack.
        self.self_stack.pop();
        Ok(())
    }

    fn op_bind(&mut self, strict: bool) -> Result<(), Error> {
        // pop val off stack.
        let (val, val_pos) = self.pop()?;
        // pop name off stack.
        let (name, name_pos) = self.pop()?;
        // TODO(jwall): We need to restrict against our reserved word list.
        if let &S(ref name) = name.as_ref() {
            self.binding_push(name.clone(), val, strict, &val_pos, &name_pos)?;
        } else {
            unreachable!();
        }
        Ok(())
    }

    fn op_field(&mut self) -> Result<(), Error> {
        // Add a Composite field value to a tuple on the stack
        // get value from stack
        //dbg!(&self.stack);
        let (val, _) = self.pop()?;
        // get name from stack.
        let (name_val, _) = self.pop()?;
        let name = if let &S(ref s) | &P(Str(ref s)) = name_val.as_ref() {
            s
        } else {
            //dbg!(name_val);
            //dbg!(val);
            unreachable!();
        };
        // get composite tuple from stack
        let (tpl, tpl_pos) = self.pop()?;
        if let &C(Tuple(ref flds)) = tpl.as_ref() {
            // add name and value to tuple
            let mut flds = flds.clone();
            self.merge_field_into_tuple(&mut flds, name.clone(), val)?;
            // place composite tuple back on stack
            self.push(Rc::new(C(Tuple(flds))), tpl_pos)?;
        } else {
            unreachable!();
        };
        Ok(())
    }

    fn op_element(&mut self) -> Result<(), Error> {
        // get element from stack.
        let (val, _) = self.pop()?;
        // get next value. It should be a Composite list.
        let (list, pos) = self.pop()?;
        if let &C(List(ref elems)) = list.as_ref() {
            // add value to list
            let mut elems = elems.clone();
            elems.push(val);
            // Add that value to the list and put list back on stack.
            self.push(Rc::new(C(List(elems))), pos)?;
        } else {
            unreachable!();
        };
        Ok(())
    }

    fn op_bang(&mut self) -> Result<(), Error> {
        Ok(())
    }

    fn op_index(&mut self, safe: bool, pos: Position) -> Result<(), Error> {
        // left and then right
        let (right, right_pos) = self.pop()?;
        let (left, _) = self.pop()?;
        match right.as_ref() {
            &P(Int(i)) => {
                if let &C(List(ref elems)) = left.as_ref() {
                    if i < (elems.len() as i64) && i >= 0 {
                        self.push(elems[i as usize].clone(), right_pos)?;
                        return Ok(());
                    }
                }
            }
            &P(Str(ref s)) => {
                if let &C(Tuple(ref flds)) = left.as_ref() {
                    for &(ref key, ref val) in flds.iter() {
                        if key == s {
                            self.push(val.clone(), right_pos)?;
                            return Ok(());
                        }
                    }
                }
            }
            _ => {
                // noop
            }
        };
        if safe {
            self.push(Rc::new(P(Empty)), pos)?;
            return Ok(());
        }
        return Err(Error::new(
            format!("Invalid selector index: {:?} target: {:?}", right, left),
            pos,
        ));
    }

    fn op_copy(&mut self, pos: Position) -> Result<(), Error> {
        // This value should always be a tuple
        let (override_val, _) = self.pop()?;
        // get targett value. It should be a Module or Tuple.
        let (tgt, tgt_pos) = self.pop()?;
        let overrides = if let &C(Tuple(ref oflds)) = override_val.as_ref() {
            oflds.clone()
        } else {
            unreachable!();
        };
        match tgt.as_ref() {
            &C(Tuple(ref flds)) => {
                let mut flds = flds.clone();
                for (name, val) in overrides {
                    self.merge_field_into_tuple(&mut flds, name, val)?;
                }
                // Put the copy on the Stack
                self.push(Rc::new(C(Tuple(flds))), tgt_pos.clone())?;
                self.last = Some((tgt.clone(), tgt_pos));
            }
            &M(Module {
                ref ptr,
                ref result_ptr,
                ref flds,
                ref pkg_ptr,
            }) => {
                let this = M(Module {
                    ptr: ptr.clone(),
                    result_ptr: result_ptr.clone(),
                    flds: flds.clone(),
                    pkg_ptr: pkg_ptr.clone(),
                });

                let mut flds = flds.clone();
                for (name, val) in overrides {
                    self.merge_field_into_tuple(&mut flds, name, val)?;
                }
                self.merge_field_into_tuple(&mut flds, "this".to_owned(), Rc::new(this))?;
                if let Some(ptr) = pkg_ptr {
                    let mut pkg_vm = Self::with_pointer(ptr.clone(), self.env.clone());
                    pkg_vm.run()?;
                    let (pkg_func, _) = pkg_vm.pop()?;
                    self.merge_field_into_tuple(&mut flds, "pkg".to_owned(), pkg_func)?;
                }

                let mut vm = Self::with_pointer(ptr.clone(), self.env.clone());
                vm.push(Rc::new(S("mod".to_owned())), pos.clone())?;
                vm.push(Rc::new(C(Tuple(flds))), pos.clone())?;
                vm.run()?;
                if let Some(ptr) = result_ptr {
                    vm.ops.jump(ptr.clone())?;
                    vm.run()?;
                    let (result_val, result_pos) = vm.pop()?;
                    self.push(result_val, result_pos)?;
                } else {
                    self.push(Rc::new(vm.symbols_to_tuple(false)), pos)?;
                }
            }
            _ => {
                return Err(Error::new(
                    format!("Expected a Tuple or a Module but got {:?}", tgt),
                    pos,
                ));
            }
        }
        Ok(())
    }

    fn merge_field_into_tuple(
        &self,
        src_fields: &'a mut Vec<(String, Rc<Value>)>,
        name: String,
        value: Rc<Value>,
    ) -> Result<(), Error> {
        for fld in src_fields.iter_mut() {
            if fld.0 == name {
                fld.1 = value;
                return Ok(());
            }
        }
        src_fields.push((name, value));
        Ok(())
    }

    fn push(&mut self, val: Rc<Value>, pos: Position) -> Result<(), Error> {
        self.stack.push((val, pos));
        Ok(())
    }

    pub fn binding_push(
        &mut self,
        name: String,
        val: Rc<Value>,
        strict: bool,
        pos: &Position,
        name_pos: &Position,
    ) -> Result<(), Error> {
        if self.reserved_words.contains(name.as_str()) {
            return Err(Error::new(
                format!("{} is a reserved word.", name),
                name_pos.clone(),
            ));
        }
        if self.symbols.is_bound(&name) && strict {
            return Err(Error::new(
                format!("Binding {} already exists", name),
                pos.clone(),
            ));
        }
        self.symbols.add(name, val, pos.clone());
        Ok(())
    }

    pub fn get_binding(
        &'a self,
        name: &str,
        pos: &Position,
    ) -> Result<(Rc<Value>, Position), Error> {
        if name == "self" {
            if let Some((val, pos)) = self.self_stack.last() {
                return Ok((val.clone(), pos.clone()));
            }
            return Err(Error::new(format!("No such binding {}", name), pos.clone()));
        }
        match self.symbols.get(name) {
            Some((ref v, ref pos)) => Ok((v.clone(), pos.clone())),
            None => {
                return Err(Error::new(format!("No such binding {}", name), pos.clone()));
            }
        }
    }

    pub fn pop(&mut self) -> Result<(Rc<Value>, Position), Error> {
        match self.stack.pop() {
            Some(v) => {
                self.last = Some(v.clone());
                Ok(v)
            }
            None => unreachable!(),
        }
    }

    fn mul(&self, left: &Value, right: &Value, pos: &Position) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), P(Int(ii))) => Int(i * ii),
            (P(Float(f)), P(Float(ff))) => Float(f * ff),
            _ => {
                return Err(Error::new(
                    format!("Expected {} but got {:?}", left.type_name(), right),
                    pos.clone(),
                ))
            }
        })
    }

    fn div(&self, left: &Value, right: &Value, pos: &Position) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), P(Int(ii))) => Int(i / ii),
            (P(Float(f)), P(Float(ff))) => Float(f / ff),
            _ => {
                return Err(Error::new(
                    format!("Expected {} but got {:?}", left.type_name(), right),
                    pos.clone(),
                ))
            }
        })
    }

    fn sub(&self, left: &Value, right: &Value, pos: &Position) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i - ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f - ff),
            _ => {
                return Err(Error::new(
                    format!("Expected {} but got {:?}", left.type_name(), right),
                    pos.clone(),
                ))
            }
        })
    }

    fn modulus(&self, left: &Value, right: &Value, pos: &Position) -> Result<Primitive, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => Int(i % ii),
            (P(Float(f)), Value::P(Float(ff))) => Float(f % ff),
            _ => {
                return Err(Error::new(
                    format!("Expected {} but got {:?}", left.type_name(), right),
                    pos.clone(),
                ))
            }
        })
    }

    fn add(&self, left: &Value, right: &Value, pos: &Position) -> Result<Value, Error> {
        Ok(match (left, right) {
            (P(Int(i)), Value::P(Int(ii))) => P(Int(i + ii)),
            (P(Float(f)), Value::P(Float(ff))) => P(Float(f + ff)),
            (P(Str(s)), Value::P(Str(ss))) => {
                let mut ns = String::new();
                ns.push_str(&s);
                ns.push_str(&ss);
                P(Str(ns))
            }
            (C(List(ref left_list)), C(List(ref right_list))) => {
                let mut new_list = Vec::with_capacity(left_list.len() + right_list.len());
                for v in left_list.iter() {
                    new_list.push(v.clone());
                }
                for v in right_list.iter() {
                    new_list.push(v.clone());
                }
                C(List(new_list))
            }
            _ => {
                return Err(Error::new(
                    format!("Expected {} but got {:?}", left.type_name(), right),
                    pos.clone(),
                ))
            }
        })
    }

    fn op_runtime(&mut self, h: Hook, pos: Position) -> Result<(), Error> {
        self.runtime.handle(
            self.ops.path.as_ref(),
            h,
            &mut self.stack,
            self.env.clone(),
            pos,
        )
    }

    fn op_render(&mut self) -> Result<(), Error> {
        let (val, pos) = self.pop()?;
        self.push(Rc::new(P(Str(val.as_ref().into()))), pos)?;
        Ok(())
    }
}
