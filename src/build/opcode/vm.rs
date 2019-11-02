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
use std::path::PathBuf;
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
    O: std::io::Write + Clone,
    E: std::io::Write + Clone,
{
    working_dir: PathBuf,
    stack: Vec<(Rc<Value>, Position)>,
    symbols: Stack,
    import_stack: Vec<String>,
    runtime: runtime::Builtins,
    ops: OpPointer,
    pub env: Rc<RefCell<Environment<O, E>>>,
    pub last: Option<(Rc<Value>, Position)>,
    self_stack: Vec<(Rc<Value>, Position)>,
    reserved_words: BTreeSet<&'static str>,
}

impl<'a, O, E> VM<O, E>
where
    O: std::io::Write + Clone,
    E: std::io::Write + Clone,
{
    pub fn new<P: Into<PathBuf>>(
        ops: Rc<PositionMap>,
        env: Rc<RefCell<Environment<O, E>>>,
        working_dir: P,
    ) -> Self {
        Self::with_pointer(OpPointer::new(ops), env, working_dir)
    }

    pub fn with_pointer<P: Into<PathBuf>>(
        ops: OpPointer,
        env: Rc<RefCell<Environment<O, E>>>,
        working_dir: P,
    ) -> Self {
        Self {
            working_dir: working_dir.into(),
            stack: Vec::new(),
            symbols: Stack::new(),
            import_stack: Vec::new(),
            runtime: runtime::Builtins::new(),
            ops: ops,
            env: env,
            last: None,
            self_stack: Vec::new(),
            reserved_words: construct_reserved_word_set(),
        }
    }

    pub fn set_path(&mut self, path: PathBuf) {
        self.ops.set_path(path);
    }

    pub fn to_new_pointer(mut self, ops: OpPointer) -> Self {
        self.ops = ops;
        self
    }

    pub fn with_import_stack(mut self, imports: Vec<String>) -> Self {
        self.import_stack = imports;
        self
    }

    pub fn enable_validate_mode(&mut self) {
        self.runtime.enable_validate_mode();
    }

    pub fn clean_copy(&self) -> Self {
        Self {
            working_dir: self.working_dir.clone(),
            stack: Vec::new(),
            symbols: Stack::new(),
            import_stack: Vec::new(),
            runtime: self.runtime.clone(),
            ops: self.ops.clone(),
            env: self.env.clone(),
            last: None,
            self_stack: self.self_stack.clone(),
            reserved_words: self.reserved_words.clone(),
        }
    }

    pub fn to_scoped(mut self, symbols: Stack) -> Self {
        self.symbols = symbols;
        self
    }

    pub fn symbols_to_tuple(&self, include_mod: bool) -> Value {
        let mut flds = Vec::new();
        let mut pos_list = Vec::new();
        for sym in self.symbols.symbol_list() {
            if include_mod || sym != "mod" {
                let (val, pos) = self.symbols.get(sym).unwrap().clone();
                pos_list.push((pos.clone(), pos.clone()));
                flds.push((sym.clone(), val));
            }
        }
        return C(Tuple(flds, pos_list));
    }
    
    pub fn remove_symbol(&mut self, sym: &str) -> Option<(Rc<Value>, Position)> {
        self.symbols.remove_symbol(sym)
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
                Op::InitList => self.push(Rc::new(C(List(Vec::new(), Vec::new()))), pos)?,
                // Add a composite tuple value to the stack
                Op::InitTuple => self.push(Rc::new(C(Tuple(Vec::new(), Vec::new()))), pos)?,
                Op::Field => self.op_field()?,
                Op::Element => self.op_element()?,
                Op::Index => self.op_index(false, pos)?,
                Op::SafeIndex => self.op_index(true, pos)?,
                Op::Exist => self.op_exist(pos)?,
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
        if let Some(p) = self.ops.path.as_ref() {
            self.import_stack.push(p.to_string_lossy().to_string());
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
            C(Tuple(_, _)) => "tuple",
            C(List(_, _)) => "list",
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
            (&S(ref fname), &P(Bool(b))) => {
                if fname == "true" && b {
                    true
                } else if fname == "false" && !b {
                    true
                } else {
                    false
                }
            }
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
        let (result_ptr, flds, pos_list) = match mod_val.as_ref() {
            &C(Tuple(ref flds, ref pos_list)) => (None, flds.clone(), pos_list.clone()),
            &T(ptr) => {
                let (tpl_val, tpl_val_pos) = self.pop()?;
                if let &C(Tuple(ref flds, ref pos_list)) = tpl_val.as_ref() {
                    (Some(ptr), flds.clone(), pos_list.clone())
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
                flds_pos_list: pos_list,
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
        if let &C(List(ref elems, _)) = list_val.as_ref() {
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
        // Our arguments will be pulled off the stack in reverse order;
        bindings.reverse();
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
        import_stack: &Vec<String>,
    ) -> Result<(Rc<Value>, Position), Error> {
        let Func {
            ref ptr,
            ref bindings,
            ref snapshot,
        } = f;
        // use the captured scope snapshot for the function.
        let mut vm = Self::with_pointer(ptr.clone(), env, std::env::current_dir()?)
            .to_scoped(snapshot.clone())
            .with_import_stack(import_stack.clone());
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
        let mut vm = self
            .clean_copy()
            .to_new_pointer(ptr)
            .to_scoped(scope_snapshot)
            .with_import_stack(self.import_stack.clone());
        vm.run()?;
        let result = vm.pop()?;
        self.push(result.0, result.1)?;
        self.op_jump(jp)?;
        Ok(())
    }

    fn op_fcall(&mut self, pos: Position) -> Result<(), Error> {
        let (f, f_pos) = self.pop()?;
        let (arg_length, _) = self.pop()?;
        if let &F(ref f) = f.as_ref() {
            if let &P(Int(arg_length)) = arg_length.as_ref() {
                let arity = f.bindings.len() as i64;
                if arg_length > arity {
                    return Err(Error::new(
                        format!(
                            "Func called with too many args expected {} args but got {}",
                            arity, arg_length
                        ),
                        pos,
                    ));
                }
                if arg_length < arity {
                    return Err(Error::new(
                        format!(
                            "Func called with too few args expected {} args but got {}",
                            arity, arg_length
                        ),
                        pos,
                    ));
                }
            }
            let (val, _) = decorate_call!(f_pos =>
                Self::fcall_impl(f, &mut self.stack, self.env.clone(), &self.import_stack))?;
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
        let (left, left_pos) = self.pop()?;
        let (right, right_pos) = self.pop()?;
        if left.type_name() != right.type_name()
            && !(left.type_name() == "NULL" || right.type_name() == "NULL")
        {
            return Err(Error::new(
                format!(
                    "Expected values of the same type but got {:?} at {} and {:?} at {} for expression",
                    left, left_pos, right, right_pos,
                ),
                pos,
            ));
        }
        self.push(Rc::new(P(Bool(left == right))), pos)?;
        Ok(())
    }

    fn op_gt(&mut self, pos: &Position) -> Result<(), Error> {
        let (left, left_pos) = self.pop()?;
        let (right, right_pos) = self.pop()?;
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
                        "Expected numeric values of the same type but got {:?} at {} and {:?} at {} for expression",
                        left, left_pos, right, right_pos,
                    ),
                    pos.clone(),
                ));
            }
        }
        Ok(())
    }

    fn op_lt(&mut self, pos: &Position) -> Result<(), Error> {
        let (left, left_pos) = self.pop()?;
        let (right, right_pos) = self.pop()?;
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
                        "Expected numeric values of the same type but got {:?} at {} and {:?} at {} for expression",
                        left, left_pos, right, right_pos,
                    ),
                    pos.clone(),
                ));
            }
        }
        Ok(())
    }

    fn op_lteq(&mut self, pos: Position) -> Result<(), Error> {
        let (left, left_pos) = self.pop()?;
        let (right, right_pos) = self.pop()?;
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
                        "Expected numeric values of the same type but got {:?} at {} and {:?} at {} for expression",
                        left, left_pos, right, right_pos,
                    ),
                    pos,
                ));
            }
        }
        Ok(())
    }

    fn op_gteq(&mut self, pos: Position) -> Result<(), Error> {
        let (left, left_pos) = self.pop()?;
        let (right, right_pos) = self.pop()?;
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
                        "Expected numeric values of the same type but got {:?} at {} and {:?} at {} for expression",
                        left, left_pos, right, right_pos,
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
        let (right, right_pos) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.modulus(&left, &right, &right_pos)?)), pos)?;
        Ok(())
    }

    fn op_add(&mut self, pos: Position) -> Result<(), Error> {
        // Adds the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, right_pos) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(self.add(&left, &right, &right_pos)?), pos)?;
        Ok(())
    }

    fn op_sub(&mut self, pos: Position) -> Result<(), Error> {
        // Subtracts the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, right_pos) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.sub(&left, &right, &right_pos)?)), pos)?;
        Ok(())
    }

    fn op_mul(&mut self, pos: Position) -> Result<(), Error> {
        // Multiplies the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, right_pos) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.mul(&left, &right, &right_pos)?)), pos)?;
        Ok(())
    }

    fn op_div(&mut self, pos: Position) -> Result<(), Error> {
        // Divides the previous two items in the stack.
        let (left, _) = self.pop()?;
        let (right, right_pos) = self.pop()?;
        // Then pushes the result onto the stack.
        self.push(Rc::new(P(self.div(&left, &right, &right_pos)?)), pos)?;
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
        let (val, val_pos) = self.pop()?;
        // get name from stack.
        let (name_val, name_pos) = self.pop()?;
        let name = if let &S(ref s) | &P(Str(ref s)) = name_val.as_ref() {
            s
        } else {
            dbg!(name_val);
            dbg!(val);
            unreachable!();
        };
        // get composite tuple from stack
        let (tpl, tpl_pos) = self.pop()?;
        if let &C(Tuple(ref flds, ref pos_list)) = tpl.as_ref() {
            // add name and value to tuple
            let mut flds = flds.clone();
            let mut pos_list = pos_list.clone();
            self.merge_field_into_tuple(
                &mut flds,
                &mut pos_list,
                name.clone(),
                &name_pos,
                val,
                &val_pos,
            )?;
            // place composite tuple back on stack
            self.push(Rc::new(C(Tuple(flds, pos_list))), tpl_pos)?;
        } else {
            unreachable!();
        };
        Ok(())
    }

    fn op_element(&mut self) -> Result<(), Error> {
        // get element from stack.
        let (val, val_pos) = self.pop()?;
        // get next value. It should be a Composite list.
        let (list, pos) = self.pop()?;
        if let &C(List(ref elems, ref pos_list)) = list.as_ref() {
            // add value to list
            let mut elems = elems.clone();
            elems.push(val);
            let mut pos_list = pos_list.clone();
            pos_list.push(val_pos);
            // Add that value to the list and put list back on stack.
            self.push(Rc::new(C(List(elems, pos_list))), pos)?;
        } else {
            unreachable!();
        };
        Ok(())
    }

    fn op_bang(&mut self) -> Result<(), Error> {
        let (msg_val, err_pos) = self.pop()?;
        if let &P(Str(ref msg)) = msg_val.as_ref() {
            return Err(Error::new(msg.clone(), err_pos));
        } else {
            unreachable!();
        }
    }

    fn op_index(&mut self, safe: bool, pos: Position) -> Result<(), Error> {
        // left and then right
        let (right, right_pos) = self.pop()?;
        let (left, _) = self.pop()?;
        match right.as_ref() {
            &P(Int(i)) => {
                if let &C(List(ref elems, _)) = left.as_ref() {
                    if i < (elems.len() as i64) && i >= 0 {
                        self.push(elems[i as usize].clone(), right_pos)?;
                        return Ok(());
                    }
                }
            }
            &P(Str(ref s)) => {
                if let &C(Tuple(ref flds, _)) = left.as_ref() {
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

    fn op_exist(&mut self, pos: Position) -> Result<(), Error> {
        let (right, right_pos) = self.pop()?;
        let (left, left_pos) = self.pop()?;
        match left.as_ref() {
            &C(Tuple(ref flds, _)) => {
                if let &P(Str(ref name)) = right.as_ref() {
                    for (ref nm, _) in flds {
                        if nm == name {
                            self.push(Rc::new(P(Bool(true))), pos)?;
                            return Ok(());
                        }
                    }
                } else {
                    return Err(Error::new(
                        format!("Expected String or Symbol got: {}", right.type_name()),
                        right_pos,
                    ));
                }
            }
            &C(List(ref elems, _)) => {
                for e in elems {
                    if dbg!(e) == &right {
                        self.push(Rc::new(P(Bool(true))), pos)?;
                        return Ok(());
                    }
                }
            }
            &P(Str(ref s)) => {
                if let &P(Str(ref part)) = right.as_ref() {
                    self.push(Rc::new(P(Bool(s.contains(part)))), pos)?;
                    return Ok(());
                }
            }
            _ => {
                return Err(Error::new(
                    format!("Expected String, Tuple, or List got: {}", left.type_name()),
                    left_pos,
                ));
            }
        };
        self.push(Rc::new(P(Bool(false))), pos)?;
        Ok(())
    }

    fn op_copy(&mut self, pos: Position) -> Result<(), Error> {
        // This value should always be a tuple
        let (override_val, val_pos) = self.pop()?;
        // get target value. It should be a Module or Tuple.
        let (tgt, tgt_pos) = self.pop()?;
        let (overrides, override_pos_list) =
            if let &C(Tuple(ref oflds, ref pos_list)) = override_val.as_ref() {
                (oflds.clone(), pos_list.clone())
            } else {
                unreachable!();
            };
        match tgt.as_ref() {
            &C(Tuple(ref flds, ref pos_list)) => {
                let mut flds = flds.clone();
                let mut pos_list = pos_list.clone();
                let mut counter = 0;
                for (name, val) in overrides {
                    let name_pos = override_pos_list[counter].0.clone();
                    let val_pos = override_pos_list[counter].1.clone();
                    self.merge_field_into_tuple(
                        &mut flds,
                        &mut pos_list,
                        name,
                        &name_pos,
                        val,
                        &val_pos,
                    )?;
                    counter += 1;
                }
                // Put the copy on the Stack
                self.push(Rc::new(C(Tuple(flds, pos_list))), tgt_pos.clone())?;
                self.last = Some((tgt.clone(), tgt_pos));
            }
            &M(Module {
                ref ptr,
                ref result_ptr,
                ref flds,
                ref flds_pos_list,
                ref pkg_ptr,
            }) => {
                let this = M(Module {
                    ptr: ptr.clone(),
                    result_ptr: result_ptr.clone(),
                    flds: flds.clone(),
                    flds_pos_list: flds_pos_list.clone(),
                    pkg_ptr: pkg_ptr.clone(),
                });

                let mut flds = flds.clone();
                let mut flds_pos_list = flds_pos_list.clone();
                let mut counter = 0;
                for (name, val) in overrides {
                    let name_pos = override_pos_list[counter].0.clone();
                    let val_pos = override_pos_list[counter].1.clone();
                    self.merge_field_into_tuple(
                        &mut flds,
                        &mut flds_pos_list,
                        name,
                        &name_pos,
                        val,
                        &val_pos,
                    )?;
                    counter += 1;
                }
                self.merge_field_into_tuple(
                    &mut flds,
                    &mut flds_pos_list,
                    "this".to_owned(),
                    &pos,
                    Rc::new(this),
                    &val_pos,
                )?;
                if let Some(ptr) = pkg_ptr {
                    let mut pkg_vm = self
                        .clean_copy()
                        .to_new_pointer(ptr.clone())
                        .with_import_stack(self.import_stack.clone());
                    pkg_vm.run()?;
                    let (pkg_func, val_pos) = pkg_vm.pop()?;
                    self.merge_field_into_tuple(
                        &mut flds,
                        &mut flds_pos_list,
                        "pkg".to_owned(),
                        &pos,
                        pkg_func,
                        &val_pos,
                    )?;
                }

                let mut vm = self
                    .clean_copy()
                    .to_new_pointer(ptr.clone())
                    .with_import_stack(self.import_stack.clone());
                vm.push(Rc::new(S("mod".to_owned())), pos.clone())?;
                vm.push(Rc::new(C(Tuple(flds, flds_pos_list))), pos.clone())?;
                decorate_call!(pos => vm.run())?;
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
                    format!("Expected a Tuple or Module but got {:?}", tgt),
                    pos,
                ));
            }
        }
        Ok(())
    }

    fn merge_field_into_tuple(
        &self,
        src_fields: &'a mut Vec<(String, Rc<Value>)>,
        pos_fields: &'a mut Vec<(Position, Position)>,
        name: String,
        name_pos: &Position,
        value: Rc<Value>,
        val_pos: &Position,
    ) -> Result<(), Error> {
        let mut counter = 0;
        for fld in src_fields.iter_mut() {
            if fld.0 == name {
                if fld.1.type_name() != value.type_name()
                    && !(fld.1.type_name() == "NULL" || value.type_name() == "NULL")
                {
                    return Err(Error::new(
                        format!(
                            "Expected type {} for field {} but got ({})",
                            fld.1.type_name(),
                            name,
                            value.type_name(),
                        ),
                        val_pos.clone(),
                    ));
                }
                pos_fields[counter].1 = val_pos.clone();
                fld.1 = value;
                return Ok(());
            }
            counter += 1;
        }
        src_fields.push((name, value));
        pos_fields.push((name_pos.clone(), val_pos.clone()));
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
            (
                C(List(ref left_list, ref left_pos_list)),
                C(List(ref right_list, ref right_pos_list)),
            ) => {
                let cap = left_list.len() + right_list.len();
                let mut new_list = Vec::with_capacity(cap);
                let mut new_pos_list = Vec::with_capacity(cap);
                let mut counter = 0;
                for v in left_list.iter() {
                    new_list.push(v.clone());
                    new_pos_list.push(left_pos_list[counter].clone());
                    counter += 1;
                }
                counter = 0;
                for v in right_list.iter() {
                    new_list.push(v.clone());
                    new_pos_list.push(right_pos_list[counter].clone());
                    counter += 1;
                }
                C(List(new_list, new_pos_list))
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
            &mut self.import_stack,
            &self.working_dir,
            pos,
        )
    }

    fn op_render(&mut self) -> Result<(), Error> {
        let (val, pos) = self.pop()?;
        self.push(Rc::new(P(Str(val.as_ref().into()))), pos)?;
        Ok(())
    }
}
