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
use std::io::Write;

use crate::ast::*;

// TODO(jwall): We really need a way to preserve comments for these.
// Perhaps for code formatting we actually want to work on the token stream instead?

pub struct AstPrinter<W>
where
    W: Write,
{
    indent: usize,
    curr_indent: usize,
    w: W,
    pub err: Option<std::io::Error>,
}

impl<W> AstPrinter<W>
where
    W: Write,
{
    pub fn new(indent: usize, w: W) -> Self {
        AstPrinter {
            indent: indent,
            curr_indent: 0,
            w: w,
            err: None,
        }
    }

    pub fn visit_token(&mut self, t: &Token) -> std::io::Result<()> {
        let w: &mut Write = self.w.borrow_mut();
        // Do we care about line length?
        match t.typ {
            TokenType::BAREWORD | TokenType::BOOLEAN | TokenType::DIGIT => {
                write!(w, "{}", t.fragment)?;
            }
            TokenType::EMPTY => {
                write!(w, "NULL")?;
            }
            TokenType::PUNCT => {
                // TODO(jwall): We need to identify the points at which we
                // introduce new lines and new indentation scopes.
            }
            TokenType::COMMENT => {
                // We need to track some state here probably.
                // Do we leave comments untouched?
            }
            TokenType::PIPEQUOTE => {
                // FIXME I think is supposed to be removed.
            }
            TokenType::QUOTED => {
                w.write(&['"' as u8])?;
                write!(w, "{}", Self::escape_quotes(&t.fragment))?;
                w.write(&['"' as u8])?;
            }
            TokenType::WS => {
                // TODO(jwall): Track some state around new lines here?
            }
            TokenType::END => {
                // NOOP
            }
        };
        Ok(())
    }

    fn make_indent(&self) -> String {
        // TODO(jwall): This is probably inefficient but we'll improve it after
        // we get it correct.
        let indent: Vec<u8> = std::iter::repeat(' ' as u8)
            .take(self.curr_indent)
            .collect();
        String::from_utf8_lossy(&indent).to_string()
    }

    fn render_list_def(&mut self, def: &ListDef) -> std::io::Result<()> {
        write!(self.w, "[\n")?;
        self.curr_indent += self.indent;
        // If the element list is just 1 we might be able to collapse the tuple.
        let indent = self.make_indent();
        for e in def.elems.iter() {
            // TODO(jwall): Now print out the elements
            write!(self.w, "{}", indent)?;
            self.render_expr(e)?;
            write!(self.w, "\n")?;
        }
        self.curr_indent -= self.indent;
        self.w.write(&[']' as u8])?;
        Ok(())
    }

    fn render_tuple_def(&mut self, def: &Vec<(Token, Expression)>) -> std::io::Result<()> {
        self.w.write(&['{' as u8])?;
        // If the field list is just 1 we might be able to collapse the tuple.
        self.curr_indent += self.indent;
        let indent = self.make_indent();
        for &(ref t, ref expr) in def.iter() {
            write!(self.w, "{}", indent)?;
            // TODO(jwall): Detect if there are strings and render as a quoted string.
            write!(&mut self.w, "{} = ", t.fragment)?;
            self.render_expr(expr)?;
            write!(&mut self.w, ",")?;
            write!(self.w, "\n")?;
        }
        self.w.write(&['}' as u8])?;
        Ok(())
    }

    fn escape_quotes(s: &str) -> String {
        let mut escaped = String::new();
        for c in s.chars() {
            if c == '"' {
                escaped.push_str("\\\"");
            } else if c == '\\' {
                escaped.push_str("\\\\");
            } else {
                escaped.push(c);
            }
        }
        escaped
    }

    pub fn render_value(&mut self, v: &Value) -> std::io::Result<()> {
        match v {
            Value::Boolean(b) => write!(self.w, "{}", if b.val { "true" } else { "false" })?,
            Value::Empty(_) => write!(self.w, "NULL")?,
            // TODO(jwall): We should maintain precision for floats?
            Value::Float(f) => write!(self.w, "{}", f.val)?,
            Value::Int(i) => write!(self.w, "{}", i.val)?,
            Value::Str(s) => write!(self.w, "\"{}\"", Self::escape_quotes(&s.val))?,
            Value::Symbol(s) => write!(self.w, "{}", s.val)?,
            Value::List(l) => self.render_list_def(l)?,
            Value::Tuple(tpl) => self.render_tuple_def(&tpl.val)?,
        };
        Ok(())
    }

    fn render_expr(&mut self, expr: &Expression) -> std::io::Result<()> {
        match expr {
            Expression::Binary(_def) => {
                let op = match _def.kind {
                    BinaryExprType::AND => " && ",
                    BinaryExprType::OR => " || ",
                    BinaryExprType::DOT => ".",
                    BinaryExprType::Equal => " = ",
                    BinaryExprType::NotEqual => " != ",
                    BinaryExprType::GTEqual => " >= ",
                    BinaryExprType::LTEqual => " <= ",
                    BinaryExprType::GT => " > ",
                    BinaryExprType::LT => " < ",
                    BinaryExprType::Add => " + ",
                    BinaryExprType::Sub => " - ",
                    BinaryExprType::Mul => " * ",
                    BinaryExprType::Div => " / ",
                    BinaryExprType::Mod => " %% ",
                    BinaryExprType::IN => " in ",
                    BinaryExprType::IS => " is ",
                    BinaryExprType::REMatch => " ~ ",
                    BinaryExprType::NotREMatch => " !~ ",
                };
                self.render_expr(&_def.left)?;
                self.w.write(op.as_bytes())?;
                self.render_expr(&_def.right)?;
            }
            Expression::Call(_def) => {
                self.render_value(&_def.funcref)?;
                self.w.write("(".as_bytes())?;
                self.curr_indent += self.indent;
                let indent = self.make_indent();
                for e in _def.arglist.iter() {
                    self.w.write(indent.as_bytes())?;
                    self.render_expr(e)?;
                    self.w.write("\n".as_bytes())?;
                }
                self.curr_indent -= self.indent;
                self.w.write("(".as_bytes())?;
            }
            Expression::Copy(_def) => {
                self.render_value(&_def.selector)?;
                self.render_tuple_def(&_def.fields)?;
            }
            Expression::Debug(_def) => {
                self.w.write("TRACE ".as_bytes())?;
                self.render_expr(&_def.expr)?;
            }
            Expression::Fail(_def) => {
                self.w.write("fail ".as_bytes())?;
                self.render_expr(&_def.message)?;
            }
            Expression::Format(_def) => {
                self.w
                    .write(Self::escape_quotes(&_def.template).as_bytes())?;
                write!(self.w, " % ")?;
                match _def.args {
                    FormatArgs::Single(ref e) => {
                        self.render_expr(e)?;
                    }
                    FormatArgs::List(ref es) => {
                        self.w.write("(".as_bytes())?;
                        self.curr_indent += self.indent;
                        let indent = self.make_indent();
                        for e in es.iter() {
                            self.w.write(indent.as_bytes())?;
                            self.render_expr(e)?;
                            self.w.write("\n".as_bytes())?;
                        }
                        self.curr_indent -= self.indent;
                        self.w.write(")".as_bytes())?;
                    }
                }
            }
            Expression::Func(_def) => {
                self.w.write("func (".as_bytes())?;
                for n in _def.argdefs.iter() {
                    write!(self.w, "{}, ", n.val)?;
                }
                self.w.write(") => ".as_bytes())?;
                self.render_expr(&_def.fields)?;
            }
            Expression::FuncOp(_def) => match _def {
                FuncOpDef::Filter(_def) => {
                    write!(self.w, "filter(")?;
                    self.render_expr(&_def.func)?;
                    write!(self.w, ", ")?;
                    self.render_expr(&_def.target)?;
                    write!(self.w, ")")?;
                }
                FuncOpDef::Reduce(_def) => {
                    write!(self.w, "reduce(")?;
                    self.render_expr(&_def.func)?;
                    write!(self.w, ", ")?;
                    self.render_expr(&_def.acc)?;
                    write!(self.w, ", ")?;
                    self.render_expr(&_def.target)?;
                    write!(self.w, ")")?;
                }
                FuncOpDef::Map(_def) => {
                    write!(self.w, "map(")?;
                    self.render_expr(&_def.func)?;
                    write!(self.w, ", ")?;
                    self.render_expr(&_def.target)?;
                    write!(self.w, ")")?;
                }
            },
            Expression::Grouped(ref expr, _) => {
                write!(self.w, "(")?;
                self.render_expr(expr)?;
                write!(self.w, ")")?;
            }
            Expression::Import(_def) => {
                write!(
                    self.w,
                    "import \"{}\"",
                    Self::escape_quotes(&_def.path.fragment)
                )?;
            }
            Expression::Include(_def) => {
                write!(
                    self.w,
                    "include {} \"{}\"",
                    _def.typ.fragment,
                    Self::escape_quotes(&_def.path.fragment)
                )?;
            }
            Expression::Module(_def) => {
                write!(self.w, "module ")?;
                self.render_tuple_def(&_def.arg_set)?;
                write!(self.w, " => ")?;
                if let Some(ref e) = _def.out_expr {
                    write!(self.w, "(")?;
                    self.render_expr(e)?;
                    write!(self.w, ") ")?;
                }
                write!(self.w, "{{")?;
                self.curr_indent += self.indent;
                let indent = self.make_indent();
                for stmt in _def.statements.iter() {
                    write!(self.w, "{}", indent)?;
                    self.render_stmt(stmt)?;
                }
                self.curr_indent -= self.indent;
                write!(self.w, "}}")?;
            }
            Expression::Not(_def) => {
                write!(self.w, "not ")?;
                self.render_expr(&_def.expr)?;
            }
            Expression::Range(_def) => {
                self.render_expr(&_def.start)?;
                write!(self.w, ":")?;
                if let Some(ref e) = _def.step {
                    write!(self.w, ":")?;
                    self.render_expr(e)?;
                }
                self.render_expr(&_def.end)?;
            }
            Expression::Select(_def) => {
                //
                write!(self.w, "select ")?;
                self.render_expr(&_def.val)?;
                write!(self.w, ", ")?;
                if let Some(ref e) = _def.default {
                    self.render_expr(e)?;
                    write!(self.w, ", ")?;
                }
                self.render_tuple_def(&_def.tuple)?;
            }
            Expression::Simple(ref _def) => {
                self.render_value(_def)?;
            }
        };
        Ok(())
    }

    fn render_stmt(&mut self, stmt: &Statement) -> std::io::Result<()> {
        // All statements start at the beginning of a line.
        match stmt {
            Statement::Let(def) => {
                write!(&mut self.w, "let {} = ", def.name.fragment)?;
                self.render_expr(&def.value)?;
            }
            Statement::Expression(_expr) => {
                self.render_expr(&_expr)?;
                //
            }
            Statement::Assert(def) => {
                write!(&mut self.w, "assert ")?;
                self.render_expr(&def)?;
                //
            }
            Statement::Output(_, _tok, _expr) => {
                write!(&mut self.w, "out {} = ", _tok.fragment)?;
                self.render_expr(&_expr)?;
                //
            }
        };
        write!(self.w, ";\n")?;
        Ok(())
    }

    pub fn render(&mut self, stmts: Vec<&mut Statement>) {
        for v in stmts {
            if let Err(e) = self.render_stmt(v) {
                self.err = Some(e);
                return;
            }
        }
    }
}
