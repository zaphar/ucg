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
use std::io::Write;

use crate::ast::*;
use crate::parse::CommentMap;

// TODO(jwall): We really need a way to preserve comments for these.
// Perhaps for code formatting we actually want to work on the token stream instead?

pub struct AstPrinter<'a, W>
where
    W: Write,
{
    indent_size: usize,
    curr_indent: usize,
    w: W,
    // Indexed by line that the comment was on.
    // We use this to determine when to print a comment in our AstPrinter
    comment_map: Option<&'a CommentMap>,
    last_line: usize,
    comment_group_lines: Vec<usize>,
}

// TODO(jwall): At some point we probably want to be more aware of line length
// in our formatting. But not at the moment.
impl<'a, W> AstPrinter<'a, W>
where
    W: Write,
{
    pub fn new(indent: usize, w: W) -> Self {
        AstPrinter {
            indent_size: indent,
            curr_indent: 0,
            comment_map: None,
            w: w,
            last_line: 0,
            comment_group_lines: Vec::new(),
        }
    }

    pub fn with_comment_map(mut self, map: &'a CommentMap) -> Self {
        self.comment_group_lines = map.keys().cloned().collect();
        self.comment_group_lines.reverse();
        self.comment_map = Some(map);
        self
    }

    fn make_indent(&self) -> String {
        // TODO(jwall): This is probably inefficient but we'll improve it after
        // we get it correct.
        let indent: Vec<u8> = std::iter::repeat(' ' as u8)
            .take(self.curr_indent)
            .collect();
        String::from_utf8_lossy(&indent).to_string()
    }

    fn is_bareword(s: &str) -> bool {
        match s.chars().nth(0) {
            Some(c) => {
                if !(c.is_ascii_alphabetic() || c == '_') {
                    return false;
                }
            }
            None => return false,
        };
        for c in s.chars() {
            if !(c.is_ascii_alphabetic() || c == '_') {
                return false;
            }
        }
        return true;
    }

    fn print_comment_group(&mut self, line: usize) -> std::io::Result<()> {
        if let Some(ref map) = self.comment_map {
            let empty: Vec<Token> = Vec::new();
            let cg = map.get(&line).unwrap_or(&empty);
            let indent = self.make_indent();
            for c in cg.iter() {
                let first_char = match c.fragment.chars().nth(0) {
                    Some(c) => c,
                    None => '\0',
                };
                if !first_char.is_whitespace() {
                    write!(self.w, "{}// {}\n", indent, c.fragment.trim_end())?;
                } else {
                    write!(self.w, "{}//{}\n", indent, c.fragment.trim_end())?;
                }
            }
            self.comment_group_lines.pop();
        }
        Ok(())
    }

    fn render_missed_comments(&mut self, line: usize) -> std::io::Result<()> {
        loop {
            if let Some(next_comment_line) = self.comment_group_lines.last() {
                let next_comment_line = *next_comment_line;
                if next_comment_line <= line {
                    self.print_comment_group(next_comment_line)?;
                } else {
                    break;
                }
                if next_comment_line < line - 1 {
                    write!(self.w, "\n")?;
                }
                continue;
            }
            break;
        }
        Ok(())
    }

    fn render_comment_if_needed(&mut self, line: usize) -> std::io::Result<()> {
        self.render_missed_comments(line)?;
        self.last_line = line;
        Ok(())
    }

    fn has_comment(&self, line: usize) -> bool {
        if let Some(next_comment_line) = self.comment_group_lines.last() {
            return *next_comment_line < line;
        }
        false
    }

    fn render_list_def(&mut self, def: &ListDef) -> std::io::Result<()> {
        write!(self.w, "[")?;
        self.curr_indent += self.indent_size;
        let indent = self.make_indent();
        let has_fields = def.elems.len() > 0;
        if has_fields {
            write!(self.w, "\n")?;
        }
        for e in def.elems.iter() {
            self.render_comment_if_needed(e.pos().line)?;
            write!(self.w, "{}", indent)?;
            self.render_expr(e)?;
            write!(self.w, ",\n")?;
        }
        self.curr_indent -= self.indent_size;
        if has_fields {
            write!(self.w, "{}", self.make_indent())?;
        }
        self.w.write(&[']' as u8])?;
        Ok(())
    }

    fn render_tuple_def(&mut self, def: &Vec<(Token, Expression)>) -> std::io::Result<()> {
        self.w.write(&['{' as u8])?;
        // If the field list is just 1 we might be able to collapse the tuple.
        self.curr_indent += self.indent_size;
        let indent = self.make_indent();
        let has_fields = def.len() > 0;
        if has_fields {
            write!(self.w, "\n")?;
        }
        for &(ref t, ref expr) in def.iter() {
            let field_line = t.pos.line;
            let expr_line = expr.pos().line;
            self.render_comment_if_needed(field_line)?;
            if expr_line != field_line {
                self.render_comment_if_needed(expr_line)?;
            }
            write!(self.w, "{}", indent)?;
            if Self::is_bareword(&t.fragment) {
                write!(&mut self.w, "{} = ", t.fragment)?;
            } else {
                write!(self.w, "\"{}\" = ", Self::escape_quotes(&t.fragment))?;
            }
            self.render_expr(expr)?;
            write!(&mut self.w, ",")?;
            write!(self.w, "\n")?;
        }
        self.curr_indent -= self.indent_size;
        if has_fields {
            write!(self.w, "{}", self.make_indent())?;
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

    pub fn render_expr(&mut self, expr: &Expression) -> std::io::Result<()> {
        let had_comment = self.has_comment(expr.pos().line);
        self.render_comment_if_needed(expr.pos().line)?;
        let indent = self.make_indent();
        if had_comment {
            write!(self.w, "{}", indent)?;
        }
        let mut did_indent = false;
        match expr {
            Expression::Binary(_def) => {
                let op = match _def.kind {
                    BinaryExprType::AND => " && ",
                    BinaryExprType::OR => " || ",
                    BinaryExprType::DOT => ".",
                    BinaryExprType::Equal => " == ",
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
                let right_line = _def.right.pos().line;
                self.render_expr(&_def.left)?;
                self.w.write(op.as_bytes())?;
                if self.has_comment(right_line) {
                    // if we'll be rendering a comment then we should
                    // add a new line here
                    self.w.write("\n".as_bytes())?;
                }
                self.render_expr(&_def.right)?;
            }
            Expression::Call(_def) => {
                self.render_value(&_def.funcref)?;
                self.w.write("(".as_bytes())?;
                self.curr_indent += self.indent_size;
                let indent = self.make_indent();
                let has_args = _def.arglist.len() > 1;
                if has_args {
                    write!(self.w, "\n")?;
                }
                for e in _def.arglist.iter() {
                    self.render_comment_if_needed(e.pos().line)?;
                    if has_args {
                        write!(self.w, "{}", indent)?;
                    }
                    self.render_expr(e)?;
                    if has_args {
                        self.w.write(",\n".as_bytes())?;
                    }
                }
                self.curr_indent -= self.indent_size;
                if has_args {
                    write!(self.w, "{}", self.make_indent())?;
                }
                self.w.write(")".as_bytes())?;
            }
            Expression::Copy(_def) => {
                self.render_value(&_def.selector)?;
                self.render_tuple_def(&_def.fields)?;
            }
            Expression::Debug(_def) => {
                self.w.write("TRACE ".as_bytes())?;
                if self.has_comment(_def.expr.pos().line) {
                    self.w.write("\n".as_bytes())?;
                }
                self.render_expr(&_def.expr)?;
            }
            Expression::Fail(_def) => {
                self.w.write("fail ".as_bytes())?;
                if self.has_comment(_def.message.pos().line) {
                    self.w.write("\n".as_bytes())?;
                }
                self.render_expr(&_def.message)?;
            }
            Expression::Format(_def) => {
                write!(self.w, "\"{}\"", Self::escape_quotes(&_def.template))?;
                write!(self.w, " % ")?;
                match _def.args {
                    FormatArgs::Single(ref e) => {
                        if self.has_comment(e.pos().line) {
                            self.w.write("\n".as_bytes())?;
                        }
                        self.render_expr(e)?;
                    }
                    FormatArgs::List(ref es) => {
                        self.w.write("(\n".as_bytes())?;
                        self.curr_indent += self.indent_size;
                        let indent = self.make_indent();
                        let mut prefix = if es
                            .first()
                            .and_then(|e| Some(self.has_comment(e.pos().line)))
                            .unwrap_or(false)
                        {
                            "\n"
                        } else {
                            ""
                        };
                        for e in es.iter() {
                            write!(self.w, "{}{}", prefix, indent)?;
                            self.render_expr(e)?;
                            prefix = ",\n";
                        }
                        self.curr_indent -= self.indent_size;
                        self.w.write(")".as_bytes())?;
                    }
                }
            }
            Expression::Func(_def) => {
                self.w.write("func (".as_bytes())?;
                if _def.argdefs.len() == 1 {
                    write!(self.w, "{}", _def.argdefs.first().unwrap())?;
                } else {
                    let mut prefix = "";
                    for n in _def.argdefs.iter() {
                        write!(self.w, "{}{}", prefix, n.val)?;
                        prefix = ", ";
                    }
                }
                self.w.write(") => ".as_bytes())?;
                self.render_expr(&_def.fields)?;
            }
            Expression::FuncOp(_def) => match _def {
                FuncOpDef::Filter(_def) => {
                    write!(self.w, "filter(")?;
                    if self.has_comment(_def.func.pos().line) {
                        self.curr_indent += self.indent_size;
                        did_indent = true;
                        write!(self.w, "\n")?;
                    }
                    self.render_expr(&_def.func)?;
                    if self.has_comment(_def.target.pos().line) {
                        write!(self.w, ",")?;
                        if !did_indent {
                            self.curr_indent += self.indent_size;
                        }
                        did_indent = true;
                        self.w.write("\n".as_bytes())?;
                    } else {
                        write!(self.w, ", ")?;
                    }
                    self.render_expr(&_def.target)?;
                    write!(self.w, ")")?;
                }
                FuncOpDef::Reduce(_def) => {
                    write!(self.w, "reduce(")?;
                    if self.has_comment(_def.func.pos().line) {
                        self.curr_indent += self.indent_size;
                        did_indent = true;
                        write!(self.w, "\n")?;
                    }
                    self.render_expr(&_def.func)?;
                    if self.has_comment(_def.acc.pos().line) {
                        write!(self.w, ",")?;
                        if !did_indent {
                            self.curr_indent += self.indent_size;
                        }
                        did_indent = true;
                        self.w.write("\n".as_bytes())?;
                    } else {
                        write!(self.w, ", ")?;
                    }
                    self.render_expr(&_def.acc)?;
                    if self.has_comment(_def.target.pos().line) {
                        write!(self.w, ",")?;
                        if !did_indent {
                            self.curr_indent += self.indent_size;
                        }
                        did_indent = true;
                        self.w.write("\n".as_bytes())?;
                    } else {
                        write!(self.w, ", ")?;
                    }
                    self.render_expr(&_def.target)?;
                    write!(self.w, ")")?;
                }
                FuncOpDef::Map(_def) => {
                    write!(self.w, "map(")?;
                    if self.has_comment(_def.func.pos().line) {
                        self.curr_indent += self.indent_size;
                        did_indent = true;
                        write!(self.w, "\n")?;
                    }
                    self.render_expr(&_def.func)?;
                    if self.has_comment(_def.target.pos().line) {
                        write!(self.w, ",")?;
                        if !did_indent {
                            self.curr_indent += self.indent_size;
                        }
                        did_indent = true;
                        self.w.write("\n".as_bytes())?;
                    } else {
                        write!(self.w, ", ")?;
                    }
                    self.render_expr(&_def.target)?;
                    write!(self.w, ")")?;
                }
            },
            Expression::Grouped(ref expr, _) => {
                write!(self.w, "(")?;
                if self.has_comment(expr.pos().line) {
                    self.curr_indent += self.indent_size;
                    did_indent = true;
                    write!(self.w, "\n")?;
                }
                self.render_expr(expr)?;
                if did_indent {
                    write!(self.w, "\n")?;
                }
                write!(self.w, ")")?;
            }
            Expression::Import(_def) => {
                if self.has_comment(_def.path.pos.line) {
                    self.render_missed_comments(_def.path.pos.line)?;
                }
                write!(
                    self.w,
                    "import \"{}\"",
                    Self::escape_quotes(&_def.path.fragment)
                )?;
            }
            Expression::Include(_def) => {
                if self.has_comment(_def.path.pos.line) {
                    self.render_missed_comments(_def.path.pos.line)?;
                }
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
                write!(self.w, "{{\n")?;
                self.curr_indent += self.indent_size;
                let indent = self.make_indent();
                for stmt in _def.statements.iter() {
                    write!(self.w, "{}", indent)?;
                    self.render_stmt(stmt)?;
                }
                self.curr_indent -= self.indent_size;
                write!(self.w, "}}")?;
            }
            Expression::Not(_def) => {
                if self.has_comment(_def.pos.line) {
                    self.render_missed_comments(_def.pos.line)?;
                }
                write!(self.w, "not ")?;
                self.render_expr(&_def.expr)?;
            }
            Expression::Range(_def) => {
                // We print all of the comments we missed before the end of this
                // expression before the entire range expression.
                let end_line = _def.end.pos().line;
                if self.has_comment(end_line) {
                    self.render_missed_comments(end_line)?;
                }
                self.render_expr(&_def.start)?;
                write!(self.w, ":")?;
                if let Some(ref e) = _def.step {
                    write!(self.w, ":")?;
                    self.render_expr(e)?;
                }
                self.render_expr(&_def.end)?;
            }
            Expression::Select(_def) => {
                let val_line = _def.val.pos().line;
                if self.has_comment(val_line) {
                    self.render_missed_comments(val_line)?;
                }
                if let Some(ref e) = _def.default {
                    let default_line = e.pos().line;
                    if self.has_comment(default_line) {
                        self.render_missed_comments(default_line)?;
                    }
                }
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
        if did_indent {
            self.curr_indent -= self.indent_size;
        }
        Ok(())
    }

    pub fn render_stmt(&mut self, stmt: &Statement) -> std::io::Result<()> {
        // All statements start at the beginning of a line.
        let line = stmt.pos().line;
        self.render_comment_if_needed(line)?;
        match stmt {
            Statement::Let(def) => {
                write!(&mut self.w, "let {} = ", def.name.fragment)?;
                self.render_expr(&def.value)?;
            }
            Statement::Expression(_expr) => {
                self.render_expr(&_expr)?;
            }
            Statement::Assert(_, def) => {
                write!(&mut self.w, "assert ")?;
                self.render_expr(&def)?;
            }
            Statement::Output(_, _tok, _expr) => {
                write!(&mut self.w, "out {} ", _tok.fragment)?;
                self.render_expr(&_expr)?;
            }
        };
        write!(self.w, ";\n\n")?;
        self.last_line = line;
        Ok(())
    }

    pub fn render(&mut self, stmts: &Vec<Statement>) -> std::io::Result<()> {
        for v in stmts {
            self.render_stmt(v)?;
        }
        if let Some(last_comment_line) = self.comment_group_lines.first() {
            self.render_missed_comments(*last_comment_line + 1)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test;
