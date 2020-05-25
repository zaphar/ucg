use crate::ast::*;

pub trait Walker {
    fn walk_statement_list(&mut self, stmts: Vec<&mut Statement>) {
        for v in stmts {
            self.walk_statement(v);
        }
    }

    fn walk_statement(&mut self, stmt: &mut Statement) {
        self.visit_statement(stmt);
        match stmt {
            Statement::Let(ref mut def) => {
                self.walk_expression(&mut def.value);
            }
            Statement::Expression(ref mut expr) => {
                self.walk_expression(expr);
            }
            Statement::Assert(_, ref mut expr) => {
                self.walk_expression(expr);
            }
            Statement::Output(_, _, ref mut expr) => {
                self.walk_expression(expr);
            }
            Statement::Print(_, _, ref mut expr) => {
                self.walk_expression(expr);
            }
        }
    }

    fn walk_fieldset(&mut self, fs: &mut FieldList) {
        for &mut (_, ref mut expr) in fs.iter_mut() {
            self.walk_expression(expr);
        }
    }

    fn walk_expression(&mut self, expr: &mut Expression) {
        self.visit_expression(expr);
        match expr {
            Expression::Call(ref mut def) => {
                for expr in def.arglist.iter_mut() {
                    self.walk_expression(expr);
                }
            }
            Expression::Cast(ref mut def) => {
                self.walk_expression(&mut def.target);
            }
            Expression::Copy(ref mut def) => {
                self.walk_fieldset(&mut def.fields);
            }
            Expression::Format(ref mut def) => match def.args {
                FormatArgs::List(ref mut args) => {
                    for expr in args.iter_mut() {
                        self.walk_expression(expr);
                    }
                }
                FormatArgs::Single(ref mut expr) => {
                    self.walk_expression(expr);
                }
            },
            Expression::FuncOp(ref mut def) => match def {
                FuncOpDef::Reduce(ref mut def) => {
                    self.walk_expression(def.target.as_mut());
                    self.walk_expression(def.acc.as_mut())
                }
                FuncOpDef::Map(ref mut def) => {
                    self.walk_expression(def.target.as_mut());
                }
                FuncOpDef::Filter(ref mut def) => {
                    self.walk_expression(def.target.as_mut());
                }
            },
            Expression::Binary(ref mut def) => {
                self.walk_expression(def.left.as_mut());
                self.walk_expression(def.right.as_mut());
            }
            Expression::Grouped(ref mut expr, _) => {
                self.walk_expression(expr);
            }
            Expression::Func(ref mut def) => self.walk_expression(def.fields.as_mut()),
            Expression::Module(ref mut def) => {
                self.walk_fieldset(&mut def.arg_set);
                for stmt in def.statements.iter_mut() {
                    self.walk_statement(stmt);
                }
            }
            Expression::Range(ref mut def) => {
                self.walk_expression(def.start.as_mut());
                self.walk_expression(def.end.as_mut());
                if let Some(ref mut expr) = def.step {
                    self.walk_expression(expr.as_mut());
                }
            }
            Expression::Select(ref mut def) => {
                match def.default {
                    Some(ref mut e) => {
                        self.walk_expression(e.as_mut());
                    }
                    None => {
                        // noop;
                    }
                };
                self.walk_expression(def.val.as_mut());
                self.walk_fieldset(&mut def.tuple);
            }
            Expression::Simple(ref mut val) => {
                self.walk_value(val);
            }

            Expression::Import(i) => {
                self.visit_import(i);
            }
            Expression::Include(i) => {
                self.visit_include(i);
            }
            Expression::Fail(f) => {
                self.visit_fail(f);
            }
            Expression::Not(ref mut def) => {
                self.walk_expression(def.expr.as_mut());
            }
            Expression::Debug(ref mut def) => {
                self.walk_expression(&mut def.expr);
            }
        }
    }

    fn walk_value(&mut self, val: &mut Value) {
        match val {
            Value::Empty(_)
            | Value::Symbol(_)
            | Value::Boolean(_)
            | Value::Int(_)
            | Value::Float(_)
            | Value::Str(_) => self.visit_value(val),
            Value::Tuple(fs) => self.walk_fieldset(&mut fs.val),
            Value::List(vs) => {
                for e in &mut vs.elems {
                    self.walk_expression(e);
                }
            }
        }
    }

    fn visit_import(&mut self, i: &mut ImportDef) {
        // noop by default;
    }

    fn visit_include(&mut self, i: &mut IncludeDef) {
        // noop by default;
    }

    fn visit_fail(&mut self, f: &mut FailDef) {
        // noop by default;
    }

    fn visit_value(&mut self, val: &mut Value);

    fn visit_expression(&mut self, expr: &mut Expression);

    fn visit_statement(&mut self, stmt: &mut Statement);
}

// TODO this would be better implemented as a Trait I think.
pub struct AstWalker<'a> {
    handle_value: Option<&'a dyn Fn(&mut Value)>,
    handle_expression: Option<&'a dyn Fn(&mut Expression)>,
    handle_statment: Option<&'a dyn Fn(&mut Statement)>,
}

impl<'a> AstWalker<'a> {
    pub fn new() -> Self {
        AstWalker {
            handle_value: None,
            handle_expression: None,
            handle_statment: None,
        }
    }

    pub fn with_value_handler(mut self, h: &'a dyn Fn(&mut Value)) -> Self {
        self.handle_value = Some(h);
        self
    }

    pub fn with_expr_handler(mut self, h: &'a dyn Fn(&mut Expression)) -> Self {
        self.handle_expression = Some(h);
        self
    }

    pub fn with_stmt_handler(mut self, h: &'a dyn Fn(&mut Statement)) -> Self {
        self.handle_statment = Some(h);
        self
    }
}

impl<'a> Walker for AstWalker<'a> {
    fn visit_value(&mut self, val: &mut Value) {
        if let Some(h) = self.handle_value {
            h(val);
        }
    }

    fn visit_expression(&mut self, expr: &mut Expression) {
        if let Some(h) = self.handle_expression {
            h(expr);
        }
    }

    fn visit_statement(&mut self, stmt: &mut Statement) {
        if let Some(h) = self.handle_statment {
            h(stmt);
        }
    }
}
