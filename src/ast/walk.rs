use crate::ast::*;

pub struct AstWalker<'a> {
    handle_value: Option<&'a Fn(&mut Value)>,
    handle_expression: Option<&'a Fn(&mut Expression)>,
    handle_statment: Option<&'a Fn(&mut Statement)>,
}

impl<'a> AstWalker<'a> {
    pub fn new() -> Self {
        AstWalker {
            handle_value: None,
            handle_expression: None,
            handle_statment: None,
        }
    }

    pub fn with_value_handler(mut self, h: &'a Fn(&mut Value)) -> Self {
        self.handle_value = Some(h);
        self
    }

    pub fn with_expr_handler(mut self, h: &'a Fn(&mut Expression)) -> Self {
        self.handle_expression = Some(h);
        self
    }

    pub fn with_stmt_handler(mut self, h: &'a Fn(&mut Statement)) -> Self {
        self.handle_statment = Some(h);
        self
    }

    pub fn walk_statement(&self, stmt: &mut Statement) {
        self.visit_statement(stmt);
        match stmt {
            Statement::Let(ref mut def) => {
                self.walk_expression(&mut def.value);
            }
            Statement::Expression(ref mut expr) => {
                self.walk_expression(expr);
            }
            Statement::Assert(ref mut expr) => {
                self.walk_expression(expr);
            }
            Statement::Output(_, ref mut expr) => {
                self.walk_expression(expr);
            }
        }
    }

    fn walk_fieldset(&self, fs: &mut FieldList) {
        for &mut (_, ref mut expr) in fs.iter_mut() {
            self.walk_expression(expr);
        }
    }

    pub fn walk_expression(&self, expr: &mut Expression) {
        self.visit_expression(expr);
        match expr {
            Expression::Call(ref mut def) => {
                for expr in def.arglist.iter_mut() {
                    self.walk_expression(expr);
                }
            }
            Expression::Copy(ref mut def) => {
                self.walk_fieldset(&mut def.fields);
            }
            Expression::Format(ref mut def) => {
                for expr in def.args.iter_mut() {
                    self.walk_expression(expr);
                }
            }
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
            Expression::Grouped(ref mut expr) => {
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
                self.walk_expression(def.default.as_mut());
                self.walk_expression(def.val.as_mut());
                self.walk_fieldset(&mut def.tuple);
            }
            Expression::Simple(ref mut val) => {
                self.visit_value(val);
            }
            Expression::Import(_) | Expression::Include(_) | Expression::Fail(_) => {
                //noop
            }
            Expression::Not(ref mut def) => {
                self.walk_expression(def.expr.as_mut());
            }
        }
    }

    fn visit_value(&self, val: &mut Value) {
        if let Some(h) = self.handle_value {
            h(val);
        }
    }

    fn visit_expression(&self, expr: &mut Expression) {
        if let Some(h) = self.handle_expression {
            h(expr);
        }
    }

    fn visit_statement(&self, stmt: &mut Statement) {
        if let Some(h) = self.handle_statment {
            h(stmt);
        }
    }
}
