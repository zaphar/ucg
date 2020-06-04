use crate::ast::*;

pub trait Visitor {
    // TODO(jwall): Should this have exit versions as well?
    fn visit_import(&mut self, _i: &mut ImportDef) {
        // noop by default;
    }

    fn visit_include(&mut self, _i: &mut IncludeDef) {
        // noop by default;
    }

    fn visit_fail(&mut self, _f: &mut FailDef) {
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

pub trait Walker: Visitor {
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
}

pub struct ChainedWalk<Visitor1, Visitor2> {
    pub visitor_1: Visitor1,
    pub visitor_2: Visitor2,
}

impl<Visitor1, Visitor2> ChainedWalk<Visitor1, Visitor2>
where
    Visitor1: Visitor,
    Visitor2: Visitor,
{
    pub fn new(visitor_1: Visitor1, visitor_2: Visitor2) -> Self {
        Self {
            visitor_1,
            visitor_2,
        }
    }
}

impl<Visitor1, Visitor2> Visitor for ChainedWalk<Visitor1, Visitor2>
where
    Visitor1: Visitor,
    Visitor2: Visitor,
{
    fn visit_import(&mut self, i: &mut ImportDef) {
        self.visitor_1.visit_import(i);
        self.visitor_2.visit_import(i);
    }
    fn visit_include(&mut self, i: &mut IncludeDef) {
        self.visitor_1.visit_include(i);
        self.visitor_2.visit_include(i);
    }
    fn visit_fail(&mut self, f: &mut FailDef) {
        self.visitor_1.visit_fail(f);
        self.visitor_2.visit_fail(f);
    }
    fn visit_value(&mut self, val: &mut Value) {
        self.visitor_1.visit_value(val);
        self.visitor_2.visit_value(val);
    }
    fn visit_expression(&mut self, expr: &mut Expression) {
        self.visitor_1.visit_expression(expr);
        self.visitor_2.visit_expression(expr);
    }
    fn visit_statement(&mut self, stmt: &mut Statement) {
        self.visitor_1.visit_statement(stmt);
        self.visitor_2.visit_statement(stmt);
    }
}

impl<Visitor1, Visitor2> Walker for ChainedWalk<Visitor1, Visitor2>
where
    Visitor1: Visitor,
    Visitor2: Visitor,
{
}
