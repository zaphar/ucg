// Copyright 2017 Jeremy Wall <jeremy@marzhillstudios.com>
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
use std::collections::HashSet;
use std::borrow::Borrow;

#[derive(Debug,PartialEq,Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

pub type FieldList = Vec<(String, Expression)>; // str is expected to be a symbol
pub type SelectorList = Vec<String>; // str is expected to always be a symbol.

#[derive(Debug,PartialEq,Clone)]
pub struct LocatedNode<T> {
    pub pos: Option<Position>,
    pub val: T,
}

impl<T> LocatedNode<T> {
    pub fn new(v: T) -> Self {
        Self {
            pos: None,
            val: v,
        }
    }

    pub fn new_with_pos(v: T, pos: Position) -> Self {
        Self {
            pos: Some(pos),
            val: v,
        }
    }
}


pub fn make_value_node<T>(v: T) -> LocatedNode<T> {
    LocatedNode::new(v)
}

/// Value represents a Value in the UCG parsed AST.
#[derive(Debug,PartialEq,Clone)]
pub enum Value {
    // Constant Values
    Int(LocatedNode<i64>),
    Float(LocatedNode<f64>),
    String(LocatedNode<String>),
    Symbol(LocatedNode<String>),
    // Complex Values
    Tuple(LocatedNode<FieldList>),
    Selector(LocatedNode<SelectorList>),
}

impl Value {
    pub fn type_name(&self) -> String {
        match self {
            &Value::Int(_) => "Integer".to_string(),
            &Value::Float(_) => "Float".to_string(),
            &Value::String(_) => "String".to_string(),
            &Value::Symbol(_) => "Symbol".to_string(),
            &Value::Tuple(_) => "Tuple".to_string(),
            &Value::Selector(_) => "Selector".to_string(),
        }
    }

    fn fields_to_string(v: &FieldList) -> String {
        let mut buf = String::new();
        buf.push_str("{\n");
        for ref t in v.iter() {
            buf.push_str("\t");
            buf.push_str(&t.0);
            buf.push_str("\n");
        }
        buf.push_str("}");
        return buf;
    }

    pub fn to_string(&self) -> String {
        match self {
            &Value::Int(ref i) => format!("{}", i.val),
            &Value::Float(ref f) => format!("{}", f.val),
            &Value::String(ref s) => format!("{}", s.val),
            &Value::Symbol(ref s) => format!("{}", s.val),
            &Value::Tuple(ref fs) => format!("{}", Self::fields_to_string(&fs.val)),
            &Value::Selector(ref v) =>  v.val.join("."),
        }
    }
}

/// CallDef represents a call to a Macro that is expected to already have been
/// defined.
#[derive(PartialEq,Debug,Clone)]
pub struct CallDef {
    pub macroref: SelectorList,
    pub arglist: Vec<Expression>,
    pub pos: Option<Position>,
}

/// SelectDef selects a value from a tuple with a default if the value doesn't
/// exist.
#[derive(PartialEq,Debug,Clone)]
pub struct SelectDef {
    pub val: Box<Expression>,
    pub default: Box<Expression>,
    pub tuple: FieldList,
    pub pos: Option<Position>,
}

/// MacroDef is a pure function that always returns a Tuple.
///
/// MacroDef's are not closures. They can not reference
/// any values except what is defined in their arguments.
#[derive(PartialEq,Debug,Clone)]
pub struct MacroDef {
    pub argdefs: Vec<String>,
    pub fields: FieldList,
    pub pos: Option<Position>,
}

impl MacroDef {
    fn validate_value_symbols<'a>(&'a self, stack: &mut Vec<&'a Expression>, val: &'a Value) -> HashSet<String> {
        let mut bad_symbols = HashSet::new();
        if let &Value::Symbol(ref name) = val {
            let mut ok = true;
            for arg in self.argdefs.iter() {
                ok &= arg == &name.val
            }
            if !ok {
                bad_symbols.insert(name.val.clone());
            }
        } else if let &Value::Selector(ref sel_node) = val {
            let list = &sel_node.val;
            let mut ok = true;
            if list.len() > 0 {
                // We only look to see if the first selector item exists.
                // This is because only the first one is a symbol all of the
                // rest of the items in the selector are fields in a tuple.
                // But we don't know at this time of the value passed into
                // this macro is a tuple since this isn't a callsite.
                for arg in self.argdefs.iter() {
                    ok &= arg == &list[0]
                }
                if !ok {
                    bad_symbols.insert(list[0].clone());
                }
            }
        } else if let &Value::Tuple(ref tuple_node) = val {
            let fields = &tuple_node.val;
            for &(_, ref expr) in fields.iter() {
                stack.push(expr);
            }
        }
        return bad_symbols;
    }

    pub fn validate_symbols(&self) -> Result<(), HashSet<String>> {
        let mut bad_symbols = HashSet::new();
        for &(_, ref expr) in self.fields.iter() {
            let mut stack = Vec::new();
            stack.push(expr);
            while stack.len() > 0 {
                match stack.pop().unwrap() {
                    &Expression::Binary(ref bexpr) => {
                        let mut syms_set = self.validate_value_symbols(&mut stack, &bexpr.left);
                        bad_symbols.extend(syms_set.drain());
                        stack.push(&bexpr.right);
                    },
                    &Expression::Grouped(ref expr) => {
                        stack.push(expr);
                    },
                    &Expression::Format(ref def) => {
                        let exprs = &def.args;
                        for arg_expr in exprs.iter() {
                            stack.push(arg_expr);
                        }
                    },
                    &Expression::Select(ref def) => {
                        stack.push(def.default.borrow());
                        stack.push(def.val.borrow());
                        for &(_, ref expr) in def.tuple.iter() {
                            stack.push(expr);
                        }
                    },
                    &Expression::Copy(ref def) => {
                        let fields = &def.fields;
                        for &(_, ref expr) in fields.iter() {
                            stack.push(expr);
                        }
                    },
                    &Expression::Call(ref def) => {
                        for expr in def.arglist.iter() {
                            stack.push(expr);
                        }
                    }
                    &Expression::Simple(ref val) => {
                        let mut syms_set = self.validate_value_symbols(&mut stack, val);
                        bad_symbols.extend(syms_set.drain());
                    },
                    &Expression::Macro(_) => {
                        // noop
                        continue;
                    },
                }
            }
        }
        if bad_symbols.len() > 0 {
            return Err(bad_symbols);
        }
        return Ok(())
    }
}

#[derive(Debug,PartialEq,Clone)]
pub enum BinaryExprType {
    Add,
    Sub,
    Mul,
    Div,
}

/// BinaryExpression represents an expression with a left and a right side.
#[derive(Debug,PartialEq,Clone)]
pub struct BinaryExpression {
    pub kind: BinaryExprType,
    pub left: Value,
    pub right: Box<Expression>,
    pub pos: Option<Position>,
}

#[derive(Debug,PartialEq,Clone)]
pub struct CopyDef {
    pub selector: SelectorList,
    pub fields: FieldList,
    pub pos: Option<Position>,
}

#[derive(Debug,PartialEq,Clone)]
pub struct FormatDef {
    pub template: String,
    pub args: Vec<Expression>,
    pub pos: Option<Position>,
}

/// Expression encodes an expression. Expressions compute a value from operands.
#[derive(Debug,PartialEq,Clone)]
pub enum Expression {
    // Base Expression
    Simple(Value),

    Binary(BinaryExpression),

    // Complex Expressions
    Copy(CopyDef),
    Grouped(Box<Expression>),

    Format(FormatDef),

    Call(CallDef),

    Macro(MacroDef),
    Select(SelectDef),
}

/// Statement encodes a parsed Statement in the UCG AST.
#[derive(Debug,PartialEq)]
pub enum Statement {
    // simple expression
    Expression(Expression),

    // Named bindings
    Let {
        name: String,
        value: Expression,
    },

    // Include a file.
    Import {
        path: String,
        name: String,
    },
}

#[cfg(test)]
mod ast_test {
    use super::*;

    #[test]
    pub fn test_macro_validation_happy_path() {
        let def = MacroDef{
            argdefs: vec![
                "foo".to_string()
            ],
            fields: vec![
                ("f1".to_string(), Expression::Binary(BinaryExpression{
                    kind: BinaryExprType::Add,
                    left: Value::Symbol(make_value_node("foo".to_string())),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1)))),
                    pos: None,
                })),
            ],
            pos: None,
        };
        assert!(def.validate_symbols().unwrap() == ());
    }

    #[test]
    pub fn test_macro_validation_fail() {
        let def = MacroDef{
            argdefs: vec![
                "foo".to_string()
            ],
            fields: vec![
                ("f1".to_string(), Expression::Binary(BinaryExpression{
                    kind: BinaryExprType::Add,
                    left: Value::Symbol(make_value_node("bar".to_string())),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1)))),
                    pos: None,
                })),
            ],
            pos: None,

        };
        let mut expected = HashSet::new();
        expected.insert("bar".to_string());
        assert_eq!(def.validate_symbols().err().unwrap(), expected);
    }

    #[test]
    pub fn test_macro_validation_selector_happy_path() {
        let def = MacroDef{
            argdefs: vec![
                "foo".to_string()
            ],
            fields: vec![
                ("f1".to_string(), Expression::Binary(BinaryExpression{
                    kind: BinaryExprType::Add,
                    left: Value::Selector(make_value_node(vec!["foo".to_string(), "quux".to_string()])),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1)))),
                    pos: None,
                })),
            ],
            pos: None,
        };
        assert!(def.validate_symbols().unwrap() == ());
    }

    #[test]
    pub fn test_macro_validation_selector_fail() {
        let def = MacroDef{
            argdefs: vec![
                "foo".to_string()
            ],
            fields: vec![
                ("f1".to_string(), Expression::Binary(BinaryExpression{
                    kind: BinaryExprType::Add,
                    left: Value::Selector(make_value_node(vec!["bar".to_string(), "quux".to_string()])),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1)))),
                    pos: None,
                })),
            ],
            pos: None,
        };
        let mut expected = HashSet::new();
        expected.insert("bar".to_string());
        assert_eq!(def.validate_symbols().err().unwrap(), expected);
    }
}
