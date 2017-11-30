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
use std::convert::Into;
use std::cmp::Ordering;
use std::cmp::PartialOrd;
use std::cmp::Eq;
use std::cmp::PartialEq;
use std::hash::Hasher;
use std::hash::Hash;

#[derive(Debug,PartialEq,Eq,Clone,PartialOrd,Ord,Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug,PartialEq,Eq,Clone,PartialOrd,Ord,Hash)]
pub struct Token {
    pub fragment: String,
    pub pos: Position,
}

impl Token {
    pub fn new(f: &str, pos: Position) -> Self {
        Token {
            fragment: f.to_string(),
            pos: pos,
        }
    }
}

impl Borrow<str> for Token {
    fn borrow(&self) -> &str {
        &self.fragment
    }
}

macro_rules! value_node {
    ($v:expr, $p:expr) => {
        LocatedNode::new($v, $p)
    };
}

pub type FieldList = Vec<(Token, Expression)>; // Token is expected to be a symbol
pub type SelectorList = Vec<Token>; // Token is expected to always be a symbol.

#[derive(Debug,PartialEq,Clone)]
pub struct LocatedNode<T> {
    // TODO(jwall): Should we just use positioned instead?
    pub pos: Position,
    pub val: T,
}

impl<T> LocatedNode<T> {
    pub fn new<P: Into<Position>>(v: T, pos: P) -> Self {
        Self {
            pos: pos.into(),
            val: v,
        }
    }

    pub fn val(&self) -> &T {
        return &self.val;
    }
}


pub fn make_value_node<T>(v: T, line: usize, column: usize) -> LocatedNode<T> {
    LocatedNode::new(v,
                     Position {
                         line: line,
                         column: column,
                     })
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
            buf.push_str(&t.0.fragment);
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
            &Value::Selector(ref v) => v.val.join("."),
        }
    }

    pub fn pos(&self) -> &Position {
        match self {
            &Value::Int(ref i) => &i.pos,
            &Value::Float(ref f) => &f.pos,
            &Value::String(ref s) => &s.pos,
            &Value::Symbol(ref s) => &s.pos,
            &Value::Tuple(ref fs) => &fs.pos,
            &Value::Selector(ref v) => &v.pos,
        }
    }
}

/// CallDef represents a call to a Macro that is expected to already have been
/// defined.
#[derive(PartialEq,Debug,Clone)]
pub struct CallDef {
    pub macroref: SelectorList,
    pub arglist: Vec<Expression>,
    pub pos: Position,
}

/// SelectDef selects a value from a tuple with a default if the value doesn't
/// exist.
#[derive(PartialEq,Debug,Clone)]
pub struct SelectDef {
    pub val: Box<Expression>,
    pub default: Box<Expression>,
    pub tuple: FieldList,
    pub pos: Position,
}

// TODO(jwall): This should have a way of rendering with position information.
#[derive(Debug,Clone)]
pub struct Positioned<T> {
    pub pos: Position,
    pub val: T,
}

impl<T> Positioned<T> {
    pub fn new(v: T, pos: Position) -> Self {
        Positioned { pos: pos, val: v }
    }
}

impl<T: PartialEq> PartialEq for Positioned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}

impl<T: Eq> Eq for Positioned<T> {}

impl<T: Ord> Ord for Positioned<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.val.cmp(&other.val)
    }
}

impl<T: PartialOrd> PartialOrd for Positioned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.val.partial_cmp(&other.val)
    }
}

impl<T: Hash> Hash for Positioned<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.val.hash(state);
    }
}

impl<'a> From<&'a Token> for Positioned<String> {
    fn from(t: &'a Token) -> Positioned<String> {
        Positioned {
            pos: t.pos.clone(),
            val: t.fragment.to_string(),
        }
    }
}

impl<'a> From<&'a LocatedNode<String>> for Positioned<String> {
    fn from(t: &LocatedNode<String>) -> Positioned<String> {
        Positioned {
            pos: t.pos.clone(),
            val: t.val.clone(),
        }
    }
}

/// MacroDef is a pure function that always returns a Tuple.
///
/// MacroDef's are not closures. They can not reference
/// any values except what is defined in their arguments.
#[derive(PartialEq,Debug,Clone)]
pub struct MacroDef {
    pub argdefs: Vec<Positioned<String>>,
    pub fields: FieldList,
    pub pos: Position,
}

impl MacroDef {
    fn validate_value_symbols<'a>(&self,
                                  stack: &mut Vec<&'a Expression>,
                                  val: &'a Value)
                                  -> HashSet<String> {
        let mut bad_symbols = HashSet::new();
        if let &Value::Symbol(ref name) = val {
            let mut ok = false;
            for arg in self.argdefs.iter() {
                if arg.val == name.val {
                    ok = true;
                }
            }
            if !ok {
                bad_symbols.insert(name.val.clone());
            }
        } else if let &Value::Selector(ref sel_node) = val {
            let list = &sel_node.val;
            let mut ok = false;
            if list.len() > 0 {
                // We only look to see if the first selector item exists.
                // This is because only the first one is a symbol all of the
                // rest of the items in the selector are fields in a tuple.
                // But we don't know at this time of the value passed into
                // this macro is a tuple since this isn't a callsite.
                println!("checking selector head {}", list[0].fragment);
                for arg in self.argdefs.iter() {
                    if arg.val == list[0].fragment {
                        ok = true;
                    }
                }
                if !ok {
                    bad_symbols.insert(list[0].fragment.to_string());
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
                    }
                    &Expression::List(ref def) => {
                        stack.extend(def.elems.iter());
                    }
                    &Expression::Grouped(ref expr) => {
                        stack.push(expr);
                    }
                    &Expression::Format(ref def) => {
                        let exprs = &def.args;
                        for arg_expr in exprs.iter() {
                            stack.push(arg_expr);
                        }
                    }
                    &Expression::Select(ref def) => {
                        stack.push(def.default.borrow());
                        stack.push(def.val.borrow());
                        for &(_, ref expr) in def.tuple.iter() {
                            stack.push(expr);
                        }
                    }
                    &Expression::Copy(ref def) => {
                        let fields = &def.fields;
                        for &(_, ref expr) in fields.iter() {
                            stack.push(expr);
                        }
                    }
                    &Expression::Call(ref def) => {
                        for expr in def.arglist.iter() {
                            stack.push(expr);
                        }
                    }
                    &Expression::Simple(ref val) => {
                        let mut syms_set = self.validate_value_symbols(&mut stack, val);
                        bad_symbols.extend(syms_set.drain());
                    }
                    &Expression::Macro(_) => {
                        // noop
                        continue;
                    }
                }
            }
        }
        if bad_symbols.len() > 0 {
            return Err(bad_symbols);
        }
        return Ok(());
    }
}

#[derive(Debug,PartialEq,Clone)]
pub enum BinaryExprType {
    Add,
    Sub,
    Mul,
    Div,
}

/// BinaryOpDef represents an expression with a left and a right side.
#[derive(Debug,PartialEq,Clone)]
pub struct BinaryOpDef {
    pub kind: BinaryExprType,
    pub left: Value,
    pub right: Box<Expression>,
    pub pos: Position,
}

#[derive(Debug,PartialEq,Clone)]
pub struct CopyDef {
    pub selector: SelectorList,
    pub fields: FieldList,
    pub pos: Position,
}

#[derive(Debug,PartialEq,Clone)]
pub struct FormatDef {
    pub template: String,
    pub args: Vec<Expression>,
    pub pos: Position,
}

#[derive(Debug,PartialEq,Clone)]
pub struct ListDef {
    pub elems: Vec<Expression>,
    pub pos: Position,
}

/// Expression encodes an expression. Expressions compute a value from operands.
#[derive(Debug,PartialEq,Clone)]
pub enum Expression {
    // Base Expression
    Simple(Value),

    Binary(BinaryOpDef),

    // Complex Expressions
    Copy(CopyDef),
    Grouped(Box<Expression>),
    List(ListDef),

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
        name: Token,
        value: Expression,
    },

    // Include a file.
    Import {
        path: String,
        name: Token,
    },
}

#[cfg(test)]
mod ast_test {
    use super::*;

    #[test]
    pub fn test_macro_validation_happy_path() {
        let def = MacroDef {
            argdefs: vec![Positioned::new("foo".to_string(),
                                          Position {
                                              line: 1,
                                              column: 0,
                                          })],
            fields: vec![
                (Token::new("f1", Position { line: 1, column: 1}), Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Symbol(make_value_node("foo".to_string(), 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
                    pos: Position{line: 1, column: 0},
                })),
            ],
            pos: Position {
                line: 1,
                column: 0,
            },
        };
        assert!(def.validate_symbols().unwrap() == ());
    }

    #[test]
    pub fn test_macro_validation_fail() {
        let def = MacroDef {
            argdefs: vec![Positioned::new("foo".to_string(),
                                          Position {
                                              line: 1,
                                              column: 0,
                                          })],
            fields: vec![
                (Token::new("f1", Position{line: 1, column: 1}), Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Symbol(make_value_node("bar".to_string(), 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
                    pos: Position{line: 1, column: 0},
                })),
            ],
            pos: Position {
                line: 1,
                column: 0,
            },
        };
        let mut expected = HashSet::new();
        expected.insert("bar".to_string());
        assert_eq!(def.validate_symbols().err().unwrap(), expected);
    }

    #[test]
    pub fn test_macro_validation_selector_happy_path() {
        let def = MacroDef {
            argdefs: vec![Positioned::new("foo".to_string(),
                                          Position {
                                              line: 1,
                                              column: 0,
                                          })],
            fields: vec![
                (Token::new("f1", Position{line: 1, column: 1}), Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Selector(make_value_node(vec![
                        Token::new("foo", Position{line: 1, column: 1}),
                        Token::new("quux", Position{line: 1, column: 1})], 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
            pos: Position{line: 1, column: 0},
                })),
            ],
            pos: Position {
                line: 1,
                column: 0,
            },
        };
        assert!(def.validate_symbols().unwrap() == ());
    }

    #[test]
    pub fn test_macro_validation_selector_fail() {
        let def = MacroDef {
            argdefs: vec![Positioned::new("foo".to_string(), Position {line: 1, column: 0})],
            fields: vec![
                (Token::new("f1", Position{line: 1, column: 1}), Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Selector(make_value_node(vec![
                        Token::new("bar", Position{line: 1, column: 1}),
                        Token::new("quux", Position{line: 1, column: 1})], 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(make_value_node(1, 1, 1)))),
            pos: Position{line: 1, column: 0},
                })),
            ],
            pos: Position {
                line: 1,
                column: 0,
            },
        };
        let mut expected = HashSet::new();
        expected.insert("bar".to_string());
        assert_eq!(def.validate_symbols(), Err(expected));
    }
}
