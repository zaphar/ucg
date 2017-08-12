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
pub type FieldList = Vec<(String, Expression)>; // str is expected to be a symbol
pub type SelectorList = Vec<String>; // str is expected to always be a symbol.

/// Value represents a Value in the UCG parsed AST.
#[derive(Debug,PartialEq,Clone)]
pub enum Value {
    // Constant Values
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    // Complex Values
    Tuple(FieldList),
    Selector(SelectorList),
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
            &Value::Int(ref i) => format!("{}", i),
            &Value::Float(ref f) => format!("{}", f),
            &Value::String(ref s) => format!("{}", s),
            &Value::Symbol(ref s) => format!("{}", s),
            &Value::Tuple(ref fs) => format!("{}", Self::fields_to_string(fs)),
            &Value::Selector(ref v) =>  v.join("."),
        }
    }
}

/// CallDef represents a call to a Macro that is expected to already have been
/// defined.
#[derive(PartialEq,Debug,Clone)]
pub struct CallDef {
    pub macroref: SelectorList,
    pub arglist: Vec<Expression>,
}

/// SelectDef selects a value from a tuple with a default if the value doesn't
/// exist.
#[derive(PartialEq,Debug,Clone)]
pub struct SelectDef {
    pub val: Box<Expression>,
    pub default: Box<Expression>,
    pub tuple: FieldList,
}

/// MacroDef is a pure function that always returns a Tuple.
///
/// MacroDef's are not closures. They can not reference
/// any values except what is defined in their arguments.
#[derive(PartialEq,Debug,Clone)]
pub struct MacroDef {
    pub argdefs: Vec<String>,
    pub fields: FieldList,
}

/// BinaryExpression represents an expression with a left and a right side.
#[derive(Debug,PartialEq,Clone)]
pub struct BinaryExpression(pub Value, pub Box<Expression>);

/// Expression encodes an expression. Expressions compute a value from operands.
#[derive(Debug,PartialEq,Clone)]
pub enum Expression {
    // Base Expression
    Simple(Value),

    // TODO(jwall): This should probably be all one type :-p
    // Binary Expressions
    Add(BinaryExpression),
    Sub(BinaryExpression),
    Mul(BinaryExpression),
    Div(BinaryExpression),

    // Complex Expressions
    Copy(SelectorList, FieldList),
    Grouped(Box<Expression>),

    Format(String, Vec<Expression>),

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
