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

//! The definitions of the ucg AST and Tokens.
use std;
use std::borrow::Borrow;
use std::cmp::Eq;
use std::cmp::Ordering;
use std::cmp::PartialEq;
use std::cmp::PartialOrd;
use std::convert::Into;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::PathBuf;
use std::rc::Rc;

use abortable_parser;

use crate::build::scope::Scope;
use crate::build::Val;

pub mod walk;

macro_rules! enum_type_equality {
    ( $slf:ident, $r:expr, $( $l:pat ),* ) => {
        match $slf {
        $(
            $l => {
                if let $l = $r {
                    true
                } else {
                    false
                }
            }
        )*
        }
    }
}

/// Represents a line and a column position in UCG code.
///
/// It is used for generating error messages mostly. Most all
/// parts of the UCG AST have a positioned associated with them.
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct Position {
    pub file: Option<PathBuf>,
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Position {
    /// Construct a new Position.
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Position {
            file: None,
            line: line,
            column: column,
            offset: offset,
        }
    }

    pub fn with_file<P: Into<PathBuf>>(mut self, file: P) -> Self {
        self.file = Some(file.into());
        self
    }
}

impl<'a> From<&'a Position> for Position {
    fn from(source: &'a Position) -> Self {
        source.clone()
    }
}

/// Defines the types of tokens in UCG syntax.
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub enum TokenType {
    EMPTY,
    BOOLEAN,
    END,
    WS,
    COMMENT,
    QUOTED,
    PIPEQUOTE,
    DIGIT,
    BAREWORD,
    PUNCT,
}

/// Defines a Token representing a building block of UCG syntax.
///
/// Token's are passed to the parser stage to be parsed into an AST.
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct Token {
    pub typ: TokenType,
    pub fragment: String,
    pub pos: Position,
}

impl Token {
    /// Constructs a new Token with a type and line and column information.
    pub fn new<S: Into<String>, P: Into<Position>>(f: S, typ: TokenType, p: P) -> Self {
        Self::new_with_pos(f, typ, p.into())
    }

    // Constructs a new Token with a type and a Position.
    pub fn new_with_pos<S: Into<String>>(f: S, typ: TokenType, pos: Position) -> Self {
        Token {
            typ: typ,
            fragment: f.into(),
            pos: pos,
        }
    }
}

impl abortable_parser::Positioned for Token {
    fn line(&self) -> usize {
        self.pos.line
    }
    fn column(&self) -> usize {
        self.pos.column
    }
}

impl Borrow<str> for Token {
    fn borrow(&self) -> &str {
        &self.fragment
    }
}

/// Helper macro for making a Positioned Value.
macro_rules! value_node {
    ($v:expr, $p:expr) => {
        PositionedItem::new_with_pos($v, $p)
    };
}

/// Helper macro for making a Token.
#[allow(unused_macros)]
macro_rules! make_tok {
    (EOF => $i:expr) => {
        Token::new("", TokenType::END, &$i)
    };

    (WS => $i:expr) => {
        Token::new("", TokenType::WS, &$i)
    };

    (CMT => $e:expr, $i:expr) => {
        Token::new($e, TokenType::COMMENT, &$i)
    };

    (QUOT => $e:expr, $i:expr) => {
        Token::new($e, TokenType::QUOTED, &$i)
    };

    (PUNCT => $e:expr, $i:expr) => {
        Token::new($e, TokenType::PUNCT, &$i)
    };

    (DIGIT => $e:expr, $i:expr) => {
        Token::new($e, TokenType::DIGIT, &$i)
    };

    ($e:expr, $i:expr) => {
        Token::new($e, TokenType::BAREWORD, &$i)
    };
}

/// Helper macro for making expressions.
#[allow(unused_macros)]
macro_rules! make_expr {
    ($e:expr, $i:expr) => {
        Expression::Simple(Value::Symbol(PositionedItem::new_with_pos(
            $e.to_string(),
            $i,
        )))
    };

    ($e:expr => int, $i:expr) => {
        Expression::Simple(Value::Int(PositionedItem::new_with_pos($e, $i)))
    };
}

/// An ordered list of Name = Value pairs.
///
/// This is usually used as the body of a tuple in the UCG AST.
pub type FieldList = Vec<(Token, Expression)>; // Token is expected to be a symbol

/// Represents a Value in the UCG parsed AST.
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    // Constant Values
    Empty(Position),
    Boolean(PositionedItem<bool>),
    Int(PositionedItem<i64>),
    Float(PositionedItem<f64>),
    Str(PositionedItem<String>),
    Symbol(PositionedItem<String>),
    // Complex Values
    Tuple(PositionedItem<FieldList>),
    List(ListDef),
}

impl Value {
    /// Returns the type name of the Value it is called on as a string.
    pub fn type_name(&self) -> String {
        match self {
            &Value::Empty(_) => "EmptyValue".to_string(),
            &Value::Boolean(_) => "Boolean".to_string(),
            &Value::Int(_) => "Integer".to_string(),
            &Value::Float(_) => "Float".to_string(),
            &Value::Str(_) => "String".to_string(),
            &Value::Symbol(_) => "Symbol".to_string(),
            &Value::Tuple(_) => "Tuple".to_string(),
            &Value::List(_) => "List".to_string(),
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

    fn elems_to_string(v: &Vec<Expression>) -> String {
        return format!("{}", v.len());
    }

    /// Returns a stringified version of the Value.
    pub fn to_string(&self) -> String {
        match self {
            &Value::Empty(_) => "EmptyValue".to_string(),
            &Value::Boolean(ref b) => format!("{}", b.val),
            &Value::Int(ref i) => format!("{}", i.val),
            &Value::Float(ref f) => format!("{}", f.val),
            &Value::Str(ref s) => format!("{}", s.val),
            &Value::Symbol(ref s) => format!("{}", s.val),
            &Value::Tuple(ref fs) => format!("{}", Self::fields_to_string(&fs.val)),
            &Value::List(ref def) => format!("[{}]", Self::elems_to_string(&def.elems)),
        }
    }

    /// Returns the position for a Value.
    pub fn pos(&self) -> &Position {
        match self {
            &Value::Empty(ref pos) => pos,
            &Value::Boolean(ref b) => &b.pos,
            &Value::Int(ref i) => &i.pos,
            &Value::Float(ref f) => &f.pos,
            &Value::Str(ref s) => &s.pos,
            &Value::Symbol(ref s) => &s.pos,
            &Value::Tuple(ref fs) => &fs.pos,
            &Value::List(ref def) => &def.pos,
        }
    }

    /// Returns true if called on a Value that is the same type as itself.
    pub fn type_equal(&self, target: &Self) -> bool {
        enum_type_equality!(
            self,
            target,
            &Value::Empty(_),
            &Value::Boolean(_),
            &Value::Int(_),
            &Value::Float(_),
            &Value::Str(_),
            &Value::Symbol(_),
            &Value::Tuple(_),
            &Value::List(_)
        )
    }
}

/// Represents an expansion of a Macro that is expected to already have been
/// defined.
#[derive(PartialEq, Debug, Clone)]
pub struct CallDef {
    pub funcref: Value,
    pub arglist: Vec<Expression>,
    pub pos: Position,
}

/// Encodes a select expression in the UCG AST.
#[derive(PartialEq, Debug, Clone)]
pub struct SelectDef {
    pub val: Box<Expression>,
    pub default: Option<Box<Expression>>,
    pub tuple: FieldList,
    pub pos: Position,
}

/// Adds position information to any type `T`.
#[derive(Debug, Clone)]
pub struct PositionedItem<T> {
    pub pos: Position,
    pub val: T,
}

impl<T: std::fmt::Display> std::fmt::Display for PositionedItem<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.val)
    }
}

impl<T> PositionedItem<T> {
    /// Constructs a new Positioned<T> with a value, line, and column information.
    pub fn new<P: Into<Position>>(v: T, p: P) -> Self {
        Self::new_with_pos(v, p.into())
    }

    /// Constructs a new Positioned<T> with a value and a Position.
    pub fn new_with_pos(v: T, pos: Position) -> Self {
        PositionedItem { pos: pos, val: v }
    }
}

impl<T: PartialEq> PartialEq for PositionedItem<T> {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}

impl<T: Eq> Eq for PositionedItem<T> {}

impl<T: Ord> Ord for PositionedItem<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.val.cmp(&other.val)
    }
}

impl<T: PartialOrd> PartialOrd for PositionedItem<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.val.partial_cmp(&other.val)
    }
}

impl<T: Hash> Hash for PositionedItem<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.val.hash(state);
    }
}

impl<'a> From<&'a Token> for PositionedItem<String> {
    fn from(t: &'a Token) -> PositionedItem<String> {
        PositionedItem {
            pos: t.pos.clone(),
            val: t.fragment.to_string(),
        }
    }
}

impl<'a> From<&'a PositionedItem<String>> for PositionedItem<String> {
    fn from(t: &PositionedItem<String>) -> PositionedItem<String> {
        PositionedItem {
            pos: t.pos.clone(),
            val: t.val.clone(),
        }
    }
}

/// Encodes a func expression in the UCG AST..
///
/// A func is a pure function over a tuple.
#[derive(PartialEq, Debug, Clone)]
pub struct FuncDef {
    pub scope: Option<Scope>,
    pub argdefs: Vec<PositionedItem<String>>,
    pub fields: Box<Expression>,
    pub pos: Position,
}

/// Specifies the types of binary operations supported in
/// UCG expression.
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryExprType {
    // Math
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // Boolean
    AND,
    OR,
    // Comparison
    Equal,
    GT,
    LT,
    NotEqual,
    GTEqual,
    LTEqual,
    REMatch,
    NotREMatch,
    IN,
    IS,
    // Selector operator
    DOT,
}

impl BinaryExprType {
    /// Returns the precedence level for the binary operator.
    ///
    /// Higher values bind tighter than lower values.
    pub fn precedence_level(&self) -> u32 {
        match self {
            // Equality operators are least tightly bound
            BinaryExprType::Equal => 1,
            BinaryExprType::NotEqual => 1,
            BinaryExprType::GTEqual => 1,
            BinaryExprType::LTEqual => 1,
            BinaryExprType::GT => 1,
            BinaryExprType::LT => 1,
            BinaryExprType::REMatch => 1,
            BinaryExprType::NotREMatch => 1,
            BinaryExprType::IN => 2,
            BinaryExprType::IS => 2,
            // Sum operators are next least tightly bound
            BinaryExprType::Add => 3,
            BinaryExprType::Sub => 3,
            // Product operators are next tightly bound
            BinaryExprType::Mul => 4,
            BinaryExprType::Div => 4,
            BinaryExprType::Mod => 4,
            // Boolean operators bind tighter than math
            BinaryExprType::AND => 5,
            BinaryExprType::OR => 5,
            // Dot operators are most tightly bound.
            BinaryExprType::DOT => 6,
        }
    }
}

/// Represents an expression with a left and a right side.
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOpDef {
    pub kind: BinaryExprType,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub pos: Position,
}

/// Encodes a tuple Copy expression in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct CopyDef {
    pub selector: Value,
    pub fields: FieldList,
    pub pos: Position,
}

/// Encodes one of two possible forms for format expression arguments.
#[derive(Debug, PartialEq, Clone)]
pub enum FormatArgs {
    List(Vec<Expression>),
    Single(Box<Expression>),
}

/// Encodes a format expression in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct FormatDef {
    pub template: String,
    pub args: FormatArgs,
    pub pos: Position,
}

/// Encodes an import statement in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct IncludeDef {
    pub pos: Position,
    pub path: Token,
    pub typ: Token,
}

/// Encodes a list expression in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct ListDef {
    pub elems: Vec<Expression>,
    pub pos: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FuncOpDef {
    Reduce(ReduceOpDef),
    Map(MapFilterOpDef),
    Filter(MapFilterOpDef),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReduceOpDef {
    pub func: Box<Expression>,
    pub acc: Box<Expression>,
    pub target: Box<Expression>,
    pub pos: Position,
}

/// MapFilterOpDef implements the list operations in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct MapFilterOpDef {
    pub func: Box<Expression>,
    pub target: Box<Expression>,
    pub pos: Position,
}

impl FuncOpDef {
    pub fn pos(&self) -> &Position {
        match self {
            FuncOpDef::Map(def) => &def.pos,
            FuncOpDef::Filter(def) => &def.pos,
            FuncOpDef::Reduce(def) => &def.pos,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ModuleDef {
    pub scope: Option<Scope>,
    pub pos: Position,
    pub arg_set: FieldList,
    pub out_expr: Option<Box<Expression>>,
    pub arg_tuple: Option<Rc<Val>>,
    pub statements: Vec<Statement>,
}

impl ModuleDef {
    pub fn new<P: Into<Position>>(arg_set: FieldList, stmts: Vec<Statement>, pos: P) -> Self {
        ModuleDef {
            scope: None,
            pos: pos.into(),
            arg_set: arg_set,
            out_expr: None,
            arg_tuple: None,
            statements: stmts,
        }
    }

    pub fn set_out_expr(&mut self, expr: Expression) {
        self.out_expr = Some(Box::new(expr));
    }

    pub fn imports_to_absolute(&mut self, base: PathBuf) {
        let rewrite_import = |e: &mut Expression| {
            if let Expression::Include(ref mut def) = e {
                let path = PathBuf::from(&def.path.fragment);
                if path.is_relative() {
                    def.path.fragment = base
                        .join(path)
                        .canonicalize()
                        .unwrap()
                        .to_string_lossy()
                        .to_string();
                }
            }
            if let Expression::Import(ref mut def) = e {
                let path = PathBuf::from(&def.path.fragment);
                // std/ paths are special and do not get made into absolute paths.
                if path.starts_with("std/") {
                    return;
                }
                if path.is_relative() {
                    def.path.fragment = base
                        .join(path)
                        .canonicalize()
                        .unwrap()
                        .to_string_lossy()
                        .to_string();
                }
            }
        };
        let walker = walk::AstWalker::new().with_expr_handler(&rewrite_import);
        for stmt in self.statements.iter_mut() {
            walker.walk_statement(stmt);
        }
    }
}

/// RangeDef defines a range with optional step.
#[derive(Debug, PartialEq, Clone)]
pub struct RangeDef {
    pub pos: Position,
    pub start: Box<Expression>,
    pub step: Option<Box<Expression>>,
    pub end: Box<Expression>,
}

/// Encodes an import expression in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct ImportDef {
    pub pos: Position,
    pub path: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IsDef {
    pub pos: Position,
    pub target: Box<Expression>,
    pub typ: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FailDef {
    pub pos: Position,
    pub message: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NotDef {
    pub pos: Position,
    pub expr: Box<Expression>,
}

/// Encodes a ucg expression. Expressions compute a value from.
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    // Base Expression
    Simple(Value),
    Not(NotDef),

    // Binary expressions
    Binary(BinaryOpDef),

    // Complex Expressions
    Copy(CopyDef),
    Range(RangeDef),
    // TODO(jwall): This should really store it's position :-(
    Grouped(Box<Expression>),
    Format(FormatDef),
    Include(IncludeDef),
    Import(ImportDef),
    Call(CallDef),
    Func(FuncDef),
    Select(SelectDef),
    FuncOp(FuncOpDef),
    Module(ModuleDef),

    // Declarative failure expressions
    Fail(FailDef),
}

impl Expression {
    /// Returns the position of the Expression.
    pub fn pos(&self) -> &Position {
        match self {
            &Expression::Simple(ref v) => v.pos(),
            &Expression::Binary(ref def) => &def.pos,
            &Expression::Copy(ref def) => &def.pos,
            &Expression::Range(ref def) => &def.pos,
            &Expression::Grouped(ref expr) => expr.pos(),
            &Expression::Format(ref def) => &def.pos,
            &Expression::Call(ref def) => &def.pos,
            &Expression::Func(ref def) => &def.pos,
            &Expression::Module(ref def) => &def.pos,
            &Expression::Select(ref def) => &def.pos,
            &Expression::FuncOp(ref def) => def.pos(),
            &Expression::Include(ref def) => &def.pos,
            &Expression::Import(ref def) => &def.pos,
            &Expression::Fail(ref def) => &def.pos,
            &Expression::Not(ref def) => &def.pos,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expression::Simple(ref v) => {
                write!(w, "{}", v.to_string())?;
            }
            &Expression::Binary(_) => {
                write!(w, "<Expr>")?;
            }
            &Expression::FuncOp(_) => {
                write!(w, "<Expr>")?;
            }
            &Expression::Copy(_) => {
                write!(w, "<Copy>")?;
            }
            &Expression::Range(_) => {
                write!(w, "<Range>")?;
            }
            &Expression::Grouped(_) => {
                write!(w, "(<Expr>)")?;
            }
            &Expression::Format(_) => {
                write!(w, "<Format Expr>")?;
            }
            &Expression::Call(_) => {
                write!(w, "<MacroCall>")?;
            }
            &Expression::Func(_) => {
                write!(w, "<Func>")?;
            }
            &Expression::Module(_) => {
                write!(w, "<Module>")?;
            }
            &Expression::Select(_) => {
                write!(w, "<Select>")?;
            }
            &Expression::Include(_) => {
                write!(w, "<Include>")?;
            }
            &Expression::Import(_) => {
                write!(w, "<Include>")?;
            }
            &Expression::Fail(_) => {
                write!(w, "<Fail>")?;
            }
            &Expression::Not(ref def) => {
                write!(w, "!{}", def.expr)?;
            }
        }
        Ok(())
    }
}

/// Encodes a let statement in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct LetDef {
    pub name: Token,
    pub value: Expression,
}

/// Encodes a parsed statement in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    // simple expression
    Expression(Expression),

    // Named bindings
    Let(LetDef),

    // Assert statement
    Assert(Expression),

    // Identify an Expression for output.
    Output(Position, Token, Expression),
}
