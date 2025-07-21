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
use std::convert::{Into, TryFrom, TryInto};
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::path::PathBuf;
use std::rc::Rc;

use abortable_parser;

use crate::build::scope::Scope;
use crate::build::Val;
use crate::error::{BuildError, ErrorType::TypeFail};

pub mod printer;
pub mod rewrite;
pub mod typecheck;
pub mod walk;

#[derive(Debug, PartialEq, Clone)]
pub enum TemplatePart {
    Str(Vec<char>),
    PlaceHolder(usize),
    Expression(Expression),
}

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

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if let Some(ref file) = self.file {
            write!(f, "file: {} ", file.to_string_lossy().to_string())?;
        }
        write!(f, "line: {} column: {}", self.line, self.column)
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

pub type ShapeTuple = Vec<(Token, Shape)>;
pub type ShapeList = Vec<Shape>;

#[derive(PartialEq, Debug, Clone)]
pub struct FuncShapeDef {
    args: Vec<Shape>,
    ret: Box<Shape>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ModuleShapeDef {
    items: ShapeTuple,
    ret: Box<Shape>,
}

macro_rules! value_enum {
    ($doc:meta $i:tt, $t:ty, $l:ty, $($extra:tt)*) => {
        #[$doc]
        #[derive(PartialEq, Debug, Clone)]
        pub enum $i {
            // Simple Values
            Empty(Position),
            Boolean(PositionedItem<bool>),
            Int(PositionedItem<i64>),
            Float(PositionedItem<f64>),
            Str(PositionedItem<String>),
            Symbol(PositionedItem<String>),
            // Complex Values
            Tuple(PositionedItem<$t>),
            List($l),
            // Extra items
            $( $extra )*
        }
    }
}

value_enum!(
    doc="Value types represent the Values that UCG can have."
    Value,
    FieldList,
    ListDef,
);

value_enum!(
    doc="Shapes represent the types that UCG values or expressions can have."
    Shape,
    ShapeTuple,
    PositionedItem<ShapeList>,
    Func(FuncShapeDef),
    Module(ModuleShapeDef),
);

impl Shape {
    pub fn merge(&self, compare: &Shape) -> Option<Self> {
        match (self, compare) {
            (Shape::Str(_), Shape::Str(_))
            | (Shape::Symbol(_), Shape::Symbol(_))
            | (Shape::Boolean(_), Shape::Boolean(_))
            | (Shape::Empty(_), Shape::Empty(_))
            | (Shape::Int(_), Shape::Int(_))
            | (Shape::Float(_), Shape::Float(_)) => Some(self.clone()),
            (Shape::List(left_slist), Shape::List(right_slist)) => {
                // TODO
                unimplemented!("Can't merge these yet.")
            }
            (Shape::Tuple(left_slist), Shape::Tuple(right_slist)) => {
                // TODO
                unimplemented!("Can't merge these yet.")
            }
            (Shape::Func(left_opshape), Shape::Func(right_opshape)) => {
                // TODO
                unimplemented!("Can't merge these yet.")
            }
            (Shape::Module(left_opshape), Shape::Module(right_opshape)) => {
                // TODO
                unimplemented!("Can't merge these yet.")
            }
            _ => None,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Shape::Str(s) => "str",
            Shape::Symbol(s) => "symbol",
            Shape::Int(s) => "int",
            Shape::Float(s) => "float",
            Shape::Boolean(b) => "boolean",
            Shape::Empty(p) => "nil",
            // TODO(jwall): make these type names account for what they contain.
            Shape::List(lst) => "list",
            Shape::Tuple(flds) => "tuple",
            Shape::Func(_) => "func",
            Shape::Module(_) => "module",
        }
    }

    pub fn pos(&self) -> &Position {
        match self {
            Shape::Str(s) => &s.pos,
            Shape::Symbol(s) => &s.pos,
            Shape::Int(s) => &s.pos,
            Shape::Float(s) => &s.pos,
            Shape::Boolean(b) => &b.pos,
            Shape::Empty(p) => p,
            Shape::List(lst) => &lst.pos,
            Shape::Tuple(flds) => &flds.pos,
            Shape::Func(def) => def.ret.pos(),
            Shape::Module(def) => def.ret.pos(),
        }
    }

    pub fn with_pos(self, pos: Position) -> Self {
        match self {
            Shape::Str(s) => Shape::Str(PositionedItem::new(s.val, pos)),
            Shape::Symbol(s) => Shape::Symbol(PositionedItem::new(s.val, pos)),
            Shape::Int(s) => Shape::Int(PositionedItem::new(s.val, pos)),
            Shape::Float(s) => Shape::Float(PositionedItem::new(s.val, pos)),
            Shape::Boolean(b) => Shape::Boolean(PositionedItem::new(b.val, pos)),
            Shape::Empty(p) => Shape::Empty(pos),
            Shape::List(lst) => Shape::List(PositionedItem::new(lst.val, pos)),
            Shape::Tuple(flds) => Shape::Tuple(PositionedItem::new(flds.val, pos)),
            Shape::Func(_) | Shape::Module(_) => self.clone(),
        }
    }
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

    fn derive_shape(&self) -> Result<Shape, BuildError> {
        let shape = match self {
            Value::Empty(p) => Shape::Empty(p.clone()),
            Value::Boolean(p) => Shape::Boolean(p.clone()),
            Value::Int(p) => Shape::Int(p.clone()),
            Value::Float(p) => Shape::Float(p.clone()),
            Value::Str(p) => Shape::Str(p.clone()),
            // Symbols in a shape are placeholders. They allow a form of genericity
            // in the shape. They can be any type and are only refined down.
            // by their presence in an expression.
            Value::Symbol(p) => Shape::Symbol(p.clone()),
            Value::Tuple(flds) => {
                let mut field_shapes = Vec::new();
                for &(ref tok, ref expr) in &flds.val {
                    field_shapes.push((tok.clone(), expr.try_into()?));
                }
                Shape::Tuple(PositionedItem::new(field_shapes, flds.pos.clone()))
            }
            Value::List(flds) => {
                let mut field_shapes = Vec::new();
                for f in &flds.elems {
                    field_shapes.push(f.try_into()?);
                }
                Shape::List(PositionedItem::new(field_shapes, flds.pos.clone()))
            }
        };
        Ok(shape)
    }
}

impl TryFrom<&Value> for Shape {
    type Error = crate::error::BuildError;

    fn try_from(v: &Value) -> Result<Self, Self::Error> {
        v.derive_shape()
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

/// The allowable types to which you can perform a primitive cast.
#[derive(PartialEq, Debug, Clone)]
pub enum CastType {
    Int,
    Float,
    Str,
    Bool,
}

impl fmt::Display for CastType {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(
            w,
            "{}",
            match self {
                CastType::Int => "int",
                CastType::Float => "float",
                CastType::Bool => "bool",
                CastType::Str => "str",
            }
        )
    }
}

/// Represents a cast of a target to a primitive type.
#[derive(PartialEq, Debug, Clone)]
pub struct CastDef {
    pub cast_type: CastType,
    pub target: Box<Expression>,
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
}

pub struct Rewriter {
    base: PathBuf,
}

impl Rewriter {
    pub fn new<P: Into<PathBuf>>(base: P) -> Self {
        Self { base: base.into() }
    }
}

fn normalize_path(p: PathBuf) -> PathBuf {
    let mut normalized = PathBuf::new();
    for segment in p.components() {
        normalized.push(segment);
    }
    return normalized;
}

impl walk::Walker for Rewriter {
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

#[derive(Debug, PartialEq, Clone)]
pub struct DebugDef {
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
    Grouped(Box<Expression>, Position),
    Format(FormatDef),
    Include(IncludeDef),
    Import(ImportDef),
    Call(CallDef),
    Cast(CastDef),
    Func(FuncDef),
    Select(SelectDef),
    FuncOp(FuncOpDef),
    Module(ModuleDef),

    // Declarative failure expressions
    Fail(FailDef),
    // Debugging assistance
    Debug(DebugDef),
}

impl Expression {
    /// Returns the position of the Expression.
    pub fn pos(&self) -> &Position {
        match self {
            &Expression::Simple(ref v) => v.pos(),
            &Expression::Binary(ref def) => &def.pos,
            &Expression::Copy(ref def) => &def.pos,
            &Expression::Range(ref def) => &def.pos,
            &Expression::Grouped(_, ref pos) => pos,
            &Expression::Format(ref def) => &def.pos,
            &Expression::Call(ref def) => &def.pos,
            &Expression::Cast(ref def) => &def.pos,
            &Expression::Func(ref def) => &def.pos,
            &Expression::Module(ref def) => &def.pos,
            &Expression::Select(ref def) => &def.pos,
            &Expression::FuncOp(ref def) => def.pos(),
            &Expression::Include(ref def) => &def.pos,
            &Expression::Import(ref def) => &def.pos,
            &Expression::Fail(ref def) => &def.pos,
            &Expression::Not(ref def) => &def.pos,
            &Expression::Debug(ref def) => &def.pos,
        }
    }

    fn derive_shape(&self) -> Result<Shape, BuildError> {
        // FIXME(jwall): Implement this
        let shape = match self {
            Expression::Simple(v) => v.try_into()?,
            Expression::Format(def) => {
                Shape::Str(PositionedItem::new("".to_owned(), def.pos.clone()))
            }
            Expression::Not(def) => {
                let shape = def.expr.as_ref().try_into()?;
                if let Shape::Boolean(b) = shape {
                    Shape::Boolean(PositionedItem::new(!b.val, def.pos.clone()))
                } else {
                    // TODO(jwall): Display implementations for shapes.
                    return Err(BuildError::new(
                        format!(
                            "Expected Boolean value in Not expression but got: {:?}",
                            shape
                        ),
                        TypeFail,
                    ));
                }
            }
            Expression::Grouped(v, _pos) => v.as_ref().try_into()?,
            Expression::Range(def) => Shape::List(PositionedItem::new(
                vec![Shape::Int(PositionedItem::new(0, def.start.pos().clone()))],
                def.pos.clone(),
            )),
            Expression::Cast(def) => match def.cast_type {
                CastType::Int => Shape::Int(PositionedItem::new(0, def.pos.clone())),
                CastType::Str => Shape::Str(PositionedItem::new("".to_owned(), def.pos.clone())),
                CastType::Float => Shape::Float(PositionedItem::new(0.0, def.pos.clone())),
                CastType::Bool => Shape::Boolean(PositionedItem::new(true, def.pos.clone())),
            },
            Expression::Import(def) => {
                // FIXME(jwall): What should this do?
                Shape::Symbol(PositionedItem::new(
                    def.path.fragment.clone(),
                    def.path.pos.clone(),
                ))
            }
            _ => Shape::Empty(Position::new(0, 0, 0)),
        };
        Ok(shape)
    }
}

impl TryFrom<&Expression> for Shape {
    type Error = crate::error::BuildError;

    fn try_from(e: &Expression) -> Result<Self, Self::Error> {
        e.derive_shape()
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
            &Expression::Grouped(_, _) => {
                write!(w, "(<Expr>)")?;
            }
            &Expression::Format(_) => {
                write!(w, "<Format Expr>")?;
            }
            &Expression::Call(_) => {
                write!(w, "<FuncCall>")?;
            }
            &Expression::Cast(_) => {
                write!(w, "<Cast>")?;
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
            &Expression::Debug(ref def) => {
                write!(w, "!{}", def.expr)?;
            }
        }
        Ok(())
    }
}

/// Encodes a let statement in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct LetDef {
    pub pos: Position,
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
    Assert(Position, Expression),

    // Identify an Expression for output.
    Output(Position, Token, Expression),

    // Print the expression to stdout.
    Print(Position, Token, Expression),
}

impl Statement {
    fn pos(&self) -> &Position {
        match self {
            Statement::Expression(ref e) => e.pos(),
            Statement::Let(ref def) => &def.pos,
            Statement::Assert(ref pos, _) => pos,
            Statement::Output(ref pos, _, _) => pos,
            Statement::Print(ref pos, _, _) => pos,
        }
    }
}

#[cfg(test)]
mod test;
