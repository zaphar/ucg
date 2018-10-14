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
use std::collections::HashSet;
use std::convert::Into;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

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
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Position {
    /// Construct a new Position.
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Position {
            line: line,
            column: column,
            offset: offset,
        }
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

impl Borrow<str> for Token {
    fn borrow(&self) -> &str {
        &self.fragment
    }
}

/// Helper macro for making a Positioned Value.
macro_rules! value_node {
    ($v:expr, $p:expr) => {
        Positioned::new_with_pos($v, $p)
    };
}

/// Helper macro for making a Token.
#[allow(unused_macros)]
macro_rules! make_tok {
    (EOF => $i:expr) => {
        Token::new("", TokenType::END, $i)
    };

    (WS => $i:expr) => {
        Token::new("", TokenType::WS, $i)
    };

    (CMT => $e:expr, $i:expr) => {
        Token::new($e, TokenType::COMMENT, $i)
    };

    (QUOT => $e:expr, $i:expr) => {
        Token::new($e, TokenType::QUOTED, $i)
    };

    (PUNCT => $e:expr, $i:expr) => {
        Token::new($e, TokenType::PUNCT, $i)
    };

    (DIGIT => $e:expr, $i:expr) => {
        Token::new($e, TokenType::DIGIT, $i)
    };

    ($e:expr, $i:expr) => {
        Token::new($e, TokenType::BAREWORD, $i)
    };
}

/// Helper macro for making expressions.
#[allow(unused_macros)]
macro_rules! make_expr {
    ($e:expr, $i:expr) => {
        Expression::Simple(Value::Symbol(Positioned::new_with_pos($e.to_string(), $i)))
    };

    ($e:expr => int, $i:expr) => {
        Expression::Simple(Value::Int(Positioned::new_with_pos($e, $i)))
    };
}

/// Helper macro for making selectors.
///
/// ```
/// make_selector!(Token::new("tpl", 1, 1), Token::new("fld", 1, 4));
///
/// make_selector!(Token::new("tpl", 1, 1), vec![Token::new("fld", 1, 4)], => 1, 1);
///
/// make_selector!(foo", ["bar"]);
///
/// make_selector!(foo", ["bar"] => 1, 0);
/// ```
#[allow(unused_macros)]
macro_rules! make_selector {
    ( $h:expr, $i:expr) => {
        SelectorDef::new(
            SelectorList{head: Box::new($h), tail: None},
            $i)
    };

    ( $h: expr, $list:expr, $i:expr) => {
        SelectorDef::new(
            SelectorList{head: Box::new($h), tail: Some($list)},
            $i)
    };

    // Tokens
    ( $h:expr => [ $( $item:expr ),* ], $i:expr ) => {
        {
            make_selector!($h => [ $( $item, )* ] => $i)
        }
    };

    ( $h:expr => [ $( $item:expr ),* ] => $i:expr ) => {
        {
            let mut list: Vec<Token> = Vec::new();

            $(
                list.push($item);
            )*

            make_selector!($h, list, $i)
        }
    };

    // Strings not tokens
    ( $h:expr => $( $item:expr ),* ) => {
        {

            let mut col = 1;
            let mut list: Vec<Token> = Vec::new();

            $(
                list.push(make_tok!($item, Position::new(1, col, col)));
                col += $item.len() + 1;
            )*

            // Shut up the lint about unused code;
            assert!(col != 0);

            make_selector!($h, list, Position::new(1, 1, 1))
        }

    };

    ( $h:expr => $( $item:expr ),* => $l:expr, $c:expr ) => {
        {
            let mut col = $c;
            let mut list: Vec<Token> = Vec::new();

            $(
                list.push(make_tok!($item, Position::new($l, col, col)));
                col += $item.len() + 1;
            )*

            // Shut up the linter about unused code;
            assert!(col != 0);

            make_selector!($h, list, Position::new($l, $c, $c))
        }
    };
}

/// An Expression with a series of symbols specifying the key
/// with which to descend into the result of the expression.
///
/// The expression must evaluate to either a tuple or an array. The token must
/// evaluate to either a bareword Symbol or an Int.
///
/// ```ucg
/// let foo = { bar = "a thing" };
/// let thing = foo.bar;
///
/// let arr = ["one", "two"];
/// let first = arr.0;
///
/// let berry = {best = "strawberry", unique = "acai"}.best;
/// let third = ["uno", "dos", "tres"].1;
/// '''
#[derive(PartialEq, Clone)]
pub struct SelectorList {
    pub head: Box<Expression>,
    pub tail: Option<Vec<Token>>,
}

impl SelectorList {
    /// Returns a stringified version of a SelectorList.
    pub fn to_string(&self) -> String {
        "TODO".to_string()
    }
}

impl fmt::Debug for SelectorList {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        write!(w, "Selector({})", self)
    }
}

impl fmt::Display for SelectorList {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(w, "{}", self.head));
        if let Some(ref tok_vec) = self.tail {
            for t in tok_vec.iter() {
                try!(write!(w, ".{}", t.fragment));
            }
        }
        return Ok(());
    }
}

/// An ordered list of Name = Value pairs.
///
/// This is usually used as the body of a tuple in the UCG AST.
pub type FieldList = Vec<(Token, Expression)>; // Token is expected to be a symbol

/// Encodes a selector expression in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct SelectorDef {
    pub pos: Position,
    pub sel: SelectorList,
}

impl SelectorDef {
    /// Constructs a new SelectorDef.
    pub fn new<P: Into<Position>>(sel: SelectorList, p: P) -> Self {
        SelectorDef {
            pos: p.into(),
            sel: sel,
        }
    }
}

/// Represents a Value in the UCG parsed AST.
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    // Constant Values
    Empty(Position),
    Boolean(Positioned<bool>),
    Int(Positioned<i64>),
    Float(Positioned<f64>),
    Str(Positioned<String>),
    Symbol(Positioned<String>),
    // Complex Values
    Tuple(Positioned<FieldList>),
    List(ListDef),
    Selector(SelectorDef),
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
            &Value::Selector(ref v) => v.sel.to_string(),
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
            &Value::Selector(ref v) => &v.pos,
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
            &Value::List(_),
            &Value::Selector(_)
        )
    }
}

/// Represents an expansion of a Macro that is expected to already have been
/// defined.
#[derive(PartialEq, Debug, Clone)]
pub struct CallDef {
    pub macroref: SelectorDef,
    pub arglist: Vec<Expression>,
    pub pos: Position,
}

/// Encodes a select expression in the UCG AST.
#[derive(PartialEq, Debug, Clone)]
pub struct SelectDef {
    pub val: Box<Expression>,
    pub default: Box<Expression>,
    pub tuple: FieldList,
    pub pos: Position,
}

/// Adds position information to any type `T`.
#[derive(Debug, Clone)]
pub struct Positioned<T> {
    pub pos: Position,
    pub val: T,
}

impl<T: std::fmt::Display> std::fmt::Display for Positioned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.val)
    }
}

impl<T> Positioned<T> {
    /// Constructs a new Positioned<T> with a value, line, and column information.
    pub fn new<P: Into<Position>>(v: T, p: P) -> Self {
        Self::new_with_pos(v, p.into())
    }

    /// Constructs a new Positioned<T> with a value and a Position.
    pub fn new_with_pos(v: T, pos: Position) -> Self {
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

impl<'a> From<&'a Positioned<String>> for Positioned<String> {
    fn from(t: &Positioned<String>) -> Positioned<String> {
        Positioned {
            pos: t.pos.clone(),
            val: t.val.clone(),
        }
    }
}

/// Encodes a macro expression in the UCG AST..
///
/// A macro is a pure function over a tuple.
/// MacroDefs are not closures. They can not reference
/// any values except what is defined in their arguments.
#[derive(PartialEq, Debug, Clone)]
pub struct MacroDef {
    pub argdefs: Vec<Positioned<String>>,
    pub fields: FieldList,
    pub pos: Position,
}

impl MacroDef {
    fn symbol_is_in_args(&self, sym: &String) -> bool {
        for arg in self.argdefs.iter() {
            if &arg.val == sym {
                return true;
            }
        }
        return false;
    }

    fn validate_value_symbols<'a>(
        &self,
        stack: &mut Vec<&'a Expression>,
        val: &'a Value,
    ) -> HashSet<String> {
        let mut bad_symbols = HashSet::new();
        if let &Value::Symbol(ref name) = val {
            if !self.symbol_is_in_args(&name.val) {
                bad_symbols.insert(name.val.clone());
            }
        } else if let &Value::Selector(ref sel_node) = val {
            stack.push(&sel_node.sel.head);
        } else if let &Value::Tuple(ref tuple_node) = val {
            let fields = &tuple_node.val;
            for &(_, ref expr) in fields.iter() {
                stack.push(expr);
            }
        } else if let &Value::List(ref def) = val {
            for elem in def.elems.iter() {
                stack.push(elem);
            }
        }
        return bad_symbols;
    }

    /// Performs typechecking of a ucg macro's arguments to ensure
    /// that they are valid for the expressions in the macro.
    pub fn validate_symbols(&self) -> Result<(), HashSet<String>> {
        let mut bad_symbols = HashSet::new();
        for &(_, ref expr) in self.fields.iter() {
            let mut stack = Vec::new();
            stack.push(expr);
            while stack.len() > 0 {
                match stack.pop().unwrap() {
                    &Expression::Binary(ref bexpr) => {
                        stack.push(&bexpr.left);
                        stack.push(&bexpr.right);
                    }
                    &Expression::Compare(ref cexpr) => {
                        stack.push(&cexpr.left);
                        stack.push(&cexpr.right);
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
                    &Expression::Call(ref def) => for expr in def.arglist.iter() {
                        stack.push(expr);
                    },
                    &Expression::Simple(ref val) => {
                        let mut syms_set = self.validate_value_symbols(&mut stack, val);
                        bad_symbols.extend(syms_set.drain());
                    }
                    &Expression::Macro(_) => {
                        // noop
                        continue;
                    }
                    &Expression::ListOp(_) => {
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

/// Specifies the types of binary operations supported in
/// UCG expression.
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryExprType {
    Add,
    Sub,
    Mul,
    Div,
}

/// CompareType signals the type of a comparison for a binary expression.
#[derive(Debug, PartialEq, Clone)]
pub enum CompareType {
    Equal,
    GT,
    LT,
    NotEqual,
    GTEqual,
    LTEqual,
}

/// ComparisonDef Represents a comparison between two expressions.
#[derive(Debug, PartialEq, Clone)]
pub struct ComparisonDef {
    pub kind: CompareType,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub pos: Position,
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
    pub selector: SelectorDef,
    pub fields: FieldList,
    pub pos: Position,
}

/// Encodes a format expression in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct FormatDef {
    pub template: String,
    pub args: Vec<Expression>,
    pub pos: Position,
}

/// Encodes a list expression in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct ListDef {
    pub elems: Vec<Expression>,
    pub pos: Position,
}

/// ListOpType represents the type of list operation for a ListOpDef.
#[derive(Debug, PartialEq, Clone)]
pub enum ListOpType {
    Map,
    Filter,
}

/// ListOpDef implements the list operations in the UCG AST.
#[derive(Debug, PartialEq, Clone)]
pub struct ListOpDef {
    pub typ: ListOpType,
    pub mac: SelectorDef,
    pub field: String,
    pub target: Box<Expression>,
    pub pos: Position,
}

/// Encodes a ucg expression. Expressions compute a value from.
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    // Base Expression
    Simple(Value),

    // Binary expressions
    Binary(BinaryOpDef),
    Compare(ComparisonDef),

    // Complex Expressions
    Copy(CopyDef),
    // TODO(jwall): This should really store it's position :-(
    Grouped(Box<Expression>),
    Format(FormatDef),
    Call(CallDef),
    Macro(MacroDef),
    Select(SelectDef),
    ListOp(ListOpDef),
}

impl Expression {
    /// Returns the position of the Expression.
    pub fn pos(&self) -> &Position {
        match self {
            &Expression::Simple(ref v) => v.pos(),
            &Expression::Binary(ref def) => &def.pos,
            &Expression::Compare(ref def) => &def.pos,
            &Expression::Copy(ref def) => &def.pos,
            &Expression::Grouped(ref expr) => expr.pos(),
            &Expression::Format(ref def) => &def.pos,
            &Expression::Call(ref def) => &def.pos,
            &Expression::Macro(ref def) => &def.pos,
            &Expression::Select(ref def) => &def.pos,
            &Expression::ListOp(ref def) => &def.pos,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expression::Simple(ref v) => {
                try!(write!(w, "{}", v.to_string()));
            }
            &Expression::Binary(_) => {
                try!(write!(w, "<Expr>"));
            }
            &Expression::Compare(_) => {
                try!(write!(w, "<Expr>"));
            }
            &Expression::ListOp(_) => {
                try!(write!(w, "<Expr>"));
            }
            &Expression::Copy(_) => {
                try!(write!(w, "<Copy>"));
            }
            &Expression::Grouped(_) => {
                try!(write!(w, "(<Expr>)"));
            }
            &Expression::Format(_) => {
                try!(write!(w, "<Format Expr>"));
            }
            &Expression::Call(_) => {
                try!(write!(w, "<MacroCall>"));
            }
            &Expression::Macro(_) => {
                try!(write!(w, "<Macro>"));
            }
            &Expression::Select(_) => {
                try!(write!(w, "<Select>"));
            }
        }
        Ok(())
    }
}

/// Encodes a let statement in the UCG AST.
#[derive(Debug, PartialEq)]
pub struct LetDef {
    pub name: Token,
    pub value: Expression,
}

/// Encodes an import statement in the UCG AST.
#[derive(Debug, PartialEq)]
pub struct ImportDef {
    pub path: Token,
    pub name: Token,
}

/// Encodes a parsed statement in the UCG AST.
#[derive(Debug, PartialEq)]
pub enum Statement {
    // simple expression
    Expression(Expression),

    // Named bindings
    Let(LetDef),

    // Import a file.
    Import(ImportDef),

    // Assert statement
    Assert(Token),

    // Identify an Expression for output.
    Output(Token, Expression),
}

#[cfg(test)]
pub mod test;
