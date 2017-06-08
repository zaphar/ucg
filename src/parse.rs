quick_error! {
    #[derive(Debug,PartialEq)]
    pub enum ParseError {
        UnexpectedToken(expected: String, actual: String) {
            description("Unexpected Token")
            display("Unexpected Token Expected {} Got {}", expected, actual)
        }
    }
}

use std::collections::HashMap;
use std::str::FromStr;
use std::str::from_utf8;
use std::error::Error;

use nom::{alpha, is_alphanumeric, digit, IResult};

type ParseResult<O> = Result<O, Box<Error>>;

type FieldList<'a> = Vec<(&'a str, Expression<'a>)>; // str is expected to be a symbol
type SelectorList<'a> = Vec<&'a str>; // str is expected to always be a symbol.

/// Value represents a Value in the UCG parsed AST.
#[derive(Debug,PartialEq)]
pub enum Value<'a> {
    Int(i64),
    Float(f64),
    String(&'a str),
    Symbol(&'a str),
    Tuple(FieldList<'a>),
}

/// Expression encodes an expression. Expressions compute a value from operands.
#[derive(Debug,PartialEq)]
pub enum Expression<'a> {
    // Base Expression
    Simple(Value<'a>),

    // Binary Expressions
    Add(Box<Value<'a>>, Box<Expression<'a>>),
    Sub(Box<Value<'a>>, Box<Expression<'a>>),
    Mul(Box<Value<'a>>, Box<Expression<'a>>),
    Div(Box<Value<'a>>, Box<Expression<'a>>),

    // Complex Expressions
    Copy(SelectorList<'a>, FieldList<'a>),
    Selector(SelectorList<'a>),
    Grouped(Box<Expression<'a>>),

    Call {
        lambda: SelectorList<'a>,
        arglist: Vec<Expression<'a>>,
    },
    Lambda {
        arglist: Vec<Value<'a>>,
        tuple: FieldList<'a>,
    },
    Select {
        val: Box<Expression<'a>>,
        default: Box<Expression<'a>>,
        tuple: FieldList<'a>,
    },
}

/// Statement encodes a parsed Statement in the UCG AST.
#[derive(Debug,PartialEq)]
pub enum Statement<'a> {
    // simple expression
    Expression(Expression<'a>),

    // Named bindings
    Let {
        name: Value<'a>,
        value: Expression<'a>,
    },

    // Include a file.
    Import {
        path: Value<'a>,
        name: Value<'a>,
    },
}

// sentinels and punctuation
named!(doublequote, tag!("\""));
named!(singlequote, tag!("'"));
named!(comma, tag!(","));
named!(lbrace, tag!("{"));
named!(rbrace, tag!("}"));
named!(lparen, tag!("("));
named!(rparen, tag!(")"));
named!(dot, tag!("."));
named!(plus, tag!("+"));
named!(minus, tag!("-"));
named!(mul, tag!("*"));
named!(div, tag!("/"));
named!(equal, tag!("="));
named!(slashes, tag!("//"));
named!(semicolon, tag!(";"));
named!(fatcomma, tag!("=>"));

// a field is the building block of symbols and tuple field names.
named!(field<&str>,
       map_res!(preceded!(peek!(alpha), take_while!(is_alphanumeric)),
                from_utf8)
);

fn symbol_to_value<'a>(s: &'a str) -> ParseResult<Value<'a>> {
    Ok(Value::Symbol(s))
}

// symbol is a bare unquoted field.
named!(symbol<Value>, map_res!(field, symbol_to_value));

// quoted is a quoted string.
named!(quoted<Value>,
       map_res!(delimited!(doublequote, take_until!("\""), doublequote),
                |s| from_utf8(s).map(|s| Value::String(s))
       )
);

// Helper function to make the return types work for down below.
fn triple_to_number<'a>(v: (Option<&'a [u8]>, Option<&'a [u8]>, Option<&'a [u8]>))
                        -> ParseResult<Value<'a>> {
    let pref = match v.0 {
        None => "",
        Some(bs) => try!(from_utf8(bs)),
    };

    let has_dot = v.1.is_some();

    if v.0.is_some() && !has_dot && v.2.is_none() {
        return Ok(Value::Int(try!(FromStr::from_str(pref))));
    }

    let suf = match v.2 {
        None => "",
        Some(bs) => try!(from_utf8(bs)),
    };

    let to_parse = pref.to_string() + "." + suf;
    let f = try!(FromStr::from_str(&to_parse));
    return Ok(Value::Float(f));
}

// NOTE(jwall): HERE THERE BE DRAGONS. The order for these matters
// alot. We need to process alternatives in order of decreasing
// specificity.  Unfortunately this means we are required to go in a
// decreasing size order which messes with alt!'s completion logic. To
// work around this we have to force Incomplete to be Error so that
// alt! will try the next in the series instead of aborting.
//
// *IMPORTANT*
// It also means this combinator is risky when used with partial
// inputs. So handle with care.
named!(number<Value>,
       map_res!(alt!(
           complete!(do_parse!( // 1.0
               prefix: digit >>
               has_dot: dot >>
               suffix: digit >>
               peek!(not!(digit)) >>
               (Some(prefix), Some(has_dot), Some(suffix))
           )) |
           complete!(do_parse!( // 1.
               prefix: digit >>
               has_dot: dot >>
               peek!(not!(digit)) >>
               (Some(prefix), Some(has_dot), None)
           )) |
           complete!(do_parse!( // .1
               has_dot: dot >>
               suffix: digit >>
               peek!(not!(digit)) >>
               (None, Some(has_dot), Some(suffix))
           )) |
           do_parse!( // 1
               prefix: digit >>
// The peek!(not!(..)) make this whole combinator slightly
// safer for partial inputs.
               peek!(not!(digit)) >>
               (Some(prefix), None, None)
           )),
           triple_to_number
       )
);

#[test]
fn test_number_parsing() {
    assert!(number(&b"."[..]).is_err() );
    assert!(number(&b". "[..]).is_err() );
    assert_eq!(number(&b"1.0"[..]),
               IResult::Done(&b""[..], Value::Float(1.0)) );
    assert_eq!(number(&b"1."[..]),
               IResult::Done(&b""[..], Value::Float(1.0)) );
    assert_eq!(number(&b"1"[..]),
               IResult::Done(&b""[..], Value::Int(1)) );
    assert_eq!(number(&b".1"[..]),
               IResult::Done(&b""[..], Value::Float(0.1)) );
}

named!(value<Value>, alt!(number | quoted | symbol | tuple));

named!(
    #[doc="Capture a field and value pair composed of `<symbol> = <value>,`"],
    field_value<(&str, Expression) >,
    do_parse!(
        field: field >>
            ws!(equal) >>
            value: expression >>
            (field, value)
    )
);

#[test]
fn test_field_value_parse() {
    assert!(field_value(&b"foo"[..]).is_incomplete() );
    assert!(field_value(&b"foo ="[..]).is_incomplete() );

    assert_eq!(field_value(&b"foo = 1"[..]),
               IResult::Done(&b""[..], ("foo", Expression::Simple(Value::Int(1)))) );
    assert_eq!(field_value(&b"foo = \"1\""[..]),
               IResult::Done(&b""[..], ("foo", Expression::Simple(Value::String("1")))) );
    assert_eq!(field_value(&b"foo = bar"[..]),
               IResult::Done(&b""[..], ("foo", Expression::Simple(Value::Symbol("bar")))) );
    assert_eq!(field_value(&b"foo = bar "[..]),
               IResult::Done(&b""[..], ("foo", Expression::Simple(Value::Symbol("bar")))) );
}

// Helper function to make the return types work for down below.
fn vec_to_tuple<'a>(v: FieldList<'a>) -> ParseResult<Value<'a>> {
    Ok(Value::Tuple(v))
}

named!(field_list<FieldList>,
       separated_list!(comma, ws!(field_value)));

named!(
    #[doc="Capture a tuple of named fields with values. {<field>=<value>,...}"],
    tuple<Value>,
    map_res!(
        delimited!(lbrace,
                   ws!(field_list),
                   rbrace),
        vec_to_tuple
    )
);

#[test]
fn test_tuple_parse() {
    assert!(tuple(&b"{"[..]).is_incomplete() );
    assert!(tuple(&b"{ foo"[..]).is_incomplete() );
    assert!(tuple(&b"{ foo ="[..]).is_incomplete() );
    assert!(tuple(&b"{ foo = 1"[..]).is_incomplete() );
    assert!(tuple(&b"{ foo = 1,"[..]).is_err() );
    assert!(tuple(&b"{ foo = 1, bar ="[..]).is_err() );

    assert_eq!(tuple(&b"{ }"[..]),
            IResult::Done(&b""[..],
                          Value::Tuple(
                              vec![])));

    assert_eq!(tuple(&b"{ foo = 1 }"[..]),
            IResult::Done(&b""[..],
                          Value::Tuple(
                              vec![
                                  ("foo", Expression::Simple(Value::Int(1)))
                              ])));

    assert_eq!(tuple(&b"{ foo = 1, bar = \"1\" }"[..]),
            IResult::Done(&b""[..],
                          Value::Tuple(
                              vec![
                                  ("foo", Expression::Simple(Value::Int(1))),
                                  ("bar", Expression::Simple(Value::String("1")))
                              ])));
    assert_eq!(tuple(&b"{ foo = 1, bar = {} }"[..]),
            IResult::Done(&b""[..],
                          Value::Tuple(
                              vec![
                                  ("foo", Expression::Simple(Value::Int(1))),
                                  ("bar", Expression::Simple(Value::Tuple(Vec::new())))
                              ])));
}

// keywords
named!(let_word, tag!("let"));
named!(select_word, tag!("select"));
named!(lambda_word, tag!("lambda"));
named!(import_word, tag!("import"));
named!(as_word, tag!("as"));

fn value_to_expression(v: Value) -> ParseResult<Expression> {
    Ok(Expression::Simple(v))
}

named!(simple_expression<Expression>,
       map_res!(
           value,
           value_to_expression
       )
);

fn tuple_to_add_expression<'a>(tpl: (Value<'a>, Expression<'a>)) -> ParseResult<Expression<'a>> {
    Ok(Expression::Add(Box::new(tpl.0), Box::new(tpl.1)))
}

named!(add_expression<Expression>,
       map_res!(
           do_parse!(
               left: value >>
                   ws!(plus) >>
                   right: expression >>
                   (left, right)
           ),
           tuple_to_add_expression
       )
);

fn tuple_to_sub_expression<'a>(tpl: (Value<'a>, Expression<'a>)) -> ParseResult<Expression<'a>> {
    Ok(Expression::Sub(Box::new(tpl.0), Box::new(tpl.1)))
}

named!(sub_expression<Expression>,
       map_res!(
           do_parse!(
               left: value >>
                   ws!(minus) >>
                   right: expression >>
                   (left, right)
           ),
           tuple_to_sub_expression
       )
);

fn tuple_to_mul_expression<'a>(tpl: (Value<'a>, Expression<'a>)) -> ParseResult<Expression<'a>> {
    Ok(Expression::Mul(Box::new(tpl.0), Box::new(tpl.1)))
}

named!(mul_expression<Expression>,
       map_res!(
           do_parse!(
               left: value >>
                   ws!(mul) >>
                   right: expression >>
                   (left, right)
           ),
           tuple_to_mul_expression
       )
);

fn tuple_to_div_expression<'a>(tpl: (Value<'a>, Expression<'a>)) -> ParseResult<Expression<'a>> {
    Ok(Expression::Div(Box::new(tpl.0), Box::new(tpl.1)))
}

named!(div_expression<Expression>,
       map_res!(
           do_parse!(
               left: value >>
                   ws!(div) >>
                   right: expression >>
                   (left, right)
           ),
           tuple_to_div_expression
       )
);

fn expression_to_grouped_expression<'a>(e: Expression<'a>) -> ParseResult<Expression<'a>> {
    Ok(Expression::Grouped(Box::new(e)))
}

named!(grouped_expression<Expression>,
       map_res!(
           preceded!(lparen, terminated!(expression, rparen)),
           expression_to_grouped_expression
       )
);

#[test]
fn test_grouped_expression_parse() {
    assert!(grouped_expression(&b"foo"[..]).is_err() );
    assert!(grouped_expression(&b"(foo"[..]).is_incomplete() );
    assert_eq!(grouped_expression(&b"(foo)"[..]),
            IResult::Done(&b""[..],
                          Expression::Grouped(
                              Box::new(
                                  Expression::Simple(
                                      Value::Symbol("foo")))))
    );
    assert_eq!(grouped_expression(&b"(1 + 1)"[..]),
            IResult::Done(&b""[..],
                          Expression::Grouped(
                              Box::new(
                                  Expression::Add(
                                      Box::new(Value::Int(1)),
                                      Box::new(Expression::Simple(
                                          Value::Int(1)))
                                  )
                              )
                          )
            )
    );
}

named!(selector_list<SelectorList>, separated_nonempty_list!(dot, field));

fn tuple_to_copy<'a>(t: (SelectorList<'a>, FieldList<'a>)) -> ParseResult<Expression<'a>> {
    Ok(Expression::Copy(t.0, t.1))
}

named!(copy_expression<Expression>,
       map_res!(
           do_parse!(
               selector: selector_list >>
                   lbrace >>
                   fields: ws!(field_list) >>
                   rbrace >>
                   (selector, fields)
           ),
           tuple_to_copy
       )
);

#[test]
fn test_copy_parse() {
    assert!(copy_expression(&b"{}"[..]).is_err() );
    assert!(copy_expression(&b"foo"[..]).is_incomplete() );
    assert!(copy_expression(&b"foo{"[..]).is_incomplete() );
    assert_eq!(copy_expression(&b"foo{}"[..]),
               IResult::Done(&b""[..],
                             Expression::Copy(vec!["foo"],
                                              Vec::new())
               )
    );
    assert_eq!(copy_expression(&b"foo{bar=1}"[..]),
               IResult::Done(&b""[..],
                             Expression::Copy(vec!["foo"],
                                              vec![("bar", Expression::Simple(Value::Int(1)))])
               )
    );
}

fn tuple_to_lambda<'a>(t: (Vec<Value<'a>>, Value<'a>)) -> ParseResult<Expression<'a>> {
    match t.1 {
        Value::Tuple(v) => {
            Ok(Expression::Lambda {
                arglist: t.0,
                tuple: v,
            })
        }
        // TODO(jwall): Show a better version of the unexpected parsed value.
        val => {
            Err(Box::new(ParseError::UnexpectedToken("{ .. }".to_string(), format!("{:?}", val))))
        }
    }
}

named!(arglist<Vec<Value> >, separated_list!(ws!(comma), symbol));

#[test]
fn test_arglist_parse() {
    assert!(arglist(&b"arg"[..]).is_done());
    assert!(arglist(&b"arg1, arg2"[..]).is_done());
    assert_eq!(arglist(&b"arg1, arg2"[..]), IResult::Done(&b""[..],
                                                  vec![
                                                      Value::Symbol("arg1"),
                                                      Value::Symbol("arg2")
                                                  ]));
}

named!(lambda_expression<Expression>,
       map_res!(
           do_parse!(
               lambda_word >>
                   ws!(lparen) >>
                   arglist: ws!(arglist) >>
                   rparen >>
                   ws!(fatcomma) >>
                   map: tuple >>
                   (arglist, map)
           ),
           tuple_to_lambda
       )
);

#[test]
fn test_lambda_expression_parsing() {
    assert!(lambda_expression(&b"foo"[..]).is_err() );
    assert!(lambda_expression(&b"lambda \"foo\""[..]).is_err() );
    assert!(lambda_expression(&b"lambda 1"[..]).is_err() );
    assert!(lambda_expression(&b"lambda"[..]).is_incomplete() );
    assert!(lambda_expression(&b"lambda ("[..]).is_incomplete() );
    assert!(lambda_expression(&b"lambda (arg"[..]).is_incomplete() );
    assert!(lambda_expression(&b"lambda (arg, arg2"[..]).is_incomplete() );
    assert!(lambda_expression(&b"lambda (arg1, arg2) =>"[..]).is_incomplete() );
    assert!(lambda_expression(&b"lambda (arg1, arg2) => {"[..]).is_incomplete() );
    assert!(lambda_expression(&b"lambda (arg1, arg2) => { foo"[..]).is_incomplete() );
    assert!(lambda_expression(&b"lambda (arg1, arg2) => { foo ="[..]).is_incomplete() );

    assert_eq!(lambda_expression(&b"lambda (arg1, arg2) => {foo=1,bar=2}"[..]),
               IResult::Done(&b""[..],
                             Expression::Lambda{
                                 arglist: vec![Value::Symbol("arg1"),
                                               Value::Symbol("arg2")],
                                 tuple: vec![("foo", Expression::Simple(Value::Int(1))),
                                             ("bar", Expression::Simple(Value::Int(2)))
                                 ]
                             }
               )
    );
}

fn tuple_to_select<'a>(t: (Expression<'a>, Expression<'a>, Value<'a>))
                       -> ParseResult<Expression<'a>> {
    match t.2 {
        Value::Tuple(v) => {
            Ok(Expression::Select {
                val: Box::new(t.0),
                default: Box::new(t.1),
                tuple: v,
            })
        }
        // TODO(jwall): Show a better version of the unexpected parsed value.
        val => {
            Err(Box::new(ParseError::UnexpectedToken("{ .. }".to_string(), format!("{:?}", val))))
        }
    }
}

named!(select_selector<Value>, alt!(symbol | quoted | number));

named!(select_expression<Expression>,
       map_res!(
           terminated!(do_parse!(
               select_word >>
                   val: ws!(terminated!(expression, comma)) >>
                   default: ws!(terminated!(expression, comma)) >>
                   map: ws!(tuple) >>
                   (val, default, map)
           ), semicolon),
           tuple_to_select
       )
);

#[test]
fn test_select_parse() {
    assert!(select_expression(&b"select"[..]).is_incomplete());
    assert!(select_expression(&b"select foo"[..]).is_incomplete());
    assert!(select_expression(&b"select foo, 1"[..]).is_incomplete());
    assert!(select_expression(&b"select foo, 1, {"[..]).is_incomplete());

    assert_eq!(select_expression(&b"select foo, 1, { foo = 2 };"[..]),
               IResult::Done(&b""[..],
                             Expression::Select{
                                 val: Box::new(Expression::Simple(Value::Symbol("foo"))),
                                 default: Box::new(Expression::Simple(Value::Int(1))),
                                 tuple: vec![
                                     ("foo", Expression::Simple(Value::Int(2)))
                                 ]
                             }
               )
    );
}

fn tuple_to_call<'a>(t: (Expression<'a>, Vec<Expression<'a>>)) -> ParseResult<Expression<'a>> {
    if let Expression::Selector(sl) = t.0 {
        Ok(Expression::Call {
            lambda: sl,
            arglist: t.1,
        })
    } else {
        Err(Box::new(ParseError::UnexpectedToken("Selector".to_string(), format!("{:?}", t.0))))
    }
}

fn vec_to_selector_expression<'a>(v: SelectorList<'a>) -> ParseResult<Expression<'a>> {
    Ok(Expression::Selector(v))
}

named!(selector_expression<Expression>,
       map_res!(
           ws!(selector_list),
           vec_to_selector_expression
       )
);

named!(call_expression<Expression>,
       map_res!(
           do_parse!(
               lambda: selector_expression >>
                   lparen >>
                   args: ws!(separated_list!(ws!(comma), expression)) >>
                   rparen >>
                   (lambda, args)
           ),
           tuple_to_call
       )
);

#[test]
fn test_call_parse() {
    assert!(call_expression(&b"foo"[..]).is_incomplete() );
    assert!(call_expression(&b"foo ("[..]).is_incomplete() );
    assert!(call_expression(&b"foo (1"[..]).is_incomplete() );
    assert!(call_expression(&b"foo (1,"[..]).is_err() );
    assert!(call_expression(&b"foo (1,2"[..]).is_incomplete() );

    assert_eq!(call_expression(&b"foo (1, \"foo\")"[..]),
               IResult::Done(&b""[..],
                             Expression::Call{
                                 lambda: vec!["foo"],
                                 arglist: vec![
                                     Expression::Simple(Value::Int(1)),
                                     Expression::Simple(Value::String("foo")),
                                 ],
                             }
               )
    );

    assert_eq!(call_expression(&b"foo.bar (1, \"foo\")"[..]),
               IResult::Done(&b""[..],
                             Expression::Call{
                                 lambda: vec!["foo","bar"],
                                 arglist: vec![
                                     Expression::Simple(Value::Int(1)),
                                     Expression::Simple(Value::String("foo")),
                                 ],
                             }
               )
    );
}

// NOTE(jwall): HERE THERE BE DRAGONS. The order for these matters
// alot. We need to process alternatives in order of decreasing
// specificity.  Unfortunately this means we are required to go in a
// decreasing size order which messes with alt!'s completion logic. To
// work around this we have to force Incomplete to be Error so that
// alt! will try the next in the series instead of aborting.
//
// *IMPORTANT*
// It also means this combinator is risky when used with partial
// inputs. So handle with care.
named!(expression<Expression>,
       alt!(
           complete!(add_expression) |
           complete!(sub_expression) |
           complete!(mul_expression) |
           complete!(div_expression) |
           complete!(grouped_expression) |
           complete!(lambda_expression) |
           complete!(select_expression) |
           complete!(call_expression) |
           complete!(copy_expression) |
           ws!(simple_expression)
       )
);

#[test]
fn test_expression_parse() {
    assert_eq!(expression(&b"1"[..]),
               IResult::Done(&b""[..], Expression::Simple(Value::Int(1))));
    assert_eq!(expression(&b"1 + 1"[..]),
               IResult::Done(&b""[..],
                             Expression::Add(Box::new(Value::Int(1)),
                                             Box::new(Expression::Simple(Value::Int(1))))));
    assert_eq!(expression(&b"1 - 1"[..]),
               IResult::Done(&b""[..],
                             Expression::Sub(Box::new(Value::Int(1)),
                                             Box::new(Expression::Simple(Value::Int(1))))));
    assert_eq!(expression(&b"1 * 1"[..]),
               IResult::Done(&b""[..],
                             Expression::Mul(Box::new(Value::Int(1)),
                                             Box::new(Expression::Simple(Value::Int(1))))));
    assert_eq!(expression(&b"1 / 1"[..]),
               IResult::Done(&b""[..],
                             Expression::Div(Box::new(Value::Int(1)),
                                             Box::new(Expression::Simple(Value::Int(1))))));

    assert_eq!(expression(&b"1+1"[..]),
               IResult::Done(&b""[..],
                             Expression::Add(Box::new(Value::Int(1)),
                                             Box::new(Expression::Simple(Value::Int(1))))));
    assert_eq!(expression(&b"1-1"[..]),
               IResult::Done(&b""[..],
                             Expression::Sub(Box::new(Value::Int(1)),
                                             Box::new(Expression::Simple(Value::Int(1))))));
    assert_eq!(expression(&b"1*1"[..]),
               IResult::Done(&b""[..],
                             Expression::Mul(Box::new(Value::Int(1)),
                                             Box::new(Expression::Simple(Value::Int(1))))));
    assert_eq!(expression(&b"1/1"[..]),
               IResult::Done(&b""[..],
                             Expression::Div(Box::new(Value::Int(1)),
                                             Box::new(Expression::Simple(Value::Int(1))))));
    assert_eq!(expression(&b"lambda (arg1, arg2) => { foo = arg1 }"[..]),
               IResult::Done(&b""[..],
                             Expression::Lambda{
                                 arglist: vec![
                                     Value::Symbol("arg1"),
                                     Value::Symbol("arg2")
                                 ],
                                 tuple: vec![
                                     ("foo", Expression::Simple(Value::Symbol("arg1"))),
                                 ],
                             }
               )
    );
    assert_eq!(expression(&b"select foo, 1, { foo = 2 };"[..]),
               IResult::Done(&b""[..],
                             Expression::Select{
                                 val: Box::new(Expression::Simple(Value::Symbol("foo"))),
                                 default: Box::new(Expression::Simple(Value::Int(1))),
                                 tuple: vec![
                                     ("foo", Expression::Simple(Value::Int(2)))
                                 ]
                             }
               )
    );
    assert_eq!(expression(&b"foo.bar (1, \"foo\")"[..]),
               IResult::Done(&b""[..],
                             Expression::Call{
                                 lambda: vec!["foo","bar"],
                                 arglist: vec![
                                     Expression::Simple(Value::Int(1)),
                                     Expression::Simple(Value::String("foo")),
                                 ],
                             }
               )
    );
    assert_eq!(expression(&b"(1 + 1)"[..]),
            IResult::Done(&b""[..],
                          Expression::Grouped(
                              Box::new(
                                  Expression::Add(
                                      Box::new(Value::Int(1)),
                                      Box::new(Expression::Simple(Value::Int(1)))
                                  )
                              )
                          )
            )
    );
}

fn expression_to_statement(v: Expression) -> ParseResult<Statement> {
    Ok(Statement::Expression(v))
}

named!(expression_statement<Statement>,
       map_res!(
           terminated!(ws!(expression), semicolon),
           expression_to_statement
       )
);

#[test]
fn test_expression_statement_parse() {
    assert!(expression_statement(&b"foo"[..]).is_incomplete() );
    assert_eq!(expression_statement(&b"1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Float(1.0)))));
    assert_eq!(expression_statement(&b"1.0 ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Float(1.0)))));
    assert_eq!(expression_statement(&b" 1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Float(1.0)))));
    assert_eq!(expression_statement(&b"foo;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol("foo")))));
    assert_eq!(expression_statement(&b"foo ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol("foo")))));
    assert_eq!(expression_statement(&b" foo;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol("foo")))));
    assert_eq!(expression_statement(&b"\"foo\";"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::String("foo")))));
    assert_eq!(expression_statement(&b"\"foo\" ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::String("foo")))));
    assert_eq!(expression_statement(&b" \"foo\";"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::String("foo")))));
}

fn tuple_to_let<'a>(t: (Value<'a>, Expression<'a>)) -> ParseResult<Statement<'a>> {
    Ok(Statement::Let {
        name: t.0,
        value: t.1,
    })
}

named!(let_statement<Statement>,
       map_res!(
           terminated!(do_parse!(
               let_word >>
                   name: ws!(symbol) >>
                   equal >>
                   val: ws!(expression) >>
                   (name, val)
           ), semicolon),
           tuple_to_let
       )
);

#[test]
fn test_let_statement_parse() {
    assert!(let_statement(&b"foo"[..]).is_err() );
    assert!(let_statement(&b"let \"foo\""[..]).is_err() );
    assert!(let_statement(&b"let 1"[..]).is_err() );
    assert!(let_statement(&b"let"[..]).is_incomplete() );
    assert!(let_statement(&b"let foo"[..]).is_incomplete() );
    assert!(let_statement(&b"let foo ="[..]).is_incomplete() );
    assert!(let_statement(&b"let foo = "[..]).is_incomplete() );
    assert!(let_statement(&b"let foo = 1"[..]).is_incomplete() );

    assert_eq!(let_statement(&b"let foo = 1.0 ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Let{name: Value::Symbol("foo"),
                                            value: Expression::Simple(Value::Float(1.0))}));
    assert_eq!(let_statement(&b"let foo= 1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Let{name: Value::Symbol("foo"),
                                            value: Expression::Simple(Value::Float(1.0))}));
    assert_eq!(let_statement(&b"let foo =1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Let{name: Value::Symbol("foo"),
                                            value: Expression::Simple(Value::Float(1.0))}));
}

fn tuple_to_import<'a>(t: (Value<'a>, Value<'a>)) -> ParseResult<Statement<'a>> {
    Ok(Statement::Import {
        name: t.0,
        path: t.1,
    })
}

named!(import_statement<Statement>,
       map_res!(
           terminated!(do_parse!(
               import_word >>
                   path: ws!(quoted) >>
                   as_word >>
                   name: ws!(symbol) >>
                   (name, path)
           ), semicolon),
           tuple_to_import
       )
);

#[test]
fn test_import_parse() {
    assert!(import_statement(&b"import"[..]).is_incomplete());
    assert!(import_statement(&b"import \"foo\""[..]).is_incomplete());
    assert!(import_statement(&b"import \"foo\" as"[..]).is_incomplete());
    assert!(import_statement(&b"import \"foo\" as foo"[..]).is_incomplete());

    assert_eq!(import_statement(&b"import \"foo\" as foo;"[..]),
               IResult::Done(&b""[..],
                             Statement::Import{
                                 path: Value::String("foo"),
                                 name: Value::Symbol("foo")
                             }
               )
    );
}

named!(statement<Statement>,
       alt_complete!(
           import_statement |
           let_statement |
           expression_statement
       )
);

#[test]
fn test_statement_parse() {
    assert_eq!(statement(&b"import \"foo\" as foo;"[..]),
               IResult::Done(&b""[..],
                             Statement::Import{
                                 path: Value::String("foo"),
                                 name: Value::Symbol("foo")
                             }
               )
    );
    assert!(statement(&b"import foo"[..]).is_err() );

    assert_eq!(statement(&b"let foo = 1.0 ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Let{name: Value::Symbol("foo"),
                                            value: Expression::Simple(Value::Float(1.0))}));
    assert_eq!(statement(&b"1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Float(1.0)))));
}

named!(pub parse<Vec<Statement> >, many1!(ws!(statement)));

// TODO(jwall): Full Statement parsing tests.

#[test]
fn test_parse() {
    let bad_input = &b"import mylib as lib;"[..];
    let bad_result = parse(bad_input);
    assert!(bad_result.is_err() );

    // Valid parsing tree
    let input = &b"import \"mylib\" as lib;\
                   let foo = 1;\
                   1+1;"[..];
    let result = parse(input);
    assert!(result.is_done() );
    let tpl = result.unwrap();
    assert_eq!(from_utf8(tpl.0).unwrap(), "");
    assert_eq!(tpl.1,
               vec![
                   Statement::Import{
                       path: Value::String("mylib"),
                       name: Value::Symbol("lib")
                   },
                   Statement::Let{
                       name: Value::Symbol("foo"),
                       value: Expression::Simple(Value::Int(1))
                   },
                   Statement::Expression(
                       Expression::Add(Box::new(Value::Int(1)),
                                       Box::new(Expression::Simple(Value::Int(1))))
                   )
               ]);
}
