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
quick_error! {
    #[derive(Debug,PartialEq)]
    pub enum ParseError {
        UnexpectedToken(expected: String, actual: String) {
            description("Unexpected Token")
            display("Unexpected Token Expected {} Got {}", expected, actual)
        }
    }
}

// TODO(jwall): Convert to tokenizer steps followed by parser steps.
// TODO(jwall): Error Reporting with Line and Column information.

use std::str::FromStr;
use std::str::from_utf8;
use std::error::Error;

use nom::{alpha, is_alphanumeric, digit};

use ast::*;

type ParseResult<O> = Result<O, Box<Error>>;

// sentinels and punctuation
named!(doublequote, tag!("\""));
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
named!(semicolon, tag!(";"));
named!(fatcomma, tag!("=>"));

fn is_symbol_char(c: u8) -> bool {
    is_alphanumeric(c) || c == '-' as u8 || c == '_' as u8
}
// a field is the building block of symbols and tuple field names.
named!(field<String>,
       map_res!(preceded!(peek!(alpha), take_while!(is_symbol_char)),
                |s| from_utf8(s).map(|s| s.to_string())
       )
);

fn symbol_to_value(s: String) -> ParseResult<Value> {
    Ok(Value::Symbol(make_value_node(s)))
}

// symbol is a bare unquoted field.
named!(symbol<Value>, map_res!(field, symbol_to_value));

// quoted is a quoted string.
named!(quoted<String>,
       map_res!(delimited!(doublequote, take_until!("\""), doublequote),
                |s| from_utf8(s).map(|s| s.to_string())
       )
);

fn str_to_value(s: String) -> ParseResult<Value> {
    Ok(Value::String(make_value_node(s)))
}

// quoted_value is a quoted string.
named!(quoted_value<Value>,
       map_res!(quoted, str_to_value)
);

// Helper function to make the return types work for down below.
fn triple_to_number(v: (Option<&[u8]>, Option<&[u8]>, Option<&[u8]>))
                        -> ParseResult<Value> {
    let pref = match v.0 {
        None => "",
        Some(bs) => try!(from_utf8(bs)),
    };

    let has_dot = v.1.is_some();

    if v.0.is_some() && !has_dot && v.2.is_none() {
        return Ok(Value::Int(make_value_node(try!(FromStr::from_str(pref)))));
    }

    let suf = match v.2 {
        None => "",
        Some(bs) => try!(from_utf8(bs)),
    };

    let to_parse = pref.to_string() + "." + suf;
    let f = try!(FromStr::from_str(&to_parse));
    return Ok(Value::Float(make_value_node(f)));
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

named!(value<Value>, alt!(number | quoted_value | symbol | tuple));

named!(
    #[doc="Capture a field and value pair composed of `<symbol> = <value>,`"],
    field_value<(String, Expression) >,
    do_parse!(
        field: field >>
            ws!(equal) >>
            value: expression >>
            (field, value)
    )
);

// Helper function to make the return types work for down below.
fn vec_to_tuple(v: FieldList) -> ParseResult<Value> {
    Ok(Value::Tuple(make_value_node(v)))
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

// keywords
named!(let_word, tag!("let"));
named!(select_word, tag!("select"));
named!(macro_word, tag!("macro"));
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

fn tuple_to_add_expression(tpl: (Value, Expression)) -> ParseResult<Expression> {
    Ok(Expression::Add(BinaryExpression(tpl.0, Box::new(tpl.1))))
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

fn tuple_to_sub_expression(tpl: (Value, Expression)) -> ParseResult<Expression> {
    Ok(Expression::Sub(BinaryExpression(tpl.0, Box::new(tpl.1))))
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

fn tuple_to_mul_expression(tpl: (Value, Expression)) -> ParseResult<Expression> {
    Ok(Expression::Mul(BinaryExpression(tpl.0, Box::new(tpl.1))))
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

fn tuple_to_div_expression(tpl: (Value, Expression)) -> ParseResult<Expression> {
    Ok(Expression::Div(BinaryExpression(tpl.0, Box::new(tpl.1))))
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

fn expression_to_grouped_expression(e: Expression) -> ParseResult<Expression> {
    Ok(Expression::Grouped(Box::new(e)))
}

named!(grouped_expression<Expression>,
       map_res!(
           preceded!(lparen, terminated!(expression, rparen)),
           expression_to_grouped_expression
       )
);

named!(selector_list<SelectorList>, separated_nonempty_list!(dot, field));

fn tuple_to_copy(t: (SelectorList, FieldList)) -> ParseResult<Expression> {
    Ok(Expression::Copy(CopyDef{selector: t.0, fields: t.1}))
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

fn tuple_to_macro(mut t: (Vec<Value>, Value)) -> ParseResult<Expression> {
    match t.1 {
        Value::Tuple(v) => {
            Ok(Expression::Macro(MacroDef {
                argdefs: t.0.drain(0..).map(|s| s.to_string()).collect(),
                fields: v.val,
            }))
        }
        // TODO(jwall): Show a better version of the unexpected parsed value.
        val => {
            Err(Box::new(ParseError::UnexpectedToken("{ .. }".to_string(), format!("{:?}", val))))
        }
    }
}

named!(arglist<Vec<Value> >, separated_list!(ws!(comma), symbol));

named!(macro_expression<Expression>,
       map_res!(
           do_parse!(
               macro_word >>
                   ws!(lparen) >>
                   arglist: ws!(arglist) >>
                   rparen >>
                   ws!(fatcomma) >>
                   map: tuple >>
                   (arglist, map)
           ),
           tuple_to_macro
       )
);

fn tuple_to_select(t: (Expression, Expression, Value))
                       -> ParseResult<Expression> {
    match t.2 {
        Value::Tuple(v) => {
            Ok(Expression::Select(SelectDef{
                val: Box::new(t.0),
                default: Box::new(t.1),
                tuple: v.val,
            }))
        }
        // TODO(jwall): Show a better version of the unexpected parsed value.
        val => {
            Err(Box::new(ParseError::UnexpectedToken("{ .. }".to_string(), format!("{:?}", val))))
        }
    }
}

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

fn tuple_to_format(t: (String, Vec<Expression>)) -> ParseResult<Expression> {
    Ok(Expression::Format(t.0, t.1))
}

named!(format_expression<Expression>,
       map_res!(
           do_parse!(
               tmpl: ws!(quoted) >>
                   ws!(tag!("%")) >>
                   lparen >>
                   args: ws!(separated_list!(ws!(comma), expression)) >>
                   rparen >>
                   (tmpl, args)
           ),
           tuple_to_format
       )
);

fn tuple_to_call(t: (Value, Vec<Expression>)) -> ParseResult<Expression> {
    if let Value::Selector(sl) = t.0 {
        Ok(Expression::Call(CallDef{
            macroref: sl.val,
            arglist: t.1,
        }))
    } else {
        Err(Box::new(ParseError::UnexpectedToken("Selector".to_string(), format!("{:?}", t.0))))
    }
}

fn vec_to_selector_value(v: SelectorList) -> ParseResult<Value> {
    Ok(Value::Selector(make_value_node(v)))
}

named!(selector_value<Value>,
       map_res!(
           ws!(selector_list),
           vec_to_selector_value
       )
);

named!(call_expression<Expression>,
       map_res!(
           do_parse!(
               macroname: selector_value >>
                   lparen >>
                   args: ws!(separated_list!(ws!(comma), expression)) >>
                   rparen >>
                   (macroname, args)
           ),
           tuple_to_call
       )
);

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
           complete!(macro_expression) |
           complete!(format_expression) |
           complete!(select_expression) |
           complete!(call_expression) |
           complete!(copy_expression) |
           ws!(simple_expression)
       )
);

fn expression_to_statement(v: Expression) -> ParseResult<Statement> {
    Ok(Statement::Expression(v))
}

named!(expression_statement<Statement>,
       map_res!(
           terminated!(ws!(expression), semicolon),
           expression_to_statement
       )
);

fn tuple_to_let(t: (String, Expression)) -> ParseResult<Statement> {
    Ok(Statement::Let {
        name: t.0.to_string(),
        value: t.1,
    })
}

named!(let_statement<Statement>,
       map_res!(
           terminated!(do_parse!(
               let_word >>
                   name: ws!(field) >>
                   equal >>
                   val: ws!(expression) >>
                   (name, val)
           ), semicolon),
           tuple_to_let
       )
);

fn tuple_to_import(t: (String, String)) -> ParseResult<Statement> {
    Ok(Statement::Import {
        name: t.0.to_string(),
        path: t.1.to_string(),
    })
}

named!(import_statement<Statement>,
       map_res!(
           terminated!(do_parse!(
               import_word >>
                   path: ws!(quoted) >>
                   as_word >>
                   name: ws!(field) >>
                   (name, path)
           ), semicolon),
           tuple_to_import
       )
);

named!(statement<Statement>,
       alt_complete!(
           import_statement |
           let_statement |
           expression_statement
       )
);

named!(pub parse<Vec<Statement> >, many1!(ws!(statement)));

// TODO(jwall): Full Statement parsing tests.

#[cfg(test)]
mod test {
    use std::str::from_utf8;

    use super::{Statement, Expression, Value, MacroDef, SelectDef, CallDef};
    use super::{number, symbol, parse, field_value, tuple, grouped_expression};
    use super::{arglist, copy_expression, macro_expression, select_expression};
    use super::{format_expression, call_expression, expression};
    use super::{expression_statement, let_statement, import_statement, statement};
    use ast::*;

    use nom::IResult;

    #[test]
    fn test_statement_parse() {
        assert_eq!(statement(&b"import \"foo\" as foo;"[..]),
               IResult::Done(&b""[..],
                             Statement::Import{
                                 path: "foo".to_string(),
                                 name: "foo".to_string()
                             }
               )
    );
        assert!(statement(&b"import foo"[..]).is_err() );

        assert_eq!(statement(&b"let foo = 1.0 ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Let{name: "foo".to_string(),
                                            value: Expression::Simple(Value::Float(make_value_node(1.0)))}));
        assert_eq!(statement(&b"1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Float(make_value_node(1.0))))));
    }

    #[test]
    fn test_import_parse() {
        assert!(import_statement(&b"import"[..]).is_incomplete());
        assert!(import_statement(&b"import \"foo\""[..]).is_incomplete());
        assert!(import_statement(&b"import \"foo\" as"[..]).is_incomplete());
        assert!(import_statement(&b"import \"foo\" as foo"[..]).is_incomplete());

        assert_eq!(import_statement(&b"import \"foo\" as foo;"[..]),
               IResult::Done(&b""[..],
                             Statement::Import{
                                 path: "foo".to_string(),
                                 name: "foo".to_string()
                             }
               )
    );
    }

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
                             Statement::Let{name: "foo".to_string(),
                                            value: Expression::Simple(Value::Float(make_value_node(1.0)))}));
        assert_eq!(let_statement(&b"let foo= 1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Let{name: "foo".to_string(),
                                            value: Expression::Simple(Value::Float(make_value_node(1.0)))}));
        assert_eq!(let_statement(&b"let foo =1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Let{name: "foo".to_string(),
                                            value: Expression::Simple(Value::Float(make_value_node(1.0)))}));
    }

    #[test]
    fn test_expression_statement_parse() {
        assert!(expression_statement(&b"foo"[..]).is_incomplete() );
        assert_eq!(expression_statement(&b"1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Float(make_value_node(1.0))))));
        assert_eq!(expression_statement(&b"1.0 ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Float(make_value_node(1.0))))));
        assert_eq!(expression_statement(&b" 1.0;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Float(make_value_node(1.0))))));
        assert_eq!(expression_statement(&b"foo;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol(make_value_node("foo".to_string()))))));
        assert_eq!(expression_statement(&b"foo ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol(make_value_node("foo".to_string()))))));
        assert_eq!(expression_statement(&b" foo;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol(make_value_node("foo".to_string()))))));
        assert_eq!(expression_statement(&b"\"foo\";"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::String(make_value_node("foo".to_string()))))));
        assert_eq!(expression_statement(&b"\"foo\" ;"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::String(make_value_node("foo".to_string()))))));
        assert_eq!(expression_statement(&b" \"foo\";"[..]),
               IResult::Done(&b""[..],
                             Statement::Expression(
                                 Expression::Simple(Value::String(make_value_node("foo".to_string()))))));
    }

    #[test]
    fn test_expression_parse() {
        assert_eq!(expression(&b"1"[..]),
               IResult::Done(&b""[..], Expression::Simple(Value::Int(make_value_node(1)))));
        assert_eq!(expression(&b"foo"[..]),
               IResult::Done(&b""[..], Expression::Simple(Value::Symbol(make_value_node("foo".to_string())))));
        assert_eq!(expression(&b"1 + 1"[..]),
               IResult::Done(&b""[..],
                             Expression::Add(BinaryExpression(Value::Int(make_value_node(1)),
                                             Box::new(Expression::Simple(Value::Int(make_value_node(1))))))));
        assert_eq!(expression(&b"1 - 1"[..]),
               IResult::Done(&b""[..],
                             Expression::Sub(BinaryExpression(Value::Int(make_value_node(1)),
                                             Box::new(Expression::Simple(Value::Int(make_value_node(1))))))));
        assert_eq!(expression(&b"1 * 1"[..]),
               IResult::Done(&b""[..],
                             Expression::Mul(BinaryExpression(Value::Int(make_value_node(1)),
                                             Box::new(Expression::Simple(Value::Int(make_value_node(1))))))));
        assert_eq!(expression(&b"1 / 1"[..]),
               IResult::Done(&b""[..],
                             Expression::Div(BinaryExpression(Value::Int(make_value_node(1)),
                                             Box::new(Expression::Simple(Value::Int(make_value_node(1))))))));

        assert_eq!(expression(&b"1+1"[..]),
               IResult::Done(&b""[..],
                             Expression::Add(BinaryExpression(Value::Int(make_value_node(1)),
                                             Box::new(Expression::Simple(Value::Int(make_value_node(1))))))));
        assert_eq!(expression(&b"1-1"[..]),
               IResult::Done(&b""[..],
                             Expression::Sub(BinaryExpression(Value::Int(make_value_node(1)),
                                             Box::new(Expression::Simple(Value::Int(make_value_node(1))))))));
        assert_eq!(expression(&b"1*1"[..]),
               IResult::Done(&b""[..],
                             Expression::Mul(BinaryExpression(Value::Int(make_value_node(1)),
                                             Box::new(Expression::Simple(Value::Int(make_value_node(1))))))));
        assert_eq!(expression(&b"1/1"[..]),
               IResult::Done(&b""[..],
                             Expression::Div(BinaryExpression(Value::Int(make_value_node(1)),
                                             Box::new(Expression::Simple(Value::Int(make_value_node(1))))))));
        assert_eq!(expression(&b"macro (arg1, arg2) => { foo = arg1 }"[..]),
               IResult::Done(&b""[..],
                             Expression::Macro(MacroDef{
                                 argdefs: vec![
                                     "arg1".to_string(),
                                     "arg2".to_string(),
                                 ],
                                 fields: vec![
                                     ("foo".to_string(), Expression::Simple(Value::Symbol(make_value_node("arg1".to_string())))),
                                 ],
                             })
               )
    );
        assert_eq!(expression(&b"select foo, 1, { foo = 2 };"[..]),
               IResult::Done(&b""[..],
                             Expression::Select(SelectDef{
                                 val: Box::new(Expression::Simple(Value::Symbol(make_value_node("foo".to_string())))),
                                 default: Box::new(Expression::Simple(Value::Int(make_value_node(1)))),
                                 tuple: vec![
                                     ("foo".to_string(), Expression::Simple(Value::Int(make_value_node(2))))
                                 ]
                             })
               )
    );
        assert_eq!(expression(&b"foo.bar (1, \"foo\")"[..]),
               IResult::Done(&b""[..],
                             Expression::Call(CallDef{
                                 macroref: vec!["foo".to_string(),"bar".to_string()],
                                 arglist: vec![
                                     Expression::Simple(Value::Int(make_value_node(1))),
                                     Expression::Simple(Value::String(make_value_node("foo".to_string()))),
                                 ],
                             })
               )
    );
        assert_eq!(expression(&b"(1 + 1)"[..]),
            IResult::Done(&b""[..],
                          Expression::Grouped(
                              Box::new(
                                  Expression::Add(
                                      BinaryExpression(Value::Int(make_value_node(1)),
                                      Box::new(Expression::Simple(Value::Int(make_value_node(1)))))
                                  )
                              )
                          )
            )
    );
    }

    #[test]
    fn test_format_parse() {
        assert!(format_expression(&b"\"foo"[..]).is_err() );
        assert!(format_expression(&b"\"foo\""[..]).is_incomplete() );
        assert!(format_expression(&b"\"foo\" %"[..]).is_incomplete() );
        assert!(format_expression(&b"\"foo\" % (1, 2"[..]).is_incomplete() );

        assert_eq!(format_expression(&b"\"foo @ @\" % (1, 2)"[..]),
               IResult::Done(&b""[..],
                             Expression::Format("foo @ @".to_string(),
                                                vec![Expression::Simple(Value::Int(make_value_node(1))),
                                                     Expression::Simple(Value::Int(make_value_node(2)))])
               )
        );
        assert_eq!(format_expression(&b"\"foo @ @\"%(1, 2)"[..]),
               IResult::Done(&b""[..],
                             Expression::Format("foo @ @".to_string(),
                                                vec![Expression::Simple(Value::Int(make_value_node(1))),
                                                     Expression::Simple(Value::Int(make_value_node(2)))])
               )
        );
    }

    #[test]
    fn test_call_parse() {
        assert!(call_expression(&b"foo"[..]).is_incomplete() );
        assert!(call_expression(&b"foo ("[..]).is_incomplete() );
        assert!(call_expression(&b"foo (1"[..]).is_incomplete() );
        assert!(call_expression(&b"foo (1,"[..]).is_err() );
        assert!(call_expression(&b"foo (1,2"[..]).is_incomplete() );

        assert_eq!(call_expression(&b"foo (1, \"foo\")"[..]),
               IResult::Done(&b""[..],
                             Expression::Call(CallDef{
                                 macroref: vec!["foo".to_string()],
                                 arglist: vec![
                                     Expression::Simple(Value::Int(make_value_node(1))),
                                     Expression::Simple(Value::String(make_value_node("foo".to_string()))),
                                 ],
                             })
               )
        );

        assert_eq!(call_expression(&b"foo.bar (1, \"foo\")"[..]),
               IResult::Done(&b""[..],
                             Expression::Call(CallDef{
                                 macroref: vec!["foo".to_string(),"bar".to_string()],
                                 arglist: vec![
                                     Expression::Simple(Value::Int(make_value_node(1))),
                                     Expression::Simple(Value::String(make_value_node("foo".to_string()))),
                                 ],
                             })
               )
    );
    }

    #[test]
    fn test_select_parse() {
        assert!(select_expression(&b"select"[..]).is_incomplete());
        assert!(select_expression(&b"select foo"[..]).is_incomplete());
        assert!(select_expression(&b"select foo, 1"[..]).is_incomplete());
        assert!(select_expression(&b"select foo, 1, {"[..]).is_incomplete());

        assert_eq!(select_expression(&b"select foo, 1, { foo = 2 };"[..]),
               IResult::Done(&b""[..],
                             Expression::Select(SelectDef{
                                 val: Box::new(Expression::Simple(Value::Symbol(make_value_node("foo".to_string())))),
                                 default: Box::new(Expression::Simple(Value::Int(make_value_node(1)))),
                                 tuple: vec![
                                     ("foo".to_string(), Expression::Simple(Value::Int(make_value_node(2))))
                                 ]
                             })
               )
    );
    }

    #[test]
    fn test_macro_expression_parsing() {
        assert!(macro_expression(&b"foo"[..]).is_err() );
        assert!(macro_expression(&b"macro \"foo\""[..]).is_err() );
        assert!(macro_expression(&b"macro 1"[..]).is_err() );
        assert!(macro_expression(&b"macro"[..]).is_incomplete() );
        assert!(macro_expression(&b"macro ("[..]).is_incomplete() );
        assert!(macro_expression(&b"macro (arg"[..]).is_incomplete() );
        assert!(macro_expression(&b"macro (arg, arg2"[..]).is_incomplete() );
        assert!(macro_expression(&b"macro (arg1, arg2) =>"[..]).is_incomplete() );
        assert!(macro_expression(&b"macro (arg1, arg2) => {"[..]).is_incomplete() );
        assert!(macro_expression(&b"macro (arg1, arg2) => { foo"[..]).is_incomplete() );
        assert!(macro_expression(&b"macro (arg1, arg2) => { foo ="[..]).is_incomplete() );

        assert_eq!(macro_expression(&b"macro (arg1, arg2) => {foo=1,bar=2}"[..]),
               IResult::Done(&b""[..],
                             Expression::Macro(MacroDef{
                                 argdefs: vec!["arg1".to_string(),
                                               "arg2".to_string()],
                                 fields: vec![("foo".to_string(), Expression::Simple(Value::Int(make_value_node(1)))),
                                             ("bar".to_string(), Expression::Simple(Value::Int(make_value_node(2))))
                                 ]
                             })
               )
    );
    }

    #[test]
    fn test_arglist_parse() {
        assert!(arglist(&b"arg"[..]).is_done());
        assert!(arglist(&b"arg1, arg2"[..]).is_done());
        assert_eq!(arglist(&b"arg1, arg2"[..]), IResult::Done(&b""[..],
                                                  vec![
                                                      Value::Symbol(make_value_node("arg1".to_string())),
                                                      Value::Symbol(make_value_node("arg2".to_string()))
                                                  ]));
    }

    #[test]
    fn test_copy_parse() {
        assert!(copy_expression(&b"{}"[..]).is_err() );
        assert!(copy_expression(&b"foo"[..]).is_incomplete() );
        assert!(copy_expression(&b"foo{"[..]).is_incomplete() );
        assert_eq!(copy_expression(&b"foo{}"[..]),
               IResult::Done(&b""[..],
                             Expression::Copy(CopyDef{selector: vec!["foo".to_string()],
                                                      fields: Vec::new()})
               )
    );
        assert_eq!(copy_expression(&b"foo{bar=1}"[..]),
               IResult::Done(&b""[..],
                             Expression::Copy(CopyDef{selector: vec!["foo".to_string()],
                                                      fields: vec![("bar".to_string(), Expression::Simple(Value::Int(make_value_node(1))))]})
               )
    );
    }

    #[test]
    fn test_grouped_expression_parse() {
        assert!(grouped_expression(&b"foo"[..]).is_err() );
        assert!(grouped_expression(&b"(foo"[..]).is_incomplete() );
        assert_eq!(grouped_expression(&b"(foo)"[..]),
            IResult::Done(&b""[..],
                          Expression::Grouped(
                              Box::new(
                                  Expression::Simple(
                                      Value::Symbol(make_value_node("foo".to_string()))))))
    );
        assert_eq!(grouped_expression(&b"(1 + 1)"[..]),
            IResult::Done(&b""[..],
                          Expression::Grouped(
                              Box::new(
                                  Expression::Add(
                                      BinaryExpression(Value::Int(make_value_node(1)),
                                                       Box::new(Expression::Simple(
                                                           Value::Int(make_value_node(1)))))
                                  )
                              )
                          )
            )
    );
    }

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
                              make_value_node(vec![]))));

        assert_eq!(tuple(&b"{ foo = 1 }"[..]),
            IResult::Done(&b""[..],
                          Value::Tuple(
                              make_value_node(vec![
                                  ("foo".to_string(), Expression::Simple(Value::Int(make_value_node(1))))
                              ]))));

        assert_eq!(tuple(&b"{ foo = 1, bar = \"1\" }"[..]),
            IResult::Done(&b""[..],
                          Value::Tuple(
                              make_value_node(vec![
                                  ("foo".to_string(), Expression::Simple(Value::Int(make_value_node(1)))),
                                  ("bar".to_string(), Expression::Simple(Value::String(make_value_node("1".to_string()))))
                              ]))));
        assert_eq!(tuple(&b"{ foo = 1, bar = {} }"[..]),
            IResult::Done(&b""[..],
                          Value::Tuple(
                              make_value_node(vec![
                                  ("foo".to_string(), Expression::Simple(Value::Int(make_value_node(1)))),
                                  ("bar".to_string(), Expression::Simple(Value::Tuple(make_value_node(Vec::new()))))
                              ]))));
    }

    #[test]
    fn test_field_value_parse() {
        assert!(field_value(&b"foo"[..]).is_incomplete() );
        assert!(field_value(&b"foo ="[..]).is_incomplete() );

        assert_eq!(field_value(&b"foo = 1"[..]),
               IResult::Done(&b""[..], ("foo".to_string(), Expression::Simple(Value::Int(make_value_node(1))))) );
        assert_eq!(field_value(&b"foo = \"1\""[..]),
               IResult::Done(&b""[..], ("foo".to_string(), Expression::Simple(Value::String(make_value_node("1".to_string()))))) );
        assert_eq!(field_value(&b"foo = bar"[..]),
               IResult::Done(&b""[..], ("foo".to_string(), Expression::Simple(Value::Symbol(make_value_node("bar".to_string()))))) );
        assert_eq!(field_value(&b"foo = bar "[..]),
               IResult::Done(&b""[..], ("foo".to_string(), Expression::Simple(Value::Symbol(make_value_node("bar".to_string()))))) );
    }

    #[test]
    fn test_number_parsing() {
        assert!(number(&b"."[..]).is_err() );
        assert!(number(&b". "[..]).is_err() );
        assert_eq!(number(&b"1.0"[..]),
               IResult::Done(&b""[..], Value::Float(make_value_node(1.0))) );
        assert_eq!(number(&b"1."[..]),
               IResult::Done(&b""[..], Value::Float(make_value_node(1.0))) );
        assert_eq!(number(&b"1"[..]),
               IResult::Done(&b""[..], Value::Int(make_value_node(1))) );
        assert_eq!(number(&b".1"[..]),
               IResult::Done(&b""[..], Value::Float(make_value_node(0.1))) );
    }

    #[test]
    fn test_symbol_parsing() {
        assert_eq!(symbol(&b"foo"[..]),
               IResult::Done(&b""[..], Value::Symbol(make_value_node("foo".to_string()))) );
        assert_eq!(symbol(&b"foo-bar"[..]),
               IResult::Done(&b""[..], Value::Symbol(make_value_node("foo-bar".to_string()))) );
        assert_eq!(symbol(&b"foo_bar"[..]),
               IResult::Done(&b""[..], Value::Symbol(make_value_node("foo_bar".to_string()))) );
    }

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
                       path: "mylib".to_string(),
                       name: "lib".to_string()
                   },
                   Statement::Let{
                       name: "foo".to_string(),
                       value: Expression::Simple(Value::Int(make_value_node(1)))
                   },
                   Statement::Expression(
                       Expression::Add(BinaryExpression(Value::Int(make_value_node(1)),
                                       Box::new(Expression::Simple(Value::Int(make_value_node(1))))))
                   )
               ]);
    }
}
