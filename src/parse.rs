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
use std::str::FromStr;
use std::error::Error;
use std::borrow::Borrow;

use nom::IResult;
use nom::InputLength;

use ast::*;
use tokenizer::*;

quick_error! {
    #[derive(Debug,PartialEq)]
    pub enum ParseError {
        UnexpectedToken(expected: String, actual: String) {
            description("Unexpected Token")
            display("Unexpected Token Expected {} Got {}", expected, actual)
        }
        EmptyExpression(msg: String ) {
            description("EmptyExpression")
            display("Unexpected EmptyExpression {}", msg)
        }
    }
}


// TODO(jwall): Error Reporting with Line and Column information.

type ParseResult<O> = Result<O, Box<Error>>;

fn symbol_to_value(s: Token) -> ParseResult<Value> {
    Ok(Value::Symbol(value_node!(s.fragment.to_string(), s.pos)))
}

// symbol is a bare unquoted field.
named!(symbol( Span ) -> Value, map_res!(barewordtok, symbol_to_value));

fn str_to_value(s: Token) -> ParseResult<Value> {
    Ok(Value::String(value_node!(s.fragment.to_string(), s.pos)))
}

// quoted_value is a quoted string.
named!(quoted_value( Span ) -> Value,
       map_res!(strtok, str_to_value)
);

// Helper function to make the return types work for down below.
fn triple_to_number(v: (Option<Token>, Option<Token>, Option<Token>)) -> ParseResult<Value> {
    let (pref, mut pref_pos) = match v.0 {
        None => {
            ("",
             Position {
                line: 0,
                column: 0,
            })
        }
        Some(ref bs) => (bs.fragment.borrow(), bs.pos.clone()),
    };

    let has_dot = v.1.is_some();

    if v.0.is_some() && !has_dot && v.2.is_none() {
        return Ok(Value::Int(value_node!(try!(FromStr::from_str(pref)), pref_pos)));
    }

    if v.0.is_none() && has_dot {
        pref_pos = v.1.unwrap().pos;
    }

    let suf = match v.2 {
        None => "",
        Some(ref bs) => &bs.fragment,
    };

    let to_parse = pref.to_string() + "." + suf;
    // TODO(jwall): if there is an error we should report where that error occured.
    let f = try!(FromStr::from_str(&to_parse));
    return Ok(Value::Float(value_node!(f, pref_pos)));
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
named!(number( Span ) -> Value,
       map_res!(alt!(
           complete!(do_parse!( // 1.0
               prefix: digittok >>
               has_dot: dottok >>
               suffix: digittok >>
               peek!(not!(digittok)) >>
               (Some(prefix), Some(has_dot), Some(suffix))
           )) |
           complete!(do_parse!( // 1.
               prefix: digittok >>
               has_dot: dottok >>
               peek!(not!(digittok)) >>
               (Some(prefix), Some(has_dot), None)
           )) |
           complete!(do_parse!( // .1
               has_dot: dottok >>
               suffix: digittok >>
               peek!(not!(digittok)) >>
               (None, Some(has_dot), Some(suffix))
           )) |
           do_parse!( // 1
               prefix: digittok >>
// The peek!(not!(..)) make this whole combinator slightly
// safer for partial inputs.
               peek!(not!(digittok)) >>
               (Some(prefix), None, None)
           )),
           triple_to_number
       )
);

named!(
    #[doc="Capture a field and value pair composed of `<symbol> = <value>,`"],
    field_value( Span ) -> (Token, Expression),
    do_parse!(
        field: barewordtok >>
            ws!(equaltok) >>
            value: expression >>
            (field, value)
    )
);

// Helper function to make the return types work for down below.
fn vec_to_tuple(t: (Span, FieldList)) -> ParseResult<Value> {
    Ok(Value::Tuple(value_node!(t.1, Position{line: t.0.line as usize, column: t.0.offset as usize})))
}

named!(field_list( Span ) -> FieldList,
       separated_list!(commatok, ws!(field_value)));

named!(
    #[doc="Capture a tuple of named fields with values. {<field>=<value>,...}"],
    tuple( Span ) -> Value,
    map_res!(
        do_parse!(
            pos: position!() >>
            v: delimited!(lbracetok,
                   ws!(field_list),
                   rbracetok) >>
                   (pos, v)
        ),
        vec_to_tuple
    )
);

pub fn selector_or_symbol(input: Span) -> IResult<Span, Value> {
    let sym = do_parse!(input,
        sym: symbol >>
             not!(dottok) >>
             (sym)
    );
    match sym {
        IResult::Incomplete(i) => {
            return IResult::Incomplete(i);
        },
        IResult::Error(_) => {
            // TODO(jwall): Maybe be smarter about the error reporting here?
            return ws!(input, selector_value);
        },
        IResult::Done(rest, val) => {
            return IResult::Done(rest, val);
        }
    }
}

named!(value( Span ) -> Value, alt!(number | quoted_value | tuple | selector_or_symbol ));

fn value_to_expression(v: Value) -> ParseResult<Expression> {
    Ok(Expression::Simple(v))
}

named!(simple_expression( Span ) -> Expression,
       map_res!(
           value,
           value_to_expression
       )
);

fn tuple_to_binary_expression(tpl: (Span, BinaryExprType, Value, Expression))
                              -> ParseResult<Expression> {
    Ok(Expression::Binary(BinaryOpDef {
        kind: tpl.1,
        left: tpl.2,
        right: Box::new(tpl.3),
        pos: Position {
            line: tpl.0.line as usize,
            column: tpl.0.offset as usize,
        },
    }))
}

macro_rules! do_binary_expr {
    ($i:expr, $fn:expr, $typ:expr) => {
        // NOTE(jwall): Nom macros do magic with their inputs. They in fact
        // rewrite your macro argumets for you. Which means we require this $i
        // paramater even though we don't explicitely pass it below. I don't
        // particularly like this but I'm living with it for now.
        map_res!(
            $i, do_parse!(
                pos: position!() >>
                left: value >>
                    ws!($fn) >>
                    right: expression >>
                    (pos, $typ, left, right)
            ),
            tuple_to_binary_expression
        )
    }
}

named!(add_expression( Span ) -> Expression,
       do_binary_expr!(plustok, BinaryExprType::Add)
);

named!(sub_expression( Span ) -> Expression,
       do_binary_expr!(dashtok, BinaryExprType::Sub)
);

named!(mul_expression( Span ) -> Expression,
       do_binary_expr!(startok, BinaryExprType::Mul)
);

named!(div_expression( Span ) -> Expression,
       do_binary_expr!(slashtok, BinaryExprType::Div)
);

fn expression_to_grouped_expression(e: Expression) -> ParseResult<Expression> {
    Ok(Expression::Grouped(Box::new(e)))
}

named!(grouped_expression( Span ) -> Expression,
       map_res!(
           preceded!(lparentok, terminated!(expression, rparentok)),
           expression_to_grouped_expression
       )
);

fn assert_nonempty_list<T>(v: Vec<T>) -> ParseResult<Vec<T>> {
    if v.is_empty() {
        return Err(Box::new(ParseError::EmptyExpression("Selectors can't be empty.".to_string())));
    }
    return Ok(v);
}

named!(selector_list( Span ) -> SelectorList,
    map_res!(
        separated_list!(dottok, barewordtok),
        assert_nonempty_list
    )
);

fn tuple_to_copy(t: (Span, SelectorList, FieldList)) -> ParseResult<Expression> {
    Ok(Expression::Copy(CopyDef {
        selector: t.1,
        fields: t.2,
        pos: Position {
            line: t.0.line as usize,
            column: t.0.offset as usize,
        },
    }))
}

named!(copy_expression( Span ) -> Expression,
       map_res!(
           do_parse!(
               pos: position!() >>
               selector: selector_list >>
               lbracetok >>
               fields: ws!(field_list) >>
               rbracetok >>
               (pos, selector, fields)
           ),
           tuple_to_copy
       )
);

fn tuple_to_macro(mut t: (Span, Vec<Value>, Value)) -> ParseResult<Expression> {
    match t.2 {
        Value::Tuple(v) => {
            Ok(Expression::Macro(MacroDef {
                argdefs: t.1
                    .drain(0..)
                    .map(|s| {
                        Positioned {
                            pos: s.pos().clone(),
                            val: s.to_string(),
                        }
                    })
                    .collect(),
                fields: v.val,
                pos: Position {
                    line: t.0.line as usize,
                    column: t.0.offset as usize,
                },
            }))
        }
        // TODO(jwall): Show a better version of the unexpected parsed value.
        val => {
            Err(Box::new(ParseError::UnexpectedToken("{ .. }".to_string(), format!("{:?}", val))))
        }
    }
}

named!(arglist( Span ) -> Vec<Value>, separated_list!(ws!(commatok), symbol));

named!(macro_expression( Span ) -> Expression,
       map_res!(
           do_parse!(
                pos: position!() >>
                macrotok >>
                ws!(lparentok) >>
                arglist: ws!(arglist) >>
                rparentok >>
                ws!(fatcommatok) >>
                map: tuple >>
                (pos, arglist, map)
           ),
           tuple_to_macro
       )
);

fn tuple_to_select(t: (Span, Expression, Expression, Value)) -> ParseResult<Expression> {
    match t.3 {
        Value::Tuple(v) => {
            Ok(Expression::Select(SelectDef {
                val: Box::new(t.1),
                default: Box::new(t.2),
                tuple: v.val,
                pos: Position {
                    line: t.0.line as usize,
                    column: t.0.offset as usize,
                },
            }))
        }
        // TODO(jwall): Show a better version of the unexpected parsed value.
        val => {
            Err(Box::new(ParseError::UnexpectedToken("{ .. }".to_string(), format!("{:?}", val))))
        }
    }
}

named!(select_expression( Span ) -> Expression,
       map_res!(
           do_parse!(
               pos: position!() >>
               selecttok >>
               val: ws!(terminated!(expression, commatok)) >>
               default: ws!(terminated!(expression, commatok)) >>
               map: ws!(tuple) >>
               (pos, val, default, map)
           ),
           tuple_to_select
       )
);

fn tuple_to_format(t: (Token, Vec<Expression>)) -> ParseResult<Expression> {
    Ok(Expression::Format(FormatDef {
        template: t.0.fragment.to_string(),
        args: t.1,
        pos: t.0.pos,
    }))
}

named!(format_expression( Span ) -> Expression,
       map_res!(
           do_parse!(
               tmpl: ws!(strtok) >>
                   ws!(pcttok) >>
                   lparentok >>
                   args: ws!(separated_list!(ws!(commatok), expression)) >>
                   rparentok >>
                   (tmpl, args)
           ),
           tuple_to_format
       )
);

fn tuple_to_call(t: (Span, Value, Vec<Expression>)) -> ParseResult<Expression> {
    if let Value::Selector(sl) = t.1 {
        Ok(Expression::Call(CallDef {
            macroref: sl.val,
            arglist: t.2,
            pos: Position {
                line: t.0.line as usize,
                column: t.0.offset as usize,
            },
        }))
    } else {
        Err(Box::new(ParseError::UnexpectedToken("Selector".to_string(), format!("{:?}", t.0))))
    }
}

fn vec_to_selector_value(t: (Span, SelectorList)) -> ParseResult<Value> {
    Ok(Value::Selector(value_node!(t.1, Position{line: t.0.line as usize, column: t.0.offset as usize})))
}

named!(selector_value( Span ) -> Value,
       map_res!(
           do_parse!(
               pos: position!() >>
               sl: ws!(selector_list) >>
               (pos, sl)
           ),
           vec_to_selector_value
       )
);

named!(call_expression( Span ) -> Expression,
       map_res!(
           do_parse!(
               pos: position!() >>
               macroname: selector_value >>
               lparentok >>
               args: ws!(separated_list!(ws!(commatok), expression)) >>
               rparentok >>
               (pos, macroname, args)
           ),
           tuple_to_call
       )
);

fn tuple_to_list<Sp: Into<Position> >(t: (Sp, Vec<Expression>)) -> ParseResult<Expression> {
    return Ok(Expression::List(ListDef{
        elems: t.1,
        pos: t.0.into(),
    }));
}

named!(list_expression( Span ) -> Expression,
       map_res!(
           do_parse!(
               pos: position!() >>
               leftsquarebracket >>
               elements: ws!(separated_list!(ws!(commatok), expression)) >>
               rightsquarebracket >>
               (pos, elements)
           ),
           tuple_to_list
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
named!(expression( Span ) -> Expression,
       alt!(
           complete!(add_expression) |
           complete!(sub_expression) |
           complete!(mul_expression) |
           complete!(div_expression) |
           complete!(grouped_expression) |
           complete!(list_expression) |
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

named!(expression_statement( Span ) -> Statement,
       map_res!(
           terminated!(ws!(expression), semicolontok),
           expression_to_statement
       )
);

fn tuple_to_let(t: (Token, Expression)) -> ParseResult<Statement> {
    Ok(Statement::Let {
        name: t.0,
        value: t.1,
    })
}

named!(let_statement( Span ) -> Statement,
       map_res!(
           terminated!(do_parse!(
               lettok >>
                   name: ws!(barewordtok) >>
                   equaltok >>
                   val: ws!(expression) >>
                   (name, val)
           ), semicolontok),
           tuple_to_let
       )
);

fn tuple_to_import(t: (Token, Token)) -> ParseResult<Statement> {
    Ok(Statement::Import {
        name: t.0,
        path: t.1.fragment.to_string(),
    })
}

named!(import_statement( Span ) -> Statement,
       map_res!(
           terminated!(do_parse!(
               importtok >>
                   path: ws!(strtok) >>
                   astok >>
                   name: ws!(barewordtok) >>
                   (name, path)
           ), semicolontok),
           tuple_to_import
       )
);

named!(statement( Span ) -> Statement,
       alt_complete!(
           import_statement |
           let_statement |
           expression_statement
       )
);

pub fn parse(input: Span) -> IResult<Span, Vec<Statement>> {
    let mut out = Vec::new();
    let mut i = input;
    loop {
        match ws!(i, statement) {
            IResult::Error(e) => {
                return IResult::Error(e);
            },
            IResult::Incomplete(i) => {
                return IResult::Incomplete(i);
            },
            IResult::Done(rest, stmt) => {
                out.push(stmt);
                i = rest;
                if i.input_len() == 0 {
                    break;
                }
            }
        }
    }
    return IResult::Done(i, out);
}

//named!(pub parse( Span ) -> Vec<Statement>, many1!());

#[cfg(test)]
mod test {
    use super::{Statement, Expression, Value, MacroDef, SelectDef, CallDef};
    use super::{number, symbol, parse, field_value, selector_value, selector_or_symbol, tuple, grouped_expression, list_expression};
    use super::{copy_expression, macro_expression, select_expression};
    use super::{format_expression, call_expression, expression};
    use super::{expression_statement, let_statement, import_statement, statement};
    use ast::*;
    use nom_locate::LocatedSpan;

    use nom::{Needed, IResult};

    #[test]
    fn test_symbol_parsing() {
        assert_eq!(symbol(LocatedSpan::new("foo")),
               IResult::Done(LocatedSpan{fragment: "", offset: 3, line: 1},
               Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 1}))) );
        assert_eq!(symbol(LocatedSpan::new("foo-bar")),
               IResult::Done(LocatedSpan{fragment: "", offset: 7, line: 1},
               Value::Symbol(value_node!("foo-bar".to_string(), Position{line: 1, column: 1}))) );
        assert_eq!(symbol(LocatedSpan::new("foo_bar")),
               IResult::Done(LocatedSpan{fragment: "", offset: 7, line: 1},
               Value::Symbol(value_node!("foo_bar".to_string(), Position{line: 1, column: 1}))) );
    }

    #[test]
    fn test_selector_parsing() {
        assert_eq!(selector_value(LocatedSpan::new("foo.")),
            IResult::Incomplete(Needed::Unknown)
        );
        assert_eq!(selector_value(LocatedSpan::new("foo.bar ")),
          IResult::Done(LocatedSpan{fragment: "", offset: 8, line: 1},
          Value::Selector(value_node!(vec![Token{fragment:"foo".to_string(), pos: Position{line: 1, column: 1}},
                                           Token{fragment:"bar".to_string(), pos: Position{line: 1, column: 5}}],
                                      Position{line: 1, column: 0})))
        );
        assert_eq!(selector_value(LocatedSpan::new("foo.bar;")),
            IResult::Done(LocatedSpan{fragment: ";", offset: 7, line: 1},
            Value::Selector(value_node!(vec![Token{fragment:"foo".to_string(), pos: Position{line: 1, column: 1}},
                                             Token{fragment:"bar".to_string(), pos: Position{line: 1, column: 5}}],
                                        Position{line: 1, column: 0})))
        );
    }

    #[test]
    fn test_selector_or_symbol_parsing() {
        assert_eq!(selector_or_symbol(LocatedSpan::new("foo.")),
            IResult::Incomplete(Needed::Unknown)
        );
        assert_eq!(selector_or_symbol(LocatedSpan::new("foo")),
               IResult::Done(LocatedSpan{fragment: "", offset: 3, line: 1},
               Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 1}))) );
        assert_eq!(selector_or_symbol(LocatedSpan::new("foo.bar ")),
          IResult::Done(LocatedSpan{fragment: "", offset: 8, line: 1},
          Value::Selector(value_node!(vec![Token{fragment:"foo".to_string(), pos: Position{line: 1, column: 1}},
                                           Token{fragment:"bar".to_string(), pos: Position{line: 1, column: 5}}],
                                      Position{line: 1, column: 0})))
        );
    }

    #[test]
    fn test_statement_parse() {
        let mut stmt = "import \"foo\" as foo;";
        let input = LocatedSpan::new(stmt);
        assert_eq!(statement(input),
               IResult::Done(
                   LocatedSpan{
                       offset: stmt.len(),
                       line: 1,
                       fragment: "",
                   },
                   Statement::Import{
                       path: "foo".to_string(),
                       name: Token{
                          fragment: "foo".to_string(),
                          pos: Position{
                              line: 1,
                              column: 17,
                          },
                      }
                   }
               )
        );

        assert!(statement(LocatedSpan::new("import foo")).is_err() );

        stmt = "let foo = 1.0 ;";
        let input = LocatedSpan::new(stmt);
        assert_eq!(statement(input),
                IResult::Done(
                    LocatedSpan{
                        offset: stmt.len(),
                        line: 1,
                        fragment: "",
                    },
                    Statement::Let{
                        name: Token{
                            fragment: "foo".to_string(),
                            pos: Position {
                                line: 1,
                                column: 5,
                           },
                       },
                       value: Expression::Simple(Value::Float(value_node!(1.0, Position{line: 1, column: 11})))
        }));
        stmt = "1.0;";
        let input = LocatedSpan::new(stmt);
        assert_eq!(statement(input),
                IResult::Done(
                    LocatedSpan{
                        offset: stmt.len(),
                        line: 1,
                        fragment: "",
                    },
                Statement::Expression(
                    Expression::Simple(Value::Float(value_node!(1.0, Position{line: 1, column: 1}))))));
    }

    #[test]
    fn test_import_parse() {
        assert!(import_statement(LocatedSpan::new("import")).is_incomplete());
        assert!(import_statement(LocatedSpan::new("import \"foo\"")).is_incomplete());
        assert!(import_statement(LocatedSpan::new("import \"foo\" as")).is_incomplete());
        assert!(import_statement(LocatedSpan::new("import \"foo\" as foo")).is_incomplete());

        let import_stmt = "import \"foo\" as foo;";
        assert_eq!(import_statement(LocatedSpan::new(import_stmt)),
               IResult::Done(LocatedSpan{
                        fragment: "",
                        line: 1,
                        offset: import_stmt.len(),
                    },
                    Statement::Import{
                        path: "foo".to_string(),
                        name: Token{
                                fragment: "foo".to_string(),
                                pos: Position{
                                    line: 1,
                                    column: 17,
                                },
                            }
                    }
               )
    );
    }

    #[test]
    fn test_let_statement_parse() {
        assert!(let_statement(LocatedSpan::new("foo")).is_err() );
        assert!(let_statement(LocatedSpan::new("let \"foo\"")).is_err() );
        assert!(let_statement(LocatedSpan::new("let 1")).is_err() );
        assert!(let_statement(LocatedSpan::new("let")).is_incomplete() );
        assert!(let_statement(LocatedSpan::new("let foo")).is_incomplete() );
        assert!(let_statement(LocatedSpan::new("let foo =")).is_incomplete() );
        assert!(let_statement(LocatedSpan::new("let foo = ")).is_incomplete() );
        assert!(let_statement(LocatedSpan::new("let foo = 1")).is_incomplete() );

        let mut let_stmt = "let foo = 1.0 ;";
        assert_eq!(let_statement(LocatedSpan::new(let_stmt)),
               IResult::Done(LocatedSpan{
                    fragment: "",
                    offset: let_stmt.len(),
                    line: 1,
                },
                Statement::Let{name: Token{
                    fragment: "foo".to_string(),
                    pos: Position{
                            line: 1,
                            column: 5,
                         },
                    },
                    value: Expression::Simple(Value::Float(value_node!(1.0, Position{line: 1, column: 11})))
                }));

        let_stmt = "let foo= 1.0;";
        assert_eq!(let_statement(LocatedSpan::new(let_stmt)),
               IResult::Done(LocatedSpan{
                    fragment: "",
                    offset: let_stmt.len(),
                    line: 1,
                },
                Statement::Let{name: Token{
                    fragment: "foo".to_string(),
                    pos: Position{
                        line: 1,
                        column: 5,
                    }
                },
                value: Expression::Simple(Value::Float(value_node!(1.0, Position{line: 1, column: 10})))}));
        let_stmt = "let foo =1.0;";
        assert_eq!(let_statement(LocatedSpan::new(let_stmt)),
               IResult::Done(LocatedSpan{
                    fragment: "",
                    offset: let_stmt.len(),
                    line: 1,
                },
                Statement::Let{name: Token{
                    fragment: "foo".to_string(),
                    pos: Position{
                        line: 1,
                        column: 5,
                    }
                },
                value: Expression::Simple(Value::Float(value_node!(1.0, Position{line: 1, column: 10})))}));
    }

    #[test]
    fn test_expression_statement_parse() {
        assert!(expression_statement(LocatedSpan::new("foo")).is_incomplete() );
        assert_eq!(expression_statement(LocatedSpan::new("1.0;")),
               IResult::Done(LocatedSpan{
                   fragment: "",
                   offset: 4,
                   line: 1,
               },
                             Statement::Expression(
                                 Expression::Simple(Value::Float(value_node!(1.0, Position{line: 1, column: 1}))))));
        assert_eq!(expression_statement(LocatedSpan::new("1.0 ;")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 5,
                  line: 1,
                  },
                             Statement::Expression(
                                 Expression::Simple(Value::Float(value_node!(1.0, Position{line: 1, column: 1}))))));
        assert_eq!(expression_statement(LocatedSpan::new(" 1.0;")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 5,
                  line: 1,
                  },
                             Statement::Expression(
                                 Expression::Simple(Value::Float(value_node!(1.0, Position{line: 1, column: 2}))))));
        assert_eq!(expression_statement(LocatedSpan::new("foo;")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 4,
                  line: 1,
                  },
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 1}))))));
        assert_eq!(expression_statement(LocatedSpan::new("foo ;")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 5,
                  line: 1,
                  },
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 1}))))));
        assert_eq!(expression_statement(LocatedSpan::new(" foo;")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 5,
                  line: 1,
                  },
                             Statement::Expression(
                                 Expression::Simple(Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 2}))))));
        assert_eq!(expression_statement(LocatedSpan::new("\"foo\";")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 6,
                  line: 1,
                  },
                             Statement::Expression(
                                 Expression::Simple(Value::String(value_node!("foo".to_string(), Position{line: 1, column: 1}))))));
        assert_eq!(expression_statement(LocatedSpan::new("\"foo\" ;")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 7,
                  line: 1,
                  },
                             Statement::Expression(
                                 Expression::Simple(Value::String(value_node!("foo".to_string(), Position{line: 1, column: 1}))))));
        assert_eq!(expression_statement(LocatedSpan::new(" \"foo\";")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 7,
                  line: 1,
                  },
                             Statement::Expression(
                                 Expression::Simple(Value::String(value_node!("foo".to_string(), Position{line: 1, column: 2}))))));
    }

    #[test]
    fn test_expression_parse() {
        assert_eq!(expression(LocatedSpan::new("1")),
              IResult::Done(LocatedSpan {
                 fragment: "",
                 offset: 1,
                 line: 1,
                 },
              Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 1})))));
        assert_eq!(expression(LocatedSpan::new("foo")),
              IResult::Done(LocatedSpan {
                 fragment: "",
                 offset: 3,
                 line: 1,
                 },
              Expression::Simple(Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 1})))));
        assert_eq!(expression(LocatedSpan::new("foo.bar ")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 8,
                  line: 1,
                  },
               Expression::Simple(Value::Selector(make_value_node(vec![Token::new("foo", Position{line: 1, column: 1}),
                                                                       Token::new("bar", Position{line: 1, column: 5})], 1, 0)))));
        assert_eq!(expression(LocatedSpan::new("1 + 1")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 5,
                  line: 1,
                  },
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Int(value_node!(1, Position{line: 1, column: 1})),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 5})))),
                    pos: Position { line: 1, column: 0 },
                })));
        assert_eq!(expression(LocatedSpan::new("1 - 1")),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 5,
                  line: 1,
                  },
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Sub,
                    left: Value::Int(value_node!(1, Position{line: 1, column: 1})),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 5})))),
                    pos: Position { line: 1, column: 0 },
                })));
        assert_eq!(expression(LocatedSpan::new("1 * 1")),
                IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 5,
                  line: 1,
                  },
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Mul,
                    left: Value::Int(value_node!(1, Position{line: 1, column: 1})),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 5})))),
                    pos: Position { line: 1, column: 0 },
                })));
        assert_eq!(expression(LocatedSpan::new("1 / 1")),
                IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 5,
                  line: 1,
                  },
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Div,
                    left: Value::Int(value_node!(1, Position{line: 1, column: 1})),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 5})))),
                    pos: Position { line: 1, column: 0 },
                })));

        assert_eq!(expression(LocatedSpan::new("1+1")),
                IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 3,
                  line: 1,
                  },
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Int(value_node!(1, Position{line: 1, column: 1})),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 3})))),
                    pos: Position { line: 1, column: 0 },
                })));
        assert_eq!(expression(LocatedSpan::new("1-1")),
                IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 3,
                  line: 1,
                  },
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Sub,
                    left: Value::Int(value_node!(1, Position{line: 1, column: 1})),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 3})))),
                    pos: Position { line: 1, column: 0 },
                })));
        assert_eq!(expression(LocatedSpan::new("1*1")),
                IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 3,
                  line: 1,
                  },
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Mul,
                    left: Value::Int(value_node!(1, Position{line: 1, column: 1})),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 3})))),
                    pos: Position { line: 1, column: 0 },
                })));
        assert_eq!(expression(LocatedSpan::new("1/1")),
                IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: 3,
                  line: 1,
                  },
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Div,
                    left: Value::Int(value_node!(1, Position{line: 1, column: 1})),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 3})))),
                    pos: Position { line: 1, column: 0 },
                })));
        let macro_expr = "macro (arg1, arg2) => { foo = arg1 }";
        assert_eq!(expression(LocatedSpan::new(macro_expr)),
                IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: macro_expr.len(),
                  line: 1,
                  },
                Expression::Macro(MacroDef{
                    argdefs: vec![
                        Positioned::new("arg1".to_string(), Position{line: 1, column: 8}),
                        Positioned::new("arg2".to_string(), Position{line: 1, column: 14}),
                    ],
                    fields: vec![
                        (Token::new("foo", Position{line: 1, column: 25}),
                         Expression::Simple(Value::Symbol(value_node!("arg1".to_string(), Position{line: 1, column: 31})))),
                    ],
                    pos: Position{line: 1, column: 0},
                })
               )
    );
        let select_expr = "select foo, 1, { foo = 2 }";
        assert_eq!(expression(LocatedSpan::new(select_expr)),
            IResult::Done(LocatedSpan {
              fragment: "",
              offset: select_expr.len(),
              line: 1,
              },
            Expression::Select(SelectDef{
                val: Box::new(Expression::Simple(Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 8})))),
                default: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 13})))),
                tuple: vec![
                    (Token::new("foo", Position{line: 1, column: 18}),
                     Expression::Simple(Value::Int(value_node!(2, Position{line: 1, column: 24}))))
                ],
                pos: Position{line: 1, column: 0},
            })
           )
    );
        let call_expr = "foo.bar (1, \"foo\")";
        assert_eq!(expression(LocatedSpan::new(call_expr)),
            IResult::Done(LocatedSpan {
               fragment: "",
               offset: call_expr.len(),
               line: 1,
               },
            Expression::Call(CallDef{
                macroref: vec![Token::new("foo", Position{line:1,column: 1}),
                               Token::new("bar", Position{line:1,column: 5})],
                arglist: vec![
                    Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 10}))),
                    Expression::Simple(Value::String(value_node!("foo".to_string(), Position{line: 1, column: 13}))),
                ],
                pos: Position{line: 1, column: 0},
            })
           )
    );
        assert_eq!(expression(LocatedSpan::new("(1 + 1)")),
            IResult::Done(LocatedSpan {
              fragment: "",
              offset: 7,
              line: 1,
              },
            Expression::Grouped(
                Box::new(
                    Expression::Binary(
                        BinaryOpDef{
                            kind: BinaryExprType::Add,
                            left: Value::Int(value_node!(1, Position{line: 1, column: 2})),
                            right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 6})))),
                            pos: Position { line: 1, column: 1 },
                        }
                    )
                )
            )
        )
    );
        assert_eq!(expression(LocatedSpan::new("[1, 1]")),
            IResult::Done(LocatedSpan{fragment: "", offset: 6, line: 1},
                Expression::List(
                    ListDef{
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 2}))),
                            Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 5}))),
                        ],
                        pos: Position{line: 1, column: 1},
                    }
                )
            )
        );
    }

    #[test]
    fn test_format_parse() {
        assert!(format_expression(LocatedSpan::new("\"foo")).is_err() );
        assert!(format_expression(LocatedSpan::new("\"foo\"")).is_incomplete() );
        assert!(format_expression(LocatedSpan::new("\"foo\" %")).is_incomplete() );
        assert!(format_expression(LocatedSpan::new("\"foo\" % (1, 2")).is_incomplete() );

        let mut fmt_expr = "\"foo @ @\" % (1, 2)";
        assert_eq!(format_expression(LocatedSpan::new(fmt_expr)),
               IResult::Done(LocatedSpan{
                        fragment: "",
                        offset: fmt_expr.len(),
                        line: 1
                    },
                    Expression::Format(
                        FormatDef{
                            template: "foo @ @".to_string(),
                            args: vec![Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 14}))),
                                       Expression::Simple(Value::Int(value_node!(2, Position{line: 1, column: 17})))],
                            pos: Position{line: 1, column: 1},
                        }
                    )
               )
        );

        fmt_expr = "\"foo @ @\"%(1, 2)";
        assert_eq!(format_expression(LocatedSpan::new(fmt_expr)),
            IResult::Done(LocatedSpan{
                     fragment: "",
                     offset: fmt_expr.len(),
                     line: 1,
                },
                Expression::Format(
                    FormatDef{
                        template: "foo @ @".to_string(),
                        args: vec![Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 12}))),
                                   Expression::Simple(Value::Int(value_node!(2, Position{line: 1, column: 15})))],
                        pos: Position { line: 1, column: 1 },
                    }
                )
            )
        );
    }

    #[test]
    fn test_call_parse() {
        assert!(call_expression(LocatedSpan::new("foo")).is_incomplete() );
        assert!(call_expression(LocatedSpan::new("foo (")).is_incomplete() );
        assert!(call_expression(LocatedSpan::new("foo (1")).is_incomplete() );
        assert!(call_expression(LocatedSpan::new("foo (1,")).is_incomplete() );
        assert!(call_expression(LocatedSpan::new("foo (1,2")).is_incomplete() );

        let mut copy_expr = "foo (1, \"foo\")";
        assert_eq!(call_expression(LocatedSpan::new(copy_expr)),
               IResult::Done(
                    LocatedSpan{
                        fragment: "",
                        line: 1,
                        offset: copy_expr.len(),
                    },
                    Expression::Call(CallDef{
                        macroref: vec![Token::new("foo", Position{line:1, column: 1})],
                        arglist: vec![
                            Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 6}))),
                            Expression::Simple(Value::String(value_node!("foo".to_string(), Position{line: 1, column: 9}))),
                        ],
                        pos: Position{line: 1, column: 0},
                    })
               )
        );

        copy_expr = "foo.bar (1, \"foo\")";
        assert_eq!(call_expression(LocatedSpan::new(copy_expr)),
               IResult::Done(
                    LocatedSpan{
                        fragment: "",
                        line: 1,
                        offset: copy_expr.len(),
                    },
                    Expression::Call(CallDef{
                        macroref: vec![Token::new("foo", Position{line: 1, column: 1}),
                                       Token::new("bar", Position{line: 1, column: 5})],
                        arglist: vec![
                            Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 10}))),
                            Expression::Simple(Value::String(value_node!("foo".to_string(), Position{line: 1, column: 13}))),
                        ],
                        pos: Position{line: 1, column: 0},
                    })
               )
    );
    }

    #[test]
    fn test_select_parse() {
        assert!(select_expression(LocatedSpan::new("select")).is_incomplete());
        assert!(select_expression(LocatedSpan::new("select foo")).is_incomplete());
        assert!(select_expression(LocatedSpan::new("select foo, 1")).is_incomplete());
        assert!(select_expression(LocatedSpan::new("select foo, 1, {")).is_incomplete());

        let select_expr = "select foo, 1, { foo = 2 }";
        assert_eq!(select_expression(LocatedSpan::new(select_expr)),
                IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: select_expr.len(),
                  line: 1,
                  },
                Expression::Select(SelectDef{
                    val: Box::new(Expression::Simple(Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 8})))),
                    default: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 13})))),
                    tuple: vec![
                        (Token::new("foo", Position{line: 1, column: 18}), Expression::Simple(Value::Int(value_node!(2, Position{line: 1, column: 24}))))
                    ],
                    pos: Position{line: 1, column: 0},
                })
               )
    );
    }

    #[test]
    fn test_macro_expression_parsing() {
        assert!(macro_expression(LocatedSpan::new("foo")).is_err() );
        assert!(macro_expression(LocatedSpan::new("macro \"foo\"")).is_err() );
        assert!(macro_expression(LocatedSpan::new("macro 1")).is_err() );
        assert!(macro_expression(LocatedSpan::new("macro")).is_incomplete() );
        assert!(macro_expression(LocatedSpan::new("macro (")).is_incomplete() );
        assert!(macro_expression(LocatedSpan::new("macro (arg")).is_incomplete() );
        assert!(macro_expression(LocatedSpan::new("macro (arg, arg2")).is_incomplete() );
        assert!(macro_expression(LocatedSpan::new("macro (arg1, arg2) =>")).is_incomplete() );
        assert!(macro_expression(LocatedSpan::new("macro (arg1, arg2) => {")).is_incomplete() );
        assert!(macro_expression(LocatedSpan::new("macro (arg1, arg2) => { foo")).is_incomplete() );
        assert!(macro_expression(LocatedSpan::new("macro (arg1, arg2) => { foo =")).is_incomplete() );

        let macro_expr = "macro (arg1, arg2) => {foo=1,bar=2}";
        assert_eq!(macro_expression(LocatedSpan::new(macro_expr)),
               IResult::Done(
                    LocatedSpan{
                        fragment: "",
                        offset: macro_expr.len(),
                        line: 1
                    },
                    Expression::Macro(MacroDef{
                        argdefs: vec![Positioned::new("arg1".to_string(), Position{line: 1, column: 8}),
                                      Positioned::new("arg2".to_string(), Position{line: 1, column: 14})],
                        fields: vec![(Token::new("foo", Position{line: 1, column: 24}), Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 28})))),
                                     (Token::new("bar", Position{line: 1, column: 30}), Expression::Simple(Value::Int(value_node!(2, Position{line: 1, column: 34}))))
                        ],
                        pos: Position{line: 1, column: 0},
                    })
               )
    );
    }

    #[test]
    fn test_copy_parse() {
        assert!(copy_expression(LocatedSpan::new("{}")).is_err() );
        assert!(copy_expression(LocatedSpan::new("foo")).is_incomplete() );
        assert!(copy_expression(LocatedSpan::new("foo{")).is_incomplete() );

        let mut copy_expr = "foo{}";
        assert_eq!(copy_expression(LocatedSpan::new(copy_expr)),
               IResult::Done(
                    LocatedSpan{
                        fragment: "",
                        offset: copy_expr.len(),
                        line: 1
                    },
                    Expression::Copy(CopyDef{
                        selector: vec![Token::new("foo", Position{line: 1, column: 1})],
                        fields: Vec::new(),
                        pos: Position{line: 1, column: 0},
                    })
               )
        );

        copy_expr = "foo{bar=1}";
        assert_eq!(copy_expression(LocatedSpan::new(copy_expr)),
            IResult::Done(
                LocatedSpan{
                    fragment: "",
                    offset: copy_expr.len(),
                    line: 1
                },
                Expression::Copy(CopyDef{
                    selector: vec![Token::new("foo", Position{line: 1, column: 1})],
                    fields: vec![(Token::new("bar", Position{line: 1, column: 5}),
                                  Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 9}))))],
                    pos: Position{line: 1, column: 0},
                })
            )
        );
    }

    #[test]
    fn test_grouped_expression_parse() {
        assert!(grouped_expression(LocatedSpan::new("foo")).is_err() );
        assert!(grouped_expression(LocatedSpan::new("(foo")).is_incomplete() );
        assert_eq!(grouped_expression(LocatedSpan::new("(foo)")),
            IResult::Done(LocatedSpan{fragment: "", offset: 5, line: 1},
                          Expression::Grouped(
                              Box::new(
                                  Expression::Simple(
                                      Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 2}))))))
    );
        assert_eq!(grouped_expression(LocatedSpan::new("(1 + 1)")),
            IResult::Done(LocatedSpan{fragment: "", offset: 7, line: 1},
                          Expression::Grouped(
                              Box::new(
                                  Expression::Binary(
                                      BinaryOpDef{
                                          kind: BinaryExprType::Add,
                                          left: Value::Int(value_node!(1, Position{line: 1, column: 2})),
                                          right: Box::new(Expression::Simple(
                                              Value::Int(value_node!(1, Position{line: 1, column: 6})))),
                                          pos: Position { line: 1, column: 1 },
                                      }
                                  )
                              )
                          )
            )
    );
    }

    #[test]
    fn test_list_expression_parse() {
        assert!(list_expression(LocatedSpan::new("foo")).is_err() );
        assert!(list_expression(LocatedSpan::new("[foo")).is_incomplete() );
        assert_eq!(list_expression(LocatedSpan::new("[foo]")),
            IResult::Done(LocatedSpan{fragment: "", offset: 5, line: 1},
                          Expression::List(
                              ListDef{
                                      elems: vec![
                                                Expression::Simple(Value::Symbol(value_node!("foo".to_string(), Position{line: 1, column: 2})))
                                             ],
                                      pos: Position{ line: 1, column: 1}
                                     }
                          )
            )
        );
        
        assert_eq!(list_expression(LocatedSpan::new("[1, 1]")),
            IResult::Done(LocatedSpan{fragment: "", offset: 6, line: 1},
                Expression::List(
                    ListDef{
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 2}))),
                            Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 5}))),
                        ],
                        pos: Position{line: 1, column: 1},
                    }
                )
            )
        );
    }

    #[test]
    fn test_tuple_parse() {
        assert!(tuple(LocatedSpan::new("{")).is_incomplete() );
        assert!(tuple(LocatedSpan::new("{ foo")).is_incomplete() );
        assert!(tuple(LocatedSpan::new("{ foo =")).is_incomplete() );
        assert!(tuple(LocatedSpan::new("{ foo = 1")).is_incomplete() );
        assert!(tuple(LocatedSpan::new("{ foo = 1,")).is_incomplete() );
        assert!(tuple(LocatedSpan::new("{ foo = 1, bar =")).is_incomplete() );

        let mut tuple_expr = "{ }";
        assert_eq!(tuple(LocatedSpan::new(tuple_expr)),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: tuple_expr.len(),
                  line: 1,
                  },
                          Value::Tuple(
                              value_node!(vec![], Position{line: 1, column: 0}))));

        tuple_expr = "{ foo = 1 }";
        assert_eq!(tuple(LocatedSpan::new(tuple_expr)),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: tuple_expr.len(),
                  line: 1,
                  },
                          Value::Tuple(
                              value_node!(vec![
                                  (Token::new("foo", Position{line:1, column: 3}),
                                   Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 9}))))
                              ], Position{line: 1, column: 0}))));

        tuple_expr = "{ foo = 1, bar = \"1\" }";
        assert_eq!(tuple(LocatedSpan::new(tuple_expr)),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: tuple_expr.len(),
                  line: 1,
                  },
                          Value::Tuple(
                              value_node!(vec![
                                  (Token::new("foo", Position{line: 1, column: 3}),
                                   Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 9})))),
                                  (Token::new("bar", Position{line: 1, column: 12}),
                                   Expression::Simple(Value::String(value_node!("1".to_string(), Position{line: 1, column: 18}))))
                              ], Position{line: 1, column: 0}))));
        tuple_expr = "{ foo = 1, bar = {} }";
        assert_eq!(tuple(LocatedSpan::new(tuple_expr)),
               IResult::Done(LocatedSpan {
                  fragment: "",
                  offset: tuple_expr.len(),
                  line: 1,
                  },
                          Value::Tuple(
                              value_node!(vec![
                                  (Token::new("foo", Position{line: 1, column: 3}),
                                   Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 9})))),
                                  (Token::new("bar", Position{line: 1, column: 12}),
                                   Expression::Simple(Value::Tuple(value_node!(Vec::new(), Position{line: 1, column: 17}))))
                              ], Position{line: 1, column: 0}))));
    }

    #[test]
    fn test_field_value_parse() {
        assert!(field_value(LocatedSpan::new("foo")).is_incomplete() );
        assert!(field_value(LocatedSpan::new("foo =")).is_incomplete() );

        assert_eq!(field_value(LocatedSpan::new("foo = 1")),
               IResult::Done(LocatedSpan { offset: 7, line: 1, fragment: "" },
               (Token::new("foo", Position{line: 1, column: 1}),
                Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 7}))))) );
        assert_eq!(field_value(LocatedSpan::new("foo = \"1\"")),
               IResult::Done(LocatedSpan { offset: 9, line: 1, fragment: "" },
               (Token::new("foo", Position{line: 1, column: 1}),
                Expression::Simple(Value::String(value_node!("1".to_string(), Position{line: 1, column: 7}))))) );
        assert_eq!(field_value(LocatedSpan::new("foo = bar")),
               IResult::Done(LocatedSpan { offset: 9, line: 1, fragment: "" },
               (Token::new("foo", Position{line: 1, column: 1}),
                Expression::Simple(Value::Symbol(value_node!("bar".to_string(), Position{line: 1, column: 7}))))) );
        assert_eq!(field_value(LocatedSpan::new("foo = bar ")),
               IResult::Done(LocatedSpan { offset: 10, line: 1, fragment: "" },
               (Token::new("foo", Position{line: 1, column: 1}),
                Expression::Simple(Value::Symbol(value_node!("bar".to_string(), Position{line: 1, column: 7}))))) );
        assert_eq!(field_value(LocatedSpan::new("foo = bar.baz ")),
               IResult::Done(LocatedSpan { offset: 14, line: 1, fragment: "" },
               (Token::new("foo", Position{line: 1, column: 1}),
               Expression::Simple(Value::Selector(make_value_node(vec![Token::new("bar", Position{line: 1, column: 7}), Token::new("baz", Position{line: 1, column: 11})], 1, 6))))));
    }

    #[test]
    fn test_number_parsing() {
        assert!(number(LocatedSpan::new(".")).is_err() );
        assert!(number(LocatedSpan::new(". ")).is_err() );
        assert_eq!(number(LocatedSpan::new("1.0")),
               IResult::Done(LocatedSpan{fragment: "", offset: 3, line: 1},
               Value::Float(value_node!(1.0, Position{line: 1, column: 1}))) );
        assert_eq!(number(LocatedSpan::new("1.")),
               IResult::Done(LocatedSpan{fragment: "", offset: 2, line: 1},
               Value::Float(value_node!(1.0, Position{line: 1, column: 1}))) );
        assert_eq!(number(LocatedSpan::new("1")),
               IResult::Done(LocatedSpan{fragment: "", offset: 1, line: 1},
               Value::Int(value_node!(1, Position{line: 1, column: 1}))) );
        assert_eq!(number(LocatedSpan::new(".1")),
               IResult::Done(LocatedSpan{fragment: "", offset: 2, line: 1},
               Value::Float(value_node!(0.1, Position{line: 1, column: 1}))) );
    }

    #[test]
    fn test_parse() {
        let bad_input = LocatedSpan::new("import mylib as lib;");
        let bad_result = parse(bad_input);
        assert!(bad_result.is_err() );

        // Valid parsing tree
        let input = LocatedSpan::new("import \"mylib\" as lib;let foo = 1;1+1;");
        let result = parse(input);
        assert!(result.is_done() );
        let tpl = result.unwrap();
        assert_eq!(tpl.0.fragment, "");
        assert_eq!(tpl.1,
               vec![
                   Statement::Import{
                       path: "mylib".to_string(),
                       name: Token{
                           fragment: "lib".to_string(),
                           pos: Position{
                               line: 1,
                               column: 19,
                           }
                       }
                   },
                   Statement::Let{
                       name: Token{
                           fragment: "foo".to_string(),
                           pos: Position{
                               line: 1,
                               column: 27,
                           }
                       },
                       value: Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 33})))
                   },
                   Statement::Expression(
                       Expression::Binary(
                           BinaryOpDef{
                               kind: BinaryExprType::Add,
                               left: Value::Int(value_node!(1, Position{line: 1, column: 35})),
                               right: Box::new(Expression::Simple(Value::Int(value_node!(1, Position{line: 1, column: 37})))),
                               pos: Position { line: 1, column: 34 },
                           })
                   )
               ]);
    }
}
