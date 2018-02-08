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

//! The Parsing stage of the ucg compiler.
use std::str::FromStr;
use std::borrow::Borrow;

use nom_locate::LocatedSpan;
use nom;
use nom::InputLength;
use nom::IResult;

use ast::*;
use tokenizer::*;

type NomResult<'a, O> = nom::IResult<TokenIter<'a>, O, ParseError>;

type ParseResult<O> = Result<O, ParseError>;

fn symbol_to_value(s: &Token) -> ParseResult<Value> {
    Ok(Value::Symbol(value_node!(s.fragment.to_string(), s.pos.clone())))
}

// symbol is a bare unquoted field.
named!(symbol<TokenIter, Value, ParseError>,
    match_type!(BAREWORD => symbol_to_value)
);

fn str_to_value(s: &Token) -> ParseResult<Value> {
    Ok(Value::String(value_node!(s.fragment.to_string(), s.pos.clone())))
}

// quoted_value is a quoted string.
named!(quoted_value<TokenIter, Value, ParseError>,
       match_type!(STR => str_to_value)
);

// Helper function to make the return types work for down below.
fn triple_to_number(v: (Option<Token>, Option<Token>, Option<Token>)) -> ParseResult<Value> {
    let (pref, mut pref_pos) = match v.0 {
        None => ("", Position::new(0, 0)),
        Some(ref bs) => (bs.fragment.borrow(), bs.pos.clone()),
    };

    let has_dot = v.1.is_some();

    if v.0.is_some() && !has_dot && v.2.is_none() {
        let i = match FromStr::from_str(pref) {
            Ok(i) => i,
            Err(_) => {
                return Err(ParseError {
                    description: format!("Not an integer! {}", pref),
                    pos: pref_pos,
                })
            }
        };
        return Ok(Value::Int(value_node!(i, pref_pos)));
    }

    if v.0.is_none() && has_dot {
        pref_pos = v.1.unwrap().pos;
    }

    let (maybepos, suf) = match v.2 {
        None => (None, "".to_string()),
        Some(bs) => (Some(bs.pos), bs.fragment),
    };

    let to_parse = pref.to_string() + "." + &suf;
    // TODO(jwall): if there is an error we should report where that error occured.
    let f = match FromStr::from_str(&to_parse) {
        Ok(f) => f,
        Err(_) => {
            return Err(ParseError {
                description: format!("Not a float! {}", to_parse),
                pos: maybepos.unwrap(),
            })
        }
    };
    return Ok(Value::Float(value_node!(f, pref_pos)));
}

// trace_macros!(true);

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
named!(number<TokenIter, Value, ParseError>,
       map_res!(alt!(
           complete!(do_parse!( // 1.0
               prefix: match_type!(DIGIT) >>
               has_dot: punct!(".") >>
               suffix: match_type!(DIGIT) >>
               (Some(prefix.clone()), Some(has_dot.clone()), Some(suffix.clone()))
           )) |
           complete!(do_parse!( // 1.
               prefix: match_type!(DIGIT) >>
               has_dot: punct!(".") >>
               (Some(prefix.clone()), Some(has_dot.clone()), None)
           )) |
           complete!(do_parse!( // .1
               has_dot: punct!(".") >>
               suffix: match_type!(DIGIT) >>
               (None, Some(has_dot.clone()), Some(suffix.clone()))
           )) |
           do_parse!( // 1
               prefix: match_type!(DIGIT) >>
// The peek!(not!(..)) make this whole combinator slightly
// safer for partial inputs.
               (Some(prefix.clone()), None, None)
           )),
           triple_to_number
       )
);
// trace_macros!(false);

named!(
    field_value<TokenIter, (Token, Expression), ParseError>,
    do_parse!(
            field: match_type!(BAREWORD) >>
            punct!("=") >>
            value: expression >>
            (field, value)
    )
);

// Helper function to make the return types work for down below.
fn vec_to_tuple(t: (Position, Option<FieldList>)) -> ParseResult<Value> {
    Ok(Value::Tuple(value_node!(t.1.unwrap_or(Vec::new()),
        t.0.line as usize, t.0.column as usize)))
}

named!(field_list<TokenIter, FieldList, ParseError>,
       separated_list!(punct!(","), field_value)
);

named!(
    #[doc="Capture a tuple of named fields with values. {<field>=<value>,...}"],
    tuple<TokenIter, Value, ParseError>,
    map_res!(
        do_parse!(
            pos: pos >>
            punct!("{") >>
            v: field_list >>
            punct!("}") >>
            (pos, Some(v))
        ),
        vec_to_tuple
    )
);

fn tuple_to_list<Sp: Into<Position>>(t: (Sp, Vec<Expression>)) -> ParseResult<Value> {
    return Ok(Value::List(ListDef {
        elems: t.1,
        pos: t.0.into(),
    }));
}

named!(list_value<TokenIter, Value, ParseError>,
       map_res!(
           do_parse!(
               start: punct!("[") >>
               elements: separated_list!(punct!(","), expression) >>
               punct!("]") >>
               (start.pos, elements)
           ),
           tuple_to_list
       )
);

named!(value<TokenIter, Value, ParseError>,
    alt!(
        number |
        quoted_value |
        list_value |
        tuple |
        selector_value )
 );

fn value_to_expression(v: Value) -> ParseResult<Expression> {
    Ok(Expression::Simple(v))
}

named!(simple_expression<TokenIter, Expression, ParseError>,
       map_res!(
           value,
           value_to_expression
       )
);

fn tuple_to_binary_expression(tpl: (Position, BinaryExprType, Value, Expression))
                              -> ParseResult<Expression> {
    Ok(Expression::Binary(BinaryOpDef {
        kind: tpl.1,
        left: tpl.2,
        right: Box::new(tpl.3),
        pos: Position::new(tpl.0.line as usize, tpl.0.column as usize),
    }))
}

macro_rules! do_binary_expr {
    ($i:expr, $subrule:ident!( $($args:tt)* ), $typ:expr) => {
        // NOTE(jwall): Nom macros do magic with their inputs. They in fact
        // rewrite your macro argumets for you. Which means we require this $i
        // paramater even though we don't explicitely pass it below. I don't
        // particularly like this but I'm living with it for now.
        map_res!($i,
            do_parse!(
                pos: pos >>
                left: value >>
                    $subrule!($($args)*) >>
                    right: expression >>
                    (pos, $typ, left, right)
            ),
            tuple_to_binary_expression
        )
    };
}

// trace_macros!(true);
named!(add_expression<TokenIter, Expression, ParseError>,
       do_binary_expr!(punct!("+"), BinaryExprType::Add)
);
// trace_macros!(false);

named!(sub_expression<TokenIter, Expression, ParseError>,
       do_binary_expr!(punct!("-"), BinaryExprType::Sub)
);

named!(mul_expression<TokenIter, Expression, ParseError>,
       do_binary_expr!(punct!("*"), BinaryExprType::Mul)
);

named!(div_expression<TokenIter, Expression, ParseError>,
       do_binary_expr!(punct!("/"), BinaryExprType::Div)
);

fn expression_to_grouped_expression(e: Expression) -> ParseResult<Expression> {
    Ok(Expression::Grouped(Box::new(e)))
}

named!(grouped_expression<TokenIter, Expression, ParseError>,
       map_res!(
           preceded!(punct!("("), terminated!(expression, punct!(")"))),
           expression_to_grouped_expression
       )
);

fn symbol_or_expression(input: TokenIter) -> NomResult<Expression> {
    let sym = do_parse!(input,
        sym: symbol >>
        (sym)
    );

    match sym {
        IResult::Incomplete(i) => {
            return IResult::Incomplete(i);
        }
        IResult::Error(_) => {
            // TODO(jwall): Still missing some. But we need to avoid recursion
            return grouped_expression(input);
        }
        IResult::Done(rest, val) => {
            return IResult::Done(rest, Expression::Simple(val));
        }
    }
}

fn selector_list(input: TokenIter) -> NomResult<SelectorList> {
    let (rest, head) = match symbol_or_expression(input) {
        IResult::Done(rest, val) => (rest, val),
        IResult::Error(e) => {
            return IResult::Error(e);
        }
        IResult::Incomplete(i) => {
            return IResult::Incomplete(i);
        }
    };

    let (rest, is_dot) = match punct!(rest, ".") {
        IResult::Done(rest, tok) => (rest, Some(tok)),
        IResult::Incomplete(i) => {
            return IResult::Incomplete(i);
        }
        IResult::Error(_) => (rest, None),
    };

    let (rest, list) = if is_dot.is_some() {
        let (rest, list) = match separated_list!(rest, punct!("."), alt!(match_type!(BAREWORD) | match_type!(DIGIT))) {
            IResult::Done(rest, val) => {
                (rest, val)
            }
            IResult::Incomplete(i) => {
                return IResult::Incomplete(i);
            }
            IResult::Error(e) => {
                return IResult::Error(e);
            }
        };

        if list.is_empty() {
            return IResult::Error(nom::ErrorKind::Custom(ParseError {
                description: "(.) with no selector fields after".to_string(),
                pos: is_dot.unwrap().pos,
            }));
        } else {
            (rest, Some(list))
        }
    } else {
        (rest, None)
    };

    let sel_list = SelectorList {
        head: Box::new(head),
        tail: list,
    };

    return IResult::Done(rest, sel_list);
}

fn tuple_to_copy(t: (SelectorDef, FieldList)) -> ParseResult<Expression> {
    let pos = t.0.pos.clone();
    Ok(Expression::Copy(CopyDef {
        selector: t.0,
        fields: t.1,
        pos: pos,
    }))
}

named!(copy_expression<TokenIter, Expression, ParseError>,
       map_res!(
           do_parse!(
               pos: pos >>
               selector: selector_list >>
               punct!("{") >>
               fields: field_list >>
               punct!("}") >>
               (SelectorDef::new(selector, pos.line, pos.column as usize), fields)
           ),
           tuple_to_copy
       )
);

fn tuple_to_macro(mut t: (Position, Vec<Value>, Value)) -> ParseResult<Expression> {
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
                pos: t.0,
            }))
        }
        // TODO(jwall): Show a better version of the unexpected parsed value.
        val => {
            Err(ParseError {
                description: format!("Expected Tuple Got {:?}", val),
                pos: t.0,
            })
        }
    }
}

named!(arglist<TokenIter, Vec<Value>, ParseError>, separated_list!(punct!(","), symbol));

named!(macro_expression<TokenIter, Expression, ParseError>,
       map_res!(
           do_parse!(
                pos: pos >>
                start: word!("macro") >>
                punct!("(") >>
                arglist: arglist >>
                punct!(")") >>
                punct!("=>") >>
                map: tuple >>
                (pos, arglist, map)
           ),
           tuple_to_macro
       )
);

fn tuple_to_select(t: (Position, Expression, Expression, Value)) -> ParseResult<Expression> {
    match t.3 {
        Value::Tuple(v) => {
            Ok(Expression::Select(SelectDef {
                val: Box::new(t.1),
                default: Box::new(t.2),
                tuple: v.val,
                pos: t.0,
            }))
        }
        val => {
            Err(ParseError {
                description: format!("Expected Tuple Got {:?}", val),
                pos: t.0,
            })
        }
    }
}

named!(select_expression<TokenIter, Expression, ParseError>,
       map_res!(
           do_parse!(
               start: word!("select") >>
               val: terminated!(expression, punct!(",")) >>
               default: terminated!(expression, punct!(",")) >>
               map: tuple >>
               (start.pos.clone(), val, default, map)
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

named!(format_expression<TokenIter, Expression, ParseError>,
       map_res!(
           do_parse!(
               tmpl: match_type!(STR) >>
                   punct!("%") >>
                   punct!("(") >>
                   args: separated_list!(punct!(","), expression) >>
                   punct!(")") >>
                   (tmpl, args)
           ),
           tuple_to_format
       )
);

fn tuple_to_call(t: (Position, Value, Vec<Expression>)) -> ParseResult<Expression> {
    if let Value::Selector(def) = t.1 {
        Ok(Expression::Call(CallDef {
            macroref: def,
            arglist: t.2,
            pos: Position::new(t.0.line as usize, t.0.column as usize),
        }))
    } else {
        Err(ParseError {
            description: format!("Expected Selector Got {:?}", t.0),
            pos: Position::new(t.0.line as usize, t.0.column as usize),
        })
    }
}

fn vec_to_selector_value(t: (Position, SelectorList)) -> ParseResult<Value> {
    Ok(Value::Selector(SelectorDef::new(t.1, t.0.line as usize, t.0.column as usize)))
}

named!(selector_value<TokenIter, Value, ParseError>,
       map_res!(
           do_parse!(
               sl: selector_list >>
               (sl.head.pos().clone(), sl)
           ),
           vec_to_selector_value
       )
);

named!(call_expression<TokenIter, Expression, ParseError>,
       map_res!(
           do_parse!(
               macroname: selector_value >>
               punct!("(") >>
               args: separated_list!(punct!(","), expression) >>
               punct!(")") >>
               (macroname.pos().clone(), macroname, args)
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
named!(expression<TokenIter, Expression, ParseError>,
    do_parse!(
        expr: alt!(
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
           simple_expression
       ) >>
       (expr)
    )
);

fn expression_to_statement(v: Expression) -> ParseResult<Statement> {
    Ok(Statement::Expression(v))
}

named!(expression_statement<TokenIter, Statement, ParseError>,
    map_res!(
        terminated!(expression, punct!(";")),
        expression_to_statement
    )
);

fn tuple_to_let(t: (Token, Expression)) -> ParseResult<Statement> {
    Ok(Statement::Let(LetDef {
        name: t.0,
        value: t.1,
    }))
}

named!(let_statement<TokenIter, Statement, ParseError>,
    map_res!(
        do_parse!(
            word!("let") >>
            name: match_type!(BAREWORD) >>
            punct!("=") >>
            val: expression >>
            punct!(";") >>
            (name, val)
        ),
        tuple_to_let
    )
);

fn tuple_to_import(t: (Token, Token)) -> ParseResult<Statement> {
    Ok(Statement::Import(ImportDef {
        path: t.0,
        name: t.1,
    }))
}

named!(import_statement<TokenIter, Statement, ParseError>,
    map_res!(
       do_parse!(
           word!("import") >>
               path: match_type!(STR) >>
               word!("as") >>
               name: match_type!(BAREWORD) >>
               punct!(";") >>
               (path, name)
       ),
       tuple_to_import
    )
);

named!(statement<TokenIter, Statement, ParseError>,
    do_parse!(
       stmt: alt_complete!(
           import_statement |
           let_statement |
           expression_statement
       ) >>
       (stmt)
    )
);

/// Parses a LocatedSpan into a list of Statements or a ParseError.
pub fn parse(input: LocatedSpan<&str>) -> Result<Vec<Statement>, ParseError> {
    match tokenize(input) {
        Ok(tokenized) => {
            let mut out = Vec::new();
            let mut i_ = TokenIter { source: tokenized.as_slice() };
            loop {
                let i = i_.clone();
                if i[0].typ == TokenType::END {
                    break;
                }
                match statement(i) {
                    IResult::Error(nom::ErrorKind::Custom(e)) => {
                        return Err(e);
                    }
                    IResult::Error(e) => {
                        return Err(ParseError {
                            description:
                                format!("Tokenization error: {:?} current token: {:?}", e, i_[0]),
                            pos: Position {
                                line: i_[0].pos.line,
                                column: i_[0].pos.column,
                            },
                        });
                    }
                    IResult::Incomplete(ei) => {
                        return Err(ParseError {
                            description: format!("Unexpected end of parsing input: {:?}", ei),
                            pos: Position {
                                line: i_[0].pos.line,
                                column: i_[0].pos.column,
                            },
                        });
                    }
                    IResult::Done(rest, stmt) => {
                        out.push(stmt);
                        i_ = rest;
                        if i_.input_len() == 0 {
                            break;
                        }
                    }
                }
            }
            return Ok(out);
        }
        Err(e) => {
            // FIXME(jwall): We should really capture the location
            // of the tokenization error here.
            return Err(ParseError {
                description: format!("Tokenize Error: {:?}", e),
                pos: Position {
                    line: 0,
                    column: 0,
                },
            });
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer::{TokenIter, tokenize};

    use nom_locate::LocatedSpan;
    use nom::IResult;

    macro_rules! assert_parse {
        ($parsemac:ident( $i:expr ), $out:expr) => {
            assert_parse!($i, $parsemac, $out)
        };
        ($i:expr, $f:expr, $out:expr) => {
            {
                let input = LocatedSpan::new($i);
                match tokenize(input) {
                    Err(e) => assert!(false, format!("Tokenizer Error: {:?}", e)),
                    Ok(val) => {
                        match $f(TokenIter{source: val.as_slice()}) {
                            IResult::Done(_, result) => assert_eq!(result, $out),
                            other => assert!(false, format!("Expected Done got {:?}", other)),
                        }
                    },
                }
            };
        }
    }

    macro_rules! assert_error {
        ($parsemac:ident( $i:expr )) => {
            assert_error!($i, $parsemac)
        };
        ($i:expr, $f:expr) => {
            {
                let input = LocatedSpan::new($i);
                match tokenize(input) {
                    Err(_) => assert!(true),
                    Ok(val) => {
                        let result = $f(TokenIter{source: val.as_slice()});
                        assert!(result.is_err(), format!("Not an error: {:?}", result))
                    },
                }
            }
        }
    }

    #[test]
    fn test_symbol_parsing() {
        assert_parse!(symbol("foo"),
               Value::Symbol(value_node!("foo".to_string(), 1, 1)) );
        assert_parse!(symbol("foo-bar"),
               Value::Symbol(value_node!("foo-bar".to_string(), 1, 1)) );
        assert_parse!(symbol("foo_bar"),
               Value::Symbol(value_node!("foo_bar".to_string(), 1, 1)) );
    }

    #[test]
    fn test_selector_parsing() {
        assert_error!(selector_value("foo."));
        assert_parse!(selector_value("foo.bar "),
          Value::Selector(make_selector!(make_expr!("foo".to_string(), 1, 1) => [
                                          make_tok!("bar", 1, 5)] =>
                                        1, 1))
        );
        assert_parse!(selector_value("foo.0 "),
          Value::Selector(make_selector!(make_expr!("foo".to_string(), 1, 1) => [
                                          make_tok!(DIGIT => "0", 1, 5)] =>
                                        1, 1))
        );
        assert_parse!(selector_value("foo.bar;"),
            Value::Selector(make_selector!(make_expr!("foo", 1, 1) =>
                                            [
                                               make_tok!("bar", 1, 5)
                                            ] =>
                                            1, 1))
        );
        assert_parse!(selector_value("({foo=1}).foo "),
            Value::Selector(make_selector!(Expression::Grouped(Box::new(Expression::Simple(
                Value::Tuple(value_node!(
                    vec![(make_tok!("foo", 1, 3), Expression::Simple(Value::Int(Positioned::new(1, 1, 7))))],
                    1, 3))
                ))) => [ make_tok!("foo", 1, 11) ] => 1, 2)
        ));
    }

    #[test]
    fn test_statement_parse() {
        let stmt = "import \"foo\" as foo;";
        assert_parse!(statement(stmt),
                   Statement::Import(ImportDef{
                       path: make_tok!(QUOT => "foo", 1,8),
                       name: make_tok!("foo", 1,17),
                   }
               )
        );

        assert_error!(import_statement("import \"foo\"") );

        assert_parse!(statement("let foo = 1.0 ;"),
                    Statement::Let(LetDef{
                        name: make_tok!("foo", 1, 5),
                        value: Expression::Simple(Value::Float(value_node!(1.0, 1, 11)))
        }));

        assert_parse!(statement("// comment\nlet foo = 1.0 ;"),
                    Statement::Let(LetDef{
                        name: make_tok!("foo", 2, 5),
                        value: Expression::Simple(Value::Float(value_node!(1.0, 2, 11)))
        }));

        assert_parse!(statement("1.0;"),
                    Statement::Expression(
                        Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))) );
    }

    #[test]
    fn test_import_statement_parse() {
        assert_error!(import_statement("import"));
        assert_error!(import_statement("import \"foo\""));
        assert_error!(import_statement("import \"foo\" as"));
        assert_error!(import_statement("import \"foo\" as foo"));

        let import_stmt = "import \"foo\" as foo;";
        assert_parse!(import_statement(import_stmt),
                    Statement::Import(ImportDef{
                        path: make_tok!(QUOT => "foo", 1, 8),
                        name: make_tok!("foo", 1,17),
                    })
        );
    }

    #[test]
    fn test_let_statement_parse() {
        assert_error!(let_statement("foo") );
        assert_error!(let_statement("let \"foo\"") );
        assert_error!(let_statement("let 1") );
        assert_error!(let_statement("let") );
        assert_error!(let_statement("let foo") );
        assert_error!(let_statement("let foo =") );
        assert_error!(let_statement("let foo = ") );
        assert_error!(let_statement("let foo = 1") );

        assert_parse!(let_statement("let foo = 1.0 ;"),
                Statement::Let(LetDef{name: make_tok!("foo", 1,5),
                    value: Expression::Simple(Value::Float(value_node!(1.0, 1, 11)))
                }));

        assert_parse!(let_statement("let foo = // comment\n1.0 ;"),
                Statement::Let(LetDef{name: make_tok!("foo", 1,5),
                    value: Expression::Simple(Value::Float(value_node!(1.0, 2, 1)))
                }));

        assert_parse!(let_statement("let foo = 1.0 // comment\n;"),
                Statement::Let(LetDef{name: make_tok!("foo", 1,5),
                    value: Expression::Simple(Value::Float(value_node!(1.0, 1, 11)))
                }));

        assert_parse!(let_statement("let foo= 1.0;"),
                Statement::Let(LetDef{name: make_tok!("foo", 1, 5),
                value: Expression::Simple(Value::Float(value_node!(1.0, 1, 10)))}) );

        assert_parse!(let_statement("let foo =1.0;"),
                Statement::Let(LetDef{name: make_tok!("foo", 1,5),
                value: Expression::Simple(Value::Float(value_node!(1.0, 1, 10)))}));
    }

    #[test]
    fn test_expression_statement_parse() {
        assert_error!(expression_statement("foo") );
        assert_parse!(expression_statement("1.0;"),
                  Statement::Expression(
                      Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))));
        assert_parse!(expression_statement("1.0 ;"),
                  Statement::Expression(
                      Expression::Simple(Value::Float(value_node!(1.0, 1, 1)))));
        assert_parse!(expression_statement(" 1.0;"),
                  Statement::Expression(
                      Expression::Simple(Value::Float(value_node!(1.0, 1, 2)))));
        assert_parse!(expression_statement("foo;"),
                  Statement::Expression(
                      Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 1), 1, 1)))));
        assert_parse!(expression_statement("foo ;"),
                  Statement::Expression(
                      Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 2), 1, 1)))));
        assert_parse!(expression_statement(" foo;"),
                  Statement::Expression(
                      Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 2), 1, 2)))));
        assert_parse!(expression_statement("\"foo\";"),
                  Statement::Expression(
                      Expression::Simple(Value::String(value_node!("foo".to_string(), 1, 1)))));
        assert_parse!(expression_statement("\"foo\" ;"),
                  Statement::Expression(
                      Expression::Simple(Value::String(value_node!("foo".to_string(), 1, 1)))));
        assert_parse!(expression_statement(" \"foo\";"),
                  Statement::Expression(
                     Expression::Simple(Value::String(value_node!("foo".to_string(), 1, 2)))));
    }

    #[test]
    fn test_expression_parse() {
        assert_parse!(expression("\"foo\""),
              Expression::Simple(Value::String(value_node!("foo".to_string(), 1, 1))));
        assert_parse!(expression("1"),
              Expression::Simple(Value::Int(value_node!(1, 1, 1))));
        assert_parse!(expression("foo "),
              Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 1), 1, 1))));
        assert_parse!(expression("foo.bar "),
               Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 1) =>
                                                                 [ make_tok!("bar", 1, 5) ] =>
                                                                 1, 1))));
        assert_parse!(expression("1 + 1"),
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Int(value_node!(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
                    pos: Position::new( 1, 1 ),
                }));
        assert_parse!(expression("1 - 1"),
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Sub,
                    left: Value::Int(value_node!(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
                    pos: Position::new(1, 1),
                }));
        assert_parse!(expression("1 * 1"),
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Mul,
                    left: Value::Int(value_node!(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
                    pos: Position::new(1, 1),
                }));
        assert_parse!(expression("1 / 1"),
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Div,
                    left: Value::Int(value_node!(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 5)))),
                    pos: Position::new(1, 1),
                }));

        assert_parse!(expression("1+1"),
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Add,
                    left: Value::Int(value_node!(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 3)))),
                    pos: Position::new(1, 1),
                }));
        assert_parse!(expression("1-1"),
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Sub,
                    left: Value::Int(value_node!(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 3)))),
                    pos: Position::new(1, 1),
                }));
        assert_parse!(expression("1*1"),
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Mul,
                    left: Value::Int(value_node!(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 3)))),
                    pos: Position::new(1, 1),
                }));
        assert_parse!(expression("1/1"),
                Expression::Binary(BinaryOpDef{
                    kind: BinaryExprType::Div,
                    left: Value::Int(value_node!(1, 1, 1)),
                    right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 3)))),
                    pos: Position::new(1, 1),
                }));
        assert_parse!(expression("macro (arg1, arg2) => { foo = arg1 }"),
                Expression::Macro(MacroDef{
                    argdefs: vec![
                        value_node!("arg1".to_string(), 1, 8),
                        value_node!("arg2".to_string(), 1, 14),
                    ],
                    fields: vec![
                        (make_tok!("foo", 1, 25),
                         Expression::Simple(Value::Selector(make_selector!(make_expr!("arg1", 1, 31), 1, 31)))),
                    ],
                    pos: Position::new(1, 1),
                }));
        assert_parse!(expression("select foo, 1, { foo = 2 }"),
            Expression::Select(SelectDef{
                val: Box::new(Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 8), 1, 8)))),
                default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 13)))),
                tuple: vec![
                    (make_tok!("foo", 1, 18),
                     Expression::Simple(Value::Int(value_node!(2, 1, 24))))
                ],
                pos: Position::new(1, 1),
            }));
        assert_parse!(expression("foo.bar (1, \"foo\")"),
            Expression::Call(CallDef{
                macroref: make_selector!(make_expr!("foo", 1, 1)  =>
                                         [ make_tok!("bar", 1, 5) ] =>
                                         1, 1),
                arglist: vec![
                    Expression::Simple(Value::Int(value_node!(1, 1, 10))),
                    Expression::Simple(Value::String(value_node!("foo".to_string(), 1, 13))),
                ],
                pos: Position::new(1, 1),
            }));
        assert_parse!(expression("(1 + 1)"),
            Expression::Grouped(
                Box::new(
                    Expression::Binary(
                        BinaryOpDef{
                            kind: BinaryExprType::Add,
                            left: Value::Int(value_node!(1, 1, 2)),
                            right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 6)))),
                            pos: Position::new(1, 2), // FIXME(jwall): grouped expressions appear to be getting positioned wrong
                        }))));
        assert_parse!(expression("[1, 1]"),
                Expression::Simple(Value::List(
                    ListDef{
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                            Expression::Simple(Value::Int(value_node!(1, 1, 5))),
                        ],
                        pos: Position::new(1, 1),
                    }
                )
            )
        );
    }

    #[test]
    fn test_format_parse() {
        assert_error!(format_expression("\"foo") );
        assert_error!(format_expression("\"foo\"") );
        assert_error!(format_expression("\"foo\" %") );
        assert_error!(format_expression("\"foo\" % (, 2") );

        assert_parse!(format_expression("\"foo @ @\" % (1, 2)"),
                    Expression::Format(
                        FormatDef{
                            template: "foo @ @".to_string(),
                            args: vec![Expression::Simple(Value::Int(value_node!(1, 1, 14))),
                                       Expression::Simple(Value::Int(value_node!(2, 1, 17)))],
                            pos: Position::new(1, 1),
                        }));

        assert_parse!(format_expression("\"foo @ @\"%(1, 2)"),
                Expression::Format(
                    FormatDef{
                        template: "foo @ @".to_string(),
                        args: vec![Expression::Simple(Value::Int(value_node!(1, 1, 12))),
                                   Expression::Simple(Value::Int(value_node!(2, 1, 15)))],
                        pos: Position::new(1, 1),
                    }));
    }

    #[test]
    fn test_call_parse() {
        assert_error!(call_expression("foo") );
        assert_error!(call_expression("foo (") );
        assert_error!(call_expression("foo (1") );
        assert_error!(call_expression("foo (1,") );
        assert_error!(call_expression("foo (1,2") );

        assert_parse!(call_expression("foo (1, \"foo\")"),
                    Expression::Call(CallDef{
                        macroref: make_selector!(make_expr!("foo", 1, 1), 1, 1),
                        arglist: vec![
                            Expression::Simple(Value::Int(value_node!(1, 1, 6))),
                            Expression::Simple(Value::String(value_node!("foo".to_string(), 1, 9))),
                        ],
                        pos: Position::new(1, 1),
                    })

        );

        assert_parse!(call_expression("foo.bar (1, \"foo\")"),
                    Expression::Call(CallDef{
                        macroref: make_selector!(make_expr!("foo") => [ make_tok!("bar", 1, 5) ] => 1, 1),
                        arglist: vec![
                            Expression::Simple(Value::Int(value_node!(1, 1, 10))),
                            Expression::Simple(Value::String(value_node!("foo".to_string(), 1, 13))),
                        ],
                        pos: Position::new(1, 1),
                    })

        );
    }

    #[test]
    fn test_select_parse() {
        assert_error!(select_expression("select"));
        assert_error!(select_expression("select foo"));
        assert_error!(select_expression("select foo, 1"));
        assert_error!(select_expression("select foo, 1, {"));

        assert_parse!(select_expression("select foo, 1, { foo = 2 }"),
                Expression::Select(SelectDef{
                    val: Box::new(Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 8), 1, 8)))),
                    default: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 13)))),
                    tuple: vec![
                        (make_tok!("foo", 1, 18), Expression::Simple(Value::Int(value_node!(2, 1, 24))))
                    ],
                    pos: Position::new(1, 1),
                }));
    }

    #[test]
    fn test_macro_expression_parsing() {
        assert_error!(macro_expression("foo") );
        assert_error!(macro_expression("macro \"foo\"") );
        assert_error!(macro_expression("macro 1") );
        assert_error!(macro_expression("macro") );
        assert_error!(macro_expression("macro (") );
        assert_error!(macro_expression("macro (arg") );
        assert_error!(macro_expression("macro (arg, arg2") );
        assert_error!(macro_expression("macro (arg1, arg2) =>") );
        assert_error!(macro_expression("macro (arg1, arg2) => {") );
        assert_error!(macro_expression("macro (arg1, arg2) => { foo") );
        assert_error!(macro_expression("macro (arg1, arg2) => { foo =") );

        assert_parse!(macro_expression("macro (arg1, arg2) => {foo=1,bar=2}"),
                    Expression::Macro(MacroDef{
                        argdefs: vec![value_node!("arg1".to_string(), 1, 8),
                                      value_node!("arg2".to_string(), 1, 14)],
                        fields: vec![(make_tok!("foo", 1, 24), Expression::Simple(Value::Int(value_node!(1, 1, 28)))),
                                     (make_tok!("bar", 1, 30), Expression::Simple(Value::Int(value_node!(2, 1, 34))))
                        ],
                        pos: Position::new(1, 1),
                    }));
    }

    #[test]
    fn test_copy_parse() {
        assert_error!(copy_expression("{}") );
        assert_error!(copy_expression("foo") );
        assert_error!(copy_expression("foo{") );

        assert_parse!(copy_expression("foo{}"),
                    Expression::Copy(CopyDef{
                        selector: make_selector!(make_expr!("foo", 1, 1), 1, 1),
                        fields: Vec::new(),
                        pos: Position::new(1, 1),
                    }));

        assert_parse!(copy_expression("foo{bar=1}"),
                Expression::Copy(CopyDef{
                    selector: make_selector!(make_expr!("foo", 1, 1), 1, 1),
                    fields: vec![(make_tok!("bar", 1, 5),
                                  Expression::Simple(Value::Int(value_node!(1, 1, 9))))],
                    pos: Position::new(1, 1),
                }));
    }

    #[test]
    fn test_grouped_expression_parse() {
        assert_error!(grouped_expression("foo") );
        assert_error!(grouped_expression("(foo") );
        assert_parse!(grouped_expression("(foo)"),
                          Expression::Grouped(
                              Box::new(
                                  Expression::Simple(
                                      Value::Selector(make_selector!(make_expr!("foo", 1, 2), 1, 2))))));
        assert_parse!(grouped_expression("(1 + 1)"),
                          Expression::Grouped(
                              Box::new(
                                  Expression::Binary(
                                        BinaryOpDef{
                                            kind: BinaryExprType::Add,
                                            left: Value::Int(value_node!(1, 1, 2)),
                                            right: Box::new(Expression::Simple(
                                                Value::Int(value_node!(1, 1, 6)))),
                                            pos: Position::new(1, 2),
                                        }))));
    }

    #[test]
    fn test_list_value_parse() {
        assert_error!(list_value("foo") );
        assert_error!(list_value("[foo") );
        assert_error!(list_value("// commen\n[foo") );

        assert_parse!(list_value("[foo]"),
                          Value::List(
                              ListDef{
                                      elems: vec![
                                                Expression::Simple(Value::Selector(make_selector!(make_expr!("foo", 1, 2), 1, 2)))
                                             ],
                                      pos: Position::new(1, 1),
                                     }
                          )
        );

        assert_parse!(list_value("[1, 1]"),
                Value::List(
                    ListDef{
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                            Expression::Simple(Value::Int(value_node!(1, 1, 5))),
                        ],
                        pos: Position::new(1, 1),
                    }
                )
        );

        assert_parse!(list_value("// comment\n[1, 1]"),
                Value::List(
                    ListDef{
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, 2, 2))),
                            Expression::Simple(Value::Int(value_node!(1, 2, 5))),
                        ],
                        pos: Position::new(2, 1),
                    }
                )
        );

        assert_parse!(list_value("[// comment\n1, 1]"),
                Value::List(
                    ListDef{
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, 2, 2))),
                            Expression::Simple(Value::Int(value_node!(1, 2, 5))),
                        ],
                        pos: Position::new(1, 1),
                    }
                )
        );

        assert_parse!(list_value("[1, // comment\n1]"),
                Value::List(
                    ListDef{
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                            Expression::Simple(Value::Int(value_node!(1, 2, 1))),
                        ],
                        pos: Position::new(1, 1),
                    }
                )
        );

        assert_parse!(list_value("[1, 1 // comment\n]"),
                Value::List(
                    ListDef{
                        elems: vec![
                            Expression::Simple(Value::Int(value_node!(1, 1, 2))),
                            Expression::Simple(Value::Int(value_node!(1, 1, 5))),
                        ],
                        pos: Position::new(1, 1),
                    }
                )
        );
    }

    #[test]
    fn test_tuple_parse() {
        assert_error!(tuple("{") );
        assert_error!(tuple("{ foo") );
        assert_error!(tuple("{ foo =") );
        assert_error!(tuple("{ foo = 1") );
        assert_error!(tuple("{ foo = 1,") );
        assert_error!(tuple("{ foo = 1, bar =") );
        assert_error!(tuple("// comment\n{ foo = 1, bar =") );

        assert_parse!(tuple("{ }"), Value::Tuple(value_node!(vec![], 1, 1)));

        assert_parse!(tuple("{ foo = 1 }"),
                            Value::Tuple(
                                value_node!(vec![
                                        (make_tok!("foo", 1, 3),
                                        Expression::Simple(Value::Int(value_node!(1, 1, 9))))
                                ], 1, 1)));

        assert_parse!(tuple("// comment\n{ foo = 1 }"),
                            Value::Tuple(
                                value_node!(vec![
                                    (make_tok!("foo", 2, 3),
                                    Expression::Simple(Value::Int(value_node!(1, 2, 9))))
                                ], 1, 1)));

        assert_parse!(tuple("{// comment\n foo = 1 }"),
                          Value::Tuple(
                              value_node!(vec![
                                  (make_tok!("foo", 2, 2),
                                   Expression::Simple(Value::Int(value_node!(1, 2, 8))))
                              ], 1, 1)));

        assert_parse!(tuple("{ foo = 1// comment\n }"),
                          Value::Tuple(
                              value_node!(vec![
                                  (make_tok!("foo", 1, 3),
                                   Expression::Simple(Value::Int(value_node!(1, 1, 9))))
                              ], 1, 1)));

        assert_parse!(tuple("{ foo = 1, bar = \"1\" }"),
                          Value::Tuple(
                              value_node!(vec![
                                  (make_tok!("foo", 1, 3),
                                   Expression::Simple(Value::Int(value_node!(1, 1, 9)))),
                                  (make_tok!("bar", 1, 12),
                                   Expression::Simple(Value::String(value_node!("1".to_string(), Position::new(1, 18)))))
                              ], 1, 1)));
        assert_parse!(tuple("{ foo = 1, // comment\nbar = \"1\" }"),
                          Value::Tuple(
                              value_node!(vec![
                                  (make_tok!("foo", 1, 3),
                                   Expression::Simple(Value::Int(value_node!(1, 1, 9)))),
                                  (make_tok!("bar", 2, 1),
                                   Expression::Simple(Value::String(value_node!("1".to_string(), Position::new(2, 7)))))
                              ], 1, 1)));
        assert_parse!(tuple("{ foo = 1, bar = {} }"),
                          Value::Tuple(
                              value_node!(vec![
                                  (make_tok!("foo", 1, 3),
                                   Expression::Simple(Value::Int(value_node!(1, Position::new(1, 9))))),
                                  (make_tok!("bar", 1, 12),
                                   Expression::Simple(Value::Tuple(value_node!(Vec::new(), Position::new(1, 17)))))
                              ], 1, 1)));
    }

    #[test]
    fn test_field_list_parse() {
        let mut f_list = "foo = 1, quux = 2;";
        assert_parse!(field_list(f_list),
            vec![
                (make_tok!("foo", 1, 1), make_expr!(1 => int, 1, 7)),
                (make_tok!("quux", 1, 10), make_expr!(2 => int, 1, 17)),
            ]);

        f_list = "foo = 1, // comment\nquux = 2;";
        assert_parse!(field_list(f_list),
            vec![
                (make_tok!("foo", 1, 1), make_expr!(1 => int, 1, 7)),
                (make_tok!("quux", 2, 1), make_expr!(2 => int, 2, 8)),
            ]);

        f_list = "foo = 1,\n// comment\nquux = 2;";
        assert_parse!(field_list(f_list),
            vec![
                (make_tok!("foo", 1, 1), make_expr!(1 => int, 1, 7)),
                (make_tok!("quux", 3, 1), make_expr!(2 => int, 3, 8)),
            ]);
    }

    #[test]
    fn test_field_value_parse() {
        assert_error!(field_value("foo") );
        assert_error!(field_value("// comment\nfoo") );
        assert_error!(field_value("foo =") );

        assert_parse!(field_value("foo = 1"),
               (make_tok!("foo", 1, 1),
                Expression::Simple(Value::Int(value_node!(1, 1, 7)))) );
        assert_parse!(field_value("foo = 1 // foo comment\n"),
               (make_tok!("foo", 1, 1),
                Expression::Simple(Value::Int(value_node!(1, 1, 7)))) );
        assert_parse!(field_value("foo // foo comment\n = 1"),
               (make_tok!("foo", 1, 1),
                Expression::Simple(Value::Int(value_node!(1, 2, 4)))) );
        assert_parse!(field_value("// foo comment\nfoo = 1"),
               (make_tok!("foo", 2, 1),
                Expression::Simple(Value::Int(value_node!(1, 2, 7)))) );
        assert_parse!(field_value("foo = \"1\""),
               (make_tok!("foo", 1, 1),
                Expression::Simple(Value::String(value_node!("1".to_string(), 1, 7)))) );
        assert_parse!(field_value("foo = bar "),
               (make_tok!("foo", 1, 1),
                Expression::Simple(Value::Selector(make_selector!(make_expr!("bar", 1, 7), 1, 7)))) );
        assert_parse!(field_value("foo = bar.baz "),
               (make_tok!("foo", 1, 1),
               Expression::Simple(Value::Selector(make_selector!(make_expr!("bar", 1, 7) => [ make_tok!("baz", 1, 11) ] => 1, 7)))) );
    }

    #[test]
    fn test_number_parsing() {
        assert_error!(number(".") );
        assert_error!(number(". ") );
        assert_parse!(number("1.0"),
               Value::Float(value_node!(1.0, 1, 1)) );
        assert_parse!(number("1."),
               Value::Float(value_node!(1.0, 1, 1)) );
        assert_parse!(number("1"),
               Value::Int(value_node!(1, 1, 1)) );
        assert_parse!(number(".1"),
               Value::Float(value_node!(0.1, 1, 1)) );
    }

    #[test]
    fn test_parse() {
        let bad_input = LocatedSpan::new("import mylib as lib;");
        let bad_result = parse(bad_input);
        assert!(bad_result.is_err() );

        // Valid parsing tree
        let input = LocatedSpan::new("import \"mylib\" as lib;let foo = 1;1+1;");
        let result = parse(input);
        assert!(result.is_ok(), format!("Expected Ok, Got {:?}", result));
        let tpl = result.unwrap();
        assert_eq!(tpl,
               vec![
                   Statement::Import(ImportDef{
                       path: make_tok!(QUOT => "mylib", 1, 8),
                       name: make_tok!("lib", 1, 19),
                   }),
                   Statement::Let(LetDef{
                       name: make_tok!("foo", 1, 27),
                       value: Expression::Simple(Value::Int(value_node!(1, 1, 33)))
                   }),
                   Statement::Expression(
                       Expression::Binary(
                           BinaryOpDef{
                               kind: BinaryExprType::Add,
                               left: Value::Int(value_node!(1, 1, 35)),
                               right: Box::new(Expression::Simple(Value::Int(value_node!(1, 1, 37)))),
                               pos: Position::new(1, 35),
                           })
                   )
               ]);
    }
}
