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
use std::borrow::Borrow;
use std::str::FromStr;

use nom;
use nom::IResult;
use nom::InputLength;
use nom_locate::LocatedSpan;

use self::precedence::op_expression;
use ast::*;
use error;
use tokenizer::*;

type NomResult<'a, O> = nom::IResult<TokenIter<'a>, O, error::Error>;

#[cfg(feature = "tracing")]
const ENABLE_TRACE: bool = true;
#[cfg(not(feature = "tracing"))]
const ENABLE_TRACE: bool = false;

type ParseResult<O> = Result<O, error::Error>;
macro_rules! trace_nom {
    ($i:expr, $rule:ident!( $($args:tt)* )) => {
        {
            use parse::ENABLE_TRACE;
            if ENABLE_TRACE {
                eprintln!("Entering Rule: {:?} {:?}", stringify!($rule), $i);
            }
            let result = $rule($i, $($args)* ); 
            if ENABLE_TRACE {
                eprintln!("Exiting Rule: {:?} with {:?}", stringify!($rule), result);
            }
            result
        }
    };

    ($i:expr, $rule:ident) => {
        {
            use parse::ENABLE_TRACE;
            if ENABLE_TRACE {
                eprintln!("Entering Rule: {:?} {:?}", stringify!($rule), $i);
            }
            let result = call!($i, $rule);
            if ENABLE_TRACE {
                eprintln!("Exiting Rule: {:?} with {:?}", stringify!($rule), result);
            }
            result
        }
    };
}

fn symbol_to_value(s: &Token) -> ParseResult<Value> {
    Ok(Value::Symbol(value_node!(
        s.fragment.to_string(),
        s.pos.clone()
    )))
}

// symbol is a bare unquoted field.
named!(symbol<TokenIter, Value, error::Error>,
    match_type!(BAREWORD => symbol_to_value)
);

fn str_to_value(s: &Token) -> ParseResult<Value> {
    Ok(Value::Str(value_node!(
        s.fragment.to_string(),
        s.pos.clone()
    )))
}

// quoted_value is a quoted string.
named!(quoted_value<TokenIter, Value, error::Error>,
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
                return Err(error::Error::new(
                    format!("Not an integer! {}", pref),
                    error::ErrorType::UnexpectedToken,
                    pref_pos,
                ))
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
    let f = match FromStr::from_str(&to_parse) {
        Ok(f) => f,
        Err(_) => {
            return Err(error::Error::new(
                format!("Not a float! {}", to_parse),
                error::ErrorType::UnexpectedToken,
                // NOTE(jwall): This is ugly. I should probably see if I can refactor
                // it to something less confusing.
                maybepos.unwrap(),
            ));
        }
    };
    return Ok(Value::Float(value_node!(f, pref_pos)));
}

/// alt_peek conditionally runs a combinator if a lookahead combinator matches.
macro_rules! alt_peek {
    (__inner $i:expr, $peekrule:ident!( $($peekargs:tt)* ) => $parserule:ident | $($rest:tt)* ) => (
        alt_peek!(__inner $i, $peekrule!($($peekargs)*) => call!($parserule) | $($rest)* )
    );

    (__inner $i:expr, $peekrule:ident => $($rest:tt)* ) => (
        alt_peek!(__inner $i, call!($peek) => $($rest)* )
    );

    (__inner $i:expr, $peekrule:ident!( $($peekargs:tt)* ) => $parserule:ident!( $($parseargs:tt)* ) | $($rest:tt)* ) => (
        {
            let _i = $i.clone();
            let pre_res = peek!(_i, $peekrule!($($peekargs)*));
            match pre_res {
                // if the peek was incomplete then it might still match so return incomplete.
                nom::IResult::Incomplete(i) => nom::IResult::Incomplete(i),
                // If the peek was in error then try the next peek => parse pair.
                nom::IResult::Error(_) =>  {
                    alt_peek!(__inner $i, $($rest)*)
                },
                // If the peek was successful then return the result of the parserule
                // regardless of it's result.
                nom::IResult::Done(_i, _) => {
                    $parserule!(_i, $($parseargs)*)
                },
            }
        }
    );

    // These are our no fallback termination cases.
    (__inner $i:expr, $peekrule:ident!( $($peekargs:tt)* ) => $parserule:ident, __end ) => (
        alt_peek!(__inner $i, $peekrule!($($peekargs)*) => call!($parserule), __end )
    );

    (__inner $i:expr, $peekrule:ident!( $($peekargs:tt)* ) => $parserule:ident!( $($parseargs:tt)* ), __end ) => (
        {
            let _i = $i.clone();
            let pre_res = peek!(_i, $peekrule!($($peekargs)*));
            match pre_res {
                // if the peek was incomplete then it might still match so return incomplete.
                nom::IResult::Incomplete(i) => nom::IResult::Incomplete(i),
                // If the peek was in error then try the next peek => parse pair.
                nom::IResult::Error(_) =>  {
                    alt_peek!(__inner $i, __end)
                },
                // If the peek was successful then return the result of the parserule
                // regardless of it's result.
                nom::IResult::Done(_i, _) => {
                    $parserule!(_i, $($parseargs)*)
                },
            }
        }
    );

    // These are our fallback termination cases.
    (__inner $i:expr, $fallback:ident, __end) => (
        {
            let _i = $i.clone();
            call!(_i, $fallback)
        }
    );
    // In the case of a fallback rule with no peek we just return whatever
    // the fallback rule returns.
    (__inner $i:expr, $fallback:ident!( $($args:tt)* ), __end) => (
        {
            let _i = $i.clone();
            $fallback!(_i, $($args)*)
        }
    );

    // This is our default termination case.
    // If there is no fallback then we return an Error.
    (__inner $i:expr, __end) => {
        // TODO(jwall): We should do a better custom error here.
        nom::IResult::Error(error_position!(nom::ErrorKind::Alt,$i))
    };

    // alt_peek entry_point.
    ($i:expr, $($rest:tt)*) => {
        // We use __end to define the termination token the recursive rule should consume.
        alt_peek!(__inner $i, $($rest)*, __end)
    };
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
named!(number<TokenIter, Value, error::Error>,
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
               (Some(prefix.clone()), None, None)
           )),
           triple_to_number
       )
);
// trace_macros!(false);

named!(boolean_value<TokenIter, Value, error::Error>,
    do_parse!(
        b: match_type!(BOOLEAN) >>
        (Value::Boolean(Positioned{
            val: b.fragment == "true",
            pos: b.pos,
        }))
    )
);

named!(
    field_value<TokenIter, (Token, Expression), error::Error>,
    do_parse!(
            field: alt!(match_type!(BAREWORD) | match_type!(STR)) >>
            punct!("=") >>
            value: expression >>
            (field, value)
    )
);

// Helper function to make the return types work for down below.
fn vec_to_tuple(t: (Position, Option<FieldList>)) -> ParseResult<Value> {
    Ok(Value::Tuple(value_node!(
        t.1.unwrap_or(Vec::new()),
        t.0.line as usize,
        t.0.column as usize
    )))
}

named!(field_list<TokenIter, FieldList, error::Error>,
       separated_list!(punct!(","), field_value)
);

named!(
    tuple<TokenIter, Value, error::Error>,
    map_res!(
        do_parse!(
            pos: pos >>
            punct!("{") >>
            v: field_list >>
            opt_res!(punct!(",")) >> // nom's opt! macro doesn't preserve error types properly but this one does.
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

named!(list_value<TokenIter, Value, error::Error>,
       map_res!(
           do_parse!(
               start: punct!("[") >>
               elements: separated_list!(punct!(","), expression) >>
               opt_res!(punct!(",")) >> // nom's opt! macro doesn't preserve error types properly but this one does.
               punct!("]") >>
               (start.pos, elements)
           ),
           tuple_to_list
       )
);

named!(empty_value<TokenIter, Value, error::Error>,
    do_parse!(
        pos: pos >>
        match_type!(EMPTY) >>
        (Value::Empty(pos))
    )
);

named!(compound_value<TokenIter, Value, error::Error>,
    alt_peek!(
        punct!("[") => trace_nom!(list_value) | 
        punct!("{") => trace_nom!(tuple)
    )
);

named!(scalar_value<TokenIter, Value, error::Error>,
    alt!(
        trace_nom!(boolean_value) |
        trace_nom!(empty_value) |
        trace_nom!(number) |
        trace_nom!(quoted_value)
    )
);

named!(value<TokenIter, Value, error::Error>,
    alt!(
        trace_nom!(selector_value)
        | trace_nom!(compound_value)
        | trace_nom!(scalar_value)
    )
 );

fn value_to_expression(v: Value) -> ParseResult<Expression> {
    Ok(Expression::Simple(v))
}

named!(simple_expression<TokenIter, Expression, error::Error>,
       map_res!(
           trace_nom!(value),
           value_to_expression
       )
);

fn expression_to_grouped_expression(e: Expression) -> ParseResult<Expression> {
    Ok(Expression::Grouped(Box::new(e)))
}

named!(grouped_expression<TokenIter, Expression, error::Error>,
       map_res!(
           preceded!(punct!("("), terminated!(trace_nom!(expression), punct!(")"))),
           expression_to_grouped_expression
       )
);

fn symbol_or_expression(input: TokenIter) -> NomResult<Expression> {
    let scalar_head = do_parse!(input, sym: alt!(symbol | compound_value) >> (sym));

    match scalar_head {
        IResult::Incomplete(i) => IResult::Incomplete(i),
        IResult::Error(_) => grouped_expression(input),
        IResult::Done(rest, val) => {
            let res = peek!(rest.clone(), punct!("."));
            match val {
                Value::Tuple(_) => {
                    if res.is_done() {
                        IResult::Done(rest, Expression::Simple(val))
                    } else {
                        return IResult::Error(nom::ErrorKind::Custom(error::Error::new(
                            "Expected (.) but no dot found".to_string(),
                            error::ErrorType::IncompleteParsing,
                            val.pos().clone(),
                        )));
                    }
                }
                Value::List(_) => {
                    if res.is_done() {
                        IResult::Done(rest, Expression::Simple(val))
                    } else {
                        return IResult::Error(nom::ErrorKind::Custom(error::Error::new(
                            "Expected (.) but no dot found".to_string(),
                            error::ErrorType::IncompleteParsing,
                            val.pos().clone(),
                        )));
                    }
                }
                _ => IResult::Done(rest, Expression::Simple(val)),
            }
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
        let (rest, list) = match separated_list!(
            rest,
            punct!("."),
            alt!(match_type!(BAREWORD) | match_type!(DIGIT) | match_type!(STR))
        ) {
            IResult::Done(rest, val) => (rest, val),
            IResult::Incomplete(i) => {
                return IResult::Incomplete(i);
            }
            IResult::Error(e) => {
                return IResult::Error(e);
            }
        };

        if list.is_empty() {
            return IResult::Error(nom::ErrorKind::Custom(error::Error::new(
                "(.) with no selector fields after".to_string(),
                error::ErrorType::IncompleteParsing,
                is_dot.unwrap().pos,
            )));
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

named!(copy_expression<TokenIter, Expression, error::Error>,
    map_res!(
        do_parse!(
            pos: pos >>
            selector: trace_nom!(selector_list) >>
            punct!("{") >>
            fields: trace_nom!(field_list) >>
            opt_res!(punct!(",")) >> // noms opt! macro does not preserve error types properly but this one does.
            punct!("}") >>
            (SelectorDef::new(selector, pos.line, pos.column as usize), fields)
        ),
        tuple_to_copy
    )
);

fn tuple_to_macro(mut t: (Position, Vec<Value>, Value)) -> ParseResult<Expression> {
    match t.2 {
        Value::Tuple(v) => Ok(Expression::Macro(MacroDef {
            argdefs: t
                .1
                .drain(0..)
                .map(|s| Positioned {
                    pos: s.pos().clone(),
                    val: s.to_string(),
                })
                .collect(),
            fields: v.val,
            pos: t.0,
        })),
        val => Err(error::Error::new(
            format!("Expected Tuple Got {:?}", val),
            error::ErrorType::UnexpectedToken,
            t.0,
        )),
    }
}

named!(arglist<TokenIter, Vec<Value>, error::Error>, separated_list!(punct!(","), symbol));

named!(macro_expression<TokenIter, Expression, error::Error>,
       map_res!(
           do_parse!(
                pos: pos >>
                word!("macro") >>
                punct!("(") >>
                arglist: trace_nom!(arglist) >>
                punct!(")") >>
                punct!("=>") >>
                map: trace_nom!(tuple) >>
                (pos, arglist, map)
           ),
           tuple_to_macro
       )
);

fn tuple_to_select(t: (Position, Expression, Expression, Value)) -> ParseResult<Expression> {
    match t.3 {
        Value::Tuple(v) => Ok(Expression::Select(SelectDef {
            val: Box::new(t.1),
            default: Box::new(t.2),
            tuple: v.val,
            pos: t.0,
        })),
        val => Err(error::Error::new(
            format!("Expected Tuple Got {:?}", val),
            error::ErrorType::UnexpectedToken,
            t.0,
        )),
    }
}

named!(select_expression<TokenIter, Expression, error::Error>,
       map_res!(
           do_parse!(
               start: word!("select") >>
               val: terminated!(trace_nom!(expression), punct!(",")) >>
               default: terminated!(trace_nom!(expression), punct!(",")) >>
               map: trace_nom!(tuple) >>
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

named!(format_expression<TokenIter, Expression, error::Error>,
       map_res!(
           do_parse!(
               tmpl: match_type!(STR) >>
                   punct!("%") >>
                   punct!("(") >>
                   args: separated_list!(punct!(","), trace_nom!(expression)) >>
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
        Err(error::Error::new(
            format!("Expected Selector Got {:?}", t.0),
            error::ErrorType::UnexpectedToken,
            Position::new(t.0.line as usize, t.0.column as usize),
        ))
    }
}

fn vec_to_selector_value(t: (Position, SelectorList)) -> ParseResult<Value> {
    Ok(Value::Selector(SelectorDef::new(
        t.1,
        t.0.line as usize,
        t.0.column as usize,
    )))
}

named!(selector_value<TokenIter, Value, error::Error>,
       map_res!(
           do_parse!(
               sl: trace_nom!(selector_list) >>
               (sl.head.pos().clone(), sl)
           ),
           vec_to_selector_value
       )
);

named!(call_expression<TokenIter, Expression, error::Error>,
       map_res!(
           do_parse!(
               macroname: trace_nom!(selector_value) >>
               punct!("(") >>
               args: separated_list!(punct!(","), trace_nom!(expression)) >>
               punct!(")") >>
               (macroname.pos().clone(), macroname, args)
           ),
           tuple_to_call
       )
);

fn tuple_to_list_op(tpl: (Position, Token, Value, Expression)) -> ParseResult<Expression> {
    let pos = tpl.0;
    let t = if &tpl.1.fragment == "map" {
        ListOpType::Map
    } else if &tpl.1.fragment == "filter" {
        ListOpType::Filter
    } else {
        return Err(error::Error::new(
            format!(
                "Expected one of 'map' or 'filter' but got '{}'",
                tpl.1.fragment
            ),
            error::ErrorType::UnexpectedToken,
            pos,
        ));
    };
    let macroname = tpl.2;
    let list = tpl.3;
    if let Value::Selector(mut def) = macroname {
        // First of all we need to assert that this is a selector of at least
        // two sections.
        let fieldname: String = match &mut def.sel.tail {
            &mut None => {
                if ENABLE_TRACE {
                    eprintln!(
                        "tuple_to_list_op had error {}",
                        "Missing a result field for the macro"
                    );
                }
                return Err(error::Error::new(
                    format!("Missing a result field for the macro"),
                    error::ErrorType::IncompleteParsing,
                    pos,
                ));
            }
            &mut Some(ref mut tl) => {
                if tl.len() < 1 {
                    if ENABLE_TRACE {
                        eprintln!(
                            "tuple_to_list_op had error {}",
                            "Missing a result field for the macro"
                        );
                    }
                    return Err(error::Error::new(
                        format!("Missing a result field for the macro"),
                        error::ErrorType::IncompleteParsing,
                        def.pos.clone(),
                    ));
                }
                let fname = tl.pop();
                fname.unwrap().fragment
            }
        };
        return Ok(Expression::ListOp(ListOpDef {
            typ: t,
            mac: def,
            field: fieldname,
            target: Box::new(list),
            pos: pos,
        }));
    }
    if ENABLE_TRACE {
        eprintln!(
            "tuple_to_list_op had error {}",
            format!("Expected a selector but got {}", macroname.type_name())
        );
    }
    return Err(error::Error::new(
        format!("Expected a selector but got {}", macroname.type_name()),
        error::ErrorType::UnexpectedToken,
        pos,
    ));
}

named!(list_op_expression<TokenIter, Expression, error::Error>,
    map_res!(
        do_parse!(
            pos: pos >>
            optype: alt!(word!("map") | word!("filter")) >>
            macroname: trace_nom!(selector_value) >>
            list: trace_nom!(non_op_expression) >>
            (pos, optype, macroname, list)
        ),
        tuple_to_list_op
    )
);

named!(non_op_expression<TokenIter, Expression, error::Error>,
    alt!(trace_nom!(list_op_expression) |
         trace_nom!(macro_expression) |
         trace_nom!(format_expression) |
         trace_nom!(select_expression) |
         trace_nom!(grouped_expression) |
         trace_nom!(call_expression) |
         trace_nom!(copy_expression) |
         trace_nom!(simple_expression))
);

named!(expression<TokenIter, Expression, error::Error>,
    alt_complete!(trace_nom!(op_expression) | trace_nom!(non_op_expression))
);

fn expression_to_statement(v: Expression) -> ParseResult<Statement> {
    Ok(Statement::Expression(v))
}

named!(expression_statement<TokenIter, Statement, error::Error>,
    map_res!(
        terminated!(trace_nom!(expression), punct!(";")),
        expression_to_statement
    )
);

fn tuple_to_let(t: (Token, Expression)) -> ParseResult<Statement> {
    Ok(Statement::Let(LetDef {
        name: t.0,
        value: t.1,
    }))
}

named!(let_stmt_body<TokenIter, Statement, error::Error>,
    map_res!(
        do_parse!(
            name: match_type!(BAREWORD) >>
            punct!("=") >>
            val: trace_nom!(expression) >>
            punct!(";") >>
            (name, val)),
        tuple_to_let
    )
);

named!(let_statement<TokenIter, Statement, error::Error>,
    do_parse!(
        word!("let") >>
        pos: pos >>
        stmt: trace_nom!(let_stmt_body) >>
        (stmt)
    )
);

fn tuple_to_import(t: (Token, Token)) -> ParseResult<Statement> {
    Ok(Statement::Import(ImportDef {
        path: t.0,
        name: t.1,
    }))
}

named!(import_stmt_body<TokenIter, Statement, error::Error>,
    map_res!(
        do_parse!(
             path: match_type!(STR) >>
             word!("as") >>
             name: match_type!(BAREWORD) >>
             punct!(";") >>
             (path, name)),
       tuple_to_import
    )
);

named!(import_statement<TokenIter, Statement, error::Error>,
    do_parse!(
        word!("import") >>
        // past this point we know this is supposed to be an import statement.
        pos: pos >>
        stmt: trace_nom!(import_stmt_body) >>
        (stmt)
    )
);

named!(assert_statement<TokenIter, Statement, error::Error>,
    do_parse!(
        word!("assert") >>
        pos: pos >>
        tok: match_type!(PIPEQUOTE) >>
        punct!(";") >>
        (Statement::Assert(tok.clone()))
    )
);

named!(out_statement<TokenIter, Statement, error::Error>,
    do_parse!(
        word!("out") >>
        pos: pos >>
        typ: match_type!(BAREWORD) >>
        expr: expression >>
        punct!(";") >>
        (Statement::Output(typ.clone(), expr.clone()))
    )
);

//trace_macros!(true);
fn statement(i: TokenIter) -> nom::IResult<TokenIter, Statement, error::Error> {
    return alt_peek!(i,
            word!("assert") => trace_nom!(assert_statement) |
            word!("import") => trace_nom!(import_statement) |
            word!("let") => trace_nom!(let_statement) |
            word!("out") => trace_nom!(out_statement) |
            trace_nom!(expression_statement)
        );
}
//trace_macros!(false);

/// Parses a LocatedSpan into a list of Statements or an error::Error.
pub fn parse(input: LocatedSpan<&str>) -> Result<Vec<Statement>, error::Error> {
    match tokenize(input) {
        Ok(tokenized) => {
            let mut out = Vec::new();
            let mut i_ = TokenIter {
                source: tokenized.as_slice(),
            };
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
                        return Err(error::Error::new_with_errorkind(
                            "Statement Parse error",
                            error::ErrorType::ParseError,
                            Position {
                                line: i_[0].pos.line,
                                column: i_[0].pos.column,
                            },
                            e,
                        ));
                    }
                    IResult::Incomplete(ei) => {
                        return Err(error::Error::new(
                            format!("Unexpected end of parsing input: {:?}", ei),
                            error::ErrorType::IncompleteParsing,
                            Position {
                                line: i_[0].pos.line,
                                column: i_[0].pos.column,
                            },
                        ));
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
            return Err(error::Error::new(
                format!("Tokenization Error {:?}", e.1),
                error::ErrorType::ParseError,
                e.0,
            ));
        }
    }
}

pub mod precedence;

#[cfg(test)]
mod test;
