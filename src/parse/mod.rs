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
use std;
use std::borrow::Borrow;
use std::str::FromStr;

use abortable_parser;
use abortable_parser::combinators::eoi;
use abortable_parser::iter::{SliceIter, StrIter};
use abortable_parser::{Error, Peekable, Result};

use self::precedence::op_expression;
use ast::*;
use tokenizer::*;

type NomResult<'a, O> = Result<SliceIter<'a, Token>, O>;

// FIXME(jwall): All the do_each mappers need to return an actual value.
#[cfg(feature = "tracing")]
const ENABLE_TRACE: bool = true;
#[cfg(not(feature = "tracing"))]
const ENABLE_TRACE: bool = false;

type ParseResult<O> = std::result::Result<O, abortable_parser::Error>;

macro_rules! trace_nom {
    ($i:expr, $rule:ident!( $($args:tt)* )) => {
        {
            use parse::ENABLE_TRACE;
            if ENABLE_TRACE {
                eprintln!("Entering Rule: {:?} {:?}", stringify!($rule), $i);
            }
            let result = $rule!($i, $($args)* );
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
            let result = run!($i, $rule);
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
make_fn!(
    symbol<SliceIter<Token>, Value>,
    match_type!(BAREWORD => symbol_to_value)
);

fn str_to_value(s: &Token) -> ParseResult<Value> {
    Ok(Value::Str(value_node!(
        s.fragment.to_string(),
        s.pos.clone()
    )))
}

// quoted_value is a quoted string.
make_fn!(
    quoted_value<SliceIter<Token>, Value>,
    match_type!(STR => str_to_value)
);

// FIXME(jwall): We need to just turn this into a custom parser function.
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
                return Err(Error::new(
                    format!("Not an integer! {}", pref),
                    // FIXME(jwall): This really needs the correct offset.
                    &0,
                ));
            }
        };
        return Ok(Value::Int(value_node!(i, pref_pos)));
    }

    if v.0.is_none() && has_dot {
        pref_pos = v.1.unwrap().pos;
    }

    let suf = match v.2 {
        None => "".to_string(),
        Some(bs) => bs.fragment,
    };

    let to_parse = pref.to_string() + "." + &suf;
    let f = match FromStr::from_str(&to_parse) {
        Ok(f) => f,
        Err(_) => {
            return Err(Error::new(
                format!("Not a float! {}", to_parse),
                // FIXME(jwall): This should take the real offset.
                &0,
            ));
        }
    };
    return Ok(Value::Float(value_node!(f, pref_pos)));
}

// FIXME(jwall): This should actually be unnecessary now.

/// alt_peek conditionally runs a combinator if a lookahead combinator matches.
macro_rules! alt_peek {
    (__inner $i:expr, $peekrule:ident!( $($peekargs:tt)* ) => $parserule:ident | $($rest:tt)* ) => (
        alt_peek!(__inner $i, $peekrule!($($peekargs)*) => run!($parserule) | $($rest)* )
    );

    (__inner $i:expr, $peekrule:ident => $($rest:tt)* ) => (
        alt_peek!(__inner $i, run!($peekrule) => $($rest)* )
    );

    (__inner $i:expr, $peekrule:ident!( $($peekargs:tt)* ) => $parserule:ident!( $($parseargs:tt)* ) | $($rest:tt)* ) => (
        {
            let _i = $i.clone();
            let pre_res = peek!(_i, $peekrule!($($peekargs)*));
            match pre_res {
                // if the peek was incomplete then it might still match so return incomplete.
                Result::Incomplete(i) => Result::Incomplete(i),
                // If the peek was in error then try the next peek => parse pair.
                Result::Fail(_) =>  {
                    alt_peek!(__inner $i, $($rest)*)
                },
                Result::Abort(e) => Result::Abort(e),
                // If the peek was successful then return the result of the parserule
                // regardless of it's result.
                Result::Complete(_i, _) => {
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
                Result::Incomplete(i) => Result::Incomplete(i),
                // If the peek was in error then try the next peek => parse pair.
                Result::Fail(_) =>  {
                    alt_peek!(__inner $i, __end)
                },
                Result::Abort(e) => Result::Abort(e),
                // If the peek was successful then return the result of the parserule
                // regardless of it's result.
                Result::Complete(_i, _) => {
                    $parserule!(_i, $($parseargs)*)
                },
            }
        }
    );

    // These are our fallback termination cases.
    (__inner $i:expr, $fallback:ident, __end) => (
        {
            let _i = $i.clone();
            run!(_i, $fallback)
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
        // FIXME(jwall): Should we make this a compile error instead?
        compile_error!("alt_peek! requirs a fallback case");
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
// decreasing size order which messes with either!'s completion logic. To
// work around this we have to force Incomplete to be Error so that
// either! will try the next in the series instead of aborting.
//
// *IMPORTANT*
// It also means this combinator is risky when used with partial
// inputs. So handle with care.
fn number(input: SliceIter<Token>) -> Result<SliceIter<Token>, Value> {
    let parsed = do_each!(input,
            num => either!(
                complete!(
                     "Not a float",
                     do_each!( // 1.0
                         prefix => match_type!(DIGIT),
                         has_dot => punct!("."),
                         suffix => match_type!(DIGIT),
                         (Some(prefix.clone()), Some(has_dot.clone()), Some(suffix.clone()))
                )),
                complete!(
                     "Not a float",
                     do_each!( // 1.
                         prefix => match_type!(DIGIT),
                         has_dot => punct!("."),
                         (Some(prefix.clone()), Some(has_dot.clone()), None)
                )),
                complete!(
                     "Not a float",
                     do_each!( // .1
                         has_dot => punct!("."),
                         suffix => match_type!(DIGIT),
                         (None, Some(has_dot.clone()), Some(suffix.clone()))
                )),
                do_each!( // 1
                    prefix => match_type!(DIGIT),
                    (Some(prefix.clone()), None, None)
                )),
            (num)
       );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, triple) => match triple_to_number(triple) {
            Ok(val) => Result::Complete(rest, val),
            Err(e) => Result::Fail(e),
        },
    }
}
// trace_macros!(false);

make_fn!(
    boolean_value<SliceIter<Token>, Value>,
    do_each!(
        b => match_type!(BOOLEAN),
        (Value::Boolean(Positioned{
            val: b.fragment == "true",
            pos: b.pos,
        }))
    )
);

make_fn!(
    field_value<SliceIter<Token>, (Token, Expression)>,
    do_each!(
            field => wrap_err!(either!(match_type!(BAREWORD), match_type!(STR)),
                               "Field names must be a bareword or a string."),
            _ => punct!("="),
            value => expression,
            (field, value)
    )
);

// Helper function to make the return types work for down below.
fn vec_to_tuple(pos: Position, fields: Option<FieldList>) -> Value {
    Value::Tuple(value_node!(
        fields.unwrap_or(Vec::new()),
        pos.line as usize,
        pos.column as usize
    ))
}

make_fn!(
    field_list<SliceIter<Token>, FieldList>,
    separated!(punct!(","), field_value)
);

make_fn!(
    tuple<SliceIter<Token>, Value>,
    do_each!(
        pos => pos,
        _ => punct!("{"),
        v => optional!(field_list),
        _ => optional!(punct!(",")),
        _ => punct!("}"),
        (vec_to_tuple(pos, v))
    )
);

fn tuple_to_list<Sp: Into<Position>>(pos: Sp, elems: Vec<Expression>) -> Value {
    Value::List(ListDef {
        elems: elems,
        pos: pos.into(),
    })
}

make_fn!(
    list_value<SliceIter<Token>, Value>,
    do_each!(
        start => punct!("["),
        elements => separated!(punct!(","), expression),
        _ => optional!(punct!(",")), // nom's opt! macro doesn't preserve error types properly but this one does.
        _ => punct!("]"),
        (tuple_to_list(start.pos, elements))
    )
);

make_fn!(
    empty_value<SliceIter<Token>, Value>,
    do_each!(
        pos => pos,
        _ => match_type!(EMPTY),
        (Value::Empty(pos.into()))
    )
);

make_fn!(
    compound_value<SliceIter<Token>, Value>,
    either!(trace_nom!(list_value), trace_nom!(tuple))
);

make_fn!(
    value<SliceIter<Token>, Value>,
    alt_peek!(
        symbol_or_expression => trace_nom!(selector_value)
        | either!(punct!("["), punct!("{")) => trace_nom!(compound_value)
        | match_type!(BOOLEAN) => trace_nom!(boolean_value)
        | match_type!(EMPTY) => trace_nom!(empty_value)
        | either!(match_type!(DIGIT), punct!(".")) => trace_nom!(number)
        | trace_nom!(quoted_value)
    )
);

fn value_to_expression(v: Value) -> Expression {
    Expression::Simple(v)
}

make_fn!(
    simple_expression<SliceIter<Token>, Expression>,
    do_each!(
        val => trace_nom!(value),
        (value_to_expression(val))
    )
);

fn expression_to_grouped_expression(e: Expression) -> Expression {
    Expression::Grouped(Box::new(e))
}

make_fn!(
    grouped_expression<SliceIter<Token>, Expression>,
    do_each!(
        _ => punct!("("),
        expr => do_each!(
            expr => trace_nom!(expression),
            _ => punct!(")"),
            (expr)
        ),
        (expression_to_grouped_expression(expr))
    )
);

fn symbol_or_expression(input: SliceIter<Token>) -> NomResult<Expression> {
    let _i = input.clone();
    let scalar_head = do_each!(input,
        sym => either!(symbol, compound_value),
        (sym)
    );

    match scalar_head {
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Fail(_) => grouped_expression(_i),
        Result::Abort(e) => Result::Abort(e),
        Result::Complete(rest, val) => {
            let res = peek!(rest.clone(), punct!("."));
            match val {
                Value::Tuple(_) => {
                    if res.is_complete() {
                        Result::Complete(rest, Expression::Simple(val))
                    } else {
                        return Result::Fail(Error::new(
                            "Expected (.) but no dot found".to_string(),
                            &rest,
                        ));
                    }
                }
                Value::List(_) => {
                    if res.is_complete() {
                        Result::Complete(rest, Expression::Simple(val))
                    } else {
                        return Result::Fail(Error::new(
                            "Expected (.) but no dot found".to_string(),
                            &rest,
                        ));
                    }
                }
                _ => Result::Complete(rest, Expression::Simple(val)),
            }
        }
    }
}

fn selector_list(input: SliceIter<Token>) -> NomResult<SelectorList> {
    let (rest, head) = match symbol_or_expression(input) {
        Result::Complete(rest, val) => (rest, val),
        Result::Fail(e) => {
            return Result::Fail(e);
        }
        Result::Incomplete(i) => {
            return Result::Incomplete(i);
        }
        Result::Abort(e) => return Result::Abort(e),
    };

    let (rest, is_dot) = match punct!(rest, ".") {
        Result::Complete(rest, tok) => (rest, Some(tok)),
        Result::Incomplete(i) => {
            return Result::Incomplete(i);
        }
        Result::Fail(_) => (rest, None),
        Result::Abort(e) => return Result::Abort(e),
    };

    let (rest, list) = if is_dot.is_some() {
        let (rest, list) = match separated!(
            rest,
            punct!("."),
            either!(match_type!(BAREWORD), match_type!(DIGIT), match_type!(STR))
        ) {
            Result::Complete(rest, val) => (rest, val),
            Result::Incomplete(i) => return Result::Incomplete(i),
            Result::Fail(e) => return Result::Fail(e),
            Result::Abort(e) => return Result::Abort(e),
        };

        if list.is_empty() {
            return Result::Fail(Error::new(
                "(.) with no selector fields after".to_string(),
                &rest,
            ));
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

    return Result::Complete(rest, sel_list);
}

fn tuple_to_copy(def: SelectorDef, fields: Option<FieldList>) -> Expression {
    let pos = def.pos.clone();
    let fields = match fields {
        Some(fields) => fields,
        None => Vec::new(),
    };
    Expression::Copy(CopyDef {
        selector: def,
        fields: fields,
        pos: pos,
    })
}

make_fn!(
    copy_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        selector => trace_nom!(selector_list),
        _ => punct!("{"),
        fields => optional!(trace_nom!(field_list)),
        _ => optional!(punct!(",")), // noms opt! macro does not preserve error types properly but this one does.
        _ => punct!("}"),
        (tuple_to_copy(SelectorDef::new(selector, pos.line, pos.column), fields))
    )
);

// FIXME(jwall): need to make this into a proper parse function.
fn tuple_to_macro(pos: Position, vals: Option<Vec<Value>>, val: Value) -> ParseResult<Expression> {
    let mut default_args = match vals {
        None => Vec::new(),
        Some(vals) => vals,
    };
    let arglist = default_args
        .drain(0..)
        .map(|s| Positioned {
            pos: s.pos().clone(),
            val: s.to_string(),
        }).collect();
    match val {
        Value::Tuple(v) => Ok(Expression::Macro(MacroDef {
            argdefs: arglist,
            fields: v.val,
            pos: pos,
        })),
        val => Err(Error::new(
            format!("Expected Tuple Got {:?}", val),
            // FIXME(jwall): Should have correct Offset here.
            &0,
        )),
    }
}

make_fn!(
    arglist<SliceIter<Token>, Vec<Value>>,
    separated!(punct!(","), symbol)
);

fn macro_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input,
        pos => pos,
        _ => word!("macro"),
        _ => punct!("("),
        arglist => trace_nom!(optional!(arglist)),
        _ => punct!(")"),
        _ => punct!("=>"),
        map =>  trace_nom!(tuple),
        (pos, arglist, map)
    );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, (pos, arglist, map)) => match tuple_to_macro(pos, arglist, map) {
            Ok(expr) => Result::Complete(rest, expr),
            Err(e) => Result::Fail(Error::caused_by("Invalid Macro syntax", &rest, Box::new(e))),
        },
    }
}

// FIXME(jwall): need to make this into a proper parse function.
fn tuple_to_select(
    pos: Position,
    e1: Expression,
    e2: Expression,
    val: Value,
) -> ParseResult<Expression> {
    match val {
        Value::Tuple(v) => Ok(Expression::Select(SelectDef {
            val: Box::new(e1),
            default: Box::new(e2),
            tuple: v.val,
            pos: pos,
        })),
        val => Err(Error::new(format!("Expected Tuple Got {:?}", val), &0)),
    }
}

fn select_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input,
        start => word!("select"),
        val => do_each!(
            expr => trace_nom!(expression),
            _ => punct!(","),
            (expr)
        ),
        default => do_each!(
            expr => trace_nom!(expression),
            _ => punct!(","),
            (expr)
        ),
        map => trace_nom!(tuple),
        (start.pos.clone(), val, default, map)
    );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, (pos, val, default, map)) => {
            match tuple_to_select(pos, val, default, map) {
                Ok(expr) => Result::Complete(rest, expr),
                Err(e) => Result::Fail(Error::caused_by(
                    "Invalid Select Expression",
                    &rest,
                    Box::new(e),
                )),
            }
        }
    }
}

fn tuple_to_format(tok: Token, exprs: Vec<Expression>) -> Expression {
    Expression::Format(FormatDef {
        template: tok.fragment.to_string(),
        args: exprs,
        pos: tok.pos,
    })
}

make_fn!(
    format_expression<SliceIter<Token>, Expression>,
    do_each!(
        tmpl => match_type!(STR),
        _ => punct!("%"),
        _ => punct!("("),
        args => separated!(punct!(","), trace_nom!(expression)),
        _ => punct!(")"),
        (tuple_to_format(tmpl, args))
    )
);

// FIXME(jwall): Convert this into a custom parser function.
fn tuple_to_call(pos: Position, val: Value, exprs: Vec<Expression>) -> ParseResult<Expression> {
    if let Value::Selector(def) = val {
        Ok(Expression::Call(CallDef {
            macroref: def,
            arglist: exprs,
            pos: pos,
        }))
    } else {
        // FIXME(jwall): Should get correct offset here.
        Err(Error::new(format!("Expected Selector Got {:?}", val), &0))
    }
}

fn vec_to_selector_value(pos: Position, list: SelectorList) -> Value {
    Value::Selector(SelectorDef::new(
        list,
        pos.line as usize,
        pos.column as usize,
    ))
}

make_fn!(
    selector_value<SliceIter<Token>, Value>,
    do_each!(
        sl => trace_nom!(selector_list),
        (vec_to_selector_value(sl.head.pos().clone(), sl))
    )
);

fn call_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input,
        macroname => trace_nom!(selector_value),
        _ => punct!("("),
        args => separated!(punct!(","), trace_nom!(expression)),
        _ => punct!(")"),
        (macroname.pos().clone(), macroname, args)
    );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, (pos, name, args)) => match tuple_to_call(pos, name, args) {
            Ok(expr) => Result::Complete(rest, expr),
            Err(e) => Result::Fail(Error::caused_by("Invalid Call Syntax", &rest, Box::new(e))),
        },
    }
}

fn tuple_to_list_op(
    pos: Position,
    kind: ListOpType,
    macroname: Value,
    list: Expression,
) -> ParseResult<Expression> {
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
                return Err(Error::new(
                    format!("Missing a result field for the macro"),
                    // FIXME(jwall): Should have correct offset.
                    &0,
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
                    return Err(Error::new(
                        format!("Missing a result field for the macro"),
                        // FIXME(jwall): Should have correct offset.
                        &0,
                    ));
                }
                let fname = tl.pop();
                fname.unwrap().fragment
            }
        };
        return Ok(Expression::ListOp(ListOpDef {
            typ: kind,
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
    return Err(Error::new(
        format!("Expected a selector but got {}", macroname.type_name()),
        // FIXME(jwall): Should have correct offset.
        &0,
    ));
}

// FIXME(jwall): need to make this a custom function to parse it.
make_fn!(
    list_op_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        optype => either!(
            do_each!(_ => word!("map"), (ListOpType::Map)),
            do_each!(_ => word!("filter"), (ListOpType::Filter))
        ),
        macroname => trace_nom!(selector_value),
        list => trace_nom!(non_op_expression),
        (tuple_to_list_op(pos, optype, macroname, list).unwrap())
    )
);

fn unprefixed_expression(input: SliceIter<Token>) -> NomResult<Expression> {
    let _input = input.clone();
    let attempt = either!(
        input,
        trace_nom!(call_expression),
        trace_nom!(copy_expression),
        trace_nom!(format_expression)
    );
    match attempt {
        Result::Incomplete(i) => Result::Incomplete(i),
        Result::Complete(rest, expr) => Result::Complete(rest, expr),
        Result::Fail(_) => trace_nom!(_input, simple_expression),
        Result::Abort(e) => Result::Abort(e),
    }
}

make_fn!(
    non_op_expression<SliceIter<Token>, Expression>,
    alt_peek!(
         either!(word!("map"), word!("filter")) => trace_nom!(list_op_expression) |
         word!("macro") => trace_nom!(macro_expression) |
         word!("select") => trace_nom!(select_expression) |
         punct!("(") => trace_nom!(grouped_expression) |
         trace_nom!(unprefixed_expression))
);

fn expression(input: SliceIter<Token>) -> NomResult<Expression> {
    let _input = input.clone();
    match trace_nom!(_input, op_expression) {
        Result::Incomplete(i) => Result::Incomplete(i),
        Result::Fail(_) => trace_nom!(input, non_op_expression),
        Result::Abort(e) => Result::Abort(e),
        Result::Complete(rest, expr) => Result::Complete(rest, expr),
    }
}

make_fn!(
    expression_statement<SliceIter<Token>, Statement>,
    do_each!(
        e => do_each!(
            expr => trace_nom!(expression),
            _ => punct!(";"),
            (expr)
        ),
        (Statement::Expression(e))
    )
);

fn tuple_to_let(tok: Token, expr: Expression) -> Statement {
    Statement::Let(LetDef {
        name: tok,
        value: expr,
    })
}

make_fn!(
    let_stmt_body<SliceIter<Token>, Statement>,
    do_each!(
        name => match_type!(BAREWORD),
        _ => punct!("="),
        val => trace_nom!(expression),
        _ => punct!(";"),
        (tuple_to_let(name, val))
    )
);

make_fn!(
    let_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("let"),
        stmt => trace_nom!(let_stmt_body),
        (stmt)
    )
);

fn tuple_to_import(tok: Token, tok2: Token) -> Statement {
    Statement::Import(ImportDef {
        path: tok,
        name: tok2,
    })
}

make_fn!(
    import_stmt_body<SliceIter<Token>, Statement>,
    do_each!(
        path => match_type!(STR),
        _ => word!("as"),
        name => match_type!(BAREWORD),
        _ => punct!(";"),
        (tuple_to_import(path, name))
    )
);

make_fn!(
    import_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("import"),
        // past this point we know this is supposed to be an import statement.
        stmt => trace_nom!(import_stmt_body),
        (stmt)
    )
);

make_fn!(
    assert_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("assert"),
            tok => match_type!(PIPEQUOTE),
            _ => punct!(";"),
            (Statement::Assert(tok.clone()))
    )
);

make_fn!(
    out_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("out"),
        typ => match_type!(BAREWORD),
        expr => expression,
        _ => punct!(";"),
        (Statement::Output(typ.clone(), expr.clone()))
    )
);

//trace_macros!(true);
fn statement(i: SliceIter<Token>) -> Result<SliceIter<Token>, Statement> {
    return alt_peek!(i,
            word!("assert") => trace_nom!(assert_statement) |
            word!("import") => trace_nom!(import_statement) |
            word!("let") => trace_nom!(let_statement) |
            word!("out") => trace_nom!(out_statement) |
            trace_nom!(expression_statement)
        );
}
//trace_macros!(false);

/// Parses a LocatedSpan into a list of Statements or an `abortable_parser::Error`.
pub fn parse(input: StrIter) -> std::result::Result<Vec<Statement>, Error> {
    match tokenize(input.clone()) {
        Ok(tokenized) => {
            let mut out = Vec::new();
            let mut i_ = SliceIter::from(&tokenized);
            loop {
                let i = i_.clone();
                if let Some(tok) = i.peek_next() {
                    if tok.typ == TokenType::END {
                        break;
                    }
                }
                match statement(i.clone()) {
                    Result::Abort(e) => {
                        return Err(e);
                    }
                    Result::Fail(e) => {
                        return Err(Error::caused_by("Statement Parse error", &i, Box::new(e)));
                    }
                    Result::Incomplete(ei) => {
                        return Err(Error::new("Unexpected end of parsing input: {:?}", &ei));
                    }
                    Result::Complete(rest, stmt) => {
                        out.push(stmt);
                        i_ = rest;
                        if eoi(i).is_complete() {
                            break;
                        }
                    }
                }
            }
            return Ok(out);
        }
        Err(e) => {
            return Err(Error::caused_by("Tokenization Error", &input, Box::new(e)));
        }
    }
}

pub mod precedence;

#[cfg(test)]
mod test;
