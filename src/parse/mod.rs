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
use abortable_parser::iter::SliceIter;
use abortable_parser::{Error, Peekable, Result};

use self::precedence::op_expression;
use crate::ast::*;
use crate::error::StackPrinter;
use crate::iter::OffsetStrIter;
use crate::tokenizer::*;

// TODO(jwall): Rename this to something better.
type NomResult<'a, O> = Result<SliceIter<'a, Token>, O>;

#[cfg(feature = "tracing")]
const ENABLE_TRACE: bool = true;
#[cfg(not(feature = "tracing"))]
const ENABLE_TRACE: bool = false;

type ParseResult<'a, O> = std::result::Result<O, abortable_parser::Error<SliceIter<'a, Token>>>;

macro_rules! trace_nom {
    ($i:expr, $rule:ident!( $($args:tt)* )) => {
        {
            use crate::parse::ENABLE_TRACE;
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
            use crate::parse::ENABLE_TRACE;
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

// Helper function to make the return types work for down below.
fn triple_to_number<'a>(
    input: SliceIter<'a, Token>,
    v: (Option<Token>, Option<Token>, Option<Token>),
) -> ParseResult<'a, Value> {
    let (pref, mut pref_pos) = match v.0 {
        None => ("", Position::new(0, 0, 0)),
        Some(ref bs) => (bs.fragment.borrow(), bs.pos.clone()),
    };

    let has_dot = v.1.is_some();

    if v.0.is_some() && !has_dot && v.2.is_none() {
        let i = match FromStr::from_str(pref) {
            Ok(i) => i,
            Err(_) => {
                return Err(Error::new(
                    format!("Not an integer! {}", pref),
                    Box::new(input.clone()),
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
                Box::new(input.clone()),
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
        Result::Complete(rest, triple) => {
            let num = triple_to_number(rest.clone(), triple);
            match num {
                Ok(val) => Result::Complete(rest, val),
                Err(e) => Result::Fail(e),
            }
        }
    }
}
// trace_macros!(false);

make_fn!(
    boolean_value<SliceIter<Token>, Value>,
    do_each!(
        b => match_type!(BOOLEAN),
        (Value::Boolean(PositionedItem{
            val: b.fragment == "true",
            pos: b.pos,
        }))
    )
);

make_fn!(
    field_value<SliceIter<Token>, (Token, Expression)>,
    do_each!(
            field => wrap_err!(either!(match_type!(BOOLEAN), match_type!(BAREWORD), match_type!(STR)),
                               "Field names must be a bareword or a string."),
            _ => punct!("="),
            value => expression,
            (field, value)
    )
);

// Helper function to make the return types work for down below.
fn vec_to_tuple(pos: Position, fields: Option<FieldList>) -> Value {
    Value::Tuple(value_node!(fields.unwrap_or(Vec::new()), pos))
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

fn tuple_to_list<Sp: Into<Position>>(pos: Sp, elems: Option<Vec<Expression>>) -> Value {
    Value::List(ListDef {
        elems: elems.unwrap_or_else(|| Vec::new()),
        pos: pos.into(),
    })
}

make_fn!(
    list_value<SliceIter<Token>, Value>,
    do_each!(
        start => punct!("["),
        elements => optional!(separated!(punct!(","), expression)),
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
                            Box::new(rest.clone()),
                        ));
                    }
                }
                Value::List(_) => {
                    if res.is_complete() {
                        Result::Complete(rest, Expression::Simple(val))
                    } else {
                        return Result::Fail(Error::new(
                            "Expected (.) but no dot found".to_string(),
                            Box::new(rest.clone()),
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
                Box::new(rest.clone()),
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
        (tuple_to_copy(SelectorDef::new(selector, pos), fields))
    )
);

fn tuple_to_macro<'a>(
    input: SliceIter<'a, Token>,
    pos: Position,
    vals: Option<Vec<Value>>,
    val: Value,
) -> ParseResult<'a, Expression> {
    let mut default_args = match vals {
        None => Vec::new(),
        Some(vals) => vals,
    };
    let arglist = default_args
        .drain(0..)
        .map(|s| PositionedItem {
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
            Box::new(input.clone()),
        )),
    }
}

make_fn!(
    arglist<SliceIter<Token>, Vec<Value>>,
    separated!(punct!(","), symbol)
);

fn module_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input,
        pos => pos,
        _ => word!("module"),
        _ => punct!("{"),
        arglist => trace_nom!(optional!(field_list)),
        _ => optional!(punct!(",")),
        _ => punct!("}"),
        _ => punct!("=>"),
        _ => punct!("{"),
        stmt_list =>  trace_nom!(repeat!(statement)),
        _ => punct!("}"),
        (pos, arglist, stmt_list)
    );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, (pos, arglist, stmt_list)) => {
            let def = ModuleDef::new(arglist.unwrap_or_else(|| Vec::new()), stmt_list, pos);
            //eprintln!(
            //    "module def at: {:?} arg_typle len {} stmts len {}",
            //    def.pos,
            //    def.arg_set.len(),
            //    def.statements.len()
            //);
            Result::Complete(rest, Expression::Module(def))
        }
    }
}

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
        Result::Complete(rest, (pos, arglist, map)) => {
            match tuple_to_macro(rest.clone(), pos, arglist, map) {
                Ok(expr) => Result::Complete(rest, expr),
                Err(e) => Result::Fail(Error::caused_by(
                    "Invalid Macro syntax",
                    Box::new(e),
                    Box::new(rest.clone()),
                )),
            }
        }
    }
}

fn tuple_to_select<'a>(
    input: SliceIter<'a, Token>,
    e1: Expression,
    e2: Expression,
    val: Value,
) -> ParseResult<'a, Expression> {
    match val {
        Value::Tuple(v) => Ok(Expression::Select(SelectDef {
            val: Box::new(e1),
            default: Box::new(e2),
            tuple: v.val,
            pos: (&input).into(),
        })),
        val => Err(Error::new(
            format!("Expected Tuple Got {:?}", val),
            Box::new(input.clone()),
        )),
    }
}

fn select_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input,
        _ => word!("select"),
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
        (val, default, map)
    );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, (val, default, map)) => {
            match tuple_to_select(input.clone(), val, default, map) {
                Ok(expr) => Result::Complete(rest, expr),
                Err(e) => Result::Fail(Error::caused_by(
                    "Invalid Select Expression",
                    Box::new(e),
                    Box::new(rest.clone()),
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

fn tuple_to_call<'a>(
    input: SliceIter<'a, Token>,
    val: Value,
    exprs: Option<Vec<Expression>>,
) -> ParseResult<'a, Expression> {
    if let Value::Selector(def) = val {
        Ok(Expression::Call(CallDef {
            macroref: def,
            arglist: exprs.unwrap_or_else(|| Vec::new()),
            pos: (&input).into(),
        }))
    } else {
        Err(Error::new(
            format!("Expected Selector Got {:?}", val),
            Box::new(input.clone()),
        ))
    }
}

fn vec_to_selector_value(pos: Position, list: SelectorList) -> Value {
    Value::Selector(SelectorDef::new(list, pos))
}

make_fn!(
    selector_value<SliceIter<Token>, Value>,
    do_each!(
        sl => trace_nom!(selector_list),
        (vec_to_selector_value(sl.head.pos().clone(), sl))
    )
);

fn call_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input.clone(),
        callee_name => trace_nom!(selector_value),
        _ => punct!("("),
        args => optional!(separated!(punct!(","), trace_nom!(expression))),
        _ => punct!(")"),
        (callee_name, args)
    );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, (name, args)) => match tuple_to_call(input.clone(), name, args) {
            Ok(expr) => Result::Complete(rest, expr),
            Err(e) => Result::Fail(Error::caused_by(
                "Invalid Call Syntax",
                Box::new(e),
                Box::new(rest),
            )),
        },
    }
}

fn tuple_to_list_op<'a>(
    input: &'a SliceIter<Token>,
    kind: ListOpType,
    macroname: Value,
    list: Expression,
) -> ParseResult<'a, Expression> {
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
                    Box::new(input.clone()),
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
                        Box::new(input.clone()),
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
            pos: input.into(),
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
        Box::new(input.clone()),
    ));
}

make_fn!(
    list_op_expression<SliceIter<Token>, Expression>,
    do_each!(
        input => input!(),
        optype => either!(
            do_each!(_ => word!("map"), (ListOpType::Map)),
            do_each!(_ => word!("filter"), (ListOpType::Filter))
        ),
        macroname => trace_nom!(selector_value),
        list => trace_nom!(non_op_expression),
        (tuple_to_list_op(&input, optype, macroname, list).unwrap())
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
         word!("module") => trace_nom!(module_expression) |
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
        name => wrap_err!(match_type!(BAREWORD), "Expected name for binding"),
        _ => punct!("="),
        // TODO(jwall): Wrap this error with an appropriate abortable_parser::Error
        val => wrap_err!(trace_nom!(expression), "Expected Expression"),
        _ => punct!(";"),
        (tuple_to_let(name, val))
    )
);

make_fn!(
    let_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("let"),
        stmt => trace_nom!(must!(let_stmt_body)),
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
        path => wrap_err!(match_type!(STR), "Expected import path"),
        _ => word!("as"),
        name => wrap_err!(match_type!(BAREWORD), "Expected import name"),
        _ => punct!(";"),
        (tuple_to_import(path, name))
    )
);

make_fn!(
    import_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("import"),
        // past this point we know this is supposed to be an import statement.
        stmt => trace_nom!(must!(import_stmt_body)),
        (stmt)
    )
);

make_fn!(
    assert_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("assert"),
            tok => must!(match_type!(PIPEQUOTE)),
            _ => must!(punct!(";")),
            (Statement::Assert(tok.clone()))
    )
);

make_fn!(
    out_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("out"),
        typ => wrap_err!(must!(match_type!(BAREWORD)), "Expected converter name"),
        expr => wrap_err!(must!(expression), "Expected Expression to export"),
        _ => must!(punct!(";")),
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

/// Parses a LocatedSpan into a list of Statements or an `error::Error`.
pub fn parse<'a>(input: OffsetStrIter<'a>) -> std::result::Result<Vec<Statement>, String> {
    match tokenize(input.clone()) {
        Ok(tokenized) => {
            let mut out = Vec::new();
            let mut i_ = SliceIter::new(&tokenized);
            loop {
                let i = i_.clone();
                if let Some(tok) = i.peek_next() {
                    if tok.typ == TokenType::END {
                        break;
                    }
                }
                match statement(i.clone()) {
                    Result::Abort(e) => {
                        let ctx_err = StackPrinter { err: e };
                        return Err(format!("{}", ctx_err));
                    }
                    Result::Fail(e) => {
                        let ctx_err = StackPrinter { err: e };
                        return Err(format!("{}", ctx_err));
                    }
                    Result::Incomplete(_ei) => {
                        let err = abortable_parser::Error::new(
                            "Unexpected end of parse input",
                            Box::new(i.clone()),
                        );
                        let ctx_err = StackPrinter { err: err };
                        return Err(format!("{}", ctx_err));
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
            return Err(e);
        }
    }
}

pub mod precedence;

#[cfg(test)]
mod test;
