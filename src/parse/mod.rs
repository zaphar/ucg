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

pub use crate::tokenizer::{CommentGroup, CommentMap};

type ParseResult<'a, O> = Result<SliceIter<'a, Token>, O>;

#[cfg(feature = "tracing")]
const ENABLE_TRACE: bool = true;
#[cfg(not(feature = "tracing"))]
const ENABLE_TRACE: bool = false;

type ConvertResult<'a, O> = std::result::Result<O, abortable_parser::Error<SliceIter<'a, Token>>>;

macro_rules! trace_parse {
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

fn symbol_to_value(s: &Token) -> ConvertResult<Value> {
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

fn str_to_value(s: &Token) -> ConvertResult<Value> {
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
) -> ConvertResult<'a, Value> {
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
            _ => must!(punct!("=")),
            value => must!(expression),
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
        _ => must!(punct!("}")),
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
        _ => optional!(punct!(",")),
        _ => must!(punct!("]")),
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
    either!(trace_parse!(list_value), trace_parse!(tuple))
);

make_fn!(
    value<SliceIter<Token>, Value>,
    either!(
        trace_parse!(symbol),
        trace_parse!(compound_value),
        trace_parse!(boolean_value),
        trace_parse!(empty_value),
        trace_parse!(number),
        trace_parse!(quoted_value)
    )
);

fn value_to_expression(v: Value) -> Expression {
    Expression::Simple(v)
}

make_fn!(
    simple_expression<SliceIter<Token>, Expression>,
    do_each!(
        val => trace_parse!(value),
        _ => not!(either!(punct!("{"), punct!("["), punct!("("))),
        (value_to_expression(val))
    )
);

fn expression_to_grouped_expression(e: Expression, pos: Position) -> Expression {
    Expression::Grouped(Box::new(e), pos)
}

make_fn!(
    grouped_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => punct!("("),
        expr => do_each!(
            expr => trace_parse!(expression),
            _ => must!(punct!(")")),
            (expr)
        ),
        (expression_to_grouped_expression(expr, pos))
    )
);

fn tuple_to_copy(sym: Value, fields: Option<FieldList>) -> Expression {
    let pos = sym.pos().clone();
    let fields = match fields {
        Some(fields) => fields,
        None => Vec::new(),
    };
    Expression::Copy(CopyDef {
        selector: sym,
        fields: fields,
        pos: pos,
    })
}

make_fn!(
    copy_expression<SliceIter<Token>, Expression>,
    do_each!(
        sym => trace_parse!(symbol),
        _ => punct!("{"),
        fields => optional!(trace_parse!(field_list)),
        _ => optional!(punct!(",")),
        _ => must!(punct!("}")),
        (tuple_to_copy(sym, fields))
    )
);

fn tuple_to_func<'a>(
    pos: Position,
    vals: Option<Vec<Value>>,
    val: Expression,
) -> ConvertResult<'a, Expression> {
    let mut default_args = match vals {
        None => Vec::new(),
        Some(vals) => vals,
    };
    let arglist = default_args
        .drain(0..)
        .map(|s| PositionedItem {
            pos: s.pos().clone(),
            val: s.to_string(),
        })
        .collect();
    Ok(Expression::Func(FuncDef {
        scope: None,
        argdefs: arglist,
        fields: Box::new(val),
        pos: pos,
    }))
}

make_fn!(
    arglist<SliceIter<Token>, Vec<Value>>,
    separated!(punct!(","), symbol)
);

fn module_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input,
        pos => pos,
        _ => word!("module"),
        _ => must!(punct!("{")),
        arglist => trace_parse!(optional!(field_list)),
        _ => optional!(punct!(",")),
        _ => must!(punct!("}")),
        _ => must!(punct!("=>")),
        out_expr => optional!(
            do_each!(
                _ => punct!("("),
                expr => must!(expression),
                _ => must!(punct!(")")),
                (expr)
            )
        ),
        _ => must!(punct!("{")),
        stmt_list =>  trace_parse!(repeat!(statement)),
        _ => must!(punct!("}")),
        (pos, arglist, out_expr, stmt_list)
    );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, (pos, arglist, out_expr, stmt_list)) => {
            let mut def = ModuleDef::new(arglist.unwrap_or_else(|| Vec::new()), stmt_list, pos);
            if let Some(expr) = out_expr {
                def.set_out_expr(expr);
            }
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

fn func_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input,
        pos => pos,
        _ => word!("func"),
        _ => must!(punct!("(")),
        arglist => trace_parse!(optional!(arglist)),
        _ => must!(punct!(")")),
        _ => must!(punct!("=>")),
        map =>  trace_parse!(expression),
        (pos, arglist, map)
    );
    match parsed {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(offset) => Result::Incomplete(offset),
        Result::Complete(rest, (pos, arglist, map)) => match tuple_to_func(pos, arglist, map) {
            Ok(expr) => Result::Complete(rest, expr),
            Err(e) => Result::Fail(Error::caused_by(
                "Invalid func syntax",
                Box::new(e),
                Box::new(rest.clone()),
            )),
        },
    }
}

fn tuple_to_select<'a>(
    input: SliceIter<'a, Token>,
    e1: Expression,
    e2: Option<Expression>,
    val: Value,
) -> ConvertResult<'a, Expression> {
    match val {
        Value::Tuple(v) => Ok(Expression::Select(SelectDef {
            val: Box::new(e1),
            default: e2.map(|e| Box::new(e)),
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
            expr => trace_parse!(must!(expression)),
            _ => must!(punct!(",")),
            (expr)
        ),
        default_and_map => either!(
            do_each!(
                default => do_each!(
                    expr => trace_parse!(expression),
                    _ => punct!(","),
                    (expr)
                ),
                map => trace_parse!(tuple),
                (Some(default), map)
            ),
            do_each!(
                map => trace_parse!(must!(tuple)),
                (None, map)
            )
        ),
        (val, default_and_map.0, default_and_map.1)
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

make_fn!(
    simple_format_args<SliceIter<Token>, FormatArgs>,
    do_each!(
        _ => punct!("("),
        args => separated!(punct!(","), trace_parse!(expression)),
        _ => must!(punct!(")")),
        (FormatArgs::List(args))
    )
);

make_fn!(
    expression_format_args<SliceIter<Token>, FormatArgs>,
    do_each!(
        expr => must!(expression),
        (FormatArgs::Single(Box::new(expr)))
    )
);

make_fn!(
    format_expression<SliceIter<Token>, Expression>,
    do_each!(
        tmpl => match_type!(STR),
        _ => punct!("%"),
        args => wrap_err!(must!(either!(simple_format_args, expression_format_args)),
            "Expected format arguments"),
        (Expression::Format(FormatDef {
            template: tmpl.fragment.to_string(),
            args: args,
            pos: tmpl.pos,
        }))
    )
);

make_fn!(
    include_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => word!("include"),
        typ => must!(match_type!(BAREWORD)),
        path => must!(match_type!(STR)),
        (Expression::Include(IncludeDef{
            pos: pos,
            typ: typ,
            path: path,
        }))
    )
);

fn tuple_to_call<'a>(
    input: SliceIter<'a, Token>,
    val: Value,
    exprs: Option<Vec<Expression>>,
) -> ConvertResult<'a, Expression> {
    if let Value::Symbol(_) = val {
        Ok(Expression::Call(CallDef {
            funcref: val,
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

fn call_expression(input: SliceIter<Token>) -> Result<SliceIter<Token>, Expression> {
    let parsed = do_each!(input.clone(),
        callee_name => trace_parse!(symbol),
        _ => punct!("("),
        args => optional!(separated!(punct!(","), trace_parse!(expression))),
        _ => optional!(punct!(",")),
        _ => must!(punct!(")")),
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

make_fn!(
    reduce_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => word!("reduce"),
        _ => must!(punct!("(")),
        func => must!(expression),
        _ => must!(punct!(",")),
        acc => must!(trace_parse!(expression)),
        _ => must!(punct!(",")),
        tgt => must!(trace_parse!(expression)),
        _ => optional!(punct!(",")),
        _ => must!(punct!(")")),
        (Expression::FuncOp(FuncOpDef::Reduce(ReduceOpDef{
            func: Box::new(func),
            acc: Box::new(acc),
            target: Box::new(tgt),
            pos: pos,
        })))
    )
);

make_fn!(
    map_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => word!("map"),
        _ => must!(punct!("(")),
        func => must!(expression),
        _ => must!(punct!(",")),
        list => must!(trace_parse!(expression)),
        _ => optional!(punct!(",")),
        _ => must!(punct!(")")),
        (Expression::FuncOp(FuncOpDef::Map(MapFilterOpDef{
            func: Box::new(func),
            target: Box::new(list),
            pos: pos,
        })))
    )
);

make_fn!(
    filter_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => word!("filter"),
        _ => must!(punct!("(")),
        func => must!(expression),
        _ => must!(punct!(",")),
        list => must!(trace_parse!(expression)),
        _ => optional!(punct!(",")),
        _ => must!(punct!(")")),
        (Expression::FuncOp(FuncOpDef::Filter(MapFilterOpDef{
            func: Box::new(func),
            target: Box::new(list),
            pos: pos,
        })))
    )
);

make_fn!(
    func_op_expression<SliceIter<Token>, Expression>,
    either!(reduce_expression, map_expression, filter_expression)
);

make_fn!(
    range_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        start => either!(simple_expression, grouped_expression),
        _ => punct!(":"),
        maybe_step => optional!(
            do_each!(
                step => either!(simple_expression, grouped_expression),
                _ => punct!(":"),
                (Box::new(step))
            )
        ),
        end => must!(wrap_err!(either!(simple_expression, grouped_expression), "Expected simple or grouped expression")),
        (Expression::Range(RangeDef{
            pos: pos,
            start: Box::new(start),
            step: maybe_step,
            end: Box::new(end),
        }))
    )
);

make_fn!(
    import_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => word!("import"),
        path => must!(wrap_err!(match_type!(STR), "Expected import path")),
        (Expression::Import(ImportDef{
            pos: pos,
            path: path,
        }))
    )
);

make_fn!(
    string_expression<SliceIter<Token>, Expression>,
    do_each!(
        val => trace_parse!(quoted_value),
        (value_to_expression(val))
    )
);

make_fn!(
    fail_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => word!("fail"),
        msg => must!(wrap_err!(expression, "Expected failure message")),
        (Expression::Fail(FailDef{
            pos: pos,
            message: Box::new(msg),
        }))
    )
);

make_fn!(
    trace_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => word!("TRACE"),
        expr => must!(wrap_err!(expression, "Expected failure message")),
        (Expression::Debug(DebugDef{
            pos: pos,
            expr: Box::new(expr),
        }))
    )
);

make_fn!(
    not_expression<SliceIter<Token>, Expression>,
    do_each!(
        pos => pos,
        _ => word!("not"),
        expr => must!(wrap_err!(expression, "Expected failure message")),
        (Expression::Not(NotDef{
            pos: pos,
            expr: Box::new(expr),
        }))
    )
);

fn unprefixed_expression(input: SliceIter<Token>) -> ParseResult<Expression> {
    let _input = input.clone();
    either!(
        input,
        trace_parse!(format_expression),
        trace_parse!(range_expression),
        trace_parse!(simple_expression),
        trace_parse!(call_expression),
        trace_parse!(copy_expression)
    )
}

make_fn!(
    non_op_expression<SliceIter<Token>, Expression>,
    either!(
        trace_parse!(func_op_expression),
        trace_parse!(func_expression),
        trace_parse!(import_expression),
        trace_parse!(trace_expression),
        trace_parse!(not_expression),
        trace_parse!(fail_expression),
        trace_parse!(module_expression),
        trace_parse!(select_expression),
        trace_parse!(grouped_expression),
        trace_parse!(include_expression),
        trace_parse!(unprefixed_expression)
    )
);

fn expression(input: SliceIter<Token>) -> ParseResult<Expression> {
    let _input = input.clone();
    match trace_parse!(_input, op_expression) {
        Result::Incomplete(i) => Result::Incomplete(i),
        Result::Fail(_) => trace_parse!(input, non_op_expression),
        Result::Abort(e) => Result::Abort(e),
        Result::Complete(rest, expr) => Result::Complete(rest, expr),
    }
}

make_fn!(
    expression_statement<SliceIter<Token>, Statement>,
    do_each!(
        e => do_each!(
            expr => trace_parse!(expression),
            _ => must!(punct!(";")),
            (expr)
        ),
        (Statement::Expression(e))
    )
);

make_fn!(
    let_stmt_body<SliceIter<Token>, Statement>,
    do_each!(
        pos => pos,
        name => wrap_err!(match_type!(BAREWORD), "Expected name for binding"),
        _ => punct!("="),
        val => trace_parse!(wrap_err!(expression, "Expected Expression to bind")),
        _ => punct!(";"),
        (Statement::Let(LetDef {
            pos: pos,
            name: name,
            value: val,
        }))
    )
);

make_fn!(
    let_statement<SliceIter<Token>, Statement>,
    do_each!(
        _ => word!("let"),
        stmt => trace_parse!(must!(let_stmt_body)),
        (stmt)
    )
);

make_fn!(
    assert_statement<SliceIter<Token>, Statement>,
    do_each!(
        pos => pos,
        _ => word!("assert"),
        expr => wrap_err!(must!(expression), "Expected Tuple {ok=<bool>, desc=<str>}"),
        _ => must!(punct!(";")),
        (Statement::Assert(pos, expr))
    )
);

make_fn!(
    out_statement<SliceIter<Token>, Statement>,
    do_each!(
        pos => pos,
        _ => word!("out"),
        typ => wrap_err!(must!(match_type!(BAREWORD)), "Expected converter name"),
        expr => wrap_err!(must!(expression), "Expected Expression to export"),
        _ => must!(punct!(";")),
        (Statement::Output(pos, typ.clone(), expr.clone()))
    )
);

//trace_macros!(true);
fn statement(i: SliceIter<Token>) -> Result<SliceIter<Token>, Statement> {
    return either!(
        i,
        trace_parse!(assert_statement),
        trace_parse!(let_statement),
        trace_parse!(out_statement),
        trace_parse!(expression_statement)
    );
}
//trace_macros!(false);

/// Parses a LocatedSpan into a list of Statements or an `error::Error`.
pub fn parse<'a>(
    input: OffsetStrIter<'a>,
    comment_map: Option<&mut CommentMap>,
) -> std::result::Result<Vec<Statement>, String> {
    match tokenize(input.clone(), comment_map) {
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
