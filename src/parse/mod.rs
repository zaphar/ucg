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

use ast::*;
use error;
use tokenizer::*;

type NomResult<'a, O> = nom::IResult<TokenIter<'a>, O, error::Error>;

type ParseResult<O> = Result<O, error::Error>;

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
    Ok(Value::String(value_node!(
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
    // TODO(jwall): if there is an error we should report where that error occured.
    let f = match FromStr::from_str(&to_parse) {
        Ok(f) => f,
        Err(_) => {
            return Err(error::Error::new(
                format!("Not a float! {}", to_parse),
                error::ErrorType::UnexpectedToken,
                maybepos.unwrap(),
            ))
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
// The peek!(not!(..)) make this whole combinator slightly
// safer for partial inputs.
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
            field: match_type!(BAREWORD) >>
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
    #[doc="Capture a tuple of named fields with values. {<field>=<value>,...}"],
    tuple<TokenIter, Value, error::Error>,
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

named!(list_value<TokenIter, Value, error::Error>,
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

named!(empty_value<TokenIter, Value, error::Error>,
    do_parse!(
        pos: pos >>
        match_type!(EMPTY) >>
        (Value::Empty(pos))
    )
);

named!(compound_value<TokenIter, Value, error::Error>,
    alt!(list_value | tuple)
);

named!(scalar_value<TokenIter, Value, error::Error>,
    alt!(
        boolean_value |
        empty_value |
        number |
        quoted_value
    )
);

named!(value<TokenIter, Value, error::Error>,
    alt!(
        selector_value
        | compound_value
        | scalar_value
    )
 );

fn value_to_expression(v: Value) -> ParseResult<Expression> {
    Ok(Expression::Simple(v))
}

named!(simple_expression<TokenIter, Expression, error::Error>,
       map_res!(
           value,
           value_to_expression
       )
);

fn tuple_to_binary_expression(
    tpl: (Position, BinaryExprType, Expression, Expression),
) -> ParseResult<Expression> {
    Ok(Expression::Binary(BinaryOpDef {
        kind: tpl.1,
        left: Box::new(tpl.2),
        right: Box::new(tpl.3),
        pos: Position::new(tpl.0.line as usize, tpl.0.column as usize),
    }))
}

/// do_binary_expr implements precedence based parsing where the more tightly bound parsers
/// are passed in as lowerrule parsers. We default to grouped_expression and simple_expression as
/// the most tightly bound expressions.
macro_rules! do_binary_expr {
    ($i:expr, $oprule:ident!( $($args:tt)* ), $typ:expr) => {
        do_binary_expr!($i, $oprule!($($args)*), $typ, non_op_expression)
    };

    ($i:expr, $oprule:ident!( $($args:tt)* ), $typ:expr, $lowerrule:ident) => {
        do_binary_expr!($i, $oprule!($($args)*), $typ, call!($lowerrule))
    };

    ($i:expr, $oprule:ident!( $($args:tt)* ), $typ:expr, $lowerrule:ident!( $($lowerargs:tt)* )) => {
        map_res!($i,
            do_parse!(
                pos: pos >>
                left: $lowerrule!($($lowerargs)*) >>
                    $oprule!($($args)*) >>
                    right: $lowerrule!($($lowerargs)*) >>
                    (pos, $typ, left, right)
            ),
            tuple_to_binary_expression
        )
    };
}

// trace_macros!(true);
named!(add_expression<TokenIter, Expression, error::Error>,
       do_binary_expr!(punct!("+"), BinaryExprType::Add, alt!(product_expression | simple_expression | grouped_expression))
);
// trace_macros!(false);

named!(sub_expression<TokenIter, Expression, error::Error>,
       do_binary_expr!(punct!("-"), BinaryExprType::Sub,  alt!(product_expression | simple_expression | grouped_expression))
);

named!(sum_expression<TokenIter, Expression, error::Error>,
    alt!(add_expression | sub_expression)
);

named!(mul_expression<TokenIter, Expression, error::Error>,
       do_binary_expr!(punct!("*"), BinaryExprType::Mul)
);

named!(div_expression<TokenIter, Expression, error::Error>,
       do_binary_expr!(punct!("/"), BinaryExprType::Div)
);

named!(product_expression<TokenIter, Expression, error::Error>,
    alt!(mul_expression | div_expression)
);

named!(math_expression<TokenIter, Expression, error::Error>,
    alt!(sum_expression | product_expression)
);

// TODO(jwall): Change comparison operators to use the do_binary_expr! with precedence?
fn tuple_to_compare_expression(
    tpl: (Position, CompareType, Expression, Expression),
) -> ParseResult<Expression> {
    Ok(Expression::Compare(ComparisonDef {
        kind: tpl.1,
        left: Box::new(tpl.2),
        right: Box::new(tpl.3),
        pos: Position::new(tpl.0.line as usize, tpl.0.column as usize),
    }))
}

macro_rules! do_compare_expr {
    ($i:expr, $subrule:ident!( $($args:tt)* ), $typ:expr) => {
        map_res!($i,
            do_parse!(
                pos: pos >>
                left: alt!(simple_expression | grouped_expression | math_expression) >>
                    $subrule!($($args)*) >>
                    right: expression >>
                    (pos, $typ, left, right)
            ),
            tuple_to_compare_expression
        )
    };
}

named!(eqeq_expression<TokenIter, Expression, error::Error>,
       do_compare_expr!(punct!("=="), CompareType::Equal)
);

named!(not_eqeq_expression<TokenIter, Expression, error::Error>,
       do_compare_expr!(punct!("!="), CompareType::NotEqual)
);

named!(lt_eqeq_expression<TokenIter, Expression, error::Error>,
       do_compare_expr!(punct!("<="), CompareType::LTEqual)
);

named!(gt_eqeq_expression<TokenIter, Expression, error::Error>,
       do_compare_expr!(punct!(">="), CompareType::GTEqual)
);

named!(gt_expression<TokenIter, Expression, error::Error>,
       do_compare_expr!(punct!(">"), CompareType::GT)
);

named!(lt_expression<TokenIter, Expression, error::Error>,
       do_compare_expr!(punct!("<"), CompareType::LT)
);

named!(compare_expression<TokenIter, Expression, error::Error>,
    alt!(
        eqeq_expression |
        not_eqeq_expression |
        lt_eqeq_expression |
        gt_eqeq_expression |
        lt_expression |
        gt_expression)
);

named!(op_expression<TokenIter, Expression, error::Error>,
    alt!(math_expression | compare_expression)
);

fn expression_to_grouped_expression(e: Expression) -> ParseResult<Expression> {
    Ok(Expression::Grouped(Box::new(e)))
}

named!(grouped_expression<TokenIter, Expression, error::Error>,
       map_res!(
           preceded!(punct!("("), terminated!(expression, punct!(")"))),
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
            alt!(match_type!(BAREWORD) | match_type!(DIGIT))
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
        Value::Tuple(v) => Ok(Expression::Macro(MacroDef {
            argdefs: t.1
                .drain(0..)
                .map(|s| Positioned {
                    pos: s.pos().clone(),
                    val: s.to_string(),
                })
                .collect(),
            fields: v.val,
            pos: t.0,
        })),
        // TODO(jwall): Show a better version of the unexpected parsed value.
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

named!(format_expression<TokenIter, Expression, error::Error>,
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
               sl: selector_list >>
               (sl.head.pos().clone(), sl)
           ),
           vec_to_selector_value
       )
);

named!(call_expression<TokenIter, Expression, error::Error>,
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

fn symbol_or_list(input: TokenIter) -> NomResult<Value> {
    let sym = do_parse!(input, sym: symbol >> (sym));

    match sym {
        IResult::Incomplete(i) => {
            return IResult::Incomplete(i);
        }
        IResult::Error(_) => {
            // TODO(jwall): Still missing some. But we need to avoid recursion
            match list_value(input) {
                IResult::Incomplete(i) => {
                    return IResult::Incomplete(i);
                }
                IResult::Error(e) => {
                    return IResult::Error(e);
                }
                IResult::Done(i, val) => {
                    return IResult::Done(i, val);
                }
            }
        }
        IResult::Done(rest, val) => {
            return IResult::Done(rest, val);
        }
    }
}

fn tuple_to_list_op(tpl: (Position, Token, Value, Value)) -> ParseResult<Expression> {
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
                return Err(error::Error::new(
                    format!("Missing a result field for the macro"),
                    error::ErrorType::IncompleteParsing,
                    pos,
                ));
            }
            &mut Some(ref mut tl) => {
                if tl.len() < 1 {
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
        if let Value::List(ldef) = list {
            return Ok(Expression::ListOp(ListOpDef {
                typ: t,
                mac: def,
                field: fieldname,
                target: ldef,
                pos: pos,
            }));
        }
        // TODO(jwall): We should print a pretter message than debug formatting here.
        return Err(error::Error::new(
            format!("Expected a list but got {:?}", list),
            error::ErrorType::UnexpectedToken,
            pos,
        ));
    }
    return Err(error::Error::new(
        format!("Expected a selector but got {:?}", macroname),
        error::ErrorType::UnexpectedToken,
        pos,
    ));
}

named!(list_op_expression<TokenIter, Expression, error::Error>,
    map_res!(
        do_parse!(
            pos: pos >>
            optype: alt!(word!("map") | word!("filter")) >>
            macroname: selector_value >>
            list: symbol_or_list >>
            (pos, optype, macroname, list)
        ),
        tuple_to_list_op
    )
);

named!(non_op_expression<TokenIter, Expression, error::Error>,
    alt!(list_op_expression |
         macro_expression |
         format_expression |
         select_expression |
         call_expression |
         copy_expression |
         grouped_expression |
         simple_expression)
);

// NOTE(jwall): HERE THERE BE DRAGONS. The order for these matters
// a lot. We need to process alternatives in order of decreasing
// specificity.  Unfortunately this means we are required to go in a
// decreasing size order which messes with alt!'s completion logic. To
// work around this we have to force Incomplete to be Error so that
// alt! will try the next in the series instead of aborting.
//
// *IMPORTANT*
// It also means this combinator is risky when used with partial
// inputs. So handle with care.
named!(expression<TokenIter, Expression, error::Error>,
    alt!(complete!(op_expression) | complete!(non_op_expression))
);

fn expression_to_statement(v: Expression) -> ParseResult<Statement> {
    Ok(Statement::Expression(v))
}

named!(expression_statement<TokenIter, Statement, error::Error>,
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

named!(let_stmt_body<TokenIter, Statement, error::Error>,
    map_res!(
        do_parse!(
            name: match_type!(BAREWORD) >>
            punct!("=") >>
            val: expression >>
            punct!(";") >>
            (name, val)),
        tuple_to_let
    )
);

named!(let_statement<TokenIter, Statement, error::Error>,
    do_parse!(
        word!("let") >>
        pos: pos >>
        stmt: add_return_error!(
            nom::ErrorKind::Custom(
                error::Error::new(
                    "Invalid syntax for let binding",
                    error::ErrorType::ParseError, pos)),
            let_stmt_body) >>
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
        stmt: add_return_error!(
            nom::ErrorKind::Custom(
                error::Error::new(
                    "Invalid syntax for import",
                    error::ErrorType::ParseError, pos)),
            import_stmt_body) >>
        (stmt)
    )
);

named!(statement<TokenIter, Statement, error::Error>,
    do_parse!(
       stmt: alt_complete!(
           import_statement |
           let_statement |
           expression_statement
       ) >>
       (stmt)
    )
);

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
                            format!("Statement Parse error: {:?} current token: {:?}", e, i_[0]),
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

#[cfg(test)]
mod test;
