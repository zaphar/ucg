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

//! Bottom up parser for precedence parsing of expressions separated by binary
//! operators.
use abortable_parser::combinators::eoi;
use abortable_parser::{Error, Peekable, Result, SliceIter};

use super::{non_op_expression, ParseResult};
use crate::ast::*;

macro_rules! abort_on_end {
    ($i:expr) => {{
        if eoi($i.clone()).is_complete() {
            return Result::Fail(Error::new(
                format!("Expected Expression found End Of Input"),
                Box::new($i.clone()),
            ));
        }
    }};
}

/// Defines the intermediate stages of our bottom up parser for precedence parsing.
#[derive(Debug, PartialEq, Clone)]
pub enum Element {
    Expr(Expression),
    Op(BinaryExprType),
}

make_fn!(
    dot_op_type<SliceIter<Token>, Element>,
    do_each!(
        _ => punct!("."),
        (Element::Op(BinaryExprType::DOT)))
);

make_fn!(
    math_op_type<SliceIter<Token>, Element>,
    either!(
        do_each!(
            _ => punct!("+"),
            (Element::Op(BinaryExprType::Add))),
        do_each!(
            _ => punct!("-"),
            (Element::Op(BinaryExprType::Sub))),
        do_each!(
            _ => punct!("*"),
            (Element::Op(BinaryExprType::Mul))),
        do_each!(
            _ => punct!("/"),
            (Element::Op(BinaryExprType::Div))),
        do_each!(
            _ => punct!("%%"),
            (Element::Op(BinaryExprType::Mod)))
    )
);

make_fn!(
    bool_op_type<SliceIter<Token>, Element>,
    either!(
        do_each!(
            _ => punct!("&&"),
            (Element::Op(BinaryExprType::AND))),
        do_each!(
            _ => punct!("||"),
            (Element::Op(BinaryExprType::OR)))
    )
);

fn parse_expression(i: SliceIter<Element>) -> Result<SliceIter<Element>, Expression> {
    let mut i_ = i.clone();
    abort_on_end!(i_);
    let el = i_.next();
    if let Some(&Element::Expr(ref expr)) = el {
        return Result::Complete(i_, expr.clone());
    }
    return Result::Fail(Error::new(
        format!(
            "Error while parsing Binary Expression Expected Expression got {:?}",
            el
        ),
        Box::new(i_),
    ));
}

fn parse_bool_operator(i: SliceIter<Element>) -> Result<SliceIter<Element>, BinaryExprType> {
    let mut i_ = i.clone();
    abort_on_end!(i_);
    let el = i_.next();
    if let Some(&Element::Op(ref op)) = el {
        match op {
            BinaryExprType::AND | BinaryExprType::OR => {
                return Result::Complete(i_, op.clone());
            }
            _other => {
                // noop
            }
        };
    }
    return Result::Fail(Error::new(
        format!(
            "Error while parsing Binary Expression Unexpected Operator {:?}",
            el
        ),
        Box::new(i_),
    ));
}

fn parse_dot_operator(i: SliceIter<Element>) -> Result<SliceIter<Element>, BinaryExprType> {
    let mut i_ = i.clone();
    abort_on_end!(i_);
    let el = i_.next();
    if let Some(&Element::Op(ref op)) = el {
        match op {
            &BinaryExprType::DOT => {
                return Result::Complete(i_, op.clone());
            }
            _other => {
                // noop
            }
        };
    }
    return Result::Fail(Error::new(
        format!(
            "Error while parsing Binary Expression Unexpected Operator {:?}",
            el
        ),
        Box::new(i_),
    ));
}

fn parse_sum_operator(i: SliceIter<Element>) -> Result<SliceIter<Element>, BinaryExprType> {
    let mut i_ = i.clone();
    abort_on_end!(i_);
    let el = i_.next();
    if let Some(&Element::Op(ref op)) = el {
        match op {
            &BinaryExprType::Add => {
                return Result::Complete(i_, op.clone());
            }
            &BinaryExprType::Sub => {
                return Result::Complete(i_, op.clone());
            }
            _other => {
                // noop
            }
        };
    }
    return Result::Fail(Error::new(
        format!(
            "Error while parsing Binary Expression Unexpected Operator {:?}",
            el
        ),
        Box::new(i_),
    ));
}

fn parse_product_operator(i: SliceIter<Element>) -> Result<SliceIter<Element>, BinaryExprType> {
    let mut i_ = i.clone();
    abort_on_end!(i_);
    let el = i_.next();
    if let Some(&Element::Op(ref op)) = el {
        match op {
            &BinaryExprType::Mul => {
                return Result::Complete(i_, op.clone());
            }
            &BinaryExprType::Div => {
                return Result::Complete(i_, op.clone());
            }
            &BinaryExprType::Mod => {
                return Result::Complete(i_, op.clone());
            }
            _other => {
                // noop
            }
        };
    }
    return Result::Fail(Error::new(
        format!(
            "Error while parsing Binary Expression Unexpected Operator {:?}",
            el
        ),
        Box::new(i_),
    ));
}

make_fn!(
    compare_op_type<SliceIter<Token>, Element>,
    either!(
        do_each!(_ => punct!("=="), (Element::Op(BinaryExprType::Equal))),
        do_each!(_ => punct!("!="), (Element::Op(BinaryExprType::NotEqual))),
        do_each!(_ => punct!("~"), (Element::Op(BinaryExprType::REMatch))),
        do_each!(_ => punct!("!~"), (Element::Op(BinaryExprType::NotREMatch))),
        do_each!(_ => punct!("<="), (Element::Op(BinaryExprType::LTEqual))),
        do_each!(_ => punct!(">="), (Element::Op(BinaryExprType::GTEqual))),
        do_each!(_ => punct!("<"),  (Element::Op(BinaryExprType::LT))),
        do_each!(_ => punct!(">"),  (Element::Op(BinaryExprType::GT))),
        do_each!(_ => word!("in"),  (Element::Op(BinaryExprType::IN))),
        do_each!(_ => word!("is"),  (Element::Op(BinaryExprType::IS)))
    )
);

fn parse_compare_operator(i: SliceIter<Element>) -> Result<SliceIter<Element>, BinaryExprType> {
    let mut i_ = i.clone();
    abort_on_end!(i_);
    let el = i_.next();
    if let Some(&Element::Op(ref op)) = el {
        match op {
            &BinaryExprType::GT
            | &BinaryExprType::GTEqual
            | &BinaryExprType::LT
            | &BinaryExprType::LTEqual
            | &BinaryExprType::NotEqual
            | &BinaryExprType::REMatch
            | &BinaryExprType::NotREMatch
            | &BinaryExprType::Equal
            | &BinaryExprType::IS
            | &BinaryExprType::IN => {
                return Result::Complete(i_, op.clone());
            }
            _other => {
                // noop
            }
        };
    }
    return Result::Fail(Error::new(
        format!(
            "Error while parsing Binary Expression Unexpected Operator {:?}",
            el
        ),
        Box::new(i),
    ));
}

/// Parse a list of expressions separated by operators into a Vec<Element>.
fn parse_operand_list<'a>(i: SliceIter<'a, Token>) -> ParseResult<'a, Vec<Element>> {
    // 1. First try to parse a non_op_expression,
    let mut _i = i.clone();
    let mut list = Vec::new();
    // 1. loop
    let mut firstrun = true;
    loop {
        // 2. Parse a non_op_expression.
        match non_op_expression(_i.clone()) {
            Result::Fail(e) => {
                // A failure to parse an expression
                // is always an error.
                if !firstrun {
                    // if we have successfully parsed an element and an operator then
                    // failing to parse a second expression is an abort since we know
                    // for sure now that the next expression is supposed to be there.
                    let err = Error::new("Missing operand for binary expression", Box::new(_i));
                    return Result::Abort(err);
                }
                return Result::Fail(e);
            }
            Result::Abort(e) => {
                // A failure to parse an expression
                // is always an error.
                return Result::Abort(e);
            }
            Result::Incomplete(i) => {
                return Result::Incomplete(i);
            }
            Result::Complete(rest, expr) => {
                list.push(Element::Expr(expr));
                _i = rest.clone();
            }
        }
        // 3. Parse an operator.
        match either!(
            _i.clone(),
            dot_op_type,
            math_op_type,
            compare_op_type,
            bool_op_type
        ) {
            Result::Fail(e) => {
                if firstrun {
                    // If we don't find an operator in our first
                    // run then this is not an operand list.
                    return Result::Fail(e);
                }
                // if we don't find one on subsequent runs then
                // that's the end of the operand list.
                break;
            }
            Result::Abort(e) => {
                // A failure to parse an operator
                // is always an error.
                return Result::Abort(e);
            }
            Result::Incomplete(i) => {
                return Result::Incomplete(i);
            }
            Result::Complete(rest, el) => {
                list.push(el);
                _i = rest.clone();
            }
        }
        firstrun = false;
    }
    return Result::Complete(_i, list);
}

make_fn!(
    parse_operator_element<SliceIter<Element>, BinaryExprType>,
    either!(
        parse_dot_operator,
        parse_sum_operator,
        parse_product_operator,
        parse_compare_operator,
        parse_bool_operator
    )
);

macro_rules! try_parse {
    ($r:expr) => {
        match $r {
            Result::Abort(e) => return Result::Abort(e),
            Result::Fail(e) => return Result::Fail(e),
            Result::Incomplete(i) => return Result::Incomplete(i),
            Result::Complete(rest, op_type) => (rest, op_type),
        }
    };
}

fn parse_op(
    mut lhs: Expression,
    mut i: SliceIter<Element>,
    min_precedence: u32,
) -> Result<SliceIter<Element>, Expression> {
    // Termination condition
    if eoi(i.clone()).is_complete() {
        return Result::Complete(i, lhs);
    }
    let (_, mut lookahead_op) = try_parse!(parse_operator_element(i.clone()));
    while !eoi(i.clone()).is_complete() && (lookahead_op.precedence_level() >= min_precedence) {
        // Stash a copy of our lookahead operator for future use.
        let op = lookahead_op.clone();
        // Advance to next element.
        i.next();
        let (rest, mut rhs) = try_parse!(parse_expression(i.clone()));
        i = rest;
        if !eoi(i.clone()).is_complete() {
            let (_, peek_op) = try_parse!(parse_operator_element(i.clone()));
            lookahead_op = peek_op;
        }
        while !eoi(i.clone()).is_complete()
            && (lookahead_op.precedence_level() > op.precedence_level())
        {
            let (rest, inner_rhs) =
                try_parse!(parse_op(rhs, i.clone(), lookahead_op.precedence_level()));
            i = rest;
            rhs = inner_rhs;
            // Before we check for another operator we should see
            // if we are at the end of the input.
            if eoi(i.clone()).is_complete() {
                break;
            }
            let (_, peek_op) = try_parse!(parse_operator_element(i.clone()));
            lookahead_op = peek_op;
        }
        let pos = lhs.pos().clone();
        lhs = Expression::Binary(BinaryOpDef {
            kind: op.clone(),
            left: Box::new(lhs.clone()),
            right: Box::new(rhs),
            pos: pos,
        });
    }
    return Result::Complete(i, lhs);
}

pub fn parse_precedence(i: SliceIter<Element>) -> Result<SliceIter<Element>, Expression> {
    match parse_expression(i) {
        Result::Abort(e) => Result::Abort(e),
        Result::Fail(e) => Result::Fail(e),
        Result::Incomplete(i) => Result::Incomplete(i),
        Result::Complete(rest, expr) => parse_op(expr, rest, 0),
    }
}

/// Parse a binary operator expression.
pub fn op_expression<'a>(i: SliceIter<'a, Token>) -> Result<SliceIter<Token>, Expression> {
    let preparse = parse_operand_list(i.clone());
    match preparse {
        Result::Fail(e) => Result::Fail(e),
        Result::Abort(e) => Result::Abort(e),
        Result::Incomplete(i) => Result::Incomplete(i),
        Result::Complete(rest, oplist) => {
            let i_ = SliceIter::new(&oplist);
            let parse_result = parse_precedence(i_);
            match parse_result {
                Result::Fail(e) => {
                    let err = Error::new(e.get_msg(), Box::new(rest.clone()));
                    Result::Fail(err)
                }
                Result::Abort(e) => {
                    let err = Error::new(e.get_msg(), Box::new(rest.clone()));
                    Result::Abort(err)
                }
                Result::Incomplete(_) => Result::Incomplete(i.clone()),
                Result::Complete(rest_ops, expr) => {
                    if rest_ops.peek_next().is_some() {
                        panic!("premature abort parsing Operator expression!");
                    }
                    Result::Complete(rest.clone(), expr)
                }
            }
        }
    }
}
