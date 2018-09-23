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
use abortable_parser::{Error, Result, SliceIter};

use super::{non_op_expression, NomResult};
use ast::*;

/// Defines the intermediate stages of our bottom up parser for precedence parsing.
#[derive(Debug, PartialEq, Clone)]
pub enum Element {
    Expr(Expression),
    MathOp(BinaryExprType),
    CompareOp(CompareType),
}

make_fn!(
    math_op_type<SliceIter<Token>, Element>,
    either!(
        do_each!(
            _ => punct!("+"),
            (Element::MathOp(BinaryExprType::Add))),
        do_each!(
            _ => punct!("-"),
            (Element::MathOp(BinaryExprType::Sub))),
        do_each!(
            _ => punct!("*"),
            (Element::MathOp(BinaryExprType::Mul))),
        do_each!(
            _ => punct!("/"),
            (Element::MathOp(BinaryExprType::Div)))
    )
);

fn parse_expression(i: SliceIter<Element>) -> Result<SliceIter<Element>, Expression> {
    let mut i_ = i.clone();
    if eoi(i_.clone()).is_complete() {
        return Result::Abort(Error::new("Expected Expression found End Of Input", &i_));
    }
    let el = i_.next();
    if let Some(&Element::Expr(ref expr)) = el {
        return Result::Complete(i_.clone(), expr.clone());
    }
    return Result::Fail(Error::new(
        format!(
            "Error while parsing Binary Expression Expected Expression got {:?}",
            el
        ),
        &i_,
    ));
}

fn parse_sum_operator(i: SliceIter<Element>) -> Result<SliceIter<Element>, BinaryExprType> {
    let mut i_ = i.clone();
    if eoi(i_.clone()).is_complete() {
        return Result::Fail(Error::new(
            format!("Expected Expression found End Of Input"),
            &i_,
        ));
    }
    let el = i_.next();
    if let Some(&Element::MathOp(ref op)) = el {
        match op {
            &BinaryExprType::Add => {
                return Result::Complete(i_.clone(), op.clone());
            }
            &BinaryExprType::Sub => {
                return Result::Complete(i_.clone(), op.clone());
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
        &i_,
    ));
}

fn tuple_to_binary_expression(
    kind: BinaryExprType,
    left: Expression,
    right: Expression,
) -> Expression {
    let pos = left.pos().clone();
    Expression::Binary(BinaryOpDef {
        kind: kind,
        left: Box::new(left),
        right: Box::new(right),
        pos: pos,
    })
}

fn parse_product_operator(i: SliceIter<Element>) -> Result<SliceIter<Element>, BinaryExprType> {
    let mut i_ = i.clone();
    if eoi(i_.clone()).is_complete() {
        return Result::Fail(Error::new(
            format!("Expected Expression found End Of Input"),
            &i_,
        ));
    }
    let el = i_.next();
    if let Some(&Element::MathOp(ref op)) = el {
        match op {
            &BinaryExprType::Mul => {
                return Result::Complete(i_.clone(), op.clone());
            }
            &BinaryExprType::Div => {
                return Result::Complete(i_.clone(), op.clone());
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
        &i_,
    ));
}

/// do_binary_expr implements precedence based parsing where the more tightly bound
/// parsers are passed in as lowerrule parsers. We default to any non_op_expression
/// as the most tightly bound expressions.
macro_rules! do_binary_expr {
    ($i:expr, $oprule:ident, $lowerrule:ident) => {
        do_binary_expr!($i, run!($oprule), $lowerrule)
    };

    ($i:expr, $oprule:ident, $lowerrule:ident!( $($lowerargs:tt)* )) => {
        do_binary_expr!($i, run!($oprule), $lowerrule!($($lowerargs)*))
    };

    ($i:expr, $oprule:ident) => {
        do_binary_expr!($i, run!($oprule))
    };

    ($i:expr, $oprule:ident!( $($args:tt)* )) => {
        do_binary_expr!($i, $oprule!($($args)*), parse_expression)
    };

    ($i:expr, $oprule:ident!( $($args:tt)* ), $lowerrule:ident) => {
        do_binary_expr!($i, $oprule!($($args)*), run!($lowerrule))
    };

    ($i:expr, $oprule:ident!( $($args:tt)* ), $lowerrule:ident!( $($lowerargs:tt)* )) => {
        do_each!($i,
            left => $lowerrule!($($lowerargs)*),
                typ => $oprule!($($args)*),
                right => $lowerrule!($($lowerargs)*),
                (tuple_to_binary_expression(typ, left, right))
        )
    };
}

make_fn!(
    sum_expression<SliceIter<Element>, Expression>,
    do_binary_expr!(
        parse_sum_operator,
        either!(trace_nom!(product_expression), trace_nom!(parse_expression))
    )
);

make_fn!(
    product_expression<SliceIter<Element>, Expression>,
    do_binary_expr!(parse_product_operator, trace_nom!(parse_expression))
);

make_fn!(
    math_expression<SliceIter<Element>, Expression>,
    either!(trace_nom!(sum_expression), trace_nom!(product_expression))
);

// TODO(jwall): Change comparison operators to use the do_binary_expr! with precedence?
fn tuple_to_compare_expression(
    kind: CompareType,
    left: Expression,
    right: Expression,
) -> Expression {
    let pos = left.pos().clone();
    Expression::Compare(ComparisonDef {
        kind: kind,
        left: Box::new(left),
        right: Box::new(right),
        pos: pos,
    })
}

make_fn!(
    compare_op_type<SliceIter<Token>, Element>,
    either!(
        do_each!(_ => punct!("=="), (Element::CompareOp(CompareType::Equal))),
        do_each!(_ => punct!("!="), (Element::CompareOp(CompareType::NotEqual))),
        do_each!(_ => punct!("<="), (Element::CompareOp(CompareType::LTEqual))),
        do_each!(_ => punct!(">="), (Element::CompareOp(CompareType::GTEqual))),
        do_each!(_ => punct!("<"),  (Element::CompareOp(CompareType::LT))),
        do_each!(_ => punct!(">"),  (Element::CompareOp(CompareType::GT)))
    )
);

fn parse_compare_operator(i: SliceIter<Element>) -> Result<SliceIter<Element>, CompareType> {
    let mut i_ = i.clone();
    if eoi(i_.clone()).is_complete() {
        return Result::Fail(Error::new(
            format!("Expected Expression found End Of Input"),
            &i_,
        ));
    }
    let el = i_.next();
    if let Some(&Element::CompareOp(ref op)) = el {
        return Result::Complete(i_.clone(), op.clone());
    }
    return Result::Fail(Error::new(
        format!(
            "Error while parsing Binary Expression Unexpected Operator {:?}",
            el
        ),
        &i,
    ));
}

make_fn!(
    compare_expression<SliceIter<Element>, Expression>,
    do_each!(
        left => either!(trace_nom!(math_expression), trace_nom!(parse_expression)),
        typ => parse_compare_operator,
        right => either!(trace_nom!(math_expression), trace_nom!(parse_expression)),
        (tuple_to_compare_expression(typ, left, right))
    )
);

/// Parse a list of expressions separated by operators into a Vec<Element>.
fn parse_operand_list<'a>(i: SliceIter<'a, Token>) -> NomResult<'a, Vec<Element>> {
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
        match either!(_i.clone(), math_op_type, compare_op_type) {
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
                // A failure to parse an expression
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

/// Parse a binary operator expression.
pub fn op_expression<'a>(i: SliceIter<'a, Token>) -> NomResult<'a, Expression> {
    let preparse = parse_operand_list(i.clone());
    match preparse {
        Result::Fail(e) => Result::Fail(e),
        Result::Abort(e) => Result::Abort(e),
        Result::Incomplete(i) => Result::Incomplete(i),
        Result::Complete(rest, oplist) => {
            let mut i_ = SliceIter::new(&oplist);
            let parse_result = either!(
                i_,
                trace_nom!(compare_expression),
                trace_nom!(math_expression)
            );

            match parse_result {
                Result::Fail(e) => Result::Fail(e),
                Result::Abort(e) => Result::Abort(e),
                Result::Incomplete(i) => Result::Incomplete(i),
                Result::Complete(_, expr) => Result::Complete(rest.clone(), expr),
            }
        }
    }
}
