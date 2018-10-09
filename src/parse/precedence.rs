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
use std;

use nom;
use nom::Context::Code;
use nom::{ErrorKind, IResult, InputIter, InputLength, Slice};

use super::{non_op_expression, NomResult, ParseResult};
use ast::*;
use error;
use tokenizer::TokenIter;

/// Defines the intermediate stages of our bottom up parser for precedence parsing.
#[derive(Debug, PartialEq, Clone)]
pub enum Element {
    Expr(Expression),
    MathOp(BinaryExprType),
    CompareOp(CompareType),
}

named!(math_op_type<TokenIter, Element, error::Error>,
    alt!(
        do_parse!(punct!("+") >> (Element::MathOp(BinaryExprType::Add))) |
        do_parse!(punct!("-") >> (Element::MathOp(BinaryExprType::Sub))) |
        do_parse!(punct!("*") >> (Element::MathOp(BinaryExprType::Mul))) |
        do_parse!(punct!("/") >> (Element::MathOp(BinaryExprType::Div)))
    )
);

fn parse_expression(i: OpListIter) -> IResult<OpListIter, Expression, error::Error> {
    let i_ = i.clone();
    if i_.input_len() == 0 {
        return Err(nom::Err::Error(Code(
            i_,
            ErrorKind::Custom(error::Error::new(
                format!("Expected Expression found End Of Input"),
                error::ErrorType::IncompleteParsing,
                // TODO(jwall): This position information is incorrect.
                Position { line: 0, column: 0 },
            )),
        )));
    }
    let el = &(i_[0]);
    if let &Element::Expr(ref expr) = el {
        return Ok((i.slice(1..), expr.clone()));
    }
    return Err(nom::Err::Error(Code(
        i_.clone(),
        ErrorKind::Custom(error::Error::new(
            format!(
                "Error while parsing Binary Expression Unexpected Operator {:?}",
                el
            ),
            error::ErrorType::ParseError,
            // TODO(jwall): This position information is incorrect.
            Position { line: 0, column: 0 },
        )),
    )));
}

fn parse_sum_operator(i: OpListIter) -> IResult<OpListIter, BinaryExprType, error::Error> {
    let i_ = i.clone();
    if i_.input_len() == 0 {
        return Err(nom::Err::Error(Code(
            i_,
            ErrorKind::Custom(error::Error::new(
                format!("Expected Expression found End Of Input"),
                error::ErrorType::IncompleteParsing,
                // TODO(jwall): This position information is incorrect.
                Position { line: 0, column: 0 },
            )),
        )));
    }
    let el = &(i_[0]);
    if let &Element::MathOp(ref op) = el {
        match op {
            &BinaryExprType::Add => {
                return Ok((i.slice(1..), op.clone()));
            }
            &BinaryExprType::Sub => {
                return Ok((i.slice(1..), op.clone()));
            }
            _other => {
                // noop
            }
        };
    }
    return Err(nom::Err::Error(Code(
        i_.clone(),
        ErrorKind::Custom(error::Error::new(
            format!(
                "Error while parsing Binary Expression Unexpected Operator {:?}",
                el
            ),
            error::ErrorType::ParseError,
            // TODO(jwall): This position information is incorrect.
            Position { line: 0, column: 0 },
        )),
    )));
}

fn tuple_to_binary_expression(
    tpl: (BinaryExprType, Expression, Expression),
) -> ParseResult<Expression> {
    let pos = tpl.1.pos().clone();
    Ok(Expression::Binary(BinaryOpDef {
        kind: tpl.0,
        left: Box::new(tpl.1),
        right: Box::new(tpl.2),
        pos: pos,
    }))
}

fn parse_product_operator(i: OpListIter) -> IResult<OpListIter, BinaryExprType, error::Error> {
    let i_ = i.clone();
    if i_.input_len() == 0 {
        return Err(nom::Err::Error(Code(
            i_,
            ErrorKind::Custom(error::Error::new(
                format!("Expected Expression found End Of Input"),
                error::ErrorType::IncompleteParsing,
                // TODO(jwall): This position information is incorrect.
                Position { line: 0, column: 0 },
            )),
        )));
    }
    let el = &(i_[0]);
    if let &Element::MathOp(ref op) = el {
        match op {
            &BinaryExprType::Mul => {
                return Ok((i.slice(1..), op.clone()));
            }
            &BinaryExprType::Div => {
                return Ok((i.slice(1..), op.clone()));
            }
            _other => {
                // noop
            }
        };
    }
    return Err(nom::Err::Error(Code(
        i_.clone(),
        ErrorKind::Custom(error::Error::new(
            format!(
                "Error while parsing Binary Expression Unexpected Operator {:?}",
                el
            ),
            error::ErrorType::ParseError,
            // TODO(jwall): This position information is incorrect.
            Position { line: 0, column: 0 },
        )),
    )));
}

/// do_binary_expr implements precedence based parsing where the more tightly bound
/// parsers are passed in as lowerrule parsers. We default to any non_op_expression
/// as the most tightly bound expressions.
macro_rules! do_binary_expr {
    ($i:expr, $oprule:ident, $lowerrule:ident) => {
        do_binary_expr!($i, call!($oprule), $lowerrule)
    };

    ($i:expr, $oprule:ident, $lowerrule:ident!( $($lowerargs:tt)* )) => {
        do_binary_expr!($i, call!($oprule), $lowerrule!($($lowerargs)*))
    };

    ($i:expr, $oprule:ident) => {
        do_binary_expr!($i, call!($oprule))
    };

    ($i:expr, $oprule:ident!( $($args:tt)* )) => {
        do_binary_expr!($i, $oprule!($($args)*), parse_expression)
    };

    ($i:expr, $oprule:ident!( $($args:tt)* ), $lowerrule:ident) => {
        do_binary_expr!($i, $oprule!($($args)*), call!($lowerrule))
    };

    ($i:expr, $oprule:ident!( $($args:tt)* ), $lowerrule:ident!( $($lowerargs:tt)* )) => {
        map_res!($i,
            do_parse!(
                left: $lowerrule!($($lowerargs)*) >>
                    typ: $oprule!($($args)*) >>
                    right: $lowerrule!($($lowerargs)*) >>
                    (typ, left, right)
            ),
            tuple_to_binary_expression
        )
    };
}

named!(sum_expression<OpListIter, Expression, error::Error>,
    do_binary_expr!(
        parse_sum_operator,
        alt!(trace_nom!(product_expression) | trace_nom!(parse_expression)))
);

named!(product_expression<OpListIter, Expression, error::Error>,
    do_binary_expr!(
       parse_product_operator,
    trace_nom!(parse_expression))
);

named!(math_expression<OpListIter, Expression, error::Error>,
    alt!(trace_nom!(sum_expression) | trace_nom!(product_expression))
);

// TODO(jwall): Change comparison operators to use the do_binary_expr! with precedence?
fn tuple_to_compare_expression(
    tpl: (CompareType, Expression, Expression),
) -> ParseResult<Expression> {
    let pos = tpl.1.pos().clone();
    Ok(Expression::Compare(ComparisonDef {
        kind: tpl.0,
        left: Box::new(tpl.1),
        right: Box::new(tpl.2),
        pos: pos,
    }))
}

named!(compare_op_type<TokenIter, Element, error::Error>,
    alt!(
        do_parse!(punct!("==") >> (Element::CompareOp(CompareType::Equal))) |
        do_parse!(punct!("!=") >> (Element::CompareOp(CompareType::NotEqual))) |
        do_parse!(punct!("<=") >> (Element::CompareOp(CompareType::LTEqual))) |
        do_parse!(punct!(">=") >> (Element::CompareOp(CompareType::GTEqual))) |
        do_parse!(punct!("<") >>  (Element::CompareOp(CompareType::LT))) |
        do_parse!(punct!(">") >>  (Element::CompareOp(CompareType::GT)))
    )
);

fn parse_compare_operator(i: OpListIter) -> IResult<OpListIter, CompareType, error::Error> {
    let i_ = i.clone();
    if i_.input_len() == 0 {
        return Err(nom::Err::Error(Code(
            i_,
            ErrorKind::Custom(error::Error::new(
                format!("Expected Expression found End Of Input"),
                error::ErrorType::IncompleteParsing,
                // TODO(jwall): This position information is incorrect.
                Position { line: 0, column: 0 },
            )),
        )));
    }
    let el = &(i_[0]);
    if let &Element::CompareOp(ref op) = el {
        return Ok((i.slice(1..), op.clone()));
    }
    return Err(nom::Err::Error(Code(
        i_.clone(),
        ErrorKind::Custom(error::Error::new(
            format!(
                "Error while parsing Binary Expression Unexpected Operator {:?}",
                el
            ),
            error::ErrorType::ParseError,
            // TODO(jwall): This position information is incorrect.
            Position { line: 0, column: 0 },
        )),
    )));
}

named!(compare_expression<OpListIter, Expression, error::Error>,
    map_res!(
        do_parse!(
            left: alt!(trace_nom!(math_expression) | trace_nom!(parse_expression)) >>
                typ: parse_compare_operator >>
                right: alt!(trace_nom!(math_expression) | trace_nom!(parse_expression)) >>
                (typ, left, right)
        ),
        tuple_to_compare_expression
    )
);

/// Parse a list of expressions separated by operators into a Vec<Element>.
fn parse_operand_list(i: TokenIter) -> NomResult<Vec<Element>> {
    // 1. First try to parse a non_op_expression,
    let mut _i = i.clone();
    let mut list = Vec::new();
    // 1. loop
    let mut firstrun = true;
    loop {
        // 2. Parse a non_op_expression.
        match non_op_expression(_i.clone()) {
            Err(nom::Err::Error(ctx)) => {
                // A failure to parse an expression
                // is always an error.
                return Err(nom::Err::Error(ctx));
            }
            Err(nom::Err::Failure(ctx)) => {
                // A failure to parse an expression
                // is always an error.
                return Err(nom::Err::Failure(ctx));
            }
            Err(nom::Err::Incomplete(i)) => {
                return Err(nom::Err::Incomplete(i));
            }
            Ok((rest, expr)) => {
                list.push(Element::Expr(expr));
                _i = rest.clone();
            }
        }
        // 3. Parse an operator.
        match alt!(_i.clone(), math_op_type | compare_op_type) {
            Err(nom::Err::Error(ctx)) => {
                if firstrun {
                    // If we don't find an operator in our first
                    // run then this is not an operand list.
                    return Err(nom::Err::Error(ctx));
                }
                // if we don't find one on subsequent runs then
                // that's the end of the operand list.
                break;
            }
            Err(nom::Err::Failure(ctx)) => {
                return Err(nom::Err::Failure(ctx));
            }
            Err(nom::Err::Incomplete(i)) => {
                return Err(nom::Err::Incomplete(i));
            }
            Ok((rest, el)) => {
                list.push(el);
                _i = rest.clone();
            }
        }
        firstrun = false;
    }
    return Ok((_i, list));
}

#[derive(Clone, Debug, PartialEq)]
pub struct OpListIter<'a> {
    pub source: &'a [Element],
}

impl<'a> OpListIter<'a> {
    pub fn len(&self) -> usize {
        self.source.len()
    }
}

impl<'a> InputLength for OpListIter<'a> {
    fn input_len(&self) -> usize {
        self.source.input_len()
    }
}

macro_rules! impl_op_iter_slice {
    ($r:ty) => {
        impl<'a> Slice<$r> for OpListIter<'a> {
            fn slice(&self, range: $r) -> Self {
                OpListIter {
                    source: self.source.slice(range),
                }
            }
        }
    };
}

impl_op_iter_slice!(std::ops::Range<usize>);
impl_op_iter_slice!(std::ops::RangeTo<usize>);
impl_op_iter_slice!(std::ops::RangeFrom<usize>);
impl_op_iter_slice!(std::ops::RangeFull);

impl<'a> std::ops::Index<usize> for OpListIter<'a> {
    type Output = Element;

    fn index(&self, i: usize) -> &Self::Output {
        &self.source[i]
    }
}

impl<'a> InputIter for OpListIter<'a> {
    type Item = &'a Element;
    type RawItem = Element;

    type Iter = std::iter::Enumerate<std::slice::Iter<'a, Self::RawItem>>;
    type IterElem = std::slice::Iter<'a, Self::RawItem>;

    fn iter_indices(&self) -> Self::Iter {
        self.source.iter().enumerate()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.source.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::RawItem) -> bool,
    {
        for (o, v) in self.iter_indices() {
            if predicate(v.clone()) {
                return Some(o);
            }
        }
        None
    }

    fn slice_index(&self, count: usize) -> Option<usize> {
        let mut cnt = 0;
        for (index, _) in self.iter_indices() {
            if cnt == count {
                return Some(index);
            }
            cnt += 1;
        }
        if cnt == count {
            return Some(self.len());
        }
        None
    }
}

/// Parse a binary operator expression.
pub fn op_expression(i: TokenIter) -> NomResult<Expression> {
    let preparse = parse_operand_list(i.clone());
    match preparse {
        Err(nom::Err::Error(ctx)) => Err(nom::Err::Error(ctx)),
        Err(nom::Err::Failure(ctx)) => Err(nom::Err::Failure(ctx)),
        Err(nom::Err::Incomplete(i)) => Err(nom::Err::Incomplete(i)),
        Ok((rest, oplist)) => {
            let mut i_ = OpListIter {
                source: oplist.as_slice(),
            };

            let parse_result = alt!(
                i_,
                trace_nom!(compare_expression) | trace_nom!(math_expression)
            );

            match parse_result {
                Err(nom::Err::Error(Code(_, e))) => Err(nom::Err::Error(Code(rest.clone(), e))),
                Err(nom::Err::Failure(Code(_, e))) => Err(nom::Err::Failure(Code(rest.clone(), e))),
                Err(nom::Err::Incomplete(i)) => Err(nom::Err::Incomplete(i)),
                Ok((_, expr)) => Ok((rest.clone(), expr)),
            }
        }
    }
}
