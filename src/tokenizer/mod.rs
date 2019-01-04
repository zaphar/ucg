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

//! The tokenization stage of the ucg compiler.
use std;

use abortable_parser::combinators::*;
use abortable_parser::iter::SliceIter;
use abortable_parser::{Error, Offsetable, Result};

use crate::ast::*;
use crate::error::StackPrinter;
use crate::iter::OffsetStrIter;

fn is_symbol_char<'a>(i: OffsetStrIter<'a>) -> Result<OffsetStrIter<'a>, u8> {
    let mut _i = i.clone();
    let c = match _i.next() {
        Some(c) => *c,
        None => {
            return Result::Fail(Error::new(
                "Unexpected End of Input".to_string(),
                Box::new(_i.clone()),
            ))
        }
    };
    if (c as char).is_ascii_alphanumeric() || c == b'-' || c == b'_' {
        Result::Complete(_i, c)
    } else {
        Result::Fail(Error::new(
            "Not a symbol character".to_string(),
            Box::new(_i.clone()),
        ))
    }
}

fn escapequoted<'a>(input: OffsetStrIter<'a>) -> Result<OffsetStrIter<'a>, String> {
    // loop until we find a " that is not preceded by \.
    // Collapse all \<char> to just char  for escaping.
    let mut frag = String::new();
    let mut escape = false;
    let mut _input = input.clone();
    loop {
        let c = match _input.next() {
            Some(c) => *c,
            None => break,
        };
        if c == '\\' as u8 && !escape {
            // eat this slash and set our escaping sentinel
            escape = true;
        } else if c == '"' as u8 && !escape {
            // Bail if this is an unescaped "
            // we exit here.
            return Result::Complete(_input, frag);
        } else {
            // we accumulate this character.
            frag.push(c as char);
            escape = false; // reset our escaping sentinel
        }
    }
    return Result::Incomplete(_input.clone());
}

make_fn!(strtok<OffsetStrIter, Token>,
       do_each!(
           span => input!(),
           _    => text_token!("\""),
           frag => escapequoted,
           (Token{
               typ: TokenType::QUOTED,
               pos: Position::from(&span),
               fragment: frag.to_string(),
           })
       )
);

make_fn!(pipequotetok<OffsetStrIter, Token>,
       do_each!(
           p    => input!(),
           _    => text_token!("|"),
           frag => until!(text_token!("|")),
           _    => text_token!("|"),
           (Token{
               typ: TokenType::PIPEQUOTE,
               pos: Position::from(&p),
               fragment: frag.to_string(),
           })
       )
);

make_fn!(barewordtok<OffsetStrIter, Token>,
       do_each!(
           span => input!(),
           _    => peek!(ascii_alpha),
           frag => consume_all!(is_symbol_char),
           (Token{
               typ: TokenType::BAREWORD,
               pos: Position::from(&span),
               fragment: frag.to_string(),
           })
       )
);

make_fn!(digittok<OffsetStrIter, Token>,
       do_each!(
           span => input!(),
           _ => peek!(ascii_digit),
           digits => consume_all!(ascii_digit),
           (Token{
               typ: TokenType::DIGIT,
               pos: Position::from(&span),
               fragment: digits.to_string(),
           })
       )
);

make_fn!(booleantok<OffsetStrIter, Token>,
    do_each!(
        span => input!(),
        token => either!(
            text_token!("true"),
            text_token!("false")
        ),
        (Token{
            typ: TokenType::BOOLEAN,
            pos: Position::from(&span),
            fragment: token.to_string(),
        })
    )
);

/// do_text_token_tok! is a helper macro to make building a simple text_token token
/// less code.
macro_rules! do_text_token_tok {
    ($i:expr, $type:expr, $text_token:expr, WS) => {
        do_each!($i,
                                                                span => input!(),
                                                                frag => text_token!($text_token),
                                                                _ => either!(whitespace, comment),
                                                                (Token {
                                                                    typ: $type,
                                                                    pos: Position::from(&span),
                                                                    fragment: frag.to_string(),
                                                                })
                                                                )
    };

    ($i:expr, $type:expr, $text_token:expr) => {
        do_each!($i,
                                                                span => input!(),
                                                                frag => text_token!($text_token),
                                                                (Token {
                                                                    typ: $type,
                                                                    pos: Position::from(&span),
                                                                    fragment: frag.to_string(),
                                                                })
                                                                )
    };
}

make_fn!(emptytok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::EMPTY, "NULL")
);

make_fn!(commatok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, ",")
);

make_fn!(lbracetok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "{")
);

make_fn!(rbracetok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "}")
);

make_fn!(lparentok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "(")
);

make_fn!(rparentok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, ")")
);

make_fn!(dottok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, ".")
);

make_fn!(plustok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "+")
);

make_fn!(dashtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "-")
);

make_fn!(startok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "*")
);

make_fn!(slashtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "/")
);

make_fn!(pcttok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "%")
);

make_fn!(eqeqtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "==")
);

make_fn!(notequaltok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "!=")
);

make_fn!(gttok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, ">")
);

make_fn!(gtequaltok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, ">=")
);

make_fn!(ltequaltok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "<=")
);

make_fn!(lttok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "<")
);

make_fn!(equaltok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "=")
);

make_fn!(semicolontok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, ";")
);

make_fn!(leftsquarebracket<OffsetStrIter, Token>,
    do_text_token_tok!(TokenType::PUNCT, "[")
);

make_fn!(rightsquarebracket<OffsetStrIter, Token>,
    do_text_token_tok!(TokenType::PUNCT, "]")
);

make_fn!(fatcommatok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "=>")
);

make_fn!(selecttok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "select", WS)
);

make_fn!(intok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "in", WS)
);

make_fn!(macrotok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "macro", WS)
);

make_fn!(moduletok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "module", WS)
);

make_fn!(lettok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "let", WS)
);

make_fn!(importtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "import", WS)
);

make_fn!(includetok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "include", WS)
);

make_fn!(asserttok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "assert", WS)
);

make_fn!(outtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "out", WS)
);

make_fn!(astok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "as", WS)
);

make_fn!(maptok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "map", WS)
);

make_fn!(filtertok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "filter", WS)
);

fn comment(input: OffsetStrIter) -> Result<OffsetStrIter, Token> {
    match text_token!(input, "//") {
        Result::Complete(rest, _) => {
            match until!(
                rest,
                either!(
                    eoi,
                    discard!(text_token!("\r\n")),
                    discard!(text_token!("\n"))
                )
            ) {
                Result::Complete(rest, cmt) => {
                    return Result::Complete(rest, make_tok!(CMT => cmt.to_string(), input));
                }
                // If we didn't find a new line then we just grab everything.
                _ => {
                    return Result::Abort(Error::new(
                        "Unparsable comment".to_string(),
                        Box::new(rest.clone()),
                    ));
                }
            }
        }
        Result::Incomplete(ctx) => return Result::Incomplete(ctx),
        Result::Fail(e) => return Result::Fail(e),
        Result::Abort(e) => return Result::Abort(e),
    }
}

make_fn!(whitespace<OffsetStrIter, Token>,
    do_each!(
        span => input!(),
        _ => peek!(ascii_ws),
        _ => repeat!(ascii_ws),
         (Token{
            typ: TokenType::WS,
            pos: Position::from(&span),
            fragment: String::new(),
         })
    )
);

make_fn!(end_of_input<OffsetStrIter, Token>,
    do_each!(
        span => input!(),
        _ => eoi,
        (Token{
            typ: TokenType::END,
            pos: Position::from(&span),
            fragment: String::new(),
        })
    )
);

fn token<'a>(input: OffsetStrIter<'a>) -> Result<OffsetStrIter<'a>, Token> {
    either!(
        input,
        strtok,
        pipequotetok,
        emptytok, // This must come before the barewordtok
        digittok,
        commatok,
        rbracetok,
        lbracetok,
        lparentok,
        rparentok,
        dottok,
        plustok,
        dashtok,
        startok,
        comment, // Note comment must come before slashtok
        slashtok,
        pcttok,
        eqeqtok,
        notequaltok,
        complete!("Not >=".to_string(), gtequaltok),
        complete!("Not <=".to_string(), ltequaltok),
        gttok,
        lttok,
        fatcommatok, // Note fatcommatok must come before equaltok
        equaltok,
        semicolontok,
        leftsquarebracket,
        rightsquarebracket,
        booleantok,
        intok,
        lettok,
        outtok,
        selecttok,
        asserttok,
        macrotok,
        moduletok,
        importtok,
        includetok,
        astok,
        maptok,
        filtertok,
        barewordtok,
        whitespace,
        end_of_input
    )
}

/// Consumes an input OffsetStrIter and returns either a Vec<Token> or a error::Error.
pub fn tokenize<'a>(input: OffsetStrIter<'a>) -> std::result::Result<Vec<Token>, String> {
    let mut out = Vec::new();
    let mut i = input.clone();
    loop {
        if let Result::Complete(_, _) = eoi(i.clone()) {
            break;
        }
        match token(i.clone()) {
            Result::Abort(e) => {
                let err = abortable_parser::Error::caused_by(
                    "Invalid Token encountered",
                    Box::new(e),
                    Box::new(i.clone()),
                );
                let ctx_err = StackPrinter { err: err };
                return Err(format!("{}", ctx_err));
            }
            Result::Fail(e) => {
                let err = abortable_parser::Error::caused_by(
                    "Invalid Token encountered",
                    Box::new(e),
                    Box::new(i.clone()),
                );
                let ctx_err = StackPrinter { err: err };
                return Err(format!("{}", ctx_err));
            }
            Result::Incomplete(_offset) => {
                let err =
                    abortable_parser::Error::new("Invalid Token encountered", Box::new(i.clone()));
                let ctx_err = StackPrinter { err: err };
                return Err(format!("{}", ctx_err));
            }
            Result::Complete(rest, tok) => {
                i = rest;
                if tok.typ == TokenType::COMMENT || tok.typ == TokenType::WS {
                    // we skip comments and whitespace
                    continue;
                }
                out.push(tok);
            }
        }
    }
    // ensure that we always have an END token to go off of.
    out.push(Token {
        fragment: String::new(),
        typ: TokenType::END,
        pos: Position::from(&i),
    });
    Ok(out)
}

/// Clones a token.
///
/// This is necessary to allow the match_type and match_token macros to work.
pub fn token_clone(t: &Token) -> std::result::Result<Token, Error<SliceIter<Token>>> {
    Ok(t.clone())
}

/// nom macro that matches a Token by type and uses an optional conversion handler
/// for the matched Token.
macro_rules! match_type {
    ($i:expr,BOOLEAN => $h:expr) => {
        match_type!($i, TokenType::BOOLEAN, "Not a Boolean", $h)
    };

    ($i:expr,BOOLEAN) => {
        match_type!($i, BOOLEAN => token_clone)
    };

    ($i:expr,COMMENT => $h:expr) => {
        match_type!($i, TokenType::COMMENT, "Not a Comment", $h)
    };

    ($i:expr,COMMENT) => {
        match_type!($i, COMMENT => token_clone)
    };

    ($i:expr,BAREWORD => $h:expr) => {
        match_type!($i, TokenType::BAREWORD, "Not a Bareword", $h)
    };

    ($i:expr,BAREWORD) => {
        match_type!($i, BAREWORD => token_clone)
    };

    ($i:expr,EMPTY => $h:expr) => {
        match_type!($i, TokenType::EMPTY, "Not NULL", $h)
    };

    ($i:expr,EMPTY) => {
        match_type!($i, EMPTY => token_clone)
    };

    ($i:expr,STR => $h:expr) => {
        match_type!($i, TokenType::QUOTED, "Not a String", $h)
    };

    ($i:expr,STR) => {
        match_type!($i, STR => token_clone)
    };

    ($i:expr,PIPEQUOTE => $h:expr) => {
        match_type!($i, TokenType::PIPEQUOTE, "Not a Pipe Quoted String", $h)
    };

    ($i:expr,PIPEQUOTE) => {
        match_type!($i, PIPEQUOTE => token_clone)
    };

    ($i:expr,DIGIT => $h:expr) => {
        match_type!($i, TokenType::DIGIT, "Not a DIGIT", $h)
    };

    ($i:expr,DIGIT) => {
        match_type!($i, DIGIT => token_clone)
    };

    ($i:expr,PUNCT => $h:expr) => {
        match_type!($i, TokenType::PUNCT, "Not PUNCTUATION", $h)
    };

    ($i:expr,PUNCT) => {
        match_type!($i, PUNCT => token_clone)
    };

    ($i:expr, $t:expr, $msg:expr, $h:expr) => {{
        use abortable_parser::combinators::eoi;
        use abortable_parser::{Error, Result};
        use std;

        let mut _i = $i.clone();
        if eoi(_i.clone()).is_complete() {
            Result::Fail(Error::new(format!("End of Input! {}", $msg), Box::new(_i)))
        } else {
            match _i.next() {
                Some(tok) => {
                    if tok.typ == $t {
                        match $h(tok) {
                            std::result::Result::Ok(v) => Result::Complete(_i.clone(), v),
                            std::result::Result::Err(e) => {
                                Result::Fail(Error::caused_by($msg, Box::new(e), Box::new(_i)))
                            }
                        }
                    } else {
                        Result::Fail(Error::new($msg.to_string(), Box::new($i)))
                    }
                }
                None => Result::Fail(Error::new($msg.to_string(), Box::new($i))),
            }
        }
    }};
}

/// nom style macro that matches various Tokens by type and value and allows optional
/// conversion handlers for the matched Token.
macro_rules! match_token {
    ($i:expr,PUNCT => $f:expr) => {{
        use crate::tokenizer::token_clone;
        match_token!($i, PUNCT => $f, token_clone)
    }};

    ($i:expr,PUNCT => $f:expr, $h:expr) => {
        match_token!($i, TokenType::PUNCT, $f, format!("({})", $f), $h)
    };

    ($i:expr,BAREWORD => $f:expr) => {{
        use crate::tokenizer::token_clone;
        match_token!($i, BAREWORD => $f, token_clone)
    }};

    ($i:expr,BAREWORD => $f:expr, $h:expr) => {
        match_token!(
            $i,
            TokenType::BAREWORD,
            $f,
            format!("Not a BAREWORD ({})", $f),
            $h
        )
    };

    ($i:expr, $t:expr, $f:expr, $msg:expr, $h:expr) => {{
        use abortable_parser::Result;
        use std;
        let mut i_ = $i.clone();
        let tok = i_.next();
        if let Some(tok) = tok {
            if tok.typ == $t && &tok.fragment == $f {
                match $h(tok) {
                    std::result::Result::Ok(v) => Result::Complete(i_.clone(), v),
                    std::result::Result::Err(e) => {
                        Result::Fail(Error::caused_by($msg, Box::new(e), Box::new(i_)))
                    }
                }
            } else {
                Result::Fail(Error::new(
                    format!("Expected {} Instead is ({})", $msg, tok.fragment),
                    Box::new(i_),
                ))
            }
        } else {
            Result::Fail(Error::new("Unexpected End Of Input", Box::new(i_)))
        }
    }};
}

/// nom style macro that matches punctuation Tokens.
macro_rules! punct {
    ($i:expr, $c:expr) => {
        match_token!($i, PUNCT => $c)
    };
}

/// nom style macro that matches any bareword Token.
macro_rules! word {
    ($i:expr, $w:expr) => {
        match_token!($i, BAREWORD => $w)
    };
}

/// pos gets the current position from a TokenIter input without consuming it.
pub fn pos<'a>(i: SliceIter<'a, Token>) -> Result<SliceIter<'a, Token>, Position> {
    let mut _i = i.clone();
    let tok = _i.next().unwrap();
    let line = tok.pos.line;
    let column = tok.pos.column;
    Result::Complete(i.clone(), Position::new(line, column, i.get_offset()))
}

#[cfg(test)]
mod test;
