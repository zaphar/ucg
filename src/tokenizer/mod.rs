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
use std::rc::Rc;

use abortable_parser::combinators::*;
use abortable_parser::iter::SliceIter;
use abortable_parser::{Error, Result};

use crate::ast::*;
use crate::error::BuildError;
use crate::iter::OffsetStrIter;

pub type CommentGroup = Vec<Token>;
pub type CommentMap = std::collections::BTreeMap<usize, CommentGroup>;

fn is_symbol_char<'a>(i: OffsetStrIter<'a>) -> Result<OffsetStrIter<'a>, u8> {
    let mut _i = i.clone();
    let c = match _i.next() {
        Some(c) => *c,
        None => {
            return Result::Fail(Error::new(
                "Unexpected End of Input".to_string(),
                Box::new(_i.clone()),
            ));
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
    // Collapse all \<char> to just char  for escaping exept for \n \r \t and \@.
    let mut frag = String::new();
    let mut escape = false;
    let mut _input = input.clone();
    loop {
        let c = match _input.next() {
            Some(c) => *c,
            None => break,
        };
        if escape {
            match c as char {
                'n' => {
                    frag.push('\n');
                    escape = false;
                    continue;
                }
                'r' => {
                    frag.push('\r');
                    escape = false;
                    continue;
                }
                't' => {
                    frag.push('\t');
                    escape = false;
                    continue;
                }
                _ => {
                    //noop
                }
            }
        }
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
               fragment: frag.into(),
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
               fragment: frag.into(),
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
               fragment: digits.into(),
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
            fragment: token.into(),
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
               fragment: frag.into(),
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
                fragment: frag.into(),
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

make_fn!(modulustok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "%%")
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

make_fn!(matchtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "~")
);

make_fn!(notmatchtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "!~")
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

make_fn!(doublecolontok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "::")
);

make_fn!(colontok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, ":")
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

make_fn!(andtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "&&")
);

make_fn!(ortok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::PUNCT, "||")
);

make_fn!(selecttok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "select", WS)
);

make_fn!(intok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "in", WS)
);

make_fn!(istok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "is", WS)
);

make_fn!(nottok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "not", WS)
);

make_fn!(tracetok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "TRACE", WS)
);

make_fn!(failtok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "fail", WS)
);

make_fn!(functok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "func", WS)
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

make_fn!(converttok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "convert", WS)
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

make_fn!(reducetok<OffsetStrIter, Token>,
       do_text_token_tok!(TokenType::BAREWORD, "reduce", WS)
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
                    // Eat the new lines here before continuing
                    let rest =
                        match optional!(rest, either!(text_token!("\r\n"), text_token!("\n"))) {
                            Result::Complete(next_rest, _) => next_rest,
                            _ => rest,
                        };
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
            fragment: "".into(),
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
            fragment: "".into(),
        })
    )
);

fn token<'a>(input: OffsetStrIter<'a>) -> Result<OffsetStrIter<'a>, Token> {
    either!(
        input,
        strtok,
        emptytok, // This must come before the barewordtok
        digittok,
        commatok,
        rbracetok,
        lbracetok,
        lparentok,
        rparentok,
        dottok,
        andtok,
        ortok,
        plustok,
        dashtok,
        startok,
        comment, // Note comment must come before slashtok
        slashtok,
        modulustok,
        pcttok,
        eqeqtok,
        notequaltok,
        matchtok,
        notmatchtok,
        complete!("Not >=".to_string(), gtequaltok),
        complete!("Not <=".to_string(), ltequaltok),
        gttok,
        lttok,
        fatcommatok, // Note fatcommatok must come before equaltok
        equaltok,
        semicolontok,
        doublecolontok,
        colontok,
        leftsquarebracket,
        rightsquarebracket,
        booleantok,
        intok,
        istok,
        nottok,
        lettok,
        outtok,
        converttok,
        selecttok,
        asserttok,
        failtok,
        tracetok,
        functok,
        moduletok,
        importtok,
        includetok,
        astok,
        maptok,
        filtertok,
        reducetok,
        barewordtok,
        whitespace,
        end_of_input
    )
}

/// Consumes an input OffsetStrIter and returns either a Vec<Token> or a error::Error.
/// If a comment_map is passed in then it will store the comments indexed by their
/// line number.
pub fn tokenize<'a>(
    input: OffsetStrIter<'a>,
    mut comment_map: Option<&mut CommentMap>,
) -> std::result::Result<Vec<Token>, BuildError> {
    let mut out = Vec::new();
    let mut i = input.clone();
    let mut comment_group = Vec::new();
    let mut comment_was_last: Option<Token> = None;
    loop {
        if let Result::Complete(_, _) = eoi(i.clone()) {
            break;
        }
        match token(i.clone()) {
            Result::Abort(e) => {
                return Err(BuildError::from(e));
            }
            Result::Fail(e) => {
                return Err(BuildError::from(e));
            }
            Result::Incomplete(_offset) => {
                let err =
                    abortable_parser::Error::new("Invalid Token encountered", Box::new(i.clone()));
                return Err(BuildError::from(err));
            }
            Result::Complete(rest, tok) => {
                i = rest;
                match (&mut comment_map, &tok.typ) {
                    // variants with a comment_map
                    (&mut Some(_), &TokenType::COMMENT) => {
                        comment_group.push(tok.clone());
                        comment_was_last = Some(tok.clone());
                        continue;
                    }
                    (&mut Some(ref mut map), _) => {
                        if tok.typ != TokenType::WS {
                            out.push(tok);
                        }
                        if let Some(tok) = comment_was_last {
                            map.insert(tok.pos.line, comment_group);
                            comment_group = Vec::new();
                        }
                    }
                    // variants without a comment_map
                    (None, TokenType::WS) | (None, TokenType::COMMENT) => continue,
                    (None, _) => {
                        out.push(tok);
                    }
                }
                comment_was_last = None;
            }
        }
    }
    // if we had a comments at the end then we need to do a final
    // insert into our map.
    if let Some(ref mut map) = comment_map {
        if let Some(ref tok) = comment_group.last() {
            let line = tok.pos.line;
            map.insert(line, comment_group);
        }
    }
    // ensure that we always have an END token to go off of.
    out.push(Token {
        fragment: "".into(),
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
            format!("Expected BAREWORD but got ({})", $f),
            $h
        )
    };

    ($i:expr, $t:expr, $f:expr, $msg:expr, $h:expr) => {{
        use abortable_parser::Result;
        use std;
        use std::rc::Rc;
        let f: Rc<str> = $f.into();
        let mut i_ = $i.clone();
        let tok = i_.next();
        if let Some(tok) = tok {
            if tok.typ == $t && tok.fragment.as_ref() == f.as_ref() {
                match $h(tok) {
                    std::result::Result::Ok(v) => Result::Complete(i_.clone(), v),
                    std::result::Result::Err(e) => {
                        Result::Fail(Error::caused_by($msg, Box::new(e), Box::new(i_)))
                    }
                }
            } else {
                Result::Fail(Error::new(
                    format!("Expected {} but got ({})", $msg, tok.fragment),
                    Box::new($i.clone()),
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
    let pos = tok.pos.clone();
    Result::Complete(i, pos)
}

#[cfg(test)]
mod test;
