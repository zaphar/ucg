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
use ast::*;
use error;
use nom;
use nom::{alpha, digit, is_alphanumeric, multispace};
use nom::{InputIter, InputLength, Slice};
use nom_locate::LocatedSpan;
use std;
use std::result::Result;

pub type Span<'a> = LocatedSpan<&'a str>;

impl<'a> From<Span<'a>> for Position {
    fn from(s: Span) -> Position {
        Position {
            line: s.line as usize,
            column: s.get_column() as usize,
        }
    }
}

fn is_symbol_char(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '-' as char || c == '_' as char
}

fn escapequoted(input: Span) -> nom::IResult<Span, String> {
    // loop until we find a " that is not preceded by \.
    // Collapse all \<char> to just char  for escaping.
    let mut frag = String::new();
    let mut escape = false;
    for (i, c) in input.iter_indices() {
        if c == '\\' && !escape {
            // eat this slash and set our escaping sentinel
            escape = true;
        } else if c == '"' && !escape {
            // Bail if this is an unescaped "
            // we exit here.
            return nom::IResult::Done(input.slice(i..), frag);
        } else {
            // we accumulate this character.
            frag.push(c);
            escape = false; // reset our escaping sentinel
        }
    }
    return nom::IResult::Incomplete(nom::Needed::Unknown);
}

named!(strtok( Span ) -> Token,
       do_parse!(
           span: position!() >>
               tag!("\"") >>
               frag: escapequoted >>
               tag!("\"") >>
           (Token{
               typ: TokenType::QUOTED,
               pos: Position::from(span),
               fragment: frag,
           })
       )
);

named!(barewordtok( Span ) -> Token,
       do_parse!(
           span: position!() >>
           frag: preceded!(peek!(alpha), take_while!(is_symbol_char)) >>
           (Token{
               typ: TokenType::BAREWORD,
               pos: Position::from(span),
               fragment: frag.fragment.to_string(),
           })
       )
);

named!(digittok( Span ) -> Token,
       do_parse!(
           span: position!() >>
               digits: digit >>
               (Token{
                   typ: TokenType::DIGIT,
                   pos: Position::from(span),
                   fragment: digits.fragment.to_string(),
               })
       )
);

named!(booleantok( Span ) -> Token,
    do_parse!(
        span: position!() >>
        b: alt!(
            tag!("true") |
            tag!("false")
        ) >>
        (Token{
            typ: TokenType::BOOLEAN,
            pos: Position::from(span),
            fragment: b.fragment.to_string(),
        })
    )
);

/// do_tag_tok! is a helper macro to make building a simple tag token
/// less code.
macro_rules! do_tag_tok {
    // NOTE(jwall): Nom macros do magic with their inputs. They in fact
    // rewrite your macro argumets for you by adding an initial argument
    // for all their sub-macros. Which means we require this $i paramater
    // on the first macro invocation but not the rest.
    ($i:expr, $type:expr, $tag:expr,WS) => {
        do_parse!(
            $i,
            span: position!() >> frag: tag!($tag) >> alt!(whitespace | comment) >> (Token {
                typ: $type,
                pos: Position::from(span),
                fragment: frag.fragment.to_string(),
            })
        )
    };
    ($i:expr, $type:expr, $tag:expr) => {
        do_parse!(
            $i,
            span: position!() >> frag: tag!($tag) >> (Token {
                typ: $type,
                pos: Position::from(span),
                fragment: frag.fragment.to_string(),
            })
        )
    };
}

named!(emptytok( Span ) -> Token,
       do_tag_tok!(TokenType::EMPTY, "NULL")
);

named!(commatok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, ",")
);

named!(lbracetok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "{")
);

named!(rbracetok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "}")
);

named!(lparentok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "(")
);

named!(rparentok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, ")")
);

named!(dottok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, ".")
);

named!(plustok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "+")
);

named!(dashtok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "-")
);

named!(startok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "*")
);

named!(slashtok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "/")
);

named!(pcttok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "%")
);

named!(eqeqtok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "==")
);

named!(notequaltok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "!=")
);

named!(gttok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, ">")
);

named!(gtequaltok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, ">=")
);

named!(ltequaltok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "<=")
);

named!(lttok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "<")
);

named!(equaltok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "=")
);

named!(semicolontok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, ";")
);

named!(leftsquarebracket( Span ) -> Token,
    do_tag_tok!(TokenType::PUNCT, "[")
);

named!(rightsquarebracket( Span ) -> Token,
    do_tag_tok!(TokenType::PUNCT, "]")
);

named!(fatcommatok( Span ) -> Token,
       do_tag_tok!(TokenType::PUNCT, "=>")
);

named!(selecttok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "select", WS)
);

named!(macrotok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "macro", WS)
);

named!(lettok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "let", WS)
);

named!(importtok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "import", WS)
);

named!(asserttok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "assert", WS)
);

named!(astok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "as", WS)
);

named!(maptok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "map", WS)
);

named!(filtertok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "filter", WS)
);

fn end_of_input(input: Span) -> nom::IResult<Span, Token> {
    match eof!(input,) {
        nom::IResult::Done(_, _) => {
            return nom::IResult::Done(
                input,
                make_tok!(EOF => input.line as usize,
                                                input.get_column() as usize),
            );
        }
        nom::IResult::Incomplete(_) => {
            return nom::IResult::Incomplete(nom::Needed::Unknown);
        }
        nom::IResult::Error(e) => {
            return nom::IResult::Error(e);
        }
    }
}

fn comment(input: Span) -> nom::IResult<Span, Token> {
    match tag!(input, "//") {
        nom::IResult::Done(rest, _) => {
            match alt!(
                rest,
                take_until_and_consume!("\r\n") | take_until_and_consume!("\n")
            ) {
                nom::IResult::Done(rest, cmt) => {
                    return nom::IResult::Done(
                        rest,
                        make_tok!(CMT => cmt.fragment.to_string(),
                                  input.line as usize,
                                  input.get_column() as usize),
                    );
                }
                // If we didn't find a new line then we just grab everything.
                _ => {
                    let blen = rest.input_len();
                    let next = rest.slice(blen..);
                    let tok = rest.slice(..blen);
                    return nom::IResult::Done(
                        next,
                        make_tok!(CMT => tok.fragment.to_string(),
                                  input.line as usize, input.get_column() as usize
                    ),
                    );
                }
            }
        }
        nom::IResult::Incomplete(i) => return nom::IResult::Incomplete(i),
        nom::IResult::Error(e) => return nom::IResult::Error(e),
    }
}

named!(whitespace( Span ) -> Token,
    do_parse!(
        span: position!() >>
        many1!(multispace) >>
         (Token{
            typ: TokenType::WS,
            pos: Position::from(span),
            fragment: String::new(),
         })
    )
);

named!(token( Span ) -> Token,
    alt!(
        strtok |
        emptytok | // This must come before the barewordtok
        digittok |
        commatok |
        rbracetok |
        lbracetok |
        lparentok |
        rparentok |
        dottok |
        plustok |
        dashtok |
        startok |
        comment | // Note comment must come before slashtok
        slashtok |
        pcttok |
        eqeqtok |
        notequaltok |
        complete!(gtequaltok) |
        complete!(ltequaltok) |
        gttok |
        lttok |
        fatcommatok | // Note fatcommatok must come before equaltok
        equaltok |
        semicolontok |
        leftsquarebracket |
        rightsquarebracket |
        booleantok |
        lettok |
        selecttok |
        asserttok |
        macrotok |
        importtok |
        astok |
        maptok |
        filtertok |
        barewordtok |
        whitespace |
        end_of_input)
);

/// Consumes an input Span and returns either a Vec<Token> or a nom::ErrorKind.
pub fn tokenize(input: Span) -> Result<Vec<Token>, (Position, nom::ErrorKind)> {
    let mut out = Vec::new();
    let mut i = input;
    loop {
        if i.input_len() == 0 {
            break;
        }
        match token(i) {
            nom::IResult::Error(e) => {
                return Err((
                    Position {
                        line: i.line as usize,
                        column: i.get_column() as usize,
                    },
                    e,
                ));
            }
            nom::IResult::Incomplete(_) => {
                return Err((
                    Position {
                        line: i.line as usize,
                        column: i.get_column() as usize,
                    },
                    nom::ErrorKind::Complete,
                ));
            }
            nom::IResult::Done(rest, tok) => {
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
        pos: Position {
            line: i.line as usize,
            column: i.get_column() as usize,
        },
    });
    Ok(out)
}

/// Clones a token.
///
/// This is necessary to allow the match_type and match_token macros to work.
pub fn token_clone(t: &Token) -> Result<Token, error::Error> {
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
        let i_ = $i.clone();
        use nom::Slice;
        use std::convert::Into;
        if i_.input_len() == 0 {
            nom::IResult::Error(nom::ErrorKind::Custom(error::Error::new(
                format!("End of Input! {}", $msg),
                error::ErrorType::IncompleteParsing,
                Position { line: 0, column: 0 },
            )))
        } else {
            let tok = &(i_[0]);
            if tok.typ == $t {
                match $h(tok) {
                    Result::Ok(v) => nom::IResult::Done($i.slice(1..), v),
                    Result::Err(e) => nom::IResult::Error(nom::ErrorKind::Custom(e.into())),
                }
            } else {
                nom::IResult::Error(nom::ErrorKind::Custom(error::Error::new(
                    $msg.to_string(),
                    error::ErrorType::UnexpectedToken,
                    tok.pos.clone(),
                )))
            }
        }
    }};
}

/// nom style macro that matches various Tokens by type and value and allows optional
/// conversion handlers for the matched Token.
macro_rules! match_token {
    ($i:expr,PUNCT => $f:expr) => {{
        use tokenizer::token_clone;
        match_token!($i, PUNCT => $f, token_clone)
    }};

    ($i:expr,PUNCT => $f:expr, $h:expr) => {
        match_token!($i, TokenType::PUNCT, $f, format!("Not PUNCT ({})", $f), $h)
    };

    ($i:expr,BAREWORD => $f:expr) => {{
        use tokenizer::token_clone;
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
        let i_ = $i.clone();
        use nom;
        use nom::Slice;
        use std::convert::Into;
        let tok = &(i_[0]);
        if tok.typ == $t && &tok.fragment == $f {
            match $h(tok) {
                Result::Ok(v) => nom::IResult::Done($i.slice(1..), v),
                Result::Err(e) => nom::IResult::Error(nom::ErrorKind::Custom(e.into())),
            }
        } else {
            nom::IResult::Error(nom::ErrorKind::Custom(error::Error::new(
                format!("{} Instead is ({})", $msg, tok.fragment),
                error::ErrorType::UnexpectedToken,
                tok.pos.clone(),
            )))
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
pub fn pos(i: TokenIter) -> nom::IResult<TokenIter, Position, error::Error> {
    let tok = &i[0];
    let line = tok.pos.line;
    let column = tok.pos.column;
    nom::IResult::Done(
        i.clone(),
        Position {
            line: line,
            column: column,
        },
    )
}

/// TokenIter wraps a slice of Tokens and implements the various necessary
/// nom traits to use it as an input to nom parsers.
#[derive(Clone, Debug, PartialEq)]
pub struct TokenIter<'a> {
    pub source: &'a [Token],
}

impl<'a> TokenIter<'a> {
    pub fn len(&self) -> usize {
        self.source.len()
    }
}

impl<'a> nom::InputLength for TokenIter<'a> {
    fn input_len(&self) -> usize {
        self.source.input_len()
    }
}

macro_rules! impl_token_iter_slice {
    ($r:ty) => {
        impl<'a> nom::Slice<$r> for TokenIter<'a> {
            fn slice(&self, range: $r) -> Self {
                TokenIter {
                    source: self.source.slice(range),
                }
            }
        }
    };
}

impl_token_iter_slice!(std::ops::Range<usize>);
impl_token_iter_slice!(std::ops::RangeTo<usize>);
impl_token_iter_slice!(std::ops::RangeFrom<usize>);
impl_token_iter_slice!(std::ops::RangeFull);

impl<'a> std::ops::Index<usize> for TokenIter<'a> {
    type Output = Token;

    fn index(&self, i: usize) -> &Self::Output {
        &self.source[i]
    }
}

impl<'a> InputIter for TokenIter<'a> {
    type Item = &'a Token;
    type RawItem = Token;

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

#[cfg(test)]
mod test;
