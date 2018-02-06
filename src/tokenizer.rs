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
use nom_locate::LocatedSpan;
use nom;
use nom::{alpha, is_alphanumeric, digit, multispace};
use nom::{InputLength, InputIter, Slice};
use ast::*;
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

// TODO(jwall): Handle escapes
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

/// do_tag_tok! is a helper macro to make building a simple tag token
/// less code.
macro_rules! do_tag_tok {
    // NOTE(jwall): Nom macros do magic with their inputs. They in fact
    // rewrite your macro argumets for you. Which means we require this $i
    // paramater even though we don't explicitely pass it below. I don't
    // particularly like this but I'm living with it for now.
    ($i:expr, $type:expr, $tag:expr) => {
       do_parse!($i,
           span: position!() >>
           frag: tag!($tag) >>
           (Token{
               typ: $type,
               pos: Position::from(span),
               fragment: frag.fragment.to_string(),
           })
       )
    }
}

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

named!(lettok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "let")
);

named!(selecttok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "select")
);

named!(macrotok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "macro")
);

named!(importtok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "import")
);

named!(astok( Span ) -> Token,
       do_tag_tok!(TokenType::BAREWORD, "as")
);

fn end_of_input(input: Span) -> nom::IResult<Span, Token> {
    match eof!(input,) {
        nom::IResult::Done(_, _) => {
            return nom::IResult::Done(input,
                                      make_tok!(EOF => input.line as usize,
                                                input.get_column() as usize));
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
            match alt!(rest, take_until_and_consume!("\r\n") | take_until_and_consume!("\n")) {
                nom::IResult::Done(rest, cmt) => {
                    return nom::IResult::Done(rest,
                                              make_tok!(CMT => cmt.fragment.to_string(),
                                  input.line as usize,
                                  input.get_column() as usize));
                }
                // If we didn't find a new line then we just grab everything.
                _ => {
                    let blen = rest.input_len();
                    let next = rest.slice(blen..);
                    let tok = rest.slice(..blen);
                    return nom::IResult::Done(next,
                                              make_tok!(CMT => tok.fragment.to_string(),
                                  input.line as usize, input.get_column() as usize
                    ));
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
        barewordtok |
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
        fatcommatok | // Note fatcommatok must come before equaltok
        equaltok |
        semicolontok |
        leftsquarebracket |
        rightsquarebracket |
        lettok |
        selecttok |
        macrotok |
        importtok |
        astok |
        whitespace |
        end_of_input)
);

// TODO(jwall): This should return a ParseError instead.
pub fn tokenize(input: Span) -> Result<Vec<Token>, nom::ErrorKind> {
    let mut out = Vec::new();
    let mut i = input;
    loop {
        if i.input_len() == 0 {
            break;
        }
        match token(i) {
            nom::IResult::Error(e) => {
                return Err(e);
            }
            nom::IResult::Incomplete(_) => {
                return Err(nom::ErrorKind::Complete);
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

pub fn token_clone(t: &Token) -> Result<Token, ParseError> {
    Ok(t.clone())
}

macro_rules! match_type {
    ($i:expr, COMMENT => $h:expr) => {
        match_type!($i, TokenType::COMMENT, "Not a Comment", $h)
    };

    ($i:expr, COMMENT) => {
        match_type!($i, COMMENT => token_clone)
    };

    ($i:expr, BAREWORD => $h:expr) => {
        match_type!($i, TokenType::BAREWORD, "Not a Bareword", $h)
    };

    ($i:expr, BAREWORD) => {
        match_type!($i, BAREWORD => token_clone)
    };

    ($i:expr, STR => $h:expr) => {
        match_type!($i, TokenType::QUOTED, "Not a String", $h)
    };

    ($i:expr, STR) => {
        match_type!($i, STR => token_clone)
    };

    ($i:expr, DIGIT => $h:expr) => {
        match_type!($i, TokenType::DIGIT, "Not a DIGIT", $h)
    };

    ($i:expr, DIGIT) => {
        match_type!($i, DIGIT => token_clone)
    };

    ($i:expr, PUNCT => $h:expr) => {
        match_type!($i, TokenType::PUNCT, "Not PUNCTUATION", $h)
    };

    ($i:expr, PUNCT) => {
        match_type!($i, PUNCT => token_clone)
    };

    ($i:expr, $t:expr, $msg:expr, $h:expr) => {
        {
            let i_ = $i.clone();
            use nom::Slice;
            use std::convert::Into;
            if i_.input_len() == 0 {
                nom::IResult::Error(
                        nom::ErrorKind::Custom(ParseError{
                            description: format!("End of Input! {}", $msg),
                            pos: Position{line: 0, column: 0}
                        }))
            } else {
                let tok = &(i_[0]);
                if tok.typ == $t {
                    match $h(tok) {
                        Result::Ok(v) => nom::IResult::Done($i.slice(1..), v),
                        Result::Err(e) => nom::IResult::Error(
                            nom::ErrorKind::Custom(e.into())),
                    }
                } else {
                    nom::IResult::Error(nom::ErrorKind::Custom(ParseError{
                        description: $msg.to_string(),
                        pos: tok.pos.clone()}))
                }
            }
        }
    };
}

macro_rules! match_token {
    ($i:expr, PUNCT => $f:expr) => {
        match_token!($i, PUNCT => $f, token_clone)
    };

    ($i:expr, PUNCT => $f:expr, $h:expr) => {
        match_token!($i, TokenType::PUNCT, $f, format!("Not PUNCT ({})", $f), $h)
    };

    ($i:expr, BAREWORD => $f:expr) => {
        match_token!($i, BAREWORD => $f, token_clone)
    };

    ($i:expr, BAREWORD => $f:expr, $h:expr) => {
        match_token!($i, TokenType::BAREWORD, $f, format!("Not a BAREWORD ({})", $f), $h)
    };

    ($i:expr, $t:expr, $f:expr, $msg:expr, $h:expr) => {
        {
            let i_ = $i.clone();
            use nom::Slice;
            use std::convert::Into;
            let tok = &(i_[0]);
            if tok.typ == $t && &tok.fragment == $f {
                match $h(tok) {
                    Result::Ok(v) => nom::IResult::Done($i.slice(1..), v),
                    Result::Err(e) => nom::IResult::Error(
                        nom::ErrorKind::Custom(e.into())),
                }
            } else {
                nom::IResult::Error(nom::ErrorKind::Custom(ParseError{
                    description: format!("{} Instead is ({})", $msg, tok.fragment),
                    pos: tok.pos.clone()}))
            }
        }
    };
}

macro_rules! punct {
    ($i:expr, $c:expr) => {
        match_token!($i, PUNCT => $c)
    };
}

macro_rules! word {
    ($i:expr, $w:expr) => {
        match_token!($i, BAREWORD => $w)
    };
}

pub fn pos(i: TokenIter) -> nom::IResult<TokenIter, Position, ParseError> {
    let tok = &i[0];
    let line = tok.pos.line;
    let column = tok.pos.column;
    nom::IResult::Done(i.clone(),
                       Position {
                           line: line,
                           column: column,
                       })
}

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
    }
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
        where P: Fn(Self::RawItem) -> bool
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
mod tokenizer_test {
    use super::*;
    use nom;
    use nom_locate::LocatedSpan;

    #[test]
    fn test_escape_quoted() {
        let result = escapequoted(LocatedSpan::new("foo \\\"bar\""));
        assert!(result.is_done(), format!("result {:?} is not ok", result));
        if let nom::IResult::Done(rest, frag) = result {
            assert_eq!(frag, "foo \"bar");
            assert_eq!(rest.fragment, "\"");
        }
    }

    #[test]
    fn test_string_with_escaping() {
        let result = strtok(LocatedSpan::new("\"foo \\\\ \\\"bar\""));
        assert!(result.is_done(), format!("result {:?} is not ok", result));
        if let nom::IResult::Done(_, tok) = result {
            assert_eq!(tok.fragment, "foo \\ \"bar".to_string());
        }
    }

    #[test]
    fn test_tokenize_one_of_each() {
        //                                                                          1 1 1 1 1 1 1 1 1 1 2 2 2 2 2   2       2            2
        //                                       1  2      3     4      5  6  7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4   5       6            7
        let result = tokenize(LocatedSpan::new("let import macro select as => [ ] { } ; = % / * \
                                                + - . ( ) , 1 . foo \"bar\" // comment\n ;"));
        assert!(result.is_ok(), format!("result {:?} is not ok", result));
        let v = result.unwrap();
        for (i, t) in v.iter().enumerate() {
            println!("{}: {:?}", i, t);
        }
        assert_eq!(v.len(), 27);
        assert_eq!(v[26].typ, TokenType::END);
    }

    #[test]
    fn test_parse_has_end() {
        let result = tokenize(LocatedSpan::new("foo"));
        assert!(result.is_ok());
        let v = result.unwrap();
        assert_eq!(v.len(), 2);
        assert_eq!(v[1].typ, TokenType::END);
    }

    #[test]
    fn test_parse_comment() {
        assert!(comment(LocatedSpan::new("// comment\n")).is_done());
        assert!(comment(LocatedSpan::new("// comment")).is_done());
        assert_eq!(comment(LocatedSpan::new("// comment\n")),
            nom::IResult::Done(LocatedSpan{fragment: "", offset: 11, line: 2},
            Token{
                typ: TokenType::COMMENT,
                fragment: " comment".to_string(),
                pos: Position{line: 1, column: 1},
            }));
        assert!(comment(LocatedSpan::new("// comment\r\n")).is_done());
        assert_eq!(comment(LocatedSpan::new("// comment\r\n")),
            nom::IResult::Done(LocatedSpan{fragment: "", offset: 12, line: 2},
                Token{
                    typ: TokenType::COMMENT,
                    fragment: " comment".to_string(),
                    pos: Position{column: 1, line: 1}
                }));
        assert!(comment(LocatedSpan::new("// comment\r\n ")).is_done());
        assert_eq!(comment(LocatedSpan::new("// comment\r\n ")),
            nom::IResult::Done(LocatedSpan{fragment: " ", offset: 12, line: 2},
                Token{
                    typ: TokenType::COMMENT,
                    fragment: " comment".to_string(),
                    pos: Position{column: 1, line: 1},
                }));
        assert!(comment(LocatedSpan::new("// comment")).is_done());
    }

    #[test]
    fn test_match_word() {
        let input = vec![Token{
            fragment: "foo".to_string(),
            typ: TokenType::BAREWORD,
            pos: Position{line: 1, column: 1}
        }];
        let result = word!(TokenIter{source: input.as_slice()}, "foo");
        match result {
            nom::IResult::Done(_, tok) => assert_eq!(tok, input[0]),
            res => assert!(false, format!("Fail: {:?}", res)),
        }
    }

    #[test]
    fn test_match_word_empty_input() {
        let input = vec![Token{
            fragment: "".to_string(),
            typ: TokenType::END,
            pos: Position{line: 1, column: 1},
        }];
        let result = word!(TokenIter{source: input.as_slice()}, "foo");
        match result {
            nom::IResult::Done(_, _) => assert!(false, "Should have been an error but was Done"),
            nom::IResult::Incomplete(_) => {
                assert!(false, "Should have been an error but was Incomplete")
            }
            nom::IResult::Error(_) => {
                // noop
            }
        }
    }

    #[test]
    fn test_match_punct() {
        let input = vec![Token{
            fragment: "!".to_string(),
            typ: TokenType::PUNCT,
            pos: Position{line: 1, column: 1}
        }];
        let result = punct!(TokenIter{source: input.as_slice()}, "!");
        match result {
            nom::IResult::Done(_, tok) => assert_eq!(tok, input[0]),
            res => assert!(false, format!("Fail: {:?}", res)),
        }
    }

    #[test]
    fn test_match_type() {
        let input = vec![Token{
            fragment: "foo".to_string(),
            typ: TokenType::BAREWORD,
            pos: Position{line: 1, column: 1}
        }];
        let result = match_type!(TokenIter{source: input.as_slice()}, BAREWORD);
        match result {
            nom::IResult::Done(_, tok) => assert_eq!(tok, input[0]),
            res => assert!(false, format!("Fail: {:?}", res)),
        }
    }
}
