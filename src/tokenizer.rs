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
use nom::{alpha, is_alphanumeric, digit};

use ast::*;

type Span<'a> = LocatedSpan<&'a str>;

impl<'a> From<Span<'a>> for Position {
    fn from(s: Span) -> Position {
        Position {
            line: s.line as usize,
            column: s.get_column() as usize,
        }
    }
}

pub struct Token<'a> {
    pub pos: Position,
    pub fragment: &'a str,
}


fn is_symbol_char(c: char) -> bool {
    is_alphanumeric(c as u8) || c == '-' as char || c == '_' as char
}

named!(pub strtok( Span ) -> Token,
       do_parse!(
           span: position!() >>
               tag!("\"") >>
               frag: take_until!("\"") >>
               tag!("\"") >>
           (Token{
               pos: Position::from(span),
               fragment: frag.fragment,
           })
       )
);

named!(pub barewordtok( Span ) -> Token,
       do_parse!(
           span: position!() >>
           frag: preceded!(peek!(alpha), take_while!(is_symbol_char)) >>
           (Token{
               pos: Position::from(span),
               fragment: frag.fragment,
           })
       )
);

named!(pub digittok( Span ) -> Token,
       do_parse!(
           span: position!() >>
               digits: digit >>
               (Token{
                   pos: Position::from(span),
                   fragment: digits.fragment,
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
    ($i:expr, $tag:expr) => {
       do_parse!($i,
           span: position!() >>
           frag: tag!($tag) >>
           (Token{
               pos: Position::from(span),
               fragment: frag.fragment,
           })
       )
    }
}

named!(pub commatok( Span ) -> Token,
       do_tag_tok!(",")
);

named!(pub lbracetok( Span ) -> Token,
       do_tag_tok!("{")
);

named!(pub rbracetok( Span ) -> Token,
       do_tag_tok!("}")
);

named!(pub lparentok( Span ) -> Token,
       do_tag_tok!("(")
);

named!(pub rparentok( Span ) -> Token,
       do_tag_tok!(")")
);

named!(pub dottok( Span ) -> Token,
       do_tag_tok!(".")
);

named!(pub plustok( Span ) -> Token,
       do_tag_tok!("+")
);

named!(pub dashtok( Span ) -> Token,
       do_tag_tok!("-")
);

named!(pub startok( Span ) -> Token,
       do_tag_tok!("*")
);

named!(pub slashtok( Span ) -> Token,
       do_tag_tok!("/")
);

named!(pub equaltok( Span ) -> Token,
       do_tag_tok!("=")
);

named!(pub semicolontok( Span ) -> Token,
       do_tag_tok!(";")
);

named!(pub fatcommatok( Span ) -> Token,
       do_tag_tok!("=>")
);

named!(pub lettok( Span ) -> Token,
       do_tag_tok!("let")
);

named!(pub selecttok( Span ) -> Token,
       do_tag_tok!("select")
);

named!(pub macrotok( Span ) -> Token,
       do_tag_tok!("macro")
);

named!(pub importtok( Span ) -> Token,
       do_tag_tok!("import")
);

named!(pub astok( Span ) -> Token,
       do_tag_tok!("as")
);
