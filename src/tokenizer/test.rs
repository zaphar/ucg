use super::*;
use nom;
use nom_locate::LocatedSpan;

#[test]
fn test_empty_token() {
    let result = emptytok(LocatedSpan::new("NULL "));
    assert!(result.is_done(), format!("result {:?} is not done", result));
    if let nom::IResult::Done(_, tok) = result {
        assert_eq!(tok.fragment, "NULL");
        assert_eq!(tok.typ, TokenType::EMPTY);
    }
}

#[test]
fn test_assert_token() {
    let result = asserttok(LocatedSpan::new("assert "));
    assert!(result.is_done(), format!("result {:?} is not done", result));
    if let nom::IResult::Done(_, tok) = result {
        assert_eq!(tok.fragment, "assert");
        assert_eq!(tok.typ, TokenType::BAREWORD);
    }
}

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
fn test_tokenize_bareword_with_dash() {
    let result = tokenize(LocatedSpan::new("foo-bar "));
    assert!(result.is_ok(), format!("result {:?} is not ok", result));
    if let Ok(toks) = result {
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].fragment, "foo-bar");
    }
}

macro_rules! assert_token {
    ($input:expr, $typ:expr, $msg:expr) => {
        let result = token(LocatedSpan::new($input));
        assert!(
            result.is_done(),
            format!("result {:?} is not a {}", result, $msg)
        );
        if let nom::IResult::Done(_, tok) = result {
            assert_eq!(tok.fragment, $input);
            assert_eq!(tok.typ, $typ);
        }
    };
}

#[test]
fn test_boolean() {
    assert_token!("true", TokenType::BOOLEAN, "boolean");
}

#[test]
fn test_eqeqtok() {
    assert_token!("==", TokenType::PUNCT, "==");
}

#[test]
fn test_notequaltok() {
    assert_token!("!=", TokenType::PUNCT, "!=");
}

#[test]
fn test_gttok() {
    assert_token!(">", TokenType::PUNCT, ">");
}

#[test]
fn test_lttok() {
    assert_token!("<", TokenType::PUNCT, "<");
}

#[test]
fn test_gteqtok() {
    assert_token!(">=", TokenType::PUNCT, ">=");
}

#[test]
fn test_lteqtok() {
    assert_token!("<=", TokenType::PUNCT, "<=");
}

#[test]
fn test_tokenize_one_of_each() {
    let result = tokenize(LocatedSpan::new(
        "map filter assert let import macro select as => [ ] { } ; = % / * \
         + - . ( ) , 1 . foo \"bar\" // comment\n ; true false == < > <= >= !=",
    ));
    assert!(result.is_ok(), format!("result {:?} is not ok", result));
    let v = result.unwrap();
    for (i, t) in v.iter().enumerate() {
        println!("{}: {:?}", i, t);
    }
    assert_eq!(v.len(), 38);
    assert_eq!(v[37].typ, TokenType::END);
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
    assert_eq!(
        comment(LocatedSpan::new("// comment\n")),
        nom::IResult::Done(
            LocatedSpan {
                fragment: "",
                offset: 11,
                line: 2,
            },
            Token {
                typ: TokenType::COMMENT,
                fragment: " comment".to_string(),
                pos: Position { line: 1, column: 1 },
            }
        )
    );
    assert!(comment(LocatedSpan::new("// comment\r\n")).is_done());
    assert_eq!(
        comment(LocatedSpan::new("// comment\r\n")),
        nom::IResult::Done(
            LocatedSpan {
                fragment: "",
                offset: 12,
                line: 2,
            },
            Token {
                typ: TokenType::COMMENT,
                fragment: " comment".to_string(),
                pos: Position { column: 1, line: 1 },
            }
        )
    );
    assert!(comment(LocatedSpan::new("// comment\r\n ")).is_done());
    assert_eq!(
        comment(LocatedSpan::new("// comment\r\n ")),
        nom::IResult::Done(
            LocatedSpan {
                fragment: " ",
                offset: 12,
                line: 2,
            },
            Token {
                typ: TokenType::COMMENT,
                fragment: " comment".to_string(),
                pos: Position { column: 1, line: 1 },
            }
        )
    );
    assert!(comment(LocatedSpan::new("// comment")).is_done());
}

#[test]
fn test_match_word() {
    let input = vec![Token {
        fragment: "foo".to_string(),
        typ: TokenType::BAREWORD,
        pos: Position { line: 1, column: 1 },
    }];
    let result = word!(
        TokenIter {
            source: input.as_slice(),
        },
        "foo"
    );
    match result {
        nom::IResult::Done(_, tok) => assert_eq!(tok, input[0]),
        res => assert!(false, format!("Fail: {:?}", res)),
    }
}

#[test]
fn test_match_word_empty_input() {
    let input = vec![Token {
        fragment: "".to_string(),
        typ: TokenType::END,
        pos: Position { line: 1, column: 1 },
    }];
    let result = word!(
        TokenIter {
            source: input.as_slice(),
        },
        "foo"
    );
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
    let input = vec![Token {
        fragment: "!".to_string(),
        typ: TokenType::PUNCT,
        pos: Position { line: 1, column: 1 },
    }];
    let result = punct!(
        TokenIter {
            source: input.as_slice(),
        },
        "!"
    );
    match result {
        nom::IResult::Done(_, tok) => assert_eq!(tok, input[0]),
        res => assert!(false, format!("Fail: {:?}", res)),
    }
}

#[test]
fn test_match_type() {
    let input = vec![Token {
        fragment: "foo".to_string(),
        typ: TokenType::BAREWORD,
        pos: Position { line: 1, column: 1 },
    }];
    let result = match_type!(
        TokenIter {
            source: input.as_slice(),
        },
        BAREWORD
    );
    match result {
        nom::IResult::Done(_, tok) => assert_eq!(tok, input[0]),
        res => assert!(false, format!("Fail: {:?}", res)),
    }
}
