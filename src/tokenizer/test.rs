use std::collections::BTreeMap;

use super::*;

use abortable_parser::{Offsetable, Result, SliceIter};

use crate::iter::OffsetStrIter;

#[test]
fn test_empty_token() {
    let result = emptytok(OffsetStrIter::new("NULL "));
    assert!(result.is_complete(), "result {:?} is not done", result);
    if let Result::Complete(_, tok) = result {
        assert_eq!(tok.fragment.as_ref(), "NULL");
        assert_eq!(tok.typ, TokenType::EMPTY);
    }
}

#[test]
fn test_assert_token() {
    let result = asserttok(OffsetStrIter::new("assert "));
    assert!(result.is_complete(), "result {:?} is not done", result);
    if let Result::Complete(_, tok) = result {
        assert_eq!(tok.fragment.as_ref(), "assert");
        assert_eq!(tok.typ, TokenType::BAREWORD);
    }
}

#[test]
fn test_out_token() {
    let result = outtok(OffsetStrIter::new("out "));
    assert!(result.is_complete(), "result {:?} is not done", result);
    if let Result::Complete(_, tok) = result {
        assert_eq!(tok.fragment.as_ref(), "out");
        assert_eq!(tok.typ, TokenType::BAREWORD);
    }
}

#[test]
fn test_out_token_with_comment() {
    let result = outtok(OffsetStrIter::new("out//comment"));
    assert!(result.is_complete(), "result {:?} is not done", result);
    if let Result::Complete(_, tok) = result {
        assert_eq!(tok.fragment.as_ref(), "out");
        assert_eq!(tok.typ, TokenType::BAREWORD);
    }
}

#[test]
fn test_not_out_token() {
    let result = outtok(OffsetStrIter::new("output"));
    assert!(result.is_fail(), "result {:?} is not fail", result);
}

#[test]
fn test_escape_quoted() {
    let result = escapequoted(OffsetStrIter::new("foo \\\"bar\""));
    assert!(result.is_complete(), "result {:?} is not ok", result);
    if let Result::Complete(_rest, frag) = result {
        assert_eq!(frag, "foo \"bar");
    }
}

#[test]
fn test_string_with_escaping() {
    let result = strtok(OffsetStrIter::new("\"foo \\\\ \\\"bar\""));
    assert!(result.is_complete(), "result {:?} is not ok", result);
    if let Result::Complete(_, tok) = result {
        assert_eq!(tok.fragment.as_ref(), "foo \\ \"bar".to_string());
    }
}

#[test]
fn test_tokenize_bareword_with_dash() {
    let input = OffsetStrIter::new("foo-bar ");
    let result = tokenize(input.clone(), None);
    assert!(result.is_ok(), "result {:?} is not ok", result);
    if let Ok(toks) = result {
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].fragment.as_ref(), "foo-bar");
    }
}

macro_rules! assert_token {
    ($input:expr, $typ:expr, $msg:expr) => {
        let result = token(OffsetStrIter::new($input));
        assert!(
            result.is_complete(),
            "result {:?} is not a {}",
            result,
            $msg
        );
        if let Result::Complete(_, tok) = result {
            assert_eq!(tok.typ, $typ);
            assert_eq!(tok.fragment.as_ref(), $input);
        }
    };
}

#[test]
fn test_digittok() {
    assert_token!("1", TokenType::DIGIT, "1");
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
    let input = OffsetStrIter::new(
        "map out filter assert let import func select as => [ ] { } ; = % / * \
         + - . ( ) , 1 . foo \"bar\" // comment\n ; true false == < > <= >= !=",
    );
    let result = tokenize(input.clone(), None);
    assert!(result.is_ok(), "result {:?} is not ok", result);
    let v = result.unwrap();
    for (i, t) in v.iter().enumerate() {
        println!("{}: {:?}", i, t);
    }
    assert_eq!(v.len(), 39);
    assert_eq!(v[38].typ, TokenType::END);
}

#[test]
fn test_tokenize_one_of_each_comment_map_path() {
    let input = OffsetStrIter::new(
        "map out filter assert let import func select as => [ ] { } ; = % / * \
         + - . ( ) , 1 . foo \"bar\" // comment\n ; true false == < > <= >= !=",
    );
    let mut comment_map = BTreeMap::new();
    let result = tokenize(input.clone(), Some(&mut comment_map));
    assert!(result.is_ok(), "result {:?} is not ok", result);
    let v = result.unwrap();
    for (i, t) in v.iter().enumerate() {
        println!("{}: {:?}", i, t);
    }
    assert_eq!(v.len(), 39);
    assert_eq!(v[38].typ, TokenType::END);
}

#[test]
fn test_parse_has_end() {
    let input = OffsetStrIter::new("foo");
    let result = tokenize(input.clone(), None);
    assert!(result.is_ok());
    let v = result.unwrap();
    assert_eq!(v.len(), 2);
    assert_eq!(v[1].typ, TokenType::END);
}

#[test]
fn test_whitespace() {
    assert!(whitespace(OffsetStrIter::new("    ")).is_complete());
    let result = whitespace(OffsetStrIter::new("  "));
    match result {
        Result::Complete(rest, o) => {
            assert_eq!(rest.get_offset(), 2);
            assert_eq!(o.typ, TokenType::WS);
        }
        _ => assert!(false, "Not complete"),
    }
}

#[test]
fn test_parse_comment() {
    assert!(comment(OffsetStrIter::new("// comment\n")).is_complete());
    assert!(comment(OffsetStrIter::new("// comment")).is_complete());
    let mut parsed = comment(OffsetStrIter::new("// comment\n"));
    assert!(parsed.is_complete());
    if let Result::Complete(_rest, cmt) = parsed {
        assert_eq!(
            cmt,
            Token {
                typ: TokenType::COMMENT,
                fragment: " comment".into(),
                pos: Position {
                    file: None,
                    line: 1,
                    column: 1,
                    offset: 0
                },
            }
        );
    }
    assert!(comment(OffsetStrIter::new("// comment\r\n")).is_complete());
    parsed = comment(OffsetStrIter::new("// comment\r\n"));
    if let Result::Complete(_rest, cmt) = parsed {
        assert_eq!(
            cmt,
            Token {
                typ: TokenType::COMMENT,
                fragment: " comment".into(),
                pos: Position {
                    file: None,
                    column: 1,
                    line: 1,
                    offset: 0
                },
            }
        );
    }
    assert!(comment(OffsetStrIter::new("// comment\r\n ")).is_complete());
    parsed = comment(OffsetStrIter::new("// comment\r\n "));
    if let Result::Complete(_rest, cmt) = parsed {
        assert_eq!(
            cmt,
            Token {
                typ: TokenType::COMMENT,
                fragment: " comment".into(),
                pos: Position {
                    file: None,
                    column: 1,
                    line: 1,
                    offset: 0
                },
            }
        );
    }
    assert!(comment(OffsetStrIter::new("// comment")).is_complete());
}

#[test]
fn test_match_word() {
    let input = vec![Token {
        fragment: "foo".into(),
        typ: TokenType::BAREWORD,
        pos: Position {
            file: None,
            line: 1,
            column: 1,
            offset: 0,
        },
    }];
    let result = word!(SliceIter::new(input.as_slice()), "foo");
    match result {
        Result::Complete(_, tok) => assert_eq!(tok, input[0]),
        res => assert!(false, "Fail: {:?}", res),
    }
}

#[test]
fn test_match_word_empty_input() {
    let input = vec![Token {
        fragment: "".into(),
        typ: TokenType::END,
        pos: Position {
            file: None,
            line: 1,
            column: 1,
            offset: 0,
        },
    }];
    let result = word!(SliceIter::new(input.as_slice()), "foo");
    match result {
        Result::Complete(_, _) => assert!(false, "Should have been an error but was Done"),
        Result::Incomplete(_) => assert!(false, "Should have been a Fail but was Incomplete"),
        Result::Fail(_) => {
            // noop
        }
        Result::Abort(_) => assert!(false, "Should have been a Fail but was Abort"),
    }
}

#[test]
fn test_match_punct() {
    let input = vec![Token {
        fragment: "!".into(),
        typ: TokenType::PUNCT,
        pos: Position {
            file: None,
            line: 1,
            column: 1,
            offset: 0,
        },
    }];
    let result = punct!(SliceIter::new(input.as_slice()), "!");
    match result {
        Result::Complete(_, tok) => assert_eq!(tok, input[0]),
        res => assert!(false, "Fail: {:?}", res),
    }
}

#[test]
fn test_match_type() {
    let input = vec![Token {
        fragment: "foo".into(),
        typ: TokenType::BAREWORD,
        pos: Position {
            file: None,
            line: 1,
            column: 1,
            offset: 0,
        },
    }];
    let result = match_type!(SliceIter::new(input.as_slice()), BAREWORD);
    match result {
        Result::Complete(_, tok) => assert_eq!(tok, input[0]),
        res => assert!(false, "Fail: {:?}", res),
    }
}

#[test]
fn test_tokenize_builds_comment_map() {
    let input = OffsetStrIter::new("// comment 1\n\n//comment 2");
    let mut comment_map = BTreeMap::new();
    let result = tokenize(input.clone(), Some(&mut comment_map));
    assert!(result.is_ok(), "result {:?} is not ok", result);

    assert_eq!(comment_map.len(), 2);
}

#[test]
fn test_tokenize_builds_comment_map_groups() {
    let input = OffsetStrIter::new("// first part\n// comment 1\n\n//comment 2");
    let mut comment_map = BTreeMap::new();
    let result = tokenize(input.clone(), Some(&mut comment_map));
    assert!(result.is_ok(), "result {:?} is not ok", result);

    assert_eq!(comment_map.len(), 2);
    assert_eq!(comment_map[&2].len(), 2);
}
