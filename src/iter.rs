//! Input stream wrappers for use in abortable_parser.
use std::convert::From;
use std::iter::Iterator;
use std::path::PathBuf;

use abortable_parser::iter::{SliceIter, StrIter};
use abortable_parser::{InputIter, Offsetable, Peekable, Positioned, Seekable, Span, SpanRange};

use crate::ast::{Position, Token};

#[derive(Debug)]
pub struct OffsetStrIter<'a> {
    source_file: Option<PathBuf>,
    contained: StrIter<'a>,
    line_offset: usize,
    col_offset: usize,
}

impl<'a> OffsetStrIter<'a> {
    pub fn new(input: &'a str) -> Self {
        Self::new_with_offsets(input, 0, 0)
    }

    pub fn new_with_offsets(input: &'a str, line_offset: usize, col_offset: usize) -> Self {
        OffsetStrIter {
            source_file: None,
            contained: StrIter::new(input),
            line_offset: line_offset,
            col_offset: col_offset,
        }
    }

    pub fn with_src_file<P: Into<PathBuf>>(mut self, file: P) -> Self {
        self.source_file = Some(file.into());
        self
    }
}

impl<'a> Iterator for OffsetStrIter<'a> {
    type Item = &'a u8;

    fn next(&mut self) -> Option<Self::Item> {
        self.contained.next()
    }
}

impl<'a> Offsetable for OffsetStrIter<'a> {
    fn get_offset(&self) -> usize {
        self.contained.get_offset()
    }
}

impl<'a> Clone for OffsetStrIter<'a> {
    fn clone(&self) -> Self {
        OffsetStrIter {
            source_file: self.source_file.clone(),
            contained: self.contained.clone(),
            line_offset: self.line_offset,
            col_offset: self.col_offset,
        }
    }
}

impl<'a> From<&'a str> for OffsetStrIter<'a> {
    fn from(source: &'a str) -> Self {
        OffsetStrIter {
            source_file: None,
            contained: StrIter::new(source),
            line_offset: 0,
            col_offset: 0,
        }
    }
}

impl<'a> Seekable for OffsetStrIter<'a> {
    fn seek(&mut self, to: usize) -> usize {
        self.contained.seek(to)
    }
}

impl<'a> Span<&'a str> for OffsetStrIter<'a> {
    fn span(&self, idx: SpanRange) -> &'a str {
        self.contained.span(idx)
    }
}

impl<'a> Peekable<&'a u8> for OffsetStrIter<'a> {
    fn peek_next(&self) -> Option<&'a u8> {
        self.contained.peek_next()
    }
}

impl<'a> Positioned for OffsetStrIter<'a> {
    fn line(&self) -> usize {
        self.contained.line() + self.line_offset
    }

    fn column(&self) -> usize {
        self.contained.column() + self.col_offset
    }
}

impl<'a> InputIter for OffsetStrIter<'a> {
    fn curr(&self) -> Self::Item {
        self.clone().peek_next().unwrap()
    }
}

impl<'a> From<&'a SliceIter<'a, Token>> for Position {
    fn from(source: &'a SliceIter<'a, Token>) -> Self {
        match source.peek_next() {
            Some(t) => t.pos.clone(),
            None => source.curr().pos.clone(),
        }
    }
}

impl<'a> From<&'a OffsetStrIter<'a>> for Position {
    fn from(s: &'a OffsetStrIter<'a>) -> Position {
        Position {
            file: s.source_file.clone(),
            line: s.line(),
            column: s.column(),
            offset: s.get_offset(),
        }
    }
}

pub trait FilePositioned: Positioned {
    fn file(&self) -> Option<&PathBuf>;
}

impl<'a> FilePositioned for OffsetStrIter<'a> {
    fn file(&self) -> Option<&PathBuf> {
        self.source_file.as_ref()
    }
}

impl<'a> FilePositioned for SliceIter<'a, Token> {
    fn file(&self) -> Option<&PathBuf> {
        match self.peek_next() {
            Some(t) => t.pos.file.as_ref(),
            None => None,
        }
    }
}
