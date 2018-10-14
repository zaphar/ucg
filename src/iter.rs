use std::convert::From;
use std::iter::Iterator;

use abortable_parser::iter::StrIter;
use abortable_parser::{
    InputIter, Offsetable, Peekable, Seekable, Span, SpanRange, TextPositionTracker,
};

#[derive(Debug)]
pub struct OffsetStrIter<'a> {
    contained: StrIter<'a>,
    idx_offset: usize,
    line_offset: usize,
    col_offset: usize,
}

impl<'a> OffsetStrIter<'a> {
    pub fn new(input: &'a str) -> Self {
        Self::new_with_offsets(input, 0, 0, 0)
    }

    pub fn new_with_offsets(
        input: &'a str,
        idx_offset: usize,
        line_offset: usize,
        col_offset: usize,
    ) -> Self {
        OffsetStrIter {
            contained: StrIter::new(input),
            idx_offset: idx_offset,
            line_offset: line_offset,
            col_offset: col_offset,
        }
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
        self.contained.get_offset() + self.idx_offset
    }
}

impl<'a> Clone for OffsetStrIter<'a> {
    fn clone(&self) -> Self {
        OffsetStrIter {
            contained: self.contained.clone(),
            idx_offset: self.idx_offset,
            line_offset: self.line_offset,
            col_offset: self.col_offset,
        }
    }
}

impl<'a> From<&'a str> for OffsetStrIter<'a> {
    fn from(source: &'a str) -> Self {
        OffsetStrIter {
            contained: StrIter::new(source),
            idx_offset: 0,
            line_offset: 0,
            col_offset: 0,
        }
    }
}

impl<'a> Seekable for OffsetStrIter<'a> {
    fn seek(&mut self, to: usize) -> usize {
        let contained_offset = self.contained.seek(to);
        self.idx_offset += contained_offset;
        contained_offset
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

impl<'a> TextPositionTracker for OffsetStrIter<'a> {
    fn line(&self) -> usize {
        self.contained.line() + self.line_offset
    }

    fn column(&self) -> usize {
        self.contained.column() + self.col_offset
    }
}

impl<'a> InputIter for OffsetStrIter<'a> {}
