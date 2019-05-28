// Copyright 2019 Jeremy Wall
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
use std::convert::Into;

/// Accumulates lines and tells you when a ucg Statement has been read.
pub struct StatementAccumulator {
    acc: Vec<String>,
}

impl StatementAccumulator {
    /// Constructs a StatementAccumulator ready for use.
    pub fn new() -> Self {
        Self { acc: Vec::new() }
    }

    pub fn next_line(&self) -> usize {
        self.acc.len() + 1
    }

    /// Tells you if the latest line ends in the statement terminator.
    ///
    /// Returns None if it wasn't a terminated statement and leaves the
    /// accumulated lines alone.
    ///
    /// Returns Some(String) with the terminated statement if it was a
    /// terminated statement and drains the accumulated statements out.
    pub fn get_statement(&mut self) -> Option<String> {
        if let Some(l) = self.acc.last() {
            if l.trim_end().ends_with(";") {
                let mut stmt = self.acc.drain(0..).fold(String::new(), |mut acc, s| {
                    acc.push_str(&s);
                    acc.push_str("\n");
                    acc
                });
                stmt.shrink_to_fit();
                return Some(stmt);
            }
        }
        None
    }

    /// Pushes a line into the Statement Accumulator. Assumes that the
    /// new line has already been trimmed. get_statement will reintroduce
    /// the new lines.
    pub fn push<S: Into<String>>(&mut self, line: S) {
        self.acc.push(line.into());
    }
}

#[cfg(test)]
mod test;
