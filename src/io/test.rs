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
use crate::io::*;

#[test]
fn test_line_accumulator_single_line_stmt() {
    let test_line = "1 + 1;";
    let mut expected = String::new();
    expected.push_str(test_line);
    expected.push_str("\n");
    let mut acc = StatementAccumulator::new();
    acc.push(test_line);
    assert_eq!(acc.get_statement().unwrap(), expected);
}

#[test]
fn test_line_accumulator_multi_line_stmt() {
    let test_line1 = "1 ";
    let test_line2 = "+ 1;";
    let mut expected = String::new();
    expected.push_str(test_line1);
    expected.push_str("\n");
    expected.push_str(test_line2);
    expected.push_str("\n");
    let mut acc = StatementAccumulator::new();
    acc.push(test_line1);
    acc.push(test_line2);
    assert_eq!(acc.get_statement().unwrap(), expected);
}
