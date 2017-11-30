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
pub mod flags;
pub mod traits;

use std::io;
use std::io::Write;
use std::rc::Rc;

use build::Val;

pub struct ConverterRunner {
    converter: Box<traits::Converter>,
}

impl ConverterRunner {
    pub fn new(typ: &str) -> Result<Self, String> {
        if typ == "flags" {
            return Ok(ConverterRunner { converter: Box::new(flags::FlagConverter::new()) });
        }
        return Err(format!("Unknown Target output type: {}", typ));
    }

    pub fn convert(&self, v: Rc<Val>, w: Box<Write>) -> io::Result<()> {
        self.converter.convert(v, w)
    }
}
