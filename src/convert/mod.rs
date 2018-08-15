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

//! The conversion stage of the ucg compiler.
pub mod env;
pub mod exec;
pub mod flags;
pub mod json;
pub mod traits;

use std::io::Write;
use std::rc::Rc;

use build::Val;

/// ConverterRunner knows how to run a given converter on a Val.
pub struct ConverterRunner {
    converter: Box<traits::Converter>,
}

impl ConverterRunner {
    /// new creates a new ConverterRunner with a converter for the provided output target.
    ///
    /// * flags
    /// * json
    /// * env
    /// * exec
    pub fn new(typ: &str) -> Result<Self, String> {
        if typ == "flags" {
            return Ok(ConverterRunner {
                converter: Box::new(flags::FlagConverter::new()),
            });
        }
        if typ == "json" {
            return Ok(ConverterRunner {
                converter: Box::new(json::JsonConverter::new()),
            });
        }
        if typ == "env" {
            return Ok(ConverterRunner {
                converter: Box::new(env::EnvConverter::new()),
            });
        }
        if typ == "exec" {
            return Ok(ConverterRunner {
                converter: Box::new(exec::ExecConverter::new()),
            });
        }
        return Err(format!("Unknown Target output type: {}", typ));
    }

    /// convert runs the Converter on a Val and writes the output to the provided writer.
    pub fn convert(&self, v: Rc<Val>, mut w: Box<Write>) -> traits::Result {
        self.converter.convert(v, &mut w)
    }

    /// ext returns the expected file extension for this conversion.
    pub fn ext(&self) -> String {
        self.converter.file_ext()
    }
}
