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

//! The traits used by the ucg compiler for converting Val intermediate format into the output formats..
use std::error::Error;
use std::io::Write;
use std::rc::Rc;
use std::result;

use crate::build::Val;

pub type Result = result::Result<(), Box<dyn Error>>;

/// The trait that Converters from Val to different output formats for the
/// final conversion stage of the ucg compiler.
pub trait Converter {
    fn convert(&self, vs: Rc<Val>, w: &mut Write) -> Result;
    fn file_ext(&self) -> String;
    fn description(&self) -> String;
}
