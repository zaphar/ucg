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

use std::rc::Rc;
use std::io::Write;
use std::io::Result;

use build::Val;
use convert::traits::Converter;

pub struct FlagConverter {}

impl FlagConverter {
    pub fn new() -> Self {
        FlagConverter {}
    }

    fn write(&self, v: &Val, w: &mut Write) -> Result<()> {
        match v {
            &Val::Float(ref f) => {
                try!(write!(w, "{} ", f));
            }
            &Val::Int(ref i) => {
                try!(write!(w, "{} ", i));
            }
            &Val::String(ref s) => {
                try!(write!(w, "'{}' ", s));
            }
            &Val::List(ref _def) => {
                // FIXME(jwall): Fill this in?
                eprintln!("Skipping List...");
            }
            &Val::Tuple(ref flds) => {
                for &(ref name, ref val) in flds.iter() {
                    if val.is_tuple() {
                        eprintln!("Skipping embedded tuple...");
                        return Ok(());
                    }
                    try!(write!(w, "--{} ", name.val));
                    // TODO(jwall): What if the value is a tuple?
                    try!(self.write(&val, w));
                }
            }
            &Val::Macro(ref _def) => {
                // This is ignored
                eprintln!("Skipping macro...");
            }
        }
        Ok(())
    }
}

impl Converter for FlagConverter {
    fn convert(&self, v: Rc<Val>, mut w: Box<Write>) -> Result<()> {
        self.write(&v, &mut w)
    }
}
