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

//! Contains code for converting a UCG Val into the environment variable output target.
use std::io::Write;
use std::rc::Rc;

use ast::Positioned;
use build::Val;
use convert::traits::{Converter, Result};

/// EnvConverter implements the conversion logic for converting a Val into a
/// set of environment variables.
pub struct EnvConverter {}

impl EnvConverter {
    pub fn new() -> Self {
        EnvConverter {}
    }

    fn convert_tuple(&self, flds: &Vec<(Positioned<String>, Rc<Val>)>, w: &mut Write) -> Result {
        for &(ref name, ref val) in flds.iter() {
            if val.is_tuple() {
                eprintln!("Skipping embedded tuple...");
                return Ok(());
            }
            if let &Val::Empty = val.as_ref() {
                eprintln!("Skipping empty variable: {}", name);
                return Ok(());
            }
            try!(write!(w, "{}=", name.val));
            try!(self.write(&val, w));
        }
        Ok(())
    }

    fn convert_list(&self, _items: &Vec<Rc<Val>>, _w: &mut Write) -> Result {
        eprintln!("Skipping List...");
        Ok(())
    }

    fn write(&self, v: &Val, w: &mut Write) -> Result {
        match v {
            &Val::Empty => {
                // Empty is a noop.
                return Ok(());
            }
            &Val::Boolean(b) => {
                try!(write!(w, "{} ", if b { "true" } else { "false" }));
            }
            &Val::Float(ref f) => {
                try!(write!(w, "{} ", f));
            }
            &Val::Int(ref i) => {
                try!(write!(w, "{} ", i));
            }
            &Val::Str(ref s) => {
                try!(write!(w, "'{}' ", s));
            }
            &Val::List(ref items) => {
                try!(self.convert_list(items, w));
            }
            &Val::Tuple(ref flds) => {
                try!(self.convert_tuple(flds, w));
            }
            &Val::Macro(ref _def) => {
                // This is ignored
                eprintln!("Skipping macro...");
            }
        }
        Ok(())
    }
}

impl Converter for EnvConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut Write) -> Result {
        self.write(&v, &mut w)
    }
}
