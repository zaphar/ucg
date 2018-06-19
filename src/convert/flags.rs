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

//! Contains code for converting a UCG Val into the command line flag output target.
use std::io::Write;
use std::rc::Rc;

use build::Val;
use convert::traits::{Converter, Result};

/// FlagConverter implements the conversion logic for converting a Val into a set of command line flags.
pub struct FlagConverter {}

impl FlagConverter {
    pub fn new() -> Self {
        FlagConverter {}
    }

    fn write_flag_name(&self, pfx: &str, name: &str, w: &mut Write) -> Result {
        if name.chars().count() > 1 || pfx.chars().count() > 0 {
            try!(write!(w, "--{}{} ", pfx, name));
        } else {
            try!(write!(w, "-{} ", name));
        }
        return Ok(());
    }

    fn write_list_flag(&self, pfx: &str, name: &str, def: &Vec<Rc<Val>>, w: &mut Write) -> Result {
        // first of all we need to make sure that each &Val is only a primitive type.
        for v in def.iter() {
            let vref = v.as_ref();
            if vref.is_list() || vref.is_tuple() || vref.is_macro() {
                eprintln!(
                    "Skipping non primitive val in list for flag {}{}",
                    pfx, name
                );
            } else {
                try!(self.write_flag_name(pfx, name, w));
                try!(self.write(pfx, vref, w));
            }
        }
        return Ok(());
    }

    fn write(&self, pfx: &str, v: &Val, w: &mut Write) -> Result {
        match v {
            &Val::Empty => {
                // Empty is a noop.
                return Ok(());
            }
            &Val::Boolean(b) => {
                try!(write!(w, "{}", if b { "true" } else { "false" }));
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
            &Val::List(ref _def) => {
                eprintln!("Skipping List...");
            }
            &Val::Tuple(ref flds) => for &(ref name, ref val) in flds.iter() {
                if let &Val::Empty = val.as_ref() {
                    try!(self.write_flag_name(pfx, &name.val, w));
                    continue;
                }
                match val.as_ref() {
                    &Val::Tuple(_) => {
                        let new_pfx = format!("{}{}.", pfx, name);
                        try!(self.write(&new_pfx, val, w));
                    }
                    &Val::List(ref def) => {
                        try!(self.write_list_flag(pfx, &name.val, def, w));
                    }
                    _ => {
                        try!(self.write_flag_name(pfx, &name.val, w));
                        try!(self.write(pfx, &val, w));
                    }
                }
            },
            &Val::Macro(ref _def) => {
                // This is ignored
                eprintln!("Skipping macro...");
            }
        }
        Ok(())
    }
}

impl Converter for FlagConverter {
    fn convert(&self, v: Rc<Val>, mut w: Box<Write>) -> Result {
        self.write("", &v, &mut w)
    }
}

// We need some unit tests for this now :D
