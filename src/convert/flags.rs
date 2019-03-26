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
use std::fmt::Write as FmtWrite;
use std::io::Write;
use std::rc::Rc;

use crate::build::Val;
use crate::convert::traits::{ConvertResult, Converter};

/// FlagConverter implements the conversion logic for converting a Val into a set
/// of command line flags.
pub struct FlagConverter {}

impl FlagConverter {
    pub fn new() -> Self {
        FlagConverter {}
    }

    fn write_flag_name(&self, pfx: &str, name: &str, w: &mut Write) -> ConvertResult {
        if name.chars().count() > 1 || pfx.chars().count() > 0 {
            write!(w, "--{}{} ", pfx, name)?;
        } else {
            write!(w, "-{} ", name)?;
        }
        return Ok(());
    }

    fn write_list_flag(
        &self,
        pfx: &str,
        name: &str,
        def: &Vec<Rc<Val>>,
        w: &mut Write,
    ) -> ConvertResult {
        // first of all we need to make sure that each &Val is only a primitive type.
        for v in def.iter() {
            let vref = v.as_ref();
            if vref.is_list() || vref.is_tuple() || vref.is_func() {
                eprintln!(
                    "Skipping non primitive val in list for flag {}{}",
                    pfx, name
                );
            } else {
                self.write_flag_name(pfx, name, w)?;
                self.write(pfx, vref, w)?;
            }
        }
        return Ok(());
    }

    fn write(&self, pfx: &str, v: &Val, w: &mut Write) -> ConvertResult {
        match v {
            &Val::Empty => {
                // Empty is a noop.
                return Ok(());
            }
            &Val::Boolean(b) => {
                write!(w, "{} ", if b { "true" } else { "false" })?;
            }
            &Val::Float(ref f) => {
                write!(w, "{} ", f)?;
            }
            &Val::Int(ref i) => {
                write!(w, "{} ", i)?;
            }
            &Val::Str(ref s) => {
                write!(w, "'{}' ", s)?;
            }
            &Val::List(ref _def) => {
                eprintln!("Skipping List...");
            }
            &Val::Tuple(ref flds) => {
                for &(ref name, ref val) in flds.iter() {
                    if let &Val::Empty = val.as_ref() {
                        self.write_flag_name(pfx, name, w)?;
                        continue;
                    }
                    match val.as_ref() {
                        &Val::Tuple(_) => {
                            let new_pfx = format!("{}{}.", pfx, name);
                            self.write(&new_pfx, val, w)?;
                        }
                        &Val::List(ref def) => {
                            self.write_list_flag(pfx, name, def, w)?;
                        }
                        _ => {
                            self.write_flag_name(pfx, name, w)?;
                            self.write(pfx, &val, w)?;
                        }
                    }
                }
            }
            &Val::Func(ref _def) => {
                // This is ignored
                eprintln!("Skipping macro...");
            }
            &Val::Env(ref _fs) => {
                // This is ignored
                eprintln!("Skipping env...");
            }
            &Val::Module(ref _def) => {
                // This is ignored
                eprintln!("Skipping module...");
            }
        }
        Ok(())
    }
}

impl Converter for FlagConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut Write) -> ConvertResult {
        self.write("", &v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("txt")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into command line flags.".to_string()
    }

    #[allow(unused_must_use)]
    fn help(&self) -> String {
        let mut h = String::new();
        writeln!(
            h,
            "Flags converts a tuple into a set of command line arguments for command line application."
        );
        writeln!(h, "");
        writeln!(h, "The flags are converted using the following rules:");
        writeln!(h, "");
        writeln!(h, "- keys in a tuple are converted into the argument name.");
        writeln!(
            h,
            "- values in a tuple are converted into the argument value."
        );
        writeln!(h, "- NULL values are not emitted");
        writeln!(
            h,
            "- lists expand out into an argument for each item in the list."
        );
        writeln!(h, "\te.g. {{foo = [1, 2]}} becomes --foo=1 --foo=2");
        writeln!(
            h,
            "- tuples expand out into an argument with the key as a prefix separated by a `.`."
        );
        writeln!(
            h,
            "\te.g. {{foo = {{bar = 1, baz = 2}}}} becomes --foo.bar=1 --foo.baz=2"
        );
        writeln!(h, "- Functions and Modules are ignored.");
        h
    }
}
