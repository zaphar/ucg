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

use crate::build::Val;
use crate::convert::traits::{ConvertResult, Converter};
use crate::error::{BuildError, ErrorType};

/// FlagConverter implements the conversion logic for converting a Val into a set
/// of command line flags.
pub struct FlagConverter {
    sep: &'static str,
}

impl Default for FlagConverter {
    fn default() -> Self {
        Self::new()
    }
}

impl FlagConverter {
    pub fn new() -> Self {
        FlagConverter { sep: "." }
    }

    pub fn with_sep(mut self, sep: &'static str) -> Self {
        self.sep = sep;
        self
    }

    fn write_flag_name(&self, pfx: &str, name: &str, w: &mut dyn Write) -> ConvertResult {
        if name.chars().count() > 1 || pfx.chars().count() > 0 {
            write!(w, "--{}{} ", pfx, name)?;
        } else {
            write!(w, "-{} ", name)?;
        }
        Ok(())
    }

    fn write_list_flag(
        &self,
        pfx: &str,
        name: &str,
        def: &[Rc<Val>],
        w: &mut dyn Write,
    ) -> ConvertResult {
        // first of all we need to make sure that each &Val is only a primitive type.
        for v in def.iter() {
            let vref = v.as_ref();
            if vref.is_list() || vref.is_tuple() {
                eprintln!(
                    "Skipping non primitive val in list for flag {}{}",
                    pfx, name
                );
            } else {
                self.write_flag_name(pfx, name, w)?;
                self.write_simple_value(vref, w)?;
            }
        }
        Ok(())
    }

    fn write_simple_value(&self, v: &Val, w: &mut dyn Write) -> ConvertResult {
        match v {
            &Val::Empty => {
                // Empty is a noop.
                return Ok(());
            }
            &Val::Boolean(b) => {
                write!(w, "{} ", if b { "true" } else { "false" })?;
            }
            Val::Float(f) => {
                write!(w, "{} ", f)?;
            }
            Val::Int(i) => {
                write!(w, "{} ", i)?;
            }
            Val::Str(s) => {
                write!(w, "'{}' ", super::shell_escape_single_quoted(s))?;
            }
            &Val::List(_) | &Val::Tuple(_) | &Val::Env(_) | &Val::Constraint(_) => {
                // This is ignored
                eprintln!("Skipping {}...", v.type_name());
            }
        }
        Ok(())
    }

    fn write(&self, pfx: &str, flds: &[(Rc<str>, Rc<Val>)], w: &mut dyn Write) -> ConvertResult {
        for (name, val) in flds.iter() {
            if let &Val::Empty = val.as_ref() {
                self.write_flag_name(pfx, name, w)?;
                continue;
            }
            match val.as_ref() {
                &Val::Tuple(_) | &Val::Env(_) | &Val::Constraint(_) => {
                    eprintln!("Skipping {} in flag output tuple.", val.type_name());
                }
                Val::List(def) => {
                    self.write_list_flag(pfx, name, def, w)?;
                }
                &Val::Boolean(_) | &Val::Empty | &Val::Float(_) | &Val::Int(_) | &Val::Str(_) => {
                    self.write_flag_name(pfx, name, w)?;
                    self.write_simple_value(val, w)?;
                }
            }
        }
        Ok(())
    }
}

impl Converter for FlagConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut dyn Write) -> ConvertResult {
        if let Val::Tuple(flds) = v.as_ref() {
            self.write("", flds, &mut w)
        } else {
            Err(Box::new(BuildError::new(
                "Flag outputs must be a tuple",
                ErrorType::ConvertError,
            )))
        }
    }

    fn file_ext(&self) -> String {
        String::from("txt")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into command line flags.".to_string()
    }

    fn help(&self) -> String {
        include_str!("flags_help.txt").to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    fn convert_to_string(v: Val) -> String {
        let conv = FlagConverter::new();
        let mut buf = Cursor::new(vec![]);
        conv.convert(Rc::new(v), &mut buf).unwrap();
        String::from_utf8(buf.into_inner()).unwrap()
    }

    #[test]
    fn convert_long_flag() {
        let v = Val::Tuple(vec![(Rc::from("verbose"), Rc::new(Val::Boolean(true)))]);
        let out = convert_to_string(v);
        assert!(out.contains("--verbose"));
        assert!(out.contains("true"));
    }

    #[test]
    fn convert_short_flag() {
        let v = Val::Tuple(vec![(Rc::from("v"), Rc::new(Val::Boolean(true)))]);
        let out = convert_to_string(v);
        assert!(out.contains("-v"));
    }

    #[test]
    fn convert_string_value() {
        let v = Val::Tuple(vec![(
            Rc::from("name"),
            Rc::new(Val::Str(Rc::from("test"))),
        )]);
        let out = convert_to_string(v);
        assert!(out.contains("--name"));
        assert!(out.contains("'test'"));
    }

    #[test]
    fn convert_int_value() {
        let v = Val::Tuple(vec![(Rc::from("count"), Rc::new(Val::Int(5)))]);
        let out = convert_to_string(v);
        assert!(out.contains("--count"));
        assert!(out.contains("5"));
    }

    #[test]
    fn convert_empty_flag_no_value() {
        let v = Val::Tuple(vec![(Rc::from("help"), Rc::new(Val::Empty))]);
        let out = convert_to_string(v);
        assert_eq!(out.trim(), "--help");
    }

    #[test]
    fn convert_list_flag_repeats() {
        let v = Val::Tuple(vec![(
            Rc::from("tag"),
            Rc::new(Val::List(vec![
                Rc::new(Val::Str(Rc::from("a"))),
                Rc::new(Val::Str(Rc::from("b"))),
            ])),
        )]);
        let out = convert_to_string(v);
        // Each list item should produce its own --tag flag
        assert_eq!(out.matches("--tag").count(), 2);
    }

    #[test]
    fn convert_non_tuple_errors() {
        let conv = FlagConverter::new();
        let mut buf = Cursor::new(vec![]);
        let result = conv.convert(Rc::new(Val::Int(1)), &mut buf);
        assert!(result.is_err());
    }

    #[test]
    fn file_ext() {
        assert_eq!(FlagConverter::new().file_ext(), "txt");
    }
}
