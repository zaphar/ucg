// Copyright 2018 Jeremy Wall
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

use std;
use std::error;
use std::error::Error;
use std::io::Write;
use std::rc::Rc;

use simple_error::SimpleError;
use toml;

use crate::build::Val;
use crate::convert::traits::{ConvertResult, Converter, ImportResult, Importer};

pub struct TomlConverter {}

type Result = std::result::Result<toml::Value, Box<dyn error::Error>>;

impl Default for TomlConverter {
    fn default() -> Self {
        Self::new()
    }
}

impl TomlConverter {
    pub fn new() -> Self {
        TomlConverter {}
    }

    fn convert_list(&self, items: &Vec<Rc<Val>>) -> Result {
        let mut v = Vec::new();
        for val in items.iter() {
            v.push(self.convert_value(val)?);
        }
        Ok(toml::Value::Array(v))
    }

    fn convert_tuple(&self, items: &Vec<(Rc<str>, Rc<Val>)>) -> Result {
        let mut mp = toml::value::Table::new();
        for (k, v) in items.iter() {
            mp.entry(k.to_string()).or_insert(self.convert_value(v)?);
        }
        Ok(toml::Value::Table(mp))
    }

    fn convert_env(&self, items: &Vec<(Rc<str>, Rc<str>)>) -> Result {
        let mut mp = toml::value::Table::new();
        for (k, v) in items.iter() {
            mp.entry(k.to_string())
                .or_insert(toml::Value::String(v.to_string()));
        }
        Ok(toml::Value::Table(mp))
    }

    fn convert_value(&self, v: &Val) -> Result {
        let toml_val = match v {
            &Val::Boolean(b) => toml::Value::Boolean(b),
            &Val::Empty => {
                let err = SimpleError::new("Nulls are not allowed in Toml Conversions!");
                return Err(Box::new(err));
            }
            &Val::Float(f) => toml::Value::Float(f),
            &Val::Int(i) => toml::Value::Integer(i),
            Val::Str(s) => toml::Value::String(s.to_string()),
            Val::Env(fs) => self.convert_env(fs)?,
            Val::List(l) => self.convert_list(l)?,
            Val::Tuple(t) => self.convert_tuple(t)?,
            &Val::Constraint(_) => {
                let err = SimpleError::new("Constraint values cannot be converted to TOML!");
                return Err(Box::new(err));
            }
        };
        Ok(toml_val)
    }

    fn convert_toml_val(&self, v: &toml::Value) -> std::result::Result<Val, Box<dyn Error>> {
        Ok(match v {
            toml::Value::String(s) => Val::Str(s.clone().into()),
            toml::Value::Integer(i) => Val::Int(*i),
            toml::Value::Float(f) => Val::Float(*f),
            toml::Value::Boolean(b) => Val::Boolean(*b),
            toml::Value::Array(l) => {
                let mut vs = Vec::with_capacity(l.len());
                for aval in l {
                    vs.push(Rc::new(self.convert_toml_val(aval)?));
                }
                Val::List(vs)
            }
            toml::Value::Table(m) => {
                let mut fs = Vec::with_capacity(m.len());
                for (key, value) in m {
                    fs.push((key.clone().into(), Rc::new(self.convert_toml_val(value)?)));
                }
                Val::Tuple(fs)
            }
            toml::Value::Datetime(d) => Val::Str(format!("{}", d).into()),
        })
    }

    fn write(&self, v: &Val, w: &mut dyn Write) -> ConvertResult {
        let toml_val = self.convert_value(v)?;
        let toml_bytes = toml::ser::to_string_pretty(&toml_val)?;
        write!(w, "{}", toml_bytes)?;
        Ok(())
    }
}

impl Converter for TomlConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut dyn Write) -> ConvertResult {
        self.write(&v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("toml")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into valid ucg.".to_string()
    }

    fn help(&self) -> String {
        include_str!("toml_help.txt").to_string()
    }
}

impl Importer for TomlConverter {
    fn import(&self, bytes: &[u8]) -> ImportResult {
        let json_val = toml::from_slice(bytes)?;
        Ok(Rc::new(self.convert_toml_val(&json_val)?))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    fn convert_to_string(v: Val) -> String {
        let conv = TomlConverter::new();
        let mut buf = Cursor::new(vec![]);
        conv.convert(Rc::new(v), &mut buf).unwrap();
        String::from_utf8(buf.into_inner()).unwrap()
    }

    #[test]
    fn convert_tuple_with_primitives() {
        let v = Val::Tuple(vec![
            (Rc::from("name"), Rc::new(Val::Str(Rc::from("alice")))),
            (Rc::from("age"), Rc::new(Val::Int(30))),
            (Rc::from("active"), Rc::new(Val::Boolean(true))),
        ]);
        let out = convert_to_string(v);
        assert!(out.contains("name = 'alice'"));
        assert!(out.contains("age = 30"));
        assert!(out.contains("active = true"));
    }

    #[test]
    fn convert_empty_errors() {
        let conv = TomlConverter::new();
        let mut buf = Cursor::new(vec![]);
        let result = conv.convert(Rc::new(Val::Empty), &mut buf);
        assert!(result.is_err());
    }

    #[test]
    fn convert_nested_tuple() {
        let v = Val::Tuple(vec![(
            Rc::from("server"),
            Rc::new(Val::Tuple(vec![(
                Rc::from("port"),
                Rc::new(Val::Int(8080)),
            )])),
        )]);
        let out = convert_to_string(v);
        assert!(out.contains("[server]"));
        assert!(out.contains("port = 8080"));
    }

    #[test]
    fn convert_list() {
        let v = Val::Tuple(vec![(
            Rc::from("ports"),
            Rc::new(Val::List(vec![
                Rc::new(Val::Int(80)),
                Rc::new(Val::Int(443)),
            ])),
        )]);
        let out = convert_to_string(v);
        assert!(out.contains("ports"));
        assert!(out.contains("80"));
        assert!(out.contains("443"));
    }

    #[test]
    fn import_toml() {
        let conv = TomlConverter::new();
        let val = conv.import(b"name = \"alice\"\nage = 30").unwrap();
        assert!(val.is_tuple());
    }

    #[test]
    fn import_toml_nested() {
        let conv = TomlConverter::new();
        let val = conv.import(b"[server]\nport = 8080").unwrap();
        assert!(val.is_tuple());
    }

    #[test]
    fn file_ext() {
        assert_eq!(TomlConverter::new().file_ext(), "toml");
    }
}
