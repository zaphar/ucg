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

type Result = std::result::Result<toml::Value, Box<error::Error>>;

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

    fn convert_tuple(&self, items: &Vec<(String, Rc<Val>)>) -> Result {
        let mut mp = toml::value::Table::new();
        for &(ref k, ref v) in items.iter() {
            mp.entry(k.clone()).or_insert(self.convert_value(v)?);
        }
        Ok(toml::Value::Table(mp))
    }

    fn convert_env(&self, items: &Vec<(String, String)>) -> Result {
        let mut mp = toml::value::Table::new();
        for &(ref k, ref v) in items.iter() {
            mp.entry(k.clone())
                .or_insert(toml::Value::String(v.clone()));
        }
        Ok(toml::Value::Table(mp))
    }

    fn convert_value(&self, v: &Val) -> Result {
        let toml_val = match v {
            &Val::Boolean(b) => toml::Value::Boolean(b),
            // TODO(jwall): This is an error apparently
            &Val::Empty => {
                let err = SimpleError::new("Nulls are not allowed in Toml Conversions!");
                return Err(Box::new(err));
            }
            &Val::Float(f) => toml::Value::Float(f),
            &Val::Int(i) => toml::Value::Integer(i),
            &Val::Str(ref s) => toml::Value::String(s.clone()),
            &Val::Func(_) => {
                let err = SimpleError::new("Functions are not allowed in Toml Conversions!");
                return Err(Box::new(err));
            }
            &Val::Module(_) => {
                let err = SimpleError::new("Modules are not allowed in Toml Conversions!");
                return Err(Box::new(err));
            }
            &Val::Env(ref fs) => self.convert_env(fs)?,
            &Val::List(ref l) => self.convert_list(l)?,
            &Val::Tuple(ref t) => self.convert_tuple(t)?,
        };
        Ok(toml_val)
    }

    fn convert_toml_val(&self, v: &toml::Value) -> std::result::Result<Val, Box<dyn Error>> {
        Ok(match v {
            toml::Value::String(s) => Val::Str(s.clone()),
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
                    fs.push((key.to_string(), Rc::new(self.convert_toml_val(value)?)));
                }
                Val::Tuple(fs)
            }
            toml::Value::Datetime(d) => Val::Str(format!("{}", d)),
        })
    }

    fn write(&self, v: &Val, w: &mut Write) -> ConvertResult {
        let toml_val = self.convert_value(v)?;
        let toml_bytes = toml::ser::to_string_pretty(&toml_val)?;
        write!(w, "{}", toml_bytes)?;
        Ok(())
    }
}

impl Converter for TomlConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut Write) -> ConvertResult {
        self.write(&v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("toml")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into valid ucg.".to_string()
    }
}

impl Importer for TomlConverter {
    fn import(&self, bytes: &[u8]) -> ImportResult {
        let json_val = toml::from_slice(bytes)?;
        Ok(Rc::new(self.convert_toml_val(&json_val)?))
    }
}
