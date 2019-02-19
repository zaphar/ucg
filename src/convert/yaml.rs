use std;
use std::error::Error;
use std::io::Write;
use std::rc::Rc;
use std::result::Result;

use serde_yaml;

use super::traits::{ConvertResult, Converter, ImportResult, Importer};
use crate::ast;
use crate::build::Val;

pub struct YamlConverter {}

impl YamlConverter {
    pub fn new() -> Self {
        YamlConverter {}
    }

    fn convert_list(&self, items: &Vec<Rc<Val>>) -> std::io::Result<serde_yaml::Value> {
        let mut v = Vec::new();
        for val in items.iter() {
            v.push(self.convert_value(val)?);
        }
        Ok(serde_yaml::Value::Sequence(v))
    }

    fn convert_env(&self, items: &Vec<(String, String)>) -> std::io::Result<serde_yaml::Value> {
        let mut mp = serde_yaml::Mapping::new();
        for &(ref k, ref v) in items.iter() {
            mp.insert(
                serde_yaml::Value::String(k.clone()),
                serde_yaml::Value::String(v.clone()),
            );
        }
        Ok(serde_yaml::Value::Mapping(mp))
    }

    fn convert_tuple(
        &self,
        items: &Vec<(ast::PositionedItem<String>, Rc<Val>)>,
    ) -> std::io::Result<serde_yaml::Value> {
        let mut mapping = serde_yaml::Mapping::new();
        for &(ref k, ref v) in items.iter() {
            mapping.insert(
                serde_yaml::Value::String(k.val.clone()),
                self.convert_value(v)?,
            );
        }
        Ok(serde_yaml::Value::Mapping(mapping))
    }

    fn convert_value(&self, v: &Val) -> std::io::Result<serde_yaml::Value> {
        let yaml_val = match v {
            &Val::Boolean(b) => serde_yaml::Value::Bool(b),
            &Val::Empty => serde_yaml::Value::Null,
            &Val::Float(f) => match serde_yaml::to_value(f) {
                Ok(v) => v,
                _ => panic!("Float is too large or not a Number {}", f),
            },
            &Val::Int(i) => match serde_yaml::to_value(i) {
                Ok(v) => v,
                _ => panic!("Int is too large or not a Number {}", i),
            },
            &Val::Str(ref s) => serde_yaml::Value::String(s.clone()),
            &Val::Func(_) => {
                eprintln!("Skipping func encoding as null...");
                serde_yaml::Value::Null
            }
            &Val::Module(_) => {
                eprintln!("Skipping module encoding as null...");
                serde_yaml::Value::Null
            }
            &Val::Env(ref fs) => self.convert_env(fs)?,
            &Val::List(ref l) => self.convert_list(l)?,
            &Val::Tuple(ref t) => self.convert_tuple(t)?,
        };
        Ok(yaml_val)
    }

    fn convert_json_val(&self, v: &serde_yaml::Value) -> Result<Val, Box<dyn Error>> {
        Ok(match v {
            serde_yaml::Value::String(s) => Val::Str(s.clone()),
            serde_yaml::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Val::Int(i)
                } else {
                    Val::Float(n.as_f64().expect("Number was not an int or a float!!"))
                }
            }
            serde_yaml::Value::Bool(b) => Val::Boolean(*b),
            serde_yaml::Value::Null => Val::Empty,
            serde_yaml::Value::Sequence(l) => {
                let mut vs = Vec::with_capacity(l.len());
                for aval in l {
                    vs.push(Rc::new(self.convert_json_val(aval)?));
                }
                Val::List(vs)
            }
            serde_yaml::Value::Mapping(m) => {
                let mut fs = Vec::with_capacity(m.len());
                for (key, value) in m {
                    // This is a little gross but since yaml allows maps to be keyed
                    // by more than just a string it's necessary.
                    let key = match key {
                        serde_yaml::Value::Bool(b) => b.to_string(),
                        serde_yaml::Value::Null => "null".to_string(),
                        serde_yaml::Value::Number(n) => n.to_string(),
                        serde_yaml::Value::String(s) => s.clone(),
                        serde_yaml::Value::Sequence(_) | serde_yaml::Value::Mapping(_) => {
                            eprintln!("Unsupported key type in yaml import skipping");
                            continue;
                        }
                    };
                    eprintln!("yaml key is: {}", key);
                    fs.push((
                        ast::PositionedItem::new(key, ast::Position::new(0, 0, 0)),
                        Rc::new(self.convert_json_val(value)?),
                    ));
                }
                Val::Tuple(fs)
            }
        })
    }

    fn write(&self, v: &Val, w: &mut Write) -> ConvertResult {
        let jsn_val = self.convert_value(v)?;
        serde_yaml::to_writer(w, &jsn_val)?;
        Ok(())
    }
}

impl Converter for YamlConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut Write) -> ConvertResult {
        self.write(&v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("yaml")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into valid yaml.".to_string()
    }
}

impl Importer for YamlConverter {
    fn import(&self, bytes: &[u8]) -> ImportResult {
        let json_val = serde_yaml::from_slice(bytes)?;
        Ok(Rc::new(self.convert_json_val(&json_val)?))
    }
}
