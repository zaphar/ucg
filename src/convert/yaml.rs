use std;
use std::collections::BTreeSet;
use std::error::Error;
use std::io::Write;
use std::rc::Rc;
use std::result::Result;

use serde_yaml;

use super::traits::{ConvertResult, Converter, ImportResult, Importer};
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

    fn convert_env(&self, items: &Vec<(Rc<str>, Rc<str>)>) -> std::io::Result<serde_yaml::Value> {
        let mut mp = serde_yaml::Mapping::new();
        for &(ref k, ref v) in items.iter() {
            mp.insert(
                serde_yaml::Value::String(k.to_string()),
                serde_yaml::Value::String(v.to_string()),
            );
        }
        Ok(serde_yaml::Value::Mapping(mp))
    }

    fn convert_tuple(&self, items: &Vec<(Rc<str>, Rc<Val>)>) -> std::io::Result<serde_yaml::Value> {
        let mut mapping = serde_yaml::Mapping::new();
        for &(ref k, ref v) in items.iter() {
            mapping.insert(serde_yaml::Value::String(k.to_string()), self.convert_value(v)?);
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
            &Val::Str(ref s) => serde_yaml::Value::String(s.to_string()),
            &Val::Env(ref fs) => self.convert_env(fs)?,
            &Val::List(ref l) => self.convert_list(l)?,
            &Val::Tuple(ref t) => self.convert_tuple(t)?,
        };
        Ok(yaml_val)
    }

    fn merge_mapping_keys(
        &self,
        mut fs: &mut Vec<(String, Rc<Val>)>,
        m: &serde_yaml::Mapping,
    ) -> Result<(), Box<dyn Error>> {
        for (key, value) in m {
            // This is a little gross but since yaml allows maps to be keyed
            // by more than just a string it's necessary.
            let key = match key {
                serde_yaml::Value::Bool(b) => b.to_string(),
                serde_yaml::Value::Null => "null".to_string(),
                serde_yaml::Value::Number(n) => n.to_string(),
                serde_yaml::Value::String(s) => s.clone(),
                serde_yaml::Value::Sequence(_)
                | serde_yaml::Value::Mapping(_)
                | serde_yaml::Value::Tagged(_) => {
                    eprintln!("Unsupported key type in yaml map key import skipping");
                    continue;
                }
            };
            if key == "<<" {
                if let serde_yaml::Value::Mapping(merge_map) = value {
                    self.merge_mapping_keys(&mut fs, merge_map)?;
                }
            } else {
                fs.push((key, Rc::new(self.convert_yaml_val(&value)?)));
            }
        }
        Ok(())
    }

    fn convert_yaml_val(&self, v: &serde_yaml::Value) -> Result<Val, Box<dyn Error>> {
        Ok(match v {
            serde_yaml::Value::String(s) => Val::Str(s.clone().into()),
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
                    vs.push(Rc::new(self.convert_yaml_val(aval)?));
                }
                Val::List(vs)
            }
            serde_yaml::Value::Mapping(m) => {
                let mut fs = Vec::with_capacity(m.len());
                self.merge_mapping_keys(&mut fs, m)?;
                fs.reverse();
                let mut seen_keys = BTreeSet::new();
                let mut collapsed = Vec::with_capacity(fs.len());
                for (k, val) in fs {
                    if !seen_keys.contains(&k) {
                        collapsed.push((k.clone().into(), val));
                        seen_keys.insert(k);
                    }
                }
                collapsed.reverse();
                Val::Tuple(collapsed)
            }
            serde_yaml::Value::Tagged(_) => {
                eprintln!(
                    "Tagged value types are not supported in yaml imports. Replacing with Empty..."
                );
                Val::Empty
            }
        })
    }

    pub fn write(&self, v: &Val, mut w: &mut dyn Write) -> ConvertResult {
        let jsn_val = self.convert_value(v)?;
        serde_yaml::to_writer(&mut w, &jsn_val)?;
        writeln!(w, "")?;
        Ok(())
    }
}

impl Converter for YamlConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut dyn Write) -> ConvertResult {
        self.write(&v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("yaml")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into valid yaml.".to_string()
    }

    #[allow(unused_must_use)]
    fn help(&self) -> String {
        include_str!("yaml_help.txt").to_string()
    }
}

impl Importer for YamlConverter {
    fn import(&self, bytes: &[u8]) -> ImportResult {
        let json_val = serde_yaml::from_slice(bytes)?;
        Ok(Rc::new(self.convert_yaml_val(&json_val)?))
    }
}
