use std;
use std::io::Write;
use std::rc::Rc;

use serde_yaml;

use super::traits::{Converter, Result};
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
            v.push(r#try!(self.convert_value(val)));
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
                r#try!(self.convert_value(v)),
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
            &Val::Macro(_) => {
                eprintln!("Skipping macro encoding as null...");
                serde_yaml::Value::Null
            }
            &Val::Module(_) => {
                eprintln!("Skipping module encoding as null...");
                serde_yaml::Value::Null
            }
            &Val::Env(ref fs) => r#try!(self.convert_env(fs)),
            &Val::List(ref l) => r#try!(self.convert_list(l)),
            &Val::Tuple(ref t) => r#try!(self.convert_tuple(t)),
        };
        Ok(yaml_val)
    }

    fn write(&self, v: &Val, w: &mut Write) -> Result {
        let jsn_val = r#try!(self.convert_value(v));
        r#try!(serde_yaml::to_writer(w, &jsn_val));
        Ok(())
    }
}

impl Converter for YamlConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut Write) -> Result {
        self.write(&v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("yaml")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into valid yaml.".to_string()
    }
}
