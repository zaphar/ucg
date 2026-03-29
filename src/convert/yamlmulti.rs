// Copyright 2020 Jeremy Wall
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
use std::io::Write;
use std::rc::Rc;

use crate::build::Val;
use crate::convert::traits::{ConvertResult, Converter};
use crate::convert::yaml::YamlConverter;

pub struct MultiYamlConverter(YamlConverter);

impl MultiYamlConverter {
    pub fn new() -> Self {
        MultiYamlConverter(YamlConverter::new())
    }

    pub fn convert_list(&self, vals: &Vec<Rc<Val>>, mut w: &mut dyn Write) -> ConvertResult {
        for val in vals {
            self.0.write(val.as_ref(), &mut w)?;
        }
        Ok(())
    }
}

impl Converter for MultiYamlConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut dyn Write) -> ConvertResult {
        if let Val::List(ref vals) = v.as_ref() {
            self.convert_list(vals, &mut w)
        } else {
            let list = vec![v];
            self.convert_list(&list, &mut w)
        }
    }

    fn file_ext(&self) -> String {
        "yaml".to_owned()
    }

    fn description(&self) -> String {
        "Convert ucg vals into valid multi document yaml.".to_owned()
    }

    #[allow(unused_must_use)]
    fn help(&self) -> String {
        include_str!("yaml_help.txt").to_owned()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::Cursor;

    fn convert_to_string(v: Val) -> String {
        let conv = MultiYamlConverter::new();
        let mut buf = Cursor::new(vec![]);
        conv.convert(Rc::new(v), &mut buf).unwrap();
        String::from_utf8(buf.into_inner()).unwrap()
    }

    #[test]
    fn convert_list_produces_multi_doc() {
        let v = Val::List(vec![Rc::new(Val::Int(1)), Rc::new(Val::Int(2))]);
        let out = convert_to_string(v);
        // Each document should appear as a separate YAML value
        assert!(out.contains("1"));
        assert!(out.contains("2"));
    }

    #[test]
    fn convert_non_list_wraps() {
        let v = Val::Int(42);
        let out = convert_to_string(v);
        assert!(out.contains("42"));
    }

    #[test]
    fn file_ext() {
        assert_eq!(MultiYamlConverter::new().file_ext(), "yaml");
    }
}
