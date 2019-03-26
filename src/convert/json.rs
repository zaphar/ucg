//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.
//! Flags contains code for converting a UCG Val into the json output target.
use std;
use std::error::Error;
use std::fmt::Write as FmtWrite;
use std::io::Write;
use std::rc::Rc;

use serde_json;

use crate::build::Val;
use crate::convert::traits::{ConvertResult, Converter, ImportResult, Importer};

/// JsonConverter implements the logic for converting a Val into the json output format.
pub struct JsonConverter {}

impl JsonConverter {
    pub fn new() -> Self {
        JsonConverter {}
    }

    fn convert_list(&self, items: &Vec<Rc<Val>>) -> std::io::Result<serde_json::Value> {
        let mut v = Vec::new();
        for val in items.iter() {
            v.push(self.convert_value(val)?);
        }
        Ok(serde_json::Value::Array(v))
    }

    fn convert_tuple(&self, items: &Vec<(String, Rc<Val>)>) -> std::io::Result<serde_json::Value> {
        let mut mp = serde_json::Map::new();
        for &(ref k, ref v) in items.iter() {
            mp.entry(k.clone()).or_insert(self.convert_value(v)?);
        }
        Ok(serde_json::Value::Object(mp))
    }

    fn convert_env(&self, items: &Vec<(String, String)>) -> std::io::Result<serde_json::Value> {
        let mut mp = serde_json::Map::new();
        for &(ref k, ref v) in items.iter() {
            mp.entry(k.clone())
                .or_insert(serde_json::Value::String(v.clone()));
        }
        Ok(serde_json::Value::Object(mp))
    }

    fn convert_value(&self, v: &Val) -> std::io::Result<serde_json::Value> {
        let jsn_val = match v {
            &Val::Boolean(b) => serde_json::Value::Bool(b),
            &Val::Empty => serde_json::Value::Null,
            &Val::Float(f) => {
                let n = match serde_json::Number::from_f64(f) {
                    Some(n) => n,
                    // In theory this should never happen. But on the off chance that it does...
                    None => panic!("Float is too large or Not a Number {}", f),
                };
                serde_json::Value::Number(n)
            }
            &Val::Int(i) => {
                let n = match serde_json::Number::from_f64(i as f64) {
                    Some(n) => n,
                    // In theory this should never happen. But on the off chance that it does...
                    None => panic!("Float is too large or Not a Number {}", i),
                };
                serde_json::Value::Number(n)
            }
            &Val::Str(ref s) => serde_json::Value::String(s.clone()),
            &Val::Func(_) => {
                eprintln!("Skipping func encoding as null...");
                serde_json::Value::Null
            }
            &Val::Module(_) => {
                eprintln!("Skipping module encoding as null...");
                serde_json::Value::Null
            }
            &Val::Env(ref fs) => self.convert_env(fs)?,
            &Val::List(ref l) => self.convert_list(l)?,
            &Val::Tuple(ref t) => self.convert_tuple(t)?,
        };
        Ok(jsn_val)
    }

    fn convert_json_val(&self, v: &serde_json::Value) -> std::result::Result<Val, Box<dyn Error>> {
        Ok(match v {
            serde_json::Value::String(s) => Val::Str(s.clone()),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Val::Int(i)
                } else {
                    Val::Float(n.as_f64().expect("Number was not an int or a float!!"))
                }
            }
            serde_json::Value::Bool(b) => Val::Boolean(*b),
            serde_json::Value::Null => Val::Empty,
            serde_json::Value::Array(l) => {
                let mut vs = Vec::with_capacity(l.len());
                for aval in l {
                    vs.push(Rc::new(self.convert_json_val(aval)?));
                }
                Val::List(vs)
            }
            serde_json::Value::Object(m) => {
                let mut fs = Vec::with_capacity(m.len());
                for (key, value) in m {
                    fs.push((key.to_string(), Rc::new(self.convert_json_val(value)?)));
                }
                Val::Tuple(fs)
            }
        })
    }

    fn write(&self, v: &Val, w: &mut Write) -> ConvertResult {
        let jsn_val = self.convert_value(v)?;
        serde_json::to_writer_pretty(w, &jsn_val)?;
        Ok(())
    }
}

impl Converter for JsonConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut Write) -> ConvertResult {
        self.write(&v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("json")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into valid json.".to_string()
    }

    #[allow(unused_must_use)]
    fn help(&self) -> String {
        let mut h = String::new();
        writeln!(h, "JSON conversions expect any ucg value.");
        writeln!(h, "");
        writeln!(
            h,
            "They are transformed into json using the following rules:"
        );
        writeln!(h, "- NULL becomes null");
        writeln!(h, "- tuples become objects {{}}");
        writeln!(h, "- lists become lists []");
        writeln!(h, "- Int and Float become numbers");
        writeln!(h, "- Strings become strings.");
        writeln!(h, "- Functions and Modules are ignored.");
        h
    }
}

impl Importer for JsonConverter {
    fn import(&self, bytes: &[u8]) -> ImportResult {
        let json_val = serde_json::from_slice(bytes)?;
        Ok(Rc::new(self.convert_json_val(&json_val)?))
    }
}
