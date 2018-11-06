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
use std::io::Write;
use std::rc::Rc;

use serde_json;

use ast;
use build::Val;
use convert::traits::{Converter, Result};

/// JsonConverter implements the logic for converting a Val into the json output format.
pub struct JsonConverter {}

impl JsonConverter {
    pub fn new() -> Self {
        JsonConverter {}
    }

    fn convert_list(&self, items: &Vec<Rc<Val>>) -> std::io::Result<serde_json::Value> {
        let mut v = Vec::new();
        for val in items.iter() {
            v.push(try!(self.convert_value(val)));
        }
        Ok(serde_json::Value::Array(v))
    }

    fn convert_tuple(
        &self,
        items: &Vec<(ast::PositionedItem<String>, Rc<Val>)>,
    ) -> std::io::Result<serde_json::Value> {
        let mut mp = serde_json::Map::new();
        for &(ref k, ref v) in items.iter() {
            mp.entry(k.val.clone())
                .or_insert(try!(self.convert_value(v)));
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
            &Val::Macro(_) => {
                eprintln!("Skipping macro encoding as null...");
                serde_json::Value::Null
            }
            &Val::List(ref l) => try!(self.convert_list(l)),
            &Val::Tuple(ref t) => try!(self.convert_tuple(t)),
        };
        Ok(jsn_val)
    }

    fn write(&self, v: &Val, w: &mut Write) -> Result {
        let jsn_val = try!(self.convert_value(v));
        try!(serde_json::to_writer(w, &jsn_val));
        Ok(())
    }
}

impl Converter for JsonConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut Write) -> Result {
        self.write(&v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("json")
    }

    fn description(&self) -> String {
        "Convert ucg Vals into valid json.".to_string()
    }
}
