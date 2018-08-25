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

//! The conversion stage of the ucg compiler.
pub mod env;
pub mod exec;
pub mod flags;
pub mod json;
pub mod traits;
pub mod yaml;

use std::collections::HashMap;

/// ConverterRunner knows how to run a given converter on a Val.
pub struct ConverterRegistry {
    converters: HashMap<String, Box<traits::Converter>>,
}

impl ConverterRegistry {
    /// new creates a new ConverterRunner with a converter for the provided output target.
    ///
    /// * flags
    /// * json
    /// * env
    /// * exec
    fn new() -> Self {
        ConverterRegistry {
            converters: HashMap::new(),
        }
    }

    pub fn make_registry() -> Self {
        let mut registry = Self::new();
        registry.register("json", Box::new(json::JsonConverter::new()));
        registry.register("env", Box::new(env::EnvConverter::new()));
        registry.register("flags", Box::new(flags::FlagConverter::new()));
        registry.register("exec", Box::new(exec::ExecConverter::new()));
        registry.register("yaml", Box::new(yaml::YamlConverter::new()));
        registry
    }

    pub fn register<S: Into<String>>(&mut self, typ: S, converter: Box<traits::Converter>) {
        self.converters.insert(typ.into(), converter);
    }

    pub fn get_converter(&self, typ: &str) -> Option<&traits::Converter> {
        self.converters.get(typ).map(|c| c.as_ref())
    }

    pub fn get_converter_list(&self) -> Vec<(&String, &Box<traits::Converter>)> {
        self.converters.iter().collect()
    }

    // TODO(jwall): Support converter help descriptions.
}
