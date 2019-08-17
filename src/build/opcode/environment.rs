// Copyright 2019 Jeremy Wall
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
use std::collections::BTreeMap;
use std::io::Write;
use std::rc::Rc;

use super::cache;
use super::Value;
use crate::convert::{ConverterRegistry, ImporterRegistry};

// Shared Environmental between VM's for runtime usage.
pub struct Environment<Stdout, Stderr>
where
    Stdout: Write,
    Stderr: Write,
{
    pub val_cache: BTreeMap<String, Rc<Value>>,
    pub op_cache: cache::Ops,                  // Shared environment
    pub converter_registry: ConverterRegistry, // Shared environment
    pub importer_registry: ImporterRegistry,   // Shared environment
    pub stdout: Stdout,                        // Shared environment
    pub stderr: Stderr,                        // Shared environment
                                               // TODO(jwall): Environment Variables
}

impl<Stdout: Write, Stderr: Write> Environment<Stdout, Stderr> {
    pub fn new(out: Stdout, err: Stderr) -> Self {
        Self {
            val_cache: BTreeMap::new(),
            op_cache: cache::Ops::new(),
            converter_registry: ConverterRegistry::make_registry(),
            importer_registry: ImporterRegistry::make_registry(),
            stdout: out,
            stderr: err,
        }
    }
}
