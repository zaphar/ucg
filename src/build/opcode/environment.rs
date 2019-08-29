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
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::rc::Rc;

use super::cache;
use super::pointer::OpPointer;
use super::Error;
use super::Value;
use crate::convert::{ConverterRegistry, ImporterRegistry};
use crate::iter::OffsetStrIter;
use crate::parse::parse;

// Shared Environmental between VM's for runtime usage.
pub struct Environment<Stdout, Stderr>
where
    Stdout: Write,
    Stderr: Write,
{
    pub val_cache: BTreeMap<String, Rc<Value>>,
    pub op_cache: cache::Ops,
    pub converter_registry: ConverterRegistry,
    pub importer_registry: ImporterRegistry,
    pub stdout: Stdout,
    pub stderr: Stderr,
    pub env_vars: BTreeMap<String, String>, // Environment Variables
}

impl<Stdout: Write, Stderr: Write> Environment<Stdout, Stderr> {
    pub fn new(out: Stdout, err: Stderr) -> Self {
        Self::new_with_vars(out, err, BTreeMap::new())
    }

    pub fn new_with_vars(out: Stdout, err: Stderr, vars: BTreeMap<String, String>) -> Self {
        Self {
            val_cache: BTreeMap::new(),
            env_vars: vars,
            op_cache: cache::Ops::new(),
            converter_registry: ConverterRegistry::make_registry(),
            importer_registry: ImporterRegistry::make_registry(),
            stdout: out,
            stderr: err,
        }
    }

    pub fn get_cached_path_val(&self, path: &String) -> Option<Rc<Value>> {
        self.val_cache.get(path).cloned()
    }

    pub fn update_path_val(&mut self, path: &String, val: Rc<Value>) {
        self.val_cache.insert(path.clone(), val);
    }

    pub fn get_ops_for_path(&mut self, path: &String) -> Result<OpPointer, Error> {
        self.op_cache.entry(path).get_pointer_or_else(
            || {
                // FIXME(jwall): We need to do proper error handling here.
                let p = PathBuf::from(&path);
                let root = p.parent().unwrap();
                // first we read in the file
                let mut f = File::open(&path)?;
                // then we parse it
                let mut contents = String::new();
                f.read_to_string(&mut contents)?;
                let iter = OffsetStrIter::new(&contents).with_src_file(&p);
                // FIXME(jwall): Unify BuildError and our other Error
                let stmts = parse(iter, None).unwrap();
                // then we create an ops from it
                let ops = super::translate::AST::translate(stmts, &root);
                Ok(ops)
            },
            &path,
        )
    }
}
