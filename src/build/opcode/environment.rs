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
use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use super::cache;
use super::pointer::OpPointer;
use super::Error;
use super::Value;
use crate::build::stdlib;
use crate::build::AssertCollector;
use crate::build::Val;
use crate::convert::{ConverterRegistry, ImporterRegistry};
use crate::iter::OffsetStrIter;
use crate::parse::parse;

// Shared Environmental between VM's for runtime usage.
pub struct Environment<Stdout, Stderr>
where
    Stdout: Write + Clone,
    Stderr: Write + Clone,
{
    pub val_cache: BTreeMap<String, Rc<Value>>,
    pub op_cache: cache::Ops,
    pub converter_registry: ConverterRegistry,
    pub importer_registry: ImporterRegistry,
    pub assert_results: AssertCollector,
    pub stdout: Stdout,
    pub stderr: Stderr,
    pub env_vars: BTreeMap<String, String>, // Environment Variables
    pub out_lock: BTreeSet<PathBuf>,
}

impl<Stdout: Write + Clone, Stderr: Write + Clone> Environment<Stdout, Stderr> {
    pub fn new(out: Stdout, err: Stderr) -> Self {
        Self::new_with_vars(out, err, BTreeMap::new())
    }

    pub fn new_with_vars(out: Stdout, err: Stderr, vars: BTreeMap<String, String>) -> Self {
        Self {
            val_cache: BTreeMap::new(),
            env_vars: vars,
            op_cache: cache::Ops::new(),
            assert_results: AssertCollector::new(),
            converter_registry: ConverterRegistry::make_registry(),
            importer_registry: ImporterRegistry::make_registry(),
            stdout: out,
            stderr: err,
            out_lock: BTreeSet::new(),
        }
    }

    pub fn get_cached_path_val(&self, path: &String) -> Option<Rc<Value>> {
        self.val_cache.get(path).cloned()
    }

    pub fn update_path_val(&mut self, path: &String, val: Rc<Value>) {
        self.val_cache.insert(path.clone(), val);
    }

    pub fn get_ops_for_path<P>(&mut self, path: P) -> Result<OpPointer, Error>
    where
        P: Into<PathBuf> + Clone,
    {
        let path_copy = path.clone();
        self.op_cache.entry(path.clone()).get_pointer_or_else(
            || {
                // FIXME(jwall): We need to do proper error handling here.
                let p = path.into();
                let root = p.parent().unwrap();
                // first we read in the file
                let mut f = File::open(&p)?;
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
            path_copy,
        )
    }

    fn add_ops_for_path_and_content<P>(&mut self, path: P, contents: &str) -> Result<(), Error>
    where
        P: Into<PathBuf> + Clone,
    {
        let path_copy = path.clone();
        self.op_cache.entry(path.clone()).get_pointer_or_else(
            || {
                let p = path.into();
                let root = p.parent().unwrap();
                let iter = OffsetStrIter::new(contents).with_src_file(&p);
                // FIXME(jwall): Unify BuildError and our other Error
                let stmts = parse(iter, None).unwrap();
                // then we create an ops from it
                let ops = super::translate::AST::translate(stmts, &root);
                Ok(ops)
            },
            path_copy,
        )?;
        Ok(())
    }

    pub fn populate_stdlib(&mut self) {
        for (p, s) in stdlib::get_libs().drain() {
            // We unwrap the error here since we expect stdlibs to
            // always compile.
            self.add_ops_for_path_and_content(p, s).unwrap();
        }
    }

    pub fn record_assert_result(&mut self, desc: &str, ok: bool) {
        self.assert_results.record_assert_result(desc, ok);
    }

    pub fn get_out_lock_for_path<P: AsRef<Path>>(&self, path: P) -> bool {
        self.out_lock.contains(path.as_ref())
    }

    pub fn set_out_lock_for_path<P: Into<PathBuf>>(&mut self, path: P) {
        self.out_lock.insert(path.into());
    }

    pub fn reset_out_lock_for_path<P: AsRef<Path>>(&mut self, path: P) {
        self.out_lock.remove(path.as_ref());
    }

    pub fn stdout(&self) -> Stdout {
        self.stdout.clone()
    }
    pub fn stderr(&self) -> Stderr {
        self.stderr.clone()
    }

    pub fn convert_val(&mut self, typ: &str, writer: &mut dyn Write, val: Rc<Val>) -> bool {
        match self.converter_registry.get_converter(typ) {
            Some(c) => {
                if let Err(e) = c.convert(val, writer) {
                    writeln!(&mut self.stderr, "{}", e).unwrap();
                    return false;
                }
            }
            None => {
                writeln!(
                    &mut self.stderr,
                    "No such format {}\nrun `ucg converters` to see available formats.",
                    typ
                )
                .unwrap();
                return false;
            }
        }
        return true;
    }
}
