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

//! The asset cache for the ucg compiler.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;
use std::rc::Rc;
use std::result;

use super::Val;

pub type Result<T> = result::Result<T, io::Error>;

/// Defines the cach interface for a UCG build. It has functions to retrieve
/// An asset for a referenced ucg file if it exists as well as to stash
/// an asset for a built ucg file.
///
/// All methods with a path do path canonicalization. As a result the path
/// is expected to exist on the filesystem. If the path does not exist on
/// the local filesystem then the Cache may return an error.
pub trait Cache {
    fn has_path(&self, path: &PathBuf) -> Result<bool>;
    fn get(&self, path: &PathBuf) -> Result<Option<Rc<Val>>>;
    fn stash(&mut self, path: PathBuf, asset: Rc<Val>) -> Result<()>;
}

pub struct MemoryCache {
    map: HashMap<PathBuf, Rc<Val>>,
}

impl MemoryCache {
    pub fn new() -> Self {
        MemoryCache {
            map: HashMap::new(),
        }
    }
}

impl Cache for MemoryCache {
    fn has_path(&self, path: &PathBuf) -> Result<bool> {
        Ok(self.map.contains_key(path))
    }

    fn get(&self, path: &PathBuf) -> Result<Option<Rc<Val>>> {
        Ok(self.map.get(path).map(|v| v.clone()))
    }

    fn stash(&mut self, path: PathBuf, asset: Rc<Val>) -> Result<()> {
        self.map.insert(path, asset);
        Ok(())
    }
}
