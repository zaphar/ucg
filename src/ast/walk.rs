// Copyright 2018 Jeremy Wall <jeremy@marzhillstudios.com>
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
use std::error;

use ast::tree;

pub trait Visit {
    fn stmt(&self, stmt: &tree::Statement) -> Result<(), Box<error::Error>>;
    fn expr(&self, expr: &tree::Expression) -> Result<(), Box<error::Error>>;
    fn val(&self, expr: &tree::Value) -> Result<(), Box<error::Error>>;
}
