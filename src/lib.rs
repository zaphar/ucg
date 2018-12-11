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
// #![feature(trace_macros,log_syntax)]

//! # ucg, A universal configuration grammar.
//!
//! Language reference is at https://ucg.marzhillstudios.com/reference

// The following is necessary to allow the macros in tokenizer and parse modules
// to succeed.
#![recursion_limit = "128"]
#[macro_use]
extern crate abortable_parser;
extern crate serde_json;
extern crate serde_yaml;
extern crate simple_error;
extern crate toml;
extern crate xml;

#[macro_use]
pub mod ast;
#[macro_use]
pub mod tokenizer;
pub mod build;
pub mod convert;
pub mod error;
pub mod iter;
pub mod parse;

mod format;

pub use crate::ast::Expression;
pub use crate::ast::Statement;
pub use crate::ast::Value;

pub use crate::build::Builder;
pub use crate::build::Val;
pub use crate::parse::parse;
