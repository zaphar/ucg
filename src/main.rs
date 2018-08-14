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
#[macro_use]
extern crate clap;
extern crate ucglib;

use std::cell::RefCell;
use std::fs::File;
use std::io;
use std::path::PathBuf;
use std::process;
use std::rc::Rc;

use ucglib::build;
use ucglib::build::assets::MemoryCache;
use ucglib::build::Val;
use ucglib::convert::traits;
use ucglib::convert::ConverterRunner;

// TODO(jwall): List the target output types automatically.
fn do_flags<'a>() -> clap::ArgMatches<'a> {
    clap_app!(
        ucg =>
            (version: crate_version!())
            (author: crate_authors!())
            (about: "Universal Configuration Grammar compiler.")
            (@subcommand build =>
             (about: "Compile a specific ucg file.")
             (@arg sym: --sym +takes_value +required "Specify a specific let binding in the ucg file to output.")
             (@arg target: --target -t +required +takes_value "Target output type. (flags, json, env, exec)")
             (@arg out: --out -o +takes_value "Output file to write to.")
             (@arg INPUT: +required "Input ucg file to build.")
            )
            (@subcommand validate =>
             (about: "Check a specific ucg file for errors.")
             (@arg INPUT: +required "Input ucg file to validate.")
            )
    ).get_matches()
}

fn run_converter(c: ConverterRunner, v: Rc<Val>, f: Option<&str>) -> traits::Result {
    let file: Box<std::io::Write> = match f {
        Some(f) => Box::new(try!(File::create(f))),
        None => Box::new(io::stdout()),
    };
    c.convert(v, file)
}

fn main() {
    let app = do_flags();
    if let Some(matches) = app.subcommand_matches("build") {
        let file = matches.value_of("INPUT").unwrap();
        let out = matches.value_of("out");
        let sym = matches.value_of("sym");
        let target = matches.value_of("target").unwrap();
        let root = PathBuf::from(file);
        let cache = Rc::new(RefCell::new(MemoryCache::new()));
        let mut builder = build::Builder::new(root.parent().unwrap(), cache);
        match ConverterRunner::new(target) {
            Ok(converter) => {
                let result = builder.build_file(file);
                if !result.is_ok() {
                    eprintln!("{:?}", result.err().unwrap());
                    process::exit(1);
                }
                let val = match sym {
                    Some(sym_name) => builder.get_out_by_name(sym_name),
                    None => builder.last,
                };
                match val {
                    Some(value) => {
                        run_converter(converter, value, out).unwrap();
                        eprintln!("Build successful");
                        process::exit(0);
                    }
                    None => {
                        eprintln!("Build results in no value.");
                        process::exit(1);
                    }
                }
            }
            Err(msg) => {
                eprintln!("{}", msg);
                process::exit(1);
            }
        }
    } else if let Some(matches) = app.subcommand_matches("validate") {
        let file = matches.value_of("INPUT").unwrap();
        let cache = Rc::new(RefCell::new(MemoryCache::new()));
        let mut builder = build::Builder::new(std::env::current_dir().unwrap(), cache);
        builder.enable_validate_mode();
        builder.build_file(file).unwrap();
        println!("File Validates");
        process::exit(0);
    }
}
