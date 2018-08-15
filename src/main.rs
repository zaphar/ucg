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
            (@subcommand inspect =>
             (about: "Inspect a specific symbol in a ucg file.")
             (@arg sym: --sym +takes_value +required "Specify a specific binding in the ucg file to output.")
             (@arg target: --format +required +takes_value "Inspect output type. (flags, json, env, exec)")
             (@arg INPUT: +required "Input ucg file to inspect symbol from.")
            )
            (@subcommand build =>
             (about: "Build a specific ucg file.")
             (@arg out: --out -o +takes_value "Output file to write to.")
             (@arg INPUT: ... +required "Input ucg files to build.")
            )
            (@subcommand validate =>
             (about: "Check a specific ucg file for errors.")
             (@arg INPUT: ... +required "Input ucg files to validate.")
            )
    ).get_matches()
}

fn run_converter(c: ConverterRunner, v: Rc<Val>, f: Option<&str>) -> traits::Result {
    let file: Box<std::io::Write> = match f {
        Some(f) => {
            let mut path_buf = PathBuf::from(f);
            path_buf.set_extension(c.ext());
            let new_path = path_buf.to_str().unwrap();
            Box::new(try!(File::create(&new_path)))
        }
        None => Box::new(io::stdout()),
    };
    c.convert(v, file)
}

fn main() {
    let app = do_flags();
    let cache = Rc::new(RefCell::new(MemoryCache::new()));
    if let Some(matches) = app.subcommand_matches("inspect") {
        let file = matches.value_of("INPUT").unwrap();
        let sym = matches.value_of("sym");
        let target = matches.value_of("target").unwrap();
        let root = PathBuf::from(file);
        let mut builder = build::Builder::new(root.parent().unwrap(), cache);
        match ConverterRunner::new(target) {
            Ok(converter) => {
                // TODO(jwall): We should warn if this is a test file.
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
                        // We use None here because we always output to stdout for an inspect.
                        run_converter(converter, value, None).unwrap();
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
    } else if let Some(matches) = app.subcommand_matches("build") {
        let files = matches.values_of("INPUT").unwrap();
        for file in files {
            let root = PathBuf::from(file);
            let mut builder = build::Builder::new(root.parent().unwrap(), cache);
            let result = builder.build_file(file);
            if !result.is_ok() {
                eprintln!("{:?}", result.err().unwrap());
                process::exit(1);
            }
            let (typ, val) = match builder.out_lock {
                Some((ref typ, ref val)) => (typ, val.clone()),
                None => {
                    eprintln!("Build results in no value.");
                    process::exit(1);
                }
            };
            match ConverterRunner::new(typ) {
                Ok(converter) => {
                    run_converter(converter, val, Some(file)).unwrap();
                    eprintln!("Build successful");
                    process::exit(0);
                }
                Err(msg) => {
                    eprintln!("{}", msg);
                    process::exit(1);
                }
            }
        }
    } else if let Some(matches) = app.subcommand_matches("validate") {
        let files = matches.values_of("INPUT").unwrap();
        let mut builder = build::Builder::new(std::env::current_dir().unwrap(), cache);
        builder.enable_validate_mode();
        for file in files {
            builder.build_file(file).unwrap();
            println!("File Validates");
        }
        process::exit(0);
    }
}
