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
use std::error::Error;
use std::fs::File;
use std::io;
use std::path::{Path, PathBuf};
use std::process;
use std::rc::Rc;

use ucglib::build;
use ucglib::build::assets::{Cache, MemoryCache};
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
             (about: "Build a list of ucg files.")
             (@arg recurse: -r +required conflicts_with[INPUT] "Whether we should recurse in directories or not.")
             (@arg INPUT: ... "Input ucg files or directories to build. If not provided then build the contents of the current directory.")
            )
            (@subcommand validate =>
             (about: "Check a list of ucg files for errors and run assertions.")
             (@arg recurse: -r +required conflicts_with[INPUT] "Whether we should recurse or not.")
             (@arg INPUT: ... "Input ucg files or directories to validate. If not provided it will scan the directories for files with _test.ucg")
            )
            (@subcommand converters =>
             (about: "list the available converters")
             (@arg name: -c --converter-name +takes_value "Optionally print help for the provided converter")
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

fn build_file(
    file: &str,
    validate: bool,
    cache: Rc<RefCell<Cache>>,
) -> Result<build::Builder, Box<Error>> {
    let root = PathBuf::from(file);
    let mut builder = build::Builder::new(root.parent().unwrap(), cache);
    if validate {
        builder.enable_validate_mode();
    }
    try!(builder.build_file(file));
    if validate {
        println!("{}", builder.assert_collector.summary);
    }
    Ok(builder)
}

fn do_validate(file: &str, cache: Rc<RefCell<Cache>>) -> bool {
    println!("Validating {}", file);
    match build_file(file, true, cache) {
        Ok(_) => println!("{} validates\n\n", file),
        Err(msg) => {
            eprintln!("Err: {}", msg);
            return false;
        }
    }
    return true;
}

fn do_compile(file: &str, cache: Rc<RefCell<Cache>>) -> bool {
    println!("Building {}", file);
    let builder = match build_file(file, false, cache.clone()) {
        Ok(builder) => builder,
        Err(err) => {
            eprintln!("{:?}", err);
            return false;
        }
    };
    let (typ, val) = match builder.out_lock {
        Some((ref typ, ref val)) => (typ, val.clone()),
        None => {
            eprintln!("Build results in no artifacts.");
            return false;
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
            return false;
        }
    }
}

fn visit_ucg_files(
    path: &Path,
    recurse: bool,
    validate: bool,
    cache: Rc<RefCell<Cache>>,
) -> Result<bool, Box<Error>> {
    let our_path = String::from(path.to_string_lossy());
    let mut result = true;
    if path.is_dir() {
        for entry in try!(std::fs::read_dir(path)) {
            let next_item = try!(entry);
            let next_path = next_item.path();
            let path_as_string = String::from(next_path.to_string_lossy());
            if next_path.is_dir() && recurse {
                if let Err(msg) = visit_ucg_files(&next_path, recurse, validate, cache.clone()) {
                    eprintln!("Err 1: {}", msg);
                    result = false;
                }
            } else {
                if validate && path_as_string.ends_with("_test.ucg") {
                    if !do_validate(&path_as_string, cache.clone()) {
                        result = false;
                    }
                } else {
                    if !do_compile(&path_as_string, cache.clone()) {
                        result = false;
                    }
                }
            }
        }
    } else if validate && our_path.ends_with("_test.ucg") {
        if !do_validate(&our_path, cache) {
            result = false;
        }
    } else {
        if !do_compile(&our_path, cache) {
            result = false;
        }
    }
    Ok(result)
}

fn main() {
    let app = do_flags();
    let cache: Rc<RefCell<Cache>> = Rc::new(RefCell::new(MemoryCache::new()));
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
        let files = matches.values_of("INPUT");
        let recurse = matches.is_present("recurse");
        let mut ok = true;
        if files.is_none() {
            let curr_dir = std::env::current_dir().unwrap();
            let ok = visit_ucg_files(curr_dir.as_path(), recurse, false, cache.clone());
            if let Ok(false) = ok {
                process::exit(1)
            }
        }
        for file in files.unwrap() {
            let pb = PathBuf::from(file);
            if let Ok(false) = visit_ucg_files(&pb, recurse, false, cache.clone()) {
                ok = false;
            }
        }
        if !ok {
            process::exit(1)
        }
    } else if let Some(matches) = app.subcommand_matches("validate") {
        let files = matches.values_of("INPUT");
        let recurse = matches.is_present("recurse");
        if files.is_none() {
            let curr_dir = std::env::current_dir().unwrap();
            let ok = visit_ucg_files(curr_dir.as_path(), recurse, true, cache.clone());
            if let Ok(false) = ok {
                process::exit(1)
            }
        } else {
            let mut ok = true;
            for file in files.unwrap() {
                let pb = PathBuf::from(file);
                if pb.is_dir() {
                    if let Ok(false) = visit_ucg_files(pb.as_path(), recurse, true, cache.clone()) {
                        ok = false;
                    }
                } else {
                    match build_file(file, true, cache.clone()) {
                        Ok(b) => {
                            if b.assert_collector.success {
                                println!("File {} Validates", file);
                            } else {
                                println!("File {} Fails", file);
                                ok = false;
                            }
                        }
                        Err(msg) => {
                            // We continue to process the other files despite this failure.
                            eprintln!("{}", msg);
                            ok = false;
                        }
                    }
                }
            }
            if !ok {
                process::exit(1)
            }
        }
        process::exit(0);
    } else if let Some(_todo) = app.subcommand_matches("converters") {
        // TODO(jwall): Flesh this command out.
    }
}
