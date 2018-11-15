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
use ucglib::convert::ConverterRegistry;

// TODO(jwall): List the target output types automatically.
fn do_flags<'a, 'b>() -> clap::App<'a, 'b> {
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
            (@subcommand test =>
             (about: "Check a list of ucg files for errors and run test assertions.")
             (@arg recurse: -r +required conflicts_with[INPUT] "Whether we should recurse or not.")
             (@arg INPUT: ... "Input ucg files or directories to run test assertions for. If not provided it will scan the current directory for files with _test.ucg")
            )
            (@subcommand converters =>
             (about: "list the available converters")
            )
    )
}

fn run_converter(c: &traits::Converter, v: Rc<Val>, f: Option<&str>) -> traits::Result {
    let mut file: Box<std::io::Write> = match f {
        Some(f) => {
            let mut path_buf = PathBuf::from(f);
            path_buf.set_extension(c.file_ext());
            let new_path = path_buf.to_str().unwrap();
            Box::new(try!(File::create(&new_path)))
        }
        None => Box::new(io::stdout()),
    };
    c.convert(v, file.as_mut())
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
        Ok(b) => {
            if b.assert_collector.success {
                println!("File {} Pass\n", file);
            } else {
                println!("File {} Fail\n", file);
                return false;
            }
        }
        Err(msg) => {
            eprintln!("Err: {}", msg);
            return false;
        }
    }
    return true;
}

fn do_compile(file: &str, cache: Rc<RefCell<Cache>>, registry: &ConverterRegistry) -> bool {
    println!("Building {}", file);
    let builder = match build_file(file, false, cache.clone()) {
        Ok(builder) => builder,
        Err(err) => {
            eprintln!("{}", err);
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
    match registry.get_converter(typ) {
        Some(converter) => {
            run_converter(converter, val, Some(file)).unwrap();
            eprintln!("Build successful");
            return true;
        }
        None => {
            eprintln!("No such converter {}", typ);
            return false;
        }
    }
}

fn visit_ucg_files(
    path: &Path,
    recurse: bool,
    validate: bool,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
) -> Result<bool, Box<Error>> {
    let our_path = String::from(path.to_string_lossy());
    let mut result = true;
    // TODO(jwall): Report the failing files at the bottom.
    let mut summary = String::new();
    if path.is_dir() {
        let mut dir_iter = try!(std::fs::read_dir(path)).peekable();
        loop {
            let entry = match dir_iter.next() {
                Some(e) => e,
                None => {
                    break;
                }
            };
            let next_item = try!(entry);
            let next_path = next_item.path();
            let path_as_string = String::from(next_path.to_string_lossy());
            if next_path.is_dir() && recurse {
                if let Err(e) =
                    visit_ucg_files(&next_path, recurse, validate, cache.clone(), registry)
                {
                    eprintln!("{}", e);
                    result = false;
                }
            } else {
                if validate && path_as_string.ends_with("_test.ucg") {
                    if !do_validate(&path_as_string, cache.clone()) {
                        result = false;
                        summary.push_str(format!("{} - FAIL\n", path_as_string).as_str())
                    } else {
                        summary.push_str(format!("{} - PASS\n", path_as_string).as_str())
                    }
                } else if !validate && path_as_string.ends_with(".ucg") {
                    if !do_compile(&path_as_string, cache.clone(), registry) {
                        result = false;
                    }
                }
            }
        }
    } else if validate && our_path.ends_with("_test.ucg") {
        if !do_validate(&our_path, cache) {
            result = false;
            summary.push_str(format!("{} - FAIL\n", our_path).as_str());
        } else {
            summary.push_str(format!("{} - PASS\n", &our_path).as_str());
        }
    } else if !validate {
        if !do_compile(&our_path, cache, registry) {
            result = false;
        }
    }
    if validate && !summary.is_empty() {
        println!("RESULTS:");
        println!("{}", summary);
    }
    Ok(result)
}

fn inspect_command(
    matches: &clap::ArgMatches,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
) {
    let file = matches.value_of("INPUT").unwrap();
    let sym = matches.value_of("sym");
    let target = matches.value_of("target").unwrap();
    let root = PathBuf::from(file);
    let mut builder = build::Builder::new(root.parent().unwrap(), cache);
    match registry.get_converter(target) {
        Some(converter) => {
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
        None => {
            eprintln!("No such converter {}", target);
            process::exit(1);
        }
    }
}

fn build_command(
    matches: &clap::ArgMatches,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
) {
    let files = matches.values_of("INPUT");
    let recurse = matches.is_present("recurse");
    let mut ok = true;
    if files.is_none() {
        let curr_dir = std::env::current_dir().unwrap();
        let ok = visit_ucg_files(curr_dir.as_path(), recurse, false, cache.clone(), &registry);
        if let Ok(false) = ok {
            process::exit(1)
        }
        process::exit(0);
    }
    for file in files.unwrap() {
        let pb = PathBuf::from(file);
        if let Ok(false) = visit_ucg_files(&pb, recurse, false, cache.clone(), &registry) {
            ok = false;
        }
    }
    if !ok {
        process::exit(1)
    }
}

fn test_command(
    matches: &clap::ArgMatches,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
) {
    let files = matches.values_of("INPUT");
    let recurse = matches.is_present("recurse");
    if files.is_none() {
        let curr_dir = std::env::current_dir().unwrap();
        let ok = visit_ucg_files(curr_dir.as_path(), recurse, true, cache.clone(), &registry);
        if let Ok(false) = ok {
            process::exit(1)
        }
    } else {
        let mut ok = true;
        for file in files.unwrap() {
            let pb = PathBuf::from(file);
            //if pb.is_dir() {
            if let Ok(false) =
                visit_ucg_files(pb.as_path(), recurse, true, cache.clone(), &registry)
            {
                ok = false;
            }
        }
        if !ok {
            process::exit(1)
        }
    }
    process::exit(0);
}

fn converters_command(registry: &ConverterRegistry) {
    println!("Available converters:");
    println!("");
    for (name, c) in registry.get_converter_list().iter() {
        println!("- {}", name);
        println!("  Description: {}", c.description());
        println!("  Output Extension: `.{}`", c.file_ext());
        println!("");
    }
}

fn main() {
    let mut app = do_flags();
    let app_matches = app.clone().get_matches();
    let cache: Rc<RefCell<Cache>> = Rc::new(RefCell::new(MemoryCache::new()));
    let registry = ConverterRegistry::make_registry();
    if let Some(matches) = app_matches.subcommand_matches("inspect") {
        inspect_command(matches, cache, &registry);
    } else if let Some(matches) = app_matches.subcommand_matches("build") {
        build_command(matches, cache, &registry);
    } else if let Some(matches) = app_matches.subcommand_matches("test") {
        test_command(matches, cache, &registry);
    } else if let Some(_) = app_matches.subcommand_matches("converters") {
        converters_command(&registry)
    } else {
        app.print_help().unwrap();
        println!("");
    }
}
