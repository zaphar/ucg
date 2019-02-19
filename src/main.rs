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
extern crate dirs;
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
use ucglib::convert::{ConverterRegistry, ImporterRegistry};

fn do_flags<'a, 'b>() -> clap::App<'a, 'b> {
    clap_app!(
        ucg =>
            (version: crate_version!())
            (author: crate_authors!())
            (about: "Universal Configuration Grammar compiler.")
            (@arg nostrict: --("no-strict") "Turn off strict checking.")
            (@subcommand eval =>
             (about: "Evaluate an expression with an optional ucg file as context.")
             (@arg expr: --expr -e +takes_value +required "Expression to evaluate.")
             (@arg target: --format +takes_value "Output type. (flags, json, env, exec) defaults to json.")
             (@arg INPUT: "ucg file to use as context for the expression.")
            )
            (@subcommand build =>
             (about: "Build a list of ucg files.")
             (@arg recurse: -r "Whether we should recurse in directories or not.")
             (@arg INPUT: ... "Input ucg files or directories to build. If not provided then build the contents of the current directory.")
            )
            (@subcommand test =>
             (about: "Check a list of ucg files for errors and run test assertions.")
             (@arg recurse: -r "Whether we should recurse or not.")
             (@arg INPUT: ... "Input ucg files or directories to run test assertions for. If not provided it will scan the current directory for files with _test.ucg")
            )
            (@subcommand converters =>
             (about: "list the available converters")
            )
            (@subcommand importers =>
             (about: "list the available importers for includes")
            )
            (@subcommand env =>
             (about: "Describe the environment variables ucg uses.")
            )
    )
}

fn run_converter(c: &traits::Converter, v: Rc<Val>, f: Option<&str>) -> traits::ConvertResult {
    let mut file: Box<std::io::Write> = match f {
        Some(f) => {
            let mut path_buf = PathBuf::from(f);
            path_buf.set_extension(c.file_ext());
            let new_path = path_buf.to_str().unwrap();
            Box::new(File::create(&new_path)?)
        }
        None => Box::new(io::stdout()),
    };
    c.convert(v, file.as_mut())
}

fn build_file<'a>(
    file: &'a str,
    validate: bool,
    strict: bool,
    import_paths: &'a Vec<PathBuf>,
    cache: Rc<RefCell<Cache>>,
) -> Result<build::FileBuilder<'a>, Box<dyn Error>> {
    let mut file_path_buf = PathBuf::from(file);
    if file_path_buf.is_relative() {
        file_path_buf = std::env::current_dir()?.join(file_path_buf);
    }
    let mut builder = build::FileBuilder::new(std::env::current_dir()?, import_paths, cache);
    builder.set_strict(strict);
    if validate {
        builder.enable_validate_mode();
    }
    builder.build(file_path_buf)?;
    if validate {
        println!("{}", builder.assert_collector.summary);
    }
    Ok(builder)
}

fn do_validate(
    file: &str,
    strict: bool,
    import_paths: &Vec<PathBuf>,
    cache: Rc<RefCell<Cache>>,
) -> bool {
    println!("Validating {}", file);
    match build_file(file, true, strict, import_paths, cache) {
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

fn do_compile(
    file: &str,
    strict: bool,
    import_paths: &Vec<PathBuf>,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
) -> bool {
    println!("Building {}", file);
    let builder = match build_file(file, false, strict, import_paths, cache.clone()) {
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
    strict: bool,
    import_paths: &Vec<PathBuf>,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
) -> Result<bool, Box<dyn Error>> {
    let our_path = String::from(path.to_string_lossy());
    let mut result = true;
    let mut summary = String::new();
    if path.is_dir() {
        let mut dir_iter = std::fs::read_dir(path)?.peekable();
        loop {
            let entry = match dir_iter.next() {
                Some(e) => e,
                None => {
                    break;
                }
            };
            let next_item = entry?;
            let next_path = next_item.path();
            let path_as_string = String::from(next_path.to_string_lossy());
            if next_path.is_dir() && recurse {
                if let Err(e) = visit_ucg_files(
                    &next_path,
                    recurse,
                    validate,
                    strict,
                    import_paths,
                    cache.clone(),
                    registry,
                ) {
                    eprintln!("{}", e);
                    result = false;
                }
            } else {
                if validate && path_as_string.ends_with("_test.ucg") {
                    if !do_validate(&path_as_string, strict, import_paths, cache.clone()) {
                        result = false;
                        summary.push_str(format!("{} - FAIL\n", path_as_string).as_str())
                    } else {
                        summary.push_str(format!("{} - PASS\n", path_as_string).as_str())
                    }
                } else if !validate && path_as_string.ends_with(".ucg") {
                    if !do_compile(
                        &path_as_string,
                        strict,
                        import_paths,
                        cache.clone(),
                        registry,
                    ) {
                        result = false;
                    }
                }
            }
        }
    } else if validate && our_path.ends_with("_test.ucg") {
        if !do_validate(&our_path, strict, import_paths, cache) {
            result = false;
            summary.push_str(format!("{} - FAIL\n", our_path).as_str());
        } else {
            summary.push_str(format!("{} - PASS\n", &our_path).as_str());
        }
    } else if !validate {
        if !do_compile(&our_path, strict, import_paths, cache, registry) {
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
    import_paths: &Vec<PathBuf>,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
    strict: bool,
) {
    let file = matches.value_of("INPUT").unwrap_or("std/functional.ucg");
    let sym = matches.value_of("expr");
    let target = matches.value_of("target").unwrap_or("json");
    let mut builder =
        build::FileBuilder::new(std::env::current_dir().unwrap(), import_paths, cache);
    builder.set_strict(strict);
    match registry.get_converter(target) {
        Some(converter) => {
            // TODO(jwall): We should warn if this is a test file.
            let result = builder.build(file);
            if !result.is_ok() {
                eprintln!("{:?}", result.err().unwrap());
                process::exit(1);
            }
            let val = match sym {
                Some(sym_name) => {
                    let normalized = if !sym_name.ends_with(";") {
                        let mut temp = sym_name.to_owned();
                        temp.push_str(";");
                        temp
                    } else {
                        sym_name.to_owned()
                    };
                    let mut builder = builder.clone_builder();
                    match builder.eval_string(&normalized) {
                        Ok(v) => Some(v.clone()),
                        Err(e) => {
                            eprintln!("Err: {}", e);
                            process::exit(1);
                        }
                    }
                }
                None => builder.last,
            };
            match val {
                Some(value) => {
                    // We use None here because we always output to stdout for an inspect.
                    run_converter(converter, value, None).unwrap();
                    println!("");
                    process::exit(0);
                }
                None => {
                    eprintln!("No value.");
                    process::exit(1);
                }
            }
        }
        None => {
            eprintln!(
                "No such format {}\nrun `ucg converters` to see available formats.",
                target
            );
            process::exit(1);
        }
    }
}

fn build_command(
    matches: &clap::ArgMatches,
    import_paths: &Vec<PathBuf>,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
    strict: bool,
) {
    let files = matches.values_of("INPUT");
    let recurse = matches.is_present("recurse");
    let mut ok = true;
    if files.is_none() {
        let curr_dir = std::env::current_dir().unwrap();
        let ok = visit_ucg_files(
            curr_dir.as_path(),
            recurse,
            false,
            strict,
            import_paths,
            cache.clone(),
            &registry,
        );
        if let Ok(false) = ok {
            process::exit(1)
        }
        process::exit(0);
    }
    for file in files.unwrap() {
        let pb = PathBuf::from(file);
        if let Ok(false) = visit_ucg_files(
            &pb,
            recurse,
            false,
            strict,
            import_paths,
            cache.clone(),
            &registry,
        ) {
            ok = false;
        }
    }
    if !ok {
        process::exit(1)
    }
}

fn test_command(
    matches: &clap::ArgMatches,
    import_paths: &Vec<PathBuf>,
    cache: Rc<RefCell<Cache>>,
    registry: &ConverterRegistry,
    strict: bool,
) {
    let files = matches.values_of("INPUT");
    let recurse = matches.is_present("recurse");
    if files.is_none() {
        let curr_dir = std::env::current_dir().unwrap();
        let ok = visit_ucg_files(
            curr_dir.as_path(),
            recurse,
            true,
            strict,
            import_paths,
            cache.clone(),
            &registry,
        );
        if let Ok(false) = ok {
            process::exit(1)
        }
    } else {
        let mut ok = true;
        for file in files.unwrap() {
            let pb = PathBuf::from(file);
            //if pb.is_dir() {
            if let Ok(false) = visit_ucg_files(
                pb.as_path(),
                recurse,
                true,
                strict,
                import_paths,
                cache.clone(),
                &registry,
            ) {
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

fn importers_command(registry: &ImporterRegistry) {
    println!("Available importers");
    println!("");
    for (name, _importer) in registry.get_importer_list().iter() {
        println!("- {}", name);
    }
}

fn env_help() {
    println!("Universal Configuration Grammar compiler.");
    println!("");
    println!("ENVIRONMENT VARIABLES:");
    println!("");
    println!(
        "
    UCG_IMPORT_PATH=\"{}\"
      A list of paths to search for imports from. Uses the same syntax
      as your platforms $PATH environment variable.
",
        std::env::var("UCG_IMPORT_PATH").unwrap_or(String::new())
    );
}

fn main() {
    let mut app = do_flags();
    let app_matches = app.clone().get_matches();
    let cache: Rc<RefCell<Cache>> = Rc::new(RefCell::new(MemoryCache::new()));
    let registry = ConverterRegistry::make_registry();
    let mut import_paths = Vec::new();
    if let Some(mut p) = dirs::home_dir() {
        p.push(".ucg");
        // Attempt to create directory if it doesn't exist.
        if !p.exists() {
            std::fs::create_dir(&p).unwrap();
        }
        import_paths.push(p);
    }
    if let Ok(path_list_str) = std::env::var("UCG_IMPORT_PATH") {
        for p in std::env::split_paths(&path_list_str) {
            import_paths.push(p);
        }
    }
    let strict = if app_matches.is_present("nostrict") {
        false
    } else {
        true
    };
    if let Some(matches) = app_matches.subcommand_matches("eval") {
        inspect_command(matches, &import_paths, cache, &registry, strict);
    } else if let Some(matches) = app_matches.subcommand_matches("build") {
        build_command(matches, &import_paths, cache, &registry, strict);
    } else if let Some(matches) = app_matches.subcommand_matches("test") {
        test_command(matches, &import_paths, cache, &registry, strict);
    } else if let Some(_) = app_matches.subcommand_matches("converters") {
        converters_command(&registry)
    } else if let Some(_) = app_matches.subcommand_matches("importers") {
        let registry = ImporterRegistry::make_registry();
        importers_command(&registry)
    } else if let Some(_) = app_matches.subcommand_matches("env") {
        env_help()
    } else {
        app.print_help().unwrap();
        println!("");
    }
}
