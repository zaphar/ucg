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
extern crate rustyline;
extern crate ucglib;

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::error::Error;
use std::fs::File;
use std::io;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process;

use ucglib::build;
use ucglib::build::opcode::Environment;
use ucglib::convert::{ConverterRegistry, ImporterRegistry};
use ucglib::iter::OffsetStrIter;
use ucglib::parse::parse;

fn do_flags<'a, 'b>() -> clap::App<'a, 'b> {
    clap_app!(
        ucg =>
            (version: crate_version!())
            (author: crate_authors!())
            (about: "Universal Configuration Grammar compiler.")
            (@arg nostrict: --("no-strict") "Turn off strict checking.")
            (@subcommand repl =>
                (about: "Start the ucg repl for interactive evaluation.")
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
            (@subcommand fmt =>
             (about: "Format ucg files automatically.")
             (@arg recurse: -r "Whether we should recurse or not.")
             (@arg indent: -i --indent "How many spaces to indent by. Defaults to 4")
             (@arg INPUT: ... "Input ucg files or directories to format")
             (@arg write: -w --overwrite "Whether to overwrite the with the formatted form")
            )
            (@subcommand converters =>
             (about: "list the available converters")
             (@arg converter: "Converter name to get help for.")
            )
            (@subcommand importers =>
             (about: "list the available importers for includes")
            )
            (@subcommand env =>
             (about: "Describe the environment variables ucg uses.")
            )
    )
}

struct StdoutWrapper(io::Stdout);

impl StdoutWrapper {
    fn new() -> Self {
        Self(io::stdout())
    }
}

impl io::Write for StdoutWrapper {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

impl Clone for StdoutWrapper {
    fn clone(&self) -> Self {
        Self(io::stdout())
    }
}

struct StderrWrapper(io::Stderr);

impl StderrWrapper {
    fn new() -> Self {
        Self(io::stderr())
    }
}

impl io::Write for StderrWrapper {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

impl Clone for StderrWrapper {
    fn clone(&self) -> Self {
        Self(io::stderr())
    }
}

// TODO(jwall): Build sharable stdout stderr providers.
fn build_file<'a>(
    file: &'a str,
    validate: bool,
    strict: bool,
    import_paths: &'a Vec<PathBuf>,
    env: &'a RefCell<Environment<StdoutWrapper, StderrWrapper>>,
) -> Result<build::FileBuilder<'a, StdoutWrapper, StderrWrapper>, Box<dyn Error>> {
    let mut file_path_buf = PathBuf::from(file);
    if file_path_buf.is_relative() {
        file_path_buf = std::env::current_dir()?.join(file_path_buf);
    }
    let mut builder = build::FileBuilder::new(std::env::current_dir()?, import_paths, env);
    builder.set_strict(strict);
    if validate {
        builder.enable_validate_mode();
    }
    builder.build(file_path_buf)?;
    if validate {
        println!("{}", builder.assert_summary());
    }
    Ok(builder)
}

fn do_validate<'a>(
    file: &'a str,
    strict: bool,
    import_paths: &'a Vec<PathBuf>,
    env: &'a RefCell<Environment<StdoutWrapper, StderrWrapper>>,
) -> bool {
    println!("Validating {}", file);
    match build_file(file, true, strict, import_paths, env) {
        Ok(b) => {
            if b.assert_results() {
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

fn do_compile<'a>(
    file: &'a str,
    strict: bool,
    import_paths: &'a Vec<PathBuf>,
    env: &'a RefCell<Environment<StdoutWrapper, StderrWrapper>>,
) -> bool {
    println!("Building {}", file);
    let builder = match build_file(file, false, strict, import_paths, env) {
        Ok(builder) => builder,
        Err(err) => {
            eprintln!("{}", err);
            return false;
        }
    };
    if builder.out.is_none() {
        eprintln!("Build results in no artifacts.");
    }
    return true;
}

fn visit_ucg_files(
    path: &Path,
    recurse: bool,
    validate: bool,
    strict: bool,
    import_paths: &Vec<PathBuf>,
    env: &RefCell<Environment<StdoutWrapper, StderrWrapper>>,
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
                if let Err(e) =
                    visit_ucg_files(&next_path, recurse, validate, strict, import_paths, env)
                {
                    eprintln!("{}", e);
                    result = false;
                }
            } else {
                if validate && path_as_string.ends_with("_test.ucg") {
                    if !do_validate(&path_as_string, strict, import_paths, env) {
                        result = false;
                        summary.push_str(format!("{} - FAIL\n", path_as_string).as_str())
                    } else {
                        summary.push_str(format!("{} - PASS\n", path_as_string).as_str())
                    }
                } else if !validate && path_as_string.ends_with(".ucg") {
                    if !do_compile(&path_as_string, strict, import_paths, env) {
                        result = false;
                    }
                }
            }
        }
    } else if validate && our_path.ends_with("_test.ucg") {
        if !do_validate(&our_path, strict, import_paths, env) {
            result = false;
            summary.push_str(format!("{} - FAIL\n", our_path).as_str());
        } else {
            summary.push_str(format!("{} - PASS\n", &our_path).as_str());
        }
    } else if !validate {
        if !do_compile(&our_path, strict, import_paths, env) {
            result = false;
        }
    }
    if validate && !summary.is_empty() {
        println!("RESULTS:");
        println!("{}", summary);
    }
    Ok(result)
}

fn build_command(
    matches: &clap::ArgMatches,
    import_paths: &Vec<PathBuf>,
    strict: bool,
    env: &RefCell<Environment<StdoutWrapper, StderrWrapper>>,
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
            env,
        );
        if let Ok(false) = ok {
            process::exit(1)
        }
        process::exit(0);
    }
    for file in files.unwrap() {
        let pb = PathBuf::from(file);
        if let Ok(false) = visit_ucg_files(&pb, recurse, false, strict, import_paths, env) {
            ok = false;
        }
    }
    if !ok {
        process::exit(1)
    }
}

fn fmt_file(p: &Path, indent: usize, overwrite: bool) -> std::result::Result<(), Box<dyn Error>> {
    let mut contents = String::new();
    {
        let mut f = File::open(p)?;
        f.read_to_string(&mut contents)?;
    }
    let mut comment_map = BTreeMap::new();
    let stmts = parse(OffsetStrIter::new(&contents), Some(&mut comment_map))?;
    if overwrite {
        let mut printer = ucglib::ast::printer::AstPrinter::new(indent, File::create(p)?)
            .with_comment_map(&comment_map);
        printer.render(&stmts)?;
    } else {
        let mut printer = ucglib::ast::printer::AstPrinter::new(indent, std::io::stdout())
            .with_comment_map(&comment_map);
        printer.render(&stmts)?;
    }
    Ok(())
}

fn fmt_dir(p: &Path, recurse: bool, indent: usize) -> std::result::Result<(), Box<dyn Error>> {
    // TODO(jwall): We should handle this error more gracefully
    // for the user here.
    let dir_iter = std::fs::read_dir(p)?.peekable();
    for entry in dir_iter {
        let next_item = entry.unwrap();
        let path = next_item.path();
        if path.is_dir() && recurse {
            fmt_dir(&path, recurse, indent)?;
        } else {
            fmt_file(&path, indent, true)?;
        }
    }
    Ok(())
}

fn fmt_command(matches: &clap::ArgMatches) -> std::result::Result<(), Box<dyn Error>> {
    let files = matches.values_of("INPUT");
    let recurse = matches.is_present("recurse");
    let overwrite = matches.is_present("write");
    let indent = match matches.value_of("indent") {
        Some(s) => s.parse::<usize>()?,
        None => 4,
    };

    let mut paths = Vec::new();
    if files.is_none() {
        paths.push(std::env::current_dir()?);
    } else {
        for f in files.unwrap() {
            paths.push(PathBuf::from(f));
        }
    }
    for p in paths {
        if p.is_dir() {
            fmt_dir(&p, recurse, indent)?;
        } else {
            fmt_file(&p, indent, overwrite)?;
        }
    }
    Ok(())
}

fn test_command(
    matches: &clap::ArgMatches,
    import_paths: &Vec<PathBuf>,
    strict: bool,
    env: &RefCell<Environment<StdoutWrapper, StderrWrapper>>,
) {
    let files = matches.values_of("INPUT");
    let recurse = matches.is_present("recurse");
    if files.is_none() {
        let curr_dir = std::env::current_dir().unwrap();
        let ok = visit_ucg_files(curr_dir.as_path(), recurse, true, strict, import_paths, env);
        if let Ok(false) = ok {
            process::exit(1)
        }
    } else {
        let mut ok = true;
        for file in files.unwrap() {
            let pb = PathBuf::from(file);
            //if pb.is_dir() {
            if let Ok(false) =
                visit_ucg_files(pb.as_path(), recurse, true, strict, import_paths, env)
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

fn converters_command(matches: &clap::ArgMatches, registry: &ConverterRegistry) {
    if let Some(ref cname) = matches.value_of("converter") {
        let mut found = false;
        for (name, c) in registry.get_converter_list().iter() {
            if cname == name {
                println!("* {}", name);
                println!("Description: {}", c.description());
                println!("Output Extension: `.{}`", c.file_ext());
                println!("");
                println!("{}", c.help());
                found = true;
            }
        }
        if !found {
            println!("No such converter {}", cname);
            process::exit(1);
        }
    } else {
        println!("Available converters:");
        println!("");
        for (name, c) in registry.get_converter_list().iter() {
            println!("* {}", name);
            println!("Description: {}", c.description());
            println!("Output Extension: `.{}`", c.file_ext());
            println!("");
        }
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
    println!(
        include_str!("help/env.txt"),
        std::env::var("UCG_IMPORT_PATH").unwrap_or(String::new())
    );
}

fn do_repl(import_paths: &Vec<PathBuf>, strict: bool) -> std::result::Result<(), Box<dyn Error>> {
    let config = rustyline::Config::builder();
    let mut editor = rustyline::Editor::<()>::with_config(
        config
            .history_ignore_space(true)
            .history_ignore_dups(false)
            .build(),
    );
    let path_home = dirs::home_dir().unwrap_or(std::env::temp_dir());
    let config_home = std::env::var("XDG_CACHE_HOME")
        .unwrap_or_else(|_| format!("{}/.cache", path_home.to_string_lossy()));
    let mut config_home = PathBuf::from(config_home);
    config_home.push("ucg");
    config_home.push("line_hist");
    if editor.load_history(&config_home).is_err() {
        eprintln!(
            "No history file {} Continuing without history.",
            config_home.to_string_lossy()
        );
        // introduce a scope so the file will get automatically closed after
        {
            let base_dir = config_home.parent().unwrap();
            if !base_dir.exists() {
                if let Err(e) = std::fs::create_dir_all(base_dir) {
                    eprintln!("{}", e);
                }
            }
            if let Err(e) = std::fs::File::create(&config_home) {
                eprintln!("{}", e);
            }
        }
    }
    let env = std::cell::RefCell::new(build::opcode::Environment::new(
        StdoutWrapper::new(),
        StderrWrapper::new(),
    ));
    let mut builder = build::FileBuilder::new(std::env::current_dir()?, import_paths, &env);
    builder.set_strict(strict);

    builder.repl(editor, config_home)?;
    Ok(())
}

fn repl(import_paths: &Vec<PathBuf>, strict: bool) {
    if let Err(e) = do_repl(import_paths, strict) {
        eprintln!("{}", e);
        process::exit(1);
    }
}

fn main() {
    let mut app = do_flags();
    let app_matches = app.clone().get_matches();
    // FIXME(jwall): Do we want these to be shared or not?
    let registry = ConverterRegistry::make_registry();
    let mut import_paths = Vec::new();
    let env = RefCell::new(Environment::new(StdoutWrapper::new(), StderrWrapper::new()));
    if let Some(mut p) = dirs::home_dir() {
        p.push(".ucg");
        // Attempt to create directory if it doesn't exist.
        if !p.exists() {
            if let Err(e) = std::fs::create_dir(&p) {
                eprintln!("Unable to create .ucg directory {}", e);
            }
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
    if let Some(matches) = app_matches.subcommand_matches("build") {
        build_command(matches, &import_paths, strict, &env);
    } else if let Some(matches) = app_matches.subcommand_matches("test") {
        test_command(matches, &import_paths, strict, &env);
    } else if let Some(matches) = app_matches.subcommand_matches("converters") {
        converters_command(matches, &registry)
    } else if let Some(_) = app_matches.subcommand_matches("importers") {
        let registry = ImporterRegistry::make_registry();
        importers_command(&registry)
    } else if let Some(_) = app_matches.subcommand_matches("env") {
        env_help()
    } else if let Some(_) = app_matches.subcommand_matches("repl") {
        repl(&import_paths, strict)
    } else if let Some(matches) = app_matches.subcommand_matches("fmt") {
        if let Err(e) = fmt_command(matches) {
            eprintln!("{}", e);
            process::exit(1);
        }
    } else {
        app.print_help().unwrap();
        println!("");
    }
}
