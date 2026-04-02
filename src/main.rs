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
use ucglib::dep;
use ucglib::dep::registry::TagSource;
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
            (@subcommand lsp =>
             (about: "Start the UCG language server (LSP) communicating over stdio.")
             (after_help: include_str!("help/lsp.txt"))
            )
            (@subcommand dep =>
             (about: "Manage UCG package dependencies.")
             (@subcommand init =>
              (about: "Initialize a new ucg-deps file.")
              (@arg vendor: --vendor +takes_value "Vendor directory path (default: \"vendor\")")
              (@arg nix: --nix "Enable automatic Nix expression generation")
             )
             (@subcommand add =>
              (about: "Add or update a dependency.")
              (@arg URL: +required "Repository URL to add")
              (@arg version: --version +takes_value "Version constraint (default: \">= <latest>\")")
              (@arg type: --type +takes_value "Repository type: \"git\" or \"hg\" (default: \"git\")")
             )
             (@subcommand remove =>
              (about: "Remove a direct dependency.")
              (@arg URL: +required "Repository URL to remove")
             )
             (@subcommand lock =>
              (about: "Re-resolve all dependencies and rewrite ucg.lock.")
             )
             (@subcommand vendor =>
              (about: "Fetch all locked dependencies into the vendor directory.")
             )
             (@subcommand nix =>
              (about: "Regenerate ucg-deps.nix from ucg.lock.")
              (@arg stdout: --stdout "Write to stdout instead of ucg-deps.nix")
             )
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
    true
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
    true
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

    // Determine vendor dir to skip during recursion
    let vendor_dir_name = {
        let env_ref = env.borrow();
        if env_ref.package_root.is_some() {
            env_ref.vendor_dir.clone()
        } else {
            String::new()
        }
    };

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
                // Skip vendor directory during recursive traversal
                if !vendor_dir_name.is_empty() {
                    if let Some(dir_name) = next_path.file_name() {
                        if dir_name == vendor_dir_name.as_str() {
                            continue;
                        }
                    }
                }
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
                } else if !validate && path_as_string.ends_with(".ucg")
                    && !do_compile(&path_as_string, strict, import_paths, env) {
                        result = false;
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
    } else if !validate
        && !do_compile(&our_path, strict, import_paths, env) {
            result = false;
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
    if let Some(files) = files {
        for f in files {
            paths.push(PathBuf::from(f));
        }
    } else {
        paths.push(std::env::current_dir()?);
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
    if let Some(files) = files {
        let mut ok = true;
        for file in files {
            let pb = PathBuf::from(file);
            if let Ok(false) =
                visit_ucg_files(pb.as_path(), recurse, true, strict, import_paths, env)
            {
                ok = false;
            }
        }
        if !ok {
            process::exit(1)
        }
    } else {
        let curr_dir = std::env::current_dir().unwrap();
        let ok = visit_ucg_files(curr_dir.as_path(), recurse, true, strict, import_paths, env);
        if let Ok(false) = ok {
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
                println!();
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
        println!();
        for (name, c) in registry.get_converter_list().iter() {
            println!("* {}", name);
            println!("Description: {}", c.description());
            println!("Output Extension: `.{}`", c.file_ext());
            println!();
        }
    }
}

fn importers_command(registry: &ImporterRegistry) {
    println!("Available importers");
    println!();
    for (name, _importer) in registry.get_importer_list().iter() {
        println!("- {}", name);
    }
}

fn dep_command(matches: &clap::ArgMatches) {
    if let Err(e) = do_dep_command(matches) {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}

fn do_dep_command(matches: &clap::ArgMatches) -> Result<(), Box<dyn Error>> {
    if let Some(matches) = matches.subcommand_matches("init") {
        dep_init(matches)
    } else if let Some(matches) = matches.subcommand_matches("add") {
        dep_add(matches)
    } else if let Some(matches) = matches.subcommand_matches("remove") {
        dep_remove(matches)
    } else if let Some(_) = matches.subcommand_matches("lock") {
        dep_lock()
    } else if let Some(_) = matches.subcommand_matches("vendor") {
        dep_vendor()
    } else if let Some(matches) = matches.subcommand_matches("nix") {
        dep_nix(matches)
    } else {
        eprintln!("No dep subcommand specified. Use --help for usage.");
        process::exit(1);
    }
}

fn dep_init(matches: &clap::ArgMatches) -> Result<(), Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let manifest_path = cwd.join(dep::MANIFEST_FILE);

    if manifest_path.exists() {
        return Err(format!("{} already exists in {}", dep::MANIFEST_FILE, cwd.display()).into());
    }

    let vendor = matches.value_of("vendor").unwrap_or("vendor");
    let nix = matches.is_present("nix");

    let mut content = String::from("[package]\n");
    if vendor != "vendor" {
        content.push_str(&format!("vendor = \"{}\"\n", vendor));
    }
    if nix {
        content.push_str("nix = true\n");
    }
    content.push_str("\n[deps]\n");

    std::fs::write(&manifest_path, &content)?;
    println!("Created {}", manifest_path.display());
    Ok(())
}

fn dep_add(matches: &clap::ArgMatches) -> Result<(), Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let manifest_path = cwd.join(dep::MANIFEST_FILE);

    if !manifest_path.exists() {
        return Err(format!(
            "{} not found. Run `ucg dep init` first.",
            dep::MANIFEST_FILE
        )
        .into());
    }

    let url = matches.value_of("URL").unwrap();
    let repo_type = matches.value_of("type").unwrap_or("git");
    let normalized = dep::url::normalize_url(url);

    // Read existing manifest
    let content = std::fs::read_to_string(&manifest_path)?;
    let mut manifest = dep::manifest::Manifest::from_toml(&content)?;

    // Determine version constraint
    let version = if let Some(v) = matches.value_of("version") {
        v.to_string()
    } else {
        // Query tags and default to ">= <latest>"
        let tag_source = dep::registry::RemoteTagSource;
        let versions = tag_source.list_semver_tags(url, repo_type)?;
        if versions.is_empty() {
            return Err(format!("dependency {} has no semver tags (v*.*.*)", normalized).into());
        }
        let mut sorted = versions;
        sorted.sort();
        let latest = sorted.last().unwrap();
        format!(">= {}", latest)
    };

    // Check if already exists (update case)
    let deps = manifest.deps();
    let existing_key = deps
        .keys()
        .find(|k| dep::url::normalize_url(k) == normalized);
    if let Some(existing) = existing_key {
        let old_version = &deps[existing].version;
        println!(
            "Updating constraint for {} from '{}' to '{}'",
            normalized, old_version, version
        );
    }

    // Update manifest deps
    let mut new_deps = manifest.deps.take().unwrap_or_default();
    // Remove any existing entry with same normalized URL
    let keys_to_remove: Vec<String> = new_deps
        .keys()
        .filter(|k| dep::url::normalize_url(k) == normalized)
        .cloned()
        .collect();
    for key in keys_to_remove {
        new_deps.remove(&key);
    }
    new_deps.insert(
        url.to_string(),
        dep::manifest::DepEntry {
            version: version.clone(),
            repo_type: repo_type.to_string(),
        },
    );
    manifest.deps = Some(new_deps);

    // Validate
    manifest.validate()?;

    // Resolve
    let tag_source = dep::registry::RemoteTagSource;
    let manifest_source = VendorManifestSource {
        vendor_dir: cwd.join(manifest.vendor_dir()),
    };
    let resolved = dep::resolve::resolve_mvs(&manifest, &tag_source, &manifest_source)?;

    // Create temp dir for fetching
    let temp_dir = std::env::temp_dir().join("ucg_dep_vendor");
    if temp_dir.exists() {
        std::fs::remove_dir_all(&temp_dir)?;
    }
    std::fs::create_dir_all(&temp_dir)?;

    // Vendor
    let vendor_dir = cwd.join(manifest.vendor_dir());
    if let Some(warning) =
        dep::vendor::check_vendor_dir_warning(&vendor_dir, cwd.join(dep::LOCK_FILE).exists())
    {
        eprintln!("Warning: {}", warning);
    }

    let locked_packages = dep::vendor::vendor_resolved(&resolved, &vendor_dir, &temp_dir)?;

    // Write lockfile
    let mut lockfile = dep::lockfile::Lockfile {
        package: locked_packages,
    };
    lockfile.sort();
    let lock_content = lockfile.to_toml()?;
    std::fs::write(cwd.join(dep::LOCK_FILE), &lock_content)?;

    // Write manifest
    let manifest_content = manifest.to_toml()?;
    std::fs::write(&manifest_path, &manifest_content)?;

    // Nix generation if enabled
    if manifest.nix_enabled() {
        emit_nix_warning_or_generate(&cwd, &lockfile, &vendor_dir)?;
    }

    // Cleanup temp
    let _ = std::fs::remove_dir_all(&temp_dir);

    println!("Added {} at {}", normalized, version);
    for dep in &resolved {
        println!("  {} v{}", dep.normalized_url, dep.version);
    }

    Ok(())
}

fn dep_remove(matches: &clap::ArgMatches) -> Result<(), Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let manifest_path = cwd.join(dep::MANIFEST_FILE);

    if !manifest_path.exists() {
        return Err(format!(
            "{} not found. Run `ucg dep init` first.",
            dep::MANIFEST_FILE
        )
        .into());
    }

    let url = matches.value_of("URL").unwrap();
    let normalized = dep::url::normalize_url(url);

    let content = std::fs::read_to_string(&manifest_path)?;
    let mut manifest = dep::manifest::Manifest::from_toml(&content)?;

    // Find and remove the dep
    let mut new_deps = manifest.deps.take().unwrap_or_default();
    let key_to_remove: Option<String> = new_deps
        .keys()
        .find(|k| dep::url::normalize_url(k) == normalized)
        .cloned();

    if key_to_remove.is_none() {
        return Err(format!(
            "dependency {} not found in {}",
            normalized,
            dep::MANIFEST_FILE
        )
        .into());
    }
    new_deps.remove(&key_to_remove.unwrap());
    manifest.deps = Some(new_deps);

    // Re-resolve
    let tag_source = dep::registry::RemoteTagSource;
    let manifest_source = VendorManifestSource {
        vendor_dir: cwd.join(manifest.vendor_dir()),
    };
    let resolved = dep::resolve::resolve_mvs(&manifest, &tag_source, &manifest_source)?;

    // Check if removed dep is still transitive
    if resolved.iter().any(|d| d.normalized_url == normalized) {
        println!(
            "Note: removed {} as direct dependency (still present as transitive dependency)",
            normalized
        );
    }

    // Vendor
    let temp_dir = std::env::temp_dir().join("ucg_dep_vendor");
    if temp_dir.exists() {
        std::fs::remove_dir_all(&temp_dir)?;
    }
    std::fs::create_dir_all(&temp_dir)?;

    let vendor_dir = cwd.join(manifest.vendor_dir());
    let locked_packages = dep::vendor::vendor_resolved(&resolved, &vendor_dir, &temp_dir)?;

    // Write lockfile
    let mut lockfile = dep::lockfile::Lockfile {
        package: locked_packages,
    };
    lockfile.sort();
    std::fs::write(cwd.join(dep::LOCK_FILE), lockfile.to_toml()?)?;

    // Write manifest
    std::fs::write(&manifest_path, manifest.to_toml()?)?;

    // Nix
    if manifest.nix_enabled() {
        emit_nix_warning_or_generate(&cwd, &lockfile, &vendor_dir)?;
    }

    let _ = std::fs::remove_dir_all(&temp_dir);
    println!("Removed {} as direct dependency", normalized);
    Ok(())
}

fn dep_lock() -> Result<(), Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let manifest_path = cwd.join(dep::MANIFEST_FILE);

    if !manifest_path.exists() {
        return Err(format!(
            "{} not found. Run `ucg dep init` first.",
            dep::MANIFEST_FILE
        )
        .into());
    }

    let content = std::fs::read_to_string(&manifest_path)?;
    let manifest = dep::manifest::Manifest::from_toml(&content)?;
    manifest.validate()?;

    let tag_source = dep::registry::RemoteTagSource;
    let manifest_source = VendorManifestSource {
        vendor_dir: cwd.join(manifest.vendor_dir()),
    };
    let resolved = dep::resolve::resolve_mvs(&manifest, &tag_source, &manifest_source)?;

    // For lock-only, we need to fetch to get commit hashes and sha256
    let temp_dir = std::env::temp_dir().join("ucg_dep_lock");
    if temp_dir.exists() {
        std::fs::remove_dir_all(&temp_dir)?;
    }
    std::fs::create_dir_all(&temp_dir)?;

    let mut locked_packages = Vec::new();
    for dep_entry in &resolved {
        let tag = format!("v{}", dep_entry.version);
        let dep_temp = temp_dir.join(&dep_entry.normalized_url);
        if dep_temp.exists() {
            std::fs::remove_dir_all(&dep_temp)?;
        }
        std::fs::create_dir_all(dep_temp.parent().unwrap_or(&temp_dir))?;

        let commit =
            dep::vendor::fetch_repo(&dep_entry.fetch_url, &dep_entry.repo_type, &tag, &dep_temp)?;
        dep::vendor::strip_dep_vendor_dir(&dep_temp)?;
        let sha256 = dep::vendor::hash_directory(&dep_temp)?;

        locked_packages.push(dep::lockfile::LockedPackage {
            git: if dep_entry.repo_type == "git" {
                Some(dep_entry.fetch_url.clone())
            } else {
                None
            },
            hg: if dep_entry.repo_type == "hg" {
                Some(dep_entry.fetch_url.clone())
            } else {
                None
            },
            version: dep_entry.version.to_string(),
            commit,
            sha256,
        });
    }

    let mut lockfile = dep::lockfile::Lockfile {
        package: locked_packages,
    };
    lockfile.sort();
    std::fs::write(cwd.join(dep::LOCK_FILE), lockfile.to_toml()?)?;

    // Nix
    if manifest.nix_enabled() {
        let vendor_dir = cwd.join(manifest.vendor_dir());
        emit_nix_warning_or_generate(&cwd, &lockfile, &vendor_dir)?;
    }

    let _ = std::fs::remove_dir_all(&temp_dir);
    println!("Wrote {}", dep::LOCK_FILE);
    for dep_entry in &resolved {
        println!("  {} v{}", dep_entry.normalized_url, dep_entry.version);
    }
    Ok(())
}

fn dep_vendor() -> Result<(), Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let manifest_path = cwd.join(dep::MANIFEST_FILE);
    let lock_path = cwd.join(dep::LOCK_FILE);

    if !manifest_path.exists() {
        return Err(format!(
            "{} not found. Run `ucg dep init` first.",
            dep::MANIFEST_FILE
        )
        .into());
    }
    if !lock_path.exists() {
        return Err(format!("{} not found. Run `ucg dep lock` first.", dep::LOCK_FILE).into());
    }

    let manifest_content = std::fs::read_to_string(&manifest_path)?;
    let manifest = dep::manifest::Manifest::from_toml(&manifest_content)?;

    let lock_content = std::fs::read_to_string(&lock_path)?;
    let lockfile = dep::lockfile::Lockfile::from_toml(&lock_content)?;

    // Check staleness
    lockfile.is_stale(&manifest)?;

    let vendor_dir = cwd.join(manifest.vendor_dir());
    if let Some(warning) = dep::vendor::check_vendor_dir_warning(&vendor_dir, true) {
        eprintln!("Warning: {}", warning);
    }

    let temp_dir = std::env::temp_dir().join("ucg_dep_vendor");
    if temp_dir.exists() {
        std::fs::remove_dir_all(&temp_dir)?;
    }
    std::fs::create_dir_all(&temp_dir)?;

    dep::vendor::vendor_from_lockfile(&lockfile, &manifest, &vendor_dir, &temp_dir)?;

    let _ = std::fs::remove_dir_all(&temp_dir);
    println!(
        "Vendored {} dependencies to {}",
        lockfile.package.len(),
        vendor_dir.display()
    );
    Ok(())
}

fn dep_nix(matches: &clap::ArgMatches) -> Result<(), Box<dyn Error>> {
    // Check nix availability
    if !is_nix_available() {
        return Err("nix is not available on PATH. Install nix to generate ucg-deps.nix.".into());
    }

    let cwd = std::env::current_dir()?;
    let lock_path = cwd.join(dep::LOCK_FILE);
    if !lock_path.exists() {
        return Err(format!("{} not found. Run `ucg dep lock` first.", dep::LOCK_FILE).into());
    }

    let lock_content = std::fs::read_to_string(&lock_path)?;
    let lockfile = dep::lockfile::Lockfile::from_toml(&lock_content)?;

    let manifest_path = cwd.join(dep::MANIFEST_FILE);
    let vendor_dir = if manifest_path.exists() {
        let mc = std::fs::read_to_string(&manifest_path)?;
        let m = dep::manifest::Manifest::from_toml(&mc)?;
        cwd.join(m.vendor_dir())
    } else {
        cwd.join("vendor")
    };

    let nix_content = dep::nix::generate_nix_expression(&lockfile, &vendor_dir)?;

    if matches.is_present("stdout") {
        print!("{}", nix_content);
    } else {
        std::fs::write(cwd.join(dep::NIX_FILE), &nix_content)?;
        println!("Wrote {}", dep::NIX_FILE);
    }
    Ok(())
}

fn emit_nix_warning_or_generate(
    cwd: &Path,
    lockfile: &dep::lockfile::Lockfile,
    vendor_dir: &Path,
) -> Result<(), Box<dyn Error>> {
    if is_nix_available() {
        let nix_content = dep::nix::generate_nix_expression(lockfile, vendor_dir)?;
        std::fs::write(cwd.join(dep::NIX_FILE), &nix_content)?;
        println!("Wrote {}", dep::NIX_FILE);
    } else {
        eprintln!(
            "Warning: nix = true but nix is not available. {} may be out of date. \
             A contributor with nix should run `ucg dep nix` to update it.",
            dep::NIX_FILE
        );
    }
    Ok(())
}

fn is_nix_available() -> bool {
    std::process::Command::new("nix")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// ManifestSource that reads from the vendor directory on disk.
struct VendorManifestSource {
    vendor_dir: PathBuf,
}

impl dep::resolve::ManifestSource for VendorManifestSource {
    fn get_manifest(
        &self,
        normalized_url: &str,
        _version: &semver::Version,
    ) -> Result<Option<dep::manifest::Manifest>, dep::error::DepError> {
        let manifest_path = self
            .vendor_dir
            .join(normalized_url)
            .join(dep::MANIFEST_FILE);
        if !manifest_path.exists() {
            return Ok(None);
        }
        let content = std::fs::read_to_string(&manifest_path)?;
        let manifest = dep::manifest::Manifest::from_toml(&content)?;
        Ok(Some(manifest))
    }
}

fn env_help() {
    println!(
        include_str!("help/env.txt"),
        std::env::var("UCG_IMPORT_PATH").unwrap_or_default()
    );
}

fn do_repl(
    import_paths: &Vec<PathBuf>,
    strict: bool,
    env: &RefCell<Environment<StdoutWrapper, StderrWrapper>>,
) -> std::result::Result<(), Box<dyn Error>> {
    let config = rustyline::Config::builder();
    let mut editor = rustyline::Editor::<()>::with_config(
        config
            .history_ignore_space(true)
            .history_ignore_dups(false)
            .build(),
    )
    .expect("Unable to start line editor");
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
            if let Some(base_dir) = config_home.parent() {
                if !base_dir.exists() {
                    if let Err(e) = std::fs::create_dir_all(base_dir) {
                        eprintln!("{}", e);
                    }
                }
            }
            if let Err(e) = std::fs::File::create(&config_home) {
                eprintln!("{}", e);
            }
        }
    }
    let mut builder = build::FileBuilder::new(std::env::current_dir()?, import_paths, env);
    builder.set_strict(strict);

    builder.repl(editor, config_home)?;
    Ok(())
}

fn repl(
    import_paths: &Vec<PathBuf>,
    strict: bool,
    env: &RefCell<Environment<StdoutWrapper, StderrWrapper>>,
) {
    if let Err(e) = do_repl(import_paths, strict, env) {
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
    let mut env_vars = BTreeMap::new();
    for (var, val) in std::env::vars() {
        env_vars.insert(var.into(), val.into());
    }
    let mut environment =
        Environment::new_with_vars(StdoutWrapper::new(), StderrWrapper::new(), env_vars);
    // Detect package root and configure vendor dir
    let cwd = std::env::current_dir().unwrap_or_default();
    if let Some(pkg_root) = dep::find_package_root(&cwd) {
        let manifest_path = pkg_root.join(dep::MANIFEST_FILE);
        let vendor_dir = if manifest_path.exists() {
            if let Ok(content) = std::fs::read_to_string(&manifest_path) {
                if let Ok(manifest) = dep::manifest::Manifest::from_toml(&content) {
                    manifest.vendor_dir().to_string()
                } else {
                    "vendor".to_string()
                }
            } else {
                "vendor".to_string()
            }
        } else {
            "vendor".to_string()
        };
        environment.set_package_root(Some(pkg_root), &vendor_dir);
    }
    let env = RefCell::new(environment);
    if let Some(mut p) = dirs::home_dir() {
        p.push(".ucg");
        // Attempt to create directory if it doesn't exist.
        if !p.exists() {
            if let Err(e) = std::fs::create_dir(&p) {
                eprintln!("Unable to create .ucg directory {}", e);
            } else {
                import_paths.push(p);
            }
        } else {
            import_paths.push(p);
        }
    }
    if let Ok(path_list_str) = std::env::var("UCG_IMPORT_PATH") {
        for p in std::env::split_paths(&path_list_str) {
            import_paths.push(p);
        }
    }
    let strict = !app_matches.is_present("nostrict");
    if let Some(matches) = app_matches.subcommand_matches("build") {
        build_command(matches, &import_paths, strict, &env);
    } else if let Some(matches) = app_matches.subcommand_matches("test") {
        test_command(matches, &import_paths, strict, &env);
    } else if let Some(matches) = app_matches.subcommand_matches("converters") {
        converters_command(matches, &registry)
    } else if app_matches.subcommand_matches("importers").is_some() {
        let registry = ImporterRegistry::make_registry();
        importers_command(&registry)
    } else if app_matches.subcommand_matches("env").is_some() {
        env_help()
    } else if app_matches.subcommand_matches("repl").is_some() {
        repl(&import_paths, strict, &env)
    } else if let Some(matches) = app_matches.subcommand_matches("fmt") {
        if let Err(e) = fmt_command(matches) {
            eprintln!("{}", e);
            process::exit(1);
        }
    } else if app_matches.subcommand_matches("lsp").is_some() {
        if let Err(e) = ucglib::lsp::run_server() {
            eprintln!("lsp server error: {}", e);
            process::exit(1);
        }
    } else if let Some(matches) = app_matches.subcommand_matches("dep") {
        dep_command(matches);
    } else {
        app.print_help().unwrap();
        println!();
    }
}
