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

use ucglib::build;

fn do_flags<'a>() -> clap::ArgMatches<'a> {
    clap_app!(
        ucg =>
            (version: crate_version!())
            (author: crate_authors!())
            (about: "Universal Configuration Grammar compiler.")
            (@subcommand build =>
             (about: "Compile a specific ucg file.")
             (@arg INPUT: +required "Input ucg file to build.")
            )
            (@subcommand validate =>
             (about: "Check a specific ucg file for errors.")
             (@arg INPUT: +required "Input ucg file to validate.")
            )
    )
        .get_matches()
}

fn main() {
    // TODO(jwall): Read and build an actual file.
    let app = do_flags();
    if let Some(matches) = app.subcommand_matches("build") {
        let file = matches.value_of("INPUT").unwrap();
        let mut builder = build::Builder::new();
        builder.build_file(file).unwrap();
        println!("Build successful");
    } else if let Some(matches) = app.subcommand_matches("validate") {
        let file = matches.value_of("INPUT").unwrap();
        let mut builder = build::Builder::new();
        builder.build_file(file).unwrap();
        println!("File Validates");
    }
}
