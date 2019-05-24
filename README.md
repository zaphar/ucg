# Universal Configuration Grammar.

[![Build Status](https://travis-ci.org/zaphar/ucg.svg?branch=master)](https://travis-ci.org/zaphar/ucg)

This is an experiment in configuration management. The approach is **not**
to create a "parsable" config file format.  We have plenty of
those. Instead we try to specify a grammar for describing
configuration values that can then target various configuration
formats to output to.

In theory this could support anything from command line flags to json
to yaml or toml or even xml.

The goal is to allow a global shared configuration repository that can
be version controlled, enforce _some_ typesafety, and output
configuration for any application regardless of that applications
preferred format.

## The Usual Warnings

This is still very much an experiment and the language and api can be expected
to change and mutate. It also probably has bugs and isn't yet the most user
friendly language and compiler to use. You have been warned.

## Usage

You can get ucg with cargo `cargo install ucg`.

Running ucg help will show the following output.

```sh
Universal Configuration Grammar compiler.

USAGE:
    ucg [FLAGS] [SUBCOMMAND]

FLAGS:
    -h, --help         Prints help information
        --no-strict    Turn off strict checking.
    -V, --version      Prints version information

SUBCOMMANDS:
    build         Build a list of ucg files.
    converters    list the available converters
    env           Describe the environment variables ucg uses.
    eval          Evaluate an expression with an optional ucg file as context.
    fmt           Format ucg files automatically.
    help          Prints this message or the help of the given subcommand(s)
    importers     list the available importers for includes
    test          Check a list of ucg files for errors and run test assertions.
```

## Compiling

```sh
Build a list of ucg files.

USAGE:
    ucg build [FLAGS] -r [INPUT]...

FLAGS:
    -h, --help       Prints help information
    -r               Whether we should recurse in directories or not.
    -V, --version    Prints version information

ARGS:
    <INPUT>...    Input ucg files or directories to build. If not provided then build the contents of the current directory.
```

## Testing
```sh
 Check a list of ucg files for errors and run test assertions.

USAGE:
    ucg test [FLAGS] -r [INPUT]...

FLAGS:
    -h, --help       Prints help information
    -r               Whether we should recurse or not.
    -V, --version    Prints version information

ARGS:
    <INPUT>...    Input ucg files or directories to run test assertions for. If not provided it will scan the current directory for files with _test.ucg
```

## Language Reference

[https://ucg.marzhillstudios.com/reference](https://ucg.marzhillstudios.com/reference)