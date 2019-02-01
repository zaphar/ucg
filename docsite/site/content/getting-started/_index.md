+++
title = "Getting Started"
slug = "getting-started"
weight = 1
sort_by = "weight"
in_search_index = true
+++
Installing
----------

### Cargo

You can install UCG using Rust's Cargo package manager.

```sh
cargo install ucg
```

### Compiling

You can also install from source yourself. First ensure that you have the latest
version of Rust installed. You can find install instructions for Rust
[here](https://www.rust-lang.org/en-US/install.html). Then you can get the source
from github and use cargo to build.

```sh
# Get the source code from github
git clone https://github.com/zaphar/ucg
cd ucg
# optionally checkout the current version
git checkout v0.5.1
# use cargo to build and install
cargo install --path .
```

A simple configuration
----------------------

To create a configuration and build it in UCG you must first create a UCG file. Copy the below contents into a file called `sample.ucg`. All UCG files must end in the UCG
extension.

```
let hostname = "www.example.com";
let mysql_host = "localhost";
let mysql_port = 3306;

let config = {
    // This uses a format string to put the hostname into
    // the baseUrl.
    baseUrl = "http://@" % (hostname),
    db = {
        host = mysql_host,
        port = myssql_port,
    },
}

// Generate a yaml file from that config.
out yaml config;
```

The above binds 3 values to names and then creates a config tuple using those values.
We'll look in more detail at UCG's syntax later.

To generate a yaml file from the above run the UCG build command.

```sh
ucg build sample.ucg
cat sample.yaml
```

UCG will generate the yaml file with the same name as the file containing the out statement.

The UCG command line
-----------

UCG has builtin help on the command line. 

```
$> ucg help
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
    help          Prints this message or the help of the given subcommand(s)
    importers     list the available importers for includes
    test          Check a list of ucg files for errors and run test assertions.
```

You can get more information about the options for each command by running `ucg help $command`

Next: <a href="/reference">Reference</a>