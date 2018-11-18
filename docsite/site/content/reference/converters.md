+++
title = "UCG Converters"
weight = 5
sort_by = "weight"
in_search_index = true
+++
UCG has several formats it can convert a value into. Each one has various limits and
restrictions on the values that can be converted to that format.

JSON and YAML
----

All UCG values can be converted into JSON or YAML. Integers and Floats are turned into 
Their equivalent number types. Tuples are turned into dicts. Lists are turned into 
arrays. Strings are turned into strings.

The NULL or empty type is turned into null in JSON or YAML.

TOML
----

TOML is very similar to the JSON and YAML formats. TOML has no notion of null though 
so NULL types result in a compile error when converting to TOML.

Command Line Flags
----

UCG is able to generate command line flags from ucg values following some translation
rules for each type.

* Empty values are not emitted.
* Boolean values are translated to "true" and "false" repsectively.
* Integers and Floats are rendered as numbers.
* Tuples are rendered as `--field value` pairs for each field.
  * Nested tuples concatenate the field names to create the field.
  * Nested Lists generate a new `--field listitem` pair for each item in the list.
  * For fields that are just one character in length use a single `-`. Use double
    dashes `--` for fields that are longer than one character.

### Example

```
let flags = {
    port = 8080,
    listen = "0.0.0.0",
    verbose = NULL,
    dir = [
        "some/dir",
        "some/other/dir",
    ],
    log = {
        debug = true,
        format = "json",
    },
}
```

Generates the following flags in a file with the `.txt` extension.

```
-port 8080 --listen '0.0.0.0' --verbose --dir 'some/dir' --dir 'some/other/dir' --log.debug true --log.format 'json'
```

Environment Variables
--------

ucg is also able to generate environment variables from ucg values following a few
translation rules.

* Booleans, Integers, Floats, and Strings are output as is.
* Lists are not output.
* Tuples are output as a `FIELD=value` for each field with strings surrounded by single
  quotes.
  * Nested Tuples are not output.

```
let vars = {
    USER = "me",
    HOSTNAME = "localhost",
    TWO = 2,
    VERBOSE = true,
};
```

Generates the following in a file with a `.env` extension.

```sh
USER='me'
HOSTNAME='localhost'
TWO=2
VERBOSE=true
```

Exec Script
-----

ucg has an exec converter that will generate a launch script in bash for applications
that are configured via command line flags or environment variables. The exec converter
uses a tuple with some required and optional fields to generate the script with.

The `command` field is the path to the application the script will be launching. It is 
expected to be a string. There must only be one `command` field in the tuple.

The `args` field is a list of command line arguments. The elements of the list can be 
strings or tuples. There must only be one `args` field in the tuple.

The `env` field is a tuple representing the environment variables that will be set for 
the application. there must only be one `env` field in the tuple.

### Example

```
let common_flags = {
    log-level = "debug",
    maxMem = "2048M",
};

let script = {
    env = {
        API_KEY = "foo-key-and-stuff",
    },
    command = "my-app",
    args = [
        common_flags,
        "serve", "--port", "8080",
    ],
};

out exec script;
```

The script tuple above will generate the following bash script:

```sh
#!/usr/bin/env bash
# Turn on unofficial Bash-Strict-Mode
set -euo pipefail

API_KEY="foo-key-and-stuff"

exec my-app --log-level debug --maxMem 2048M serve --port 8080
```

The items in the args should be either strings or tuples. The tuples are turned into
flags using the builtin flag converter.

Next: <a href="/how-to">HowTo Guides</a>