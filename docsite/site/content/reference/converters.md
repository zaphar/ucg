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

XML
---

XML can be output using a custom xml DSL.

XML documents start with a tuple defining the document declaration and a root element.

```
{
    version = "1.1" // Optional, Defaults to 1.1
    encoding = "utf-8" // Optional, Defaults to UTF-8
    standalone = true // Optional Defaults to false
    root = { // Required defines the root element of the document.
        name = "top",
    }
};
```

Nodes in the document can be defined as either xml elements or text nodes. An
xml element is defined as a tuple with a required `name` field and the three
optional fields `attrs`, `ns`, and `children`. 

The `name` field is required and may optionally contain a namespace prefix like so:
`prefix:element-name`.

`attrs` if set is required to be a
tuple or NULL. If NULL then no attributes are emitted If a tuple then each
field is turned into an attribute on the element. The tuple should have only
string values or null for each field. If NULL then the attribute is not set.

`ns` if set is required to be either a string in which case the default xml namespace
is set or a tuple with `prefix` and `uri` fields. which will define the prefix and uri for a namespace on that element.

`children` is required to be a list of elements and text nodes or NULL. If NULL then no children are output.

```
{
    name = "ns:element-name",
    ns = {
        prefix = "myns",
        uri = "http://example.org",
    },
    attrs = {
        id = "foo",
    },
    children = [
        // child elements go here.
    ],
};
```

Text nodes can be output via two ways. Either just a string or as a tuple with a
single field named text.

```
{
    text = "This is a valid text node",
};

"This is also a valid text node";
```

### Example document
```
let doc = {
    root = {
        ns = {
            prefix = "myns",
            uri = "http://example.com",
        },
        name = "top",
        attrs = {id = "foo"},
        children = [
            {
                name = "child1",
                ns = "http://example.org",
                attrs = { attr1 = "value1", attr2 = "value2"},
                children = [
                    "inner text node",
                    {
                        name = "myns:grandchild",
                        children = [
                            {
                                text = "Another text node",
                            },
                        ],
                    },
                ],
            },
        ],
    },
};

out xml doc;
```
This will result in the following xml document.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<top xmlns:myns="http://example.com" id="foo">
  <child1 xmlns="http://example.org/" attr1="value1" attr2="value2">inner text node<myns:grandchild>Another text node</myns:grandchild>
  </child1>
</top>
```

### Caveats

We don't support character CDATA sections in our xml document DSL at this time.

Next: <a href="/how-to">HowTo Guides</a>