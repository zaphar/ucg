# Major Planned Features

## Boolean operations and type

* contains (for lists or strings)

## Query Language (Experimental)

You should be able to ask the compiler to tell you any value or set of values in the
compiled configuration.

## Translation Language (Experimental)

For some configuration file formats we need a way to specify a particular
organiztion for a given configuration structure (i.e. xml attribute or tag?).

Some options here could be:

* Simple data export (json) 
* A Functional Transform similar to xslt or css transforms
* A Templating language
* DSL's
* Annotations

## Built In testing as a part of the language

* A DSL for assertions.
* A DSL for identifying tests.
* A way to run tests only during validation stage.

# Minor Fixes and Polish

* Compiler caching (interface has been defined)
* Strings as tuple fields?
* Streaming Parsing?
* Casting between types?
* Better error messages.
* Flags should allow different seperators for prefixed flags.
* YAML export
* HCL export

# Release Checklist

* Cargo test
* Cargo fmt
* Update Cargo.toml version.
* Tag git commit with version tag.
* Cargo publish