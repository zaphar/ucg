# Documentation TODO

* Schema Checking With Modules

# Major Planned Features

## Standard Library

* String processing

## Query Language (Experimental)

You should be able to ask the compiler to tell you any value or set of values in the
compiled configuration.

Eval is probably the correct location for this.

# Minor Fixes and Polish

* Compiler caching (interface has been defined)
* Streaming Parsing?
* Casting between types?
* Better error messages.
* Flags should allow different seperators for prefixed flags.
* HCL export

# Release Checklist

* Cargo test
* Cargo fmt
* Update Cargo.toml version.
* Tag git commit with version tag.
* Cargo publish