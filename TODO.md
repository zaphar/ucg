# Major Planned Features

## Boolean operations and type

* contains (for lists or strings)

## Query Language (Experimental)

You should be able to ask the compiler to tell you any value or set of values in the
compiled configuration.

## Shape equality as a form of type assertion?

# Minor Fixes and Polish

* Compiler caching (interface has been defined)
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