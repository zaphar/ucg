# Major Planned Features

## Compile Errors as expression

## String handling

Mostly handled by ranges and indexing for strings?

Type comparisons?

```
let foo = "foo";
foo is str; // evaluates to true
foo is int; // evaluates to false
```

## Query Language (Experimental)

You should be able to ask the compiler to tell you any value or set of values in the
compiled configuration.

Inspect is probably the correct location for this.

## Shape equality as a form of type assertion?

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