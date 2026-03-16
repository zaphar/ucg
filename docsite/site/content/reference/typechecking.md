+++
title = "Type Checking & Shape Constraints"
weight = 5
sort_by = "weight"
in_search_index = true
+++
Type Checking
-------------

UCG has a forward type inference system that statically checks your
configuration for type errors at compile time. The type checker runs before
code generation and catches mismatches like adding a string to an integer,
accessing a field that doesn't exist on a tuple, or passing the wrong number
of arguments to a function.

Type inference is automatic — you don't need to annotate types for it to work.
The checker infers types from values, narrows them through operations, and
reports errors when it finds incompatible types.

```
let x = 1;
let y = "hello";
let z = x + y; // Compile error: incompatible types
```

Type errors are reported with file, line, and column information to help you
find the problem quickly.

### Strict and Non-Strict Mode

By default, UCG runs in strict mode where type errors halt compilation. You
can disable strict checking with the `--no-strict` flag, which suppresses
errors involving NULL values. This is useful when you have optional
configuration values that might not be set.

Shape Constraints
-----------------

Shape constraints let you explicitly declare what type a value must conform
to. They use the `::` syntax with a zero-value exemplar — a value whose type
represents the constraint. Constraints are checked at compile time by the type
checker.

Shape constraints are optional everywhere. Existing code without `::`
continues to work unchanged.

### Zero-Value Exemplars

The constraint expression is a value whose type serves as the constraint. Use
zero-values of the desired type:

* `0` — constrains to integer
* `0.0` — constrains to float
* `""` — constrains to string
* `true` or `false` — constrains to boolean
* `{field = 0, other = ""}` — constrains to a tuple with those field types

### Let Statement Constraints

You can constrain a let binding to ensure the bound value matches a specific
shape:

```
let port :: 0 = 8080;           // must be an integer
let host :: "" = "localhost";    // must be a string
let verbose :: true = false;     // must be a boolean
```

If the value doesn't match the constraint, you get a compile error:

```
let port :: 0 = "not a number"; // Compile error: incompatible types
```

### Named Shape Constraints

Since the constraint is an expression, you can use named bindings to define
reusable shapes:

```
let ServerConfig = {
    host = "",
    port = 0,
    verbose = false,
};

let my_server :: ServerConfig = {
    host = "example.com",
    port = 443,
    verbose = true,
};

// This would fail:
// let bad :: ServerConfig = {
//     host = "example.com",
//     port = "not a number",  // Error: port must be int
//     verbose = true,
// };
```

This is particularly useful for defining configuration schemas that multiple
bindings must conform to.

### Tuple Field Constraints

Individual tuple fields can be constrained:

```
let config = {
    host :: "" = "localhost",
    port :: 0 = 8080,
    tags :: [] = ["web", "api"],
};
```

### Function Argument Constraints

Function arguments can carry shape constraints to document and enforce the
expected types:

```
let add = func(a :: 0, b :: 0) => a + b;

add(1, 2);         // OK: both are integers
// add("x", "y");  // Would be flagged by the type checker
```

### Module Parameter Constraints

Module parameters support constraints just like tuple fields, since the
parameter list is a tuple:

```
let WebServer = module {
    host :: "" = "localhost",
    port :: 0 = 80,
} => {
    let url = "http://@:@" % (mod.host, mod.port);
};
```

### Module Output Constraints

Modules can constrain their output expression to enforce the shape of what the
module produces:

```
let Endpoint = module {
    host :: "" = "localhost",
    port :: 0 = 80,
} => (result :: {url = "", port = 0}) {
    let result = {
        url = "http://@" % (mod.host),
        port = mod.port,
    };
};
```

### How Constraints Work

Under the hood, shape constraints use the same type narrowing mechanism as the
rest of UCG's type system. The constraint expression's type is derived and then
narrowed against the value's type. If the types are incompatible (e.g.,
constraining to integer but providing a string), the narrowing produces a type
error.

This means constraints interact naturally with UCG's existing type inference:

* NULL values are compatible with any constraint (they represent "any type")
* Tuple constraints check field names and types structurally
* Constraints propagate through copy expressions and module instantiation
