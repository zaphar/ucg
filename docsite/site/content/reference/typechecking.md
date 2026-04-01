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

Range Constraints
-----------------

Range constraints restrict a value to a numeric range using the `in` keyword
and `..` syntax. They work for both integers and floats. The type is checked
at compile time and the value is checked at runtime.

```
let port :: in 1..65535 = 8080;         // must be between 1 and 65535
let ratio :: in 0.0..1.0 = 0.5;        // float range
```

Ranges can be open-ended — at least one bound must be specified:

```
let positive :: in 0.. = 42;            // >= 0, no upper bound
let small :: in ..100 = 50;             // <= 100, no lower bound
```

If the value falls outside the range, you get a runtime error:

```
let port :: in 1..1024 = 9999;          // Error: does not satisfy constraint
```

Alternation Constraints
-----------------------

Alternation constraints restrict a value to one of several specific values
using the `|` syntax. They work with any value type.

```
let status :: "active" | "inactive" | "pending" = "active";
let code :: 200 | 404 | 500 = 200;
```

If the value doesn't match any alternative, you get a runtime error:

```
let status :: "active" | "inactive" = "unknown";  // Error
```

### Combining Ranges and Alternations

Ranges and exact values can be combined with `|`:

```
let port :: in 1..1024 | 8080 | 8443 = 80;   // system ports or specific app ports
let port2 :: in 1..10 | in 20..30 = 25;       // multiple ranges
```

Named Constraints
-----------------

The `constraint` statement defines a reusable named constraint. Named
constraints are contracts — they describe what values are acceptable but are
not configuration values themselves.

```
constraint port_range = in 1..65535;
constraint log_level = "debug" | "info" | "warn" | "error";
constraint valid_port = in 1..1024 | 8080 | 8443;
```

Use named constraints with `::` in let bindings:

```
let http_port :: port_range = 80;
let level :: log_level = "info";
let app_port :: valid_port = 8080;
```

Named constraints are especially useful when multiple bindings share the same
contract, or when defining module parameter constraints.

### Recursive Constraints

Named constraints can reference themselves, enabling recursive type
definitions for tree-like data structures. A self-reference must appear
inside a list or as one arm of an alternation with a non-recursive base
case — otherwise the type would be impossible to construct.

```
// An XML-like tree node: either a text string or a tag with children
constraint xml_node = "" | {name="", attrs={}, children=[xml_node]};

// Base case: a text node
let leaf :: xml_node = "hello";

// One level deep
let tag :: xml_node = {
    name = "div",
    attrs = {},
    children = ["text", {name = "span", attrs = {}, children = []}],
};

// Arbitrarily deep nesting is valid
let tree :: xml_node = {
    name = "html",
    attrs = {},
    children = [
        {name = "body", attrs = {}, children = [
            {name = "p", attrs = {}, children = ["paragraph"]},
        ]},
    ],
};
```

The type checker validates the shape at all nesting depths statically.
Wrong types at any depth produce a compile error:

```
// Error: 42 is not a valid xml_node (not a string or matching tuple)
let bad :: xml_node = {
    name = "div",
    attrs = {},
    children = [42],
};
```

Self-references must have a base case. The following is rejected because
there is no way to construct a value that satisfies it:

```
// Error: unconstructible — no base case
constraint bad = {child = bad};
```

Valid positions for self-references:
* Inside a list: `[my_constraint]` — the list can be empty, providing the base case
* As one arm of an alternation: `"" | {next = my_constraint}` — the other arm is the base case

### How Constraints Work

Under the hood, shape constraints use the same type narrowing mechanism as the
rest of UCG's type system. The constraint expression's type is derived and then
narrowed against the value's type. If the types are incompatible (e.g.,
constraining to integer but providing a string), the narrowing produces a type
error.

Range and alternation constraints add runtime value checking on top of
compile-time type checking. The type checker verifies that the constraint and
value are type-compatible (e.g., both are integers), and the VM verifies at
runtime that the actual value falls within the specified range or matches one
of the alternatives.

This means constraints interact naturally with UCG's existing type inference:

* NULL values are compatible with any constraint (they represent "any type")
* Tuple constraints check field names and types structurally
* Constraints propagate through copy expressions and module instantiation
* Range constraints use `..` syntax (distinct from `:` list ranges)
* The `|` operator joins constraint arms (distinct from `||` boolean OR)
