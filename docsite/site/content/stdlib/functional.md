+++
title = "Functional operations"
weight = 5
sort_by = "weight"
in_search_index = true
+++

UCG comes with some builtin functional operations.

## Useful functions

* identity - the Identity function. Returns it's argument unchanged.

## Maybe module

Maybe is a monadic style wrapper for values that might be NULL. It provides a several operations for the wrapped value.

* do(op) - runs op which is a function of one argument against the wrapped value if it is not null. Returns the result or NULL wrapped in another maybe.
* or(op) - runs op which is a function of no arguments if the wrapped value is null. Returns the result wrapped in another maybe.
* is_null() - returns true if the wrapped value is null, false otherwise.
* unwrap() - returns the wrapped value
* expect(msg) - returns the wrapped value if it is not null. Throws a compile error with the user provided message otherwise.

```
let maybe = import "std/functional.ucg".maybe;

let result = maybe{val=NULL}.or(func () => "foo").do(func(v) => v + "bar");
result == "foobar";
```