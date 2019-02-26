+++
title = "Schema Helpers"
weight = 4
sort_by = "weight"
in_search_index = true
+++

UCG's std lib has a selection of functions and modules to help assert a base type and shape for a value. They are located in the `std/schema.ucg` import.

## Schema shapes

The primary tool in this import is the shaped module. This module allows you to match a value against a stereotype value. If the provided value is the same base type and has the same shape of that base type as the provided stereotype then the
result field of the computed tuple will be true.

```
let fits = schema.shaped{
    val={foo="bar", inner=[1, 2]},
    shape={foo="", inner=[]}
};

fits == true;
```

By default the matching allows the shape to specify a partial or minimum shape. But you can specify exact matching if desired by setting the partial paramater to false..

```
let exact_fit = schema.shaped{
    partial=false,
    val={foo="bar", count=1},
    shape={foo=""},
 };

 exact_fit == false;
```

The shape algorithm does not enforce a length or set of contained types for lists at this time.

Besides the shaped module there are also some useful utility modules and functions to assist in some addtional type checking logic.
## any

The `any` module tests a value against a list of possible shapes. If the value
fits any of the candidate shapes then it stores true in the result field. If it does not then it returns false in that result field.

```
let fits_one_of = any{val={foo="bar"}, types=[1, {foo=""}]};
fits_one_of == true;
```

## base_type_of function.

The `base_type_of` function computes the base type of a value.

```
let foo = 1;
base_type_of(foo) == "int";
```

## must

The `must` function turns any schema check module failure into a compile failure. It composes well with the modules in the `std/schema.ucg` import and automatically checks the result field they produce. If that field is true then it returns that true value. If the field is false they produce a compile failure.

```
// results in a compile failure "Must be a string"
must(shaped{val="foo", shape=1}, "Must be a string");
```