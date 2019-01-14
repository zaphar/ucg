+++
title = "UCG Tuple Modules"
weight = 2
sort_by = "weight"
in_search_index = true
+++

The UCG tuples modules can be imported like so `let t = import "std/tuples.ucg";` 
and contains a number of useful operations on tuples.

## fields

The `fields` module retrieves all the field names in a tuple. It has one
parameter.

* `tpl` which is required and is the tuple to process.

```
let tpl = import "std/tuples.ucg";
tpl.fields{tpl={foo=1, bar=2}}.result == ["foo", "bar"];
```

## values

The `values` module retrieves all the values in a tuple. It has one parameter.

* `tpl` which is required and is the tuple to process.

```
let tpl = import "std/tuples.ucg";
tpl.values{tpl={foo=1, bar=2}}.result == [1, 2];
```

## iter

The `iter` module retrieves a list of all the field and value pairs in a tuple.
It has one parameter.

* `tpl` which is required and is the tuple to process.

```
let tpl = import "std/tuples.ucg";
tpl.enumerate{tpl={foo=1, bar=2}}.result == [["foo", 1], ["bar", 2]];
```