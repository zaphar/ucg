+++
title = "List Operations"
weight = 1
sort_by = "weight"
in_search_index = true
+++

The UCG list modules can be imported like so `let l = import "std/lists.ucg";` It has a number of useful operations.

## reverse

The reverse module reverses a list. It has a single required parameter:

* `list` the list to reverse.

```
let l = import "std/lists.ucg";
l.reverse{list=[1, 2, 3]} == [3, 2, 1];
```

## str_join

The str_join module joins a list with the string representation of each element.
It has two parameters:

* `list` which is required. The list to reverse.
* `sep` which is optional and defines the separater to use when joining the elements. Defaults to a single space

```
let l = import "std/lists.ucg";
l.str_join{
    sep=" ",
    list=[1, 2, 3]
} == "1,2,3";
```

## len

The len function returns the length of a list. It has a single required parameter.

* `list` The list to reverse.

```
let l = import "std/lists.ucg";
l.len{list=[0, 1, 2, 3]} == 4;
```

## head and tail

The `tail` function returns the tail of a list minus it's head.

```
let l = import "std/lists.ucg";
let tail = l.tail([0,1,2,3]);
tail == [1,2,3];
```

The `head` function returns the head of the list as a list of one item.

```
let l = import "std/lists.ucg";
let hd = l.head([0,1,2,3]);
tail == [0];
```

## enumerate

The enumerate module enumerates the elements of a list. It has three parameters.

* `list` which is required and is the list to enumerate.
* `start` which is optional and defines the start number of the enumeration. (defaults to 0)
* `step` which is optional and defines the step amount for the enumeration. (defaults to 1)

```
let l = import "std/lists.ucg";

// with defaults
l.enumerate{list=[1, 2, 3]} == [[0, 1], [1, 2], [3, 4]];

// With all parameters
l.enumerate{
    start=1,
    step=2,
    list=["foo", "bar", "foobar"],
} == [[1, "foo"], [3, "bar"], [5, "foobar"]];
```