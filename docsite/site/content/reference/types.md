+++
title = "UCG types"
weight = 2
sort_by = "weight"
in_search_index = true
+++
Primitive Values
-------------

UCG has a few primitive types.

### Boolean

Boolean types can be either `true` or `false`. They are represented by symbols of the
same name.

```
true;
false;
```

### Integer

An Integer is any 64 bit integer number.

```
1;
```

### Float

A Float is any 64 bit floating point number. You indicate a number is a Float by
including a decimal point. Any number with a decimal point is a float.

```
1.0;
.0;
1.;
```

### String

Strings are any double quoted text. You can use the `\` to esacpe characters in the text.

```
"This is a string";
"This is an escaped \"string\"";
```

### NULL or the Empty type

NULL is the empty type. It represents the absence of a value. It is represented by the
symbol `NULL`.

```
let empty = NULL;
```

Complex types
-----------

UCG also has two complex types.

### Tuples

Tuples are an ordered set of name value pairs. They are delimited by braces and should
contain 1 or more `name = expression` pairs separated by commas. Trailing commas are allowed.

```
let tuple = {
    field = "value",
    inner = {
        number = 1,
    },
};
```

### Lists

Lists are a 0 indexed heterogenous list of expressions. The are delimited by square 
brackets and should contain 1 or more expressions separated by commas. Trailing commas 
are allowed.

```
let list = [1, "two", {three = 3},];
```

Next: <a href="/reference/expressions">Expressions</a>