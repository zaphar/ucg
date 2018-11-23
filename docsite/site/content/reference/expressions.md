+++
title = "UCG Expressions"
weight = 3
sort_by = "weight"
in_search_index = true
+++
Ucg expressions can reference a bound name, do math, concatenate lists or
strings, copy and modify a struct, or format a string.

Symbols
-------

Many ucg expressions or statements use a symbol. A symbol might be used as
either a name for a binding or a name for a field. Symbols must start with an
ascii letter and can contain any ascii letter, number, `_`, or `-` characters.

Selectors
------

A UCG selector references a bound value by name. They can descend into tuples
or lists or refer to symbols in imported files. They start with a symbol followed
optionally by a list of other symbols or numbers separated by `.` characters to
reference subfields or indexes in a list.

You can reference a field in a tuple by putting the field name after a dot.
Lists are always 0 indexed. You can index into a list by referencing the index
after a `.`.

```
let tuple = {
    inner = {
        field = "value",
    },
    list = [1, 2, 3],
};

// reference the field in the inner tuple in our tuple defined above.
tuple.inner.field;

// reference the field in the list contained in our tuple defined above.
tuple.list.0;
```

### The environment Selector

There is a special selector in ucg for obtaining a value from the environment.
The `env` selector references the environment variables in environment at the
time of the build. You reference an environment variable just like it was in a
tuple. Attempting to reference a variable that doesn't exist will be a compile
error.

```
let env_name = env.DEPLOY_ENV;
```

Binary Operators
----------

UCG has a number of binary infix operators. Some work only on numeric values and others
work on more than one type.

### Numeric Operators

ucg supports the following numeric operators, `+`, `-`, `*`, `/` Each one is type safe 
and infers the types from the values they operate on. The operators expect both the 
left and right operands to be of the same type.

```
1 + 1;
1.0 - 1.0;
```

### Concatenation

The `+` operator can also do concatenation on strings and lists. As with the numeric
version both sides must be the same type, either string or list.

```
"Hello " + "World"; // "Hello World"
[1, 2] + [3]; // [1, 2, 3]
```

### Comparison Operators

UCG supports the comparison operators `==`, `!=`, `>=`, `<=`, `<`, and `>`.
They all expect both sides to be of the same type.

The `>`, `<`, `>=`, and `>=` operators are only supported on numeric types
(i.e. int, and float).

```
1 > 2; // result is false
2 < 3; // result is true
10 > "9"; // This is a compile error.
(1+2) == 3;
```

The equality operators `==` and `!=` are supported for all types and will
perform deep equal comparisons on complex types.

```
let tpl1 = {
  foo = "bar",
  one = 1
};
let tpl2 = {
  foo = "bar",
  one = 1
};
tpl1 == tpl2; // returns true
let tpl2 = {
  foo = "bar",
  one = 1
  duck = "quack",
};
tpl1 == tpl3; // returns false
```

Because tuples are an ordered set both tuples in a comparison must have their
fields in the same order to compare as equal.

#### Operator Precedence

UCG binary operators follow the typical operator precedence for math. `*` and
`/` are higher precendence than `+` and `-` which are higher precedence than
any of the comparison operators.

Copy Expressions
----------------

UCG expressions have a special copy expression for tuples. These faciliate a
form of data reuse as well as a way to get a modified version of a tuple. Copy
expressions start with a selector referencing a tuple followed by braces `{}`
with `name = value` pairs separated by commas. Trailing commas are allowed.

Copied expressions can change base fields in the copied tuple or add new
fields. If you are changing the value of a base field in the copy then the new
value must be of the same type as the base field's value. This allows you to
define a base "type" of sorts and ensure that any modified fields stay the
same.

```
let base = {
    field1 = "value1",
    field2 = 100,
    field3 = 5.6,
};

let overridden = base{
    field1 = "new value"
};

let expanded = base{
    field2 = 200,
    field3 = "look ma a new field",
};

let bad = base{
    field1 = 300, // Error!!! must be a string.
};

```

There is a special selector that can be used in a copy expression to refer to
the base tuple in a copy called `self`. `self` can only be used in the body of
the copy.

```
let nestedtpl = {
    field1 = "value1",
    inner = {
        field2 = 2
        inner = {
            field3 = "three",
        },
    },
};

let copiedtpl = nestedtpl{
    inner = self.inner{
        inner = self.inner{
            field4 = 4,
        },
    },
};
```

Format Expressions
----------

UCG has a format expression that has a limited form of string templating. A
format expression starts with a string followed by the `%` operator and a list
of arguments in parentheses separated by commas. Trailing commas are allowed.
The format string should have `@` characters in each location where a value
should be placed. Any primitive value can be used as an argument.

```
"https://@:@/" % (host, port)
```

Conditionals
----------

UCG supports a limited conditional expression called a select. A select
expression starts with the `select` keyword and is followed by a an expression
resolving to a string naming the field to select, an expression resolving to
the default value, and finally a tuple literal to select the field from. If the
field selected is not in the tuple then the default value will be used.

```
let want = "baz";

//     field  default
select want, "quux", {
    baz = "foo",
    fuzz = "bang",
}; // result will be "foo"

//     field    default
select "quack", "quux", {
    baz = "foo",
    fuzz = "bang",
}; // result will be "quux"
```

Macros
-----

Macros look like functions but they are resolved at compile time and
configurations don't execute so they never appear in output. Macros do not
close over their environment so they can only reference values defined in their
arguments. They can't refer to bindings or other macros defined elsewhere. They
are useful for constructing tuples of a certain shape or otherwise promoting
data reuse. You define a macro with the `macro` keyword followed by the
arguments in parentheses, a `=>`, and then a tuple literal.

```
let mymacro = macro (arg1, arg2) => {
    host = arg1,
    port = arg2,
    connstr = "couchdb://@:@" % (arg1, arg2),
}

let my_dbconf = mymacro("couchdb.example.org", "9090");

let my_dbhost = dbconf.host;
```

Modules
-------

UCG has another form of reusable execution that is a little more composable than macros
are. Modules allow you to parameterize a set of statements and build the statements
later. Modules are an expression. They can be bound to a value and then reused later.
Modules do not close over their environment by they can import other ucg files into
the module using import statements.

Module expressions start with the module keyword followed by a tuple representing their
parameters with any associated default values. The body of the module is separated from
the parameter tuple by the `=>` symbol and is delimited by `{` and `}` respectively.

The body of the module can contain any valid ucg statement.

```
let top_mod = module {
    deep_value = "None",
} => {
    import "shared.ucg" as shared_macros;

    let embedded_def = module {
        deep_value = "None",
    } => {
        let value = mod.deep_value;
    };

    let embedded = embedded_def{deep_value = mod.deep_value};
};
```

You instantiate a module via the copy expression. The resulting module instance can
reference the bindings in the module similarly to selecting a tuple field or a binding
from an imported file.

```
let embedded_default_params = top_mod{};
embedded_default_params.embedded.value == "None";

let embedded_with_params = embedded_mod{deep_value = "Some"};
embedded_with_params.embedded.value == "Some";
```

Next: <a href="/reference/statements">Statements</a>