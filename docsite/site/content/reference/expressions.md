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

Many UCG expressions or statements use a symbol. A symbol might be used as
either a name for a binding or a name for a field. Symbols must start with an
ascii letter and can contain any ascii letter, number, `_`, or `-` characters.

### The environment symbol

There is a special symbol in UCG for obtaining a value from the environment.
The `env` symbol references the environment variables in environment at the
time of the build. You reference an environment variable just like it was in a
tuple. By default, attempting to reference a variable that doesn't exist will
be a compile error. You can turn this behavior off with the `--nostrict`
argument to the compiler. When in nostrict mode nonexistent variables will
result in a warning and be set to the NULL empty value.

```
let env_name = env.DEPLOY_ENV;
```

Binary Operators
----------

UCG has a number of binary infix operators. Some work only on numeric values and others
work on more than one type.

### Selector operators

The UCG selector operator `.` selects a field or index from tuples or lists.
They can descend arbitrarily deep into data structures. 

You can reference a field in a tuple by putting the field name after a dot. You
can index into a list by referencing the index after the `.`. Lists are always
0 indexed.

```
let tuple = {
    inner = {
        field = "value",
    },
    list = [1, 2, 3],
    "quoted field" = "quoted value",
};

// reference the field in the inner tuple in our tuple defined above.
tuple.inner.field;

// reference the field in the list contained in our tuple defined above.
tuple.list.0;
```

Selectors can quote fields if there are quoted fields with spaces in the tuple.

```
tuple."quoted field";
```

### Numeric Operators

UCG supports the following numeric operators, `+`, `-`, `*`, `/` Each one is type safe 
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

UCG supports the comparison operators `==`, `!=`, `>=`, `<=`, `<`, `>`, and `in`.
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

The `in` operator tests for the existence of a field in a tuple or an element in a
list.

```
let tpl = { foo = "bar" };
foo in tpl; // evaluates to true
"foo" in tpl; // also evaluates to true.
```

Lists do a deep equal comparison when testing for the existence of an element.

```
let lst = [1, "two", {three = 3}];
1 in lst; // evaluates to true;
{three = 3} in lst; // evaluates to true
{three = "3"} in lst; // evaluates to false
{three = 3, two = 2} in lst // evaluates to false
```

### Boolean Operators

UCG has the standard boolean operators: `&&` and `||`. Both of them short circuit and they require the expressions on each
side to be boolean.

```
true && false == false;
false || true == true;
```

#### Operator Precedence

UCG binary operators follow the typical operator precedence for math. `*` and
`/` are higher precendence than `+` and `-` which are higher precedence than
any of the comparison operators.

Type test expressions
---------------------

ucg has the `is` operator for testing that something is of a given base type.

```
("foo" is str) == true;
```

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

Import Expressions
------------------

Import expressions bring in a ucg file and expose their bound values as a tuple
in the current file. Import expressions are idempotent and cached so you can
use them than once in a file safely. Import expressions start with the `import`
keyword and are followed by a string containing the path of the file to import.

```
// You can import an entire file into the namespace.
let imported = import "some_file.ucg";

// Or you can just import a single value from that file.
let imported_val = (import "some_file.ucg").val;
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

Range Expression
----------------

UCG can generate lists from a range with an optional step.

```
1:10 == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
0:2:10 == [0, 2, 4, 6, 8, 10];
```

Macros
-----

Macros look like functions but they are resolved at compile time and
configurations don't execute so they never appear in output. Macros close over
the environment up to the point where they are declared in the file. One
consequence of this is that they can not call themselves so recursive macros
are not possible. This is probably a feature. They are useful for constructing
tuples of a certain shape or otherwise promoting data reuse. You define a macro
with the `macro` keyword followed by the arguments in parentheses, a `=>`, and
then a valid expression.

```
let mymacro = macro (arg1, arg2) => {
    host = arg1,
    port = arg2,
    connstr = "couchdb://@:@" % (arg1, arg2),
};

let my_dbconf = mymacro("couchdb.example.org", "9090");

let my_dbhost = dbconf.host;

let add = macro(arg1, arg2) => arg1 + arg2;
add(1, 1) == 2;
```

Functional processing expressions
---------------------------------

UCG has a few functional processing expressions called `map`, `filter`, and
`reduce`. All of them can process a list or tuple.

Their syntax starts with either `map` `filter`, or `reduce followed by a symbol
that references a valid macro and finally an expression that resolves to either
a list or a tuple.

### Map expressions

Map macros should produce either a valid value or a list of [field, value] that
will replace the element or field it is curently processing.

**For Lists**

When mapping a macro across a list the result field can be any valid value. The
macro is expected to take a single argument.

```
let list1 = [1, 2, 3, 4];

let mapper = macro(item) =>  item + 1;
map mapper list1 == [2, 3, 4, 5];
```

**For Tuples**

Macros for mapping across a tuple are expected to take two arguments. The first
argument is the name of the field. The second argument is the value in that
field. The result should be a two item list with the first item being the new
field name and the second item being the new value.

```
let test_tpl = {
    foo = "bar",
    quux = "baz",
};
let tpl_mapper = macro(name, val) =>  select name, [name, val], {
    "foo" = ["foo", "barbar"],
    quux = ["cute", "pygmy"],
};
map tpl_mapper test_tpl == {foo = "barbar", cute = "pygmy"};
```

### Filter expressions

Filter expressions should return a field with false or NULL for items to
filter out of the list or tuple. Any other value in the return field results in
the item or field staying in the resulting list or tuple.

**Lists**

```
let list2 = ["foo", "bar", "foo", "bar"];
let filtrator = macro(item) => select item, NULL, {
    foo = item,
};

filter filtrator.result list2 == ["foo", "foo"];
```

**Tuples**

```
let test_tpl = {
    foo = "bar",
    quux = "baz",
};
let tpl_filter = macro(name, val) =>  name != "foo";
filter tpl_filter test_tpl == { quux = "baz" };
```

### Reduce expressions

Reduce expressions start with the reduce keyword followed by a symbol referencing a macro an expression for the accumulator and finally the tuple or list to process.

**Tuples**

```
let test_tpl = {
    foo = "bar",
    quux = "baz",
};
let tpl_reducer = macro(acc, name, val) =>  acc{
    keys = self.keys + [name],
    vals = self.vals + [val],
};

reduce tpl_reducer {keys = [], vals = []}, test_tpl == {keys = ["foo", "quux"], vals = ["bar", "baz"]};
```

**Lists**

```
let list1 = [1, 2, 3, 4];
let list_reducer = macro(acc, item) =>  acc + item;

 list_reducer 0, list1 == 0 + 1 + 2 + 3 + 4;
```

Include expressions
-------------------

UCG can include the contents of other files as an expression. Currently we only
support strings but we plan to support yaml, and json and possibly base64 encoding
in the future. include expressions start with the `include` keyword a type (currently only `str`), and a path. Relative paths are calculated relative to the including file.

```
let script = include str "./script.sh";
```

Conditionals
----------

UCG supports a limited conditional expression called a select. A select
expression starts with the `select` keyword and is followed by a an expression
resolving to a string or boolean naming the field to select, an expression
resolving to the default value, and finally a tuple literal to select the field
from. If the field selected is not in the tuple then the default value will be
used.

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

let ifresult = select true, NULL, {
    true = "true result",
    false = "false result",
}; // result will be "true result"
```

Modules
-------

UCG has another form of reusable execution that is a little more composable than macros
are. Modules allow you to parameterize a set of statements and build the statements
later. Modules are an expression. They can be bound to a value and then reused later.
Modules do not close over their environment by they can import other UCG files into
the module using import statements.

Module expressions start with the module keyword followed by a tuple representing their
parameters with any associated default values. The body of the module is separated from
the parameter tuple by the `=>` symbol and is delimited by `{` and `}` respectively.

The body of the module can contain any valid UCG statement.

```
let top_mod = module {
    deep_value = "None",
} => {
    import "shared.UCG" as shared_macros;

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