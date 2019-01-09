+++
title = "UCG Statements"
weight = 4
sort_by = "weight"
in_search_index = true
+++
Expression Statements
-------

The simplest and least useful is the expression statement. It is any valid expression 
followed by a semicolon.

```
1;
4 / 2;
"foo";
"foo" + "bar";
```

Despite the fact that these are valid the results are thrown away and can essentially 
be considered a noop.

Named Value Statements
--------

### Let statements

There are two statements that can introduce a named value for a given UCG file. Let 
statements and import statements. Any collisions in binding names inside a file are 
treated as compile errors. Bindings are immutable and once bound they can't be 
modified.

```
let name = "foo";
```

### Import Statement

The import statement imports the contents of another UCG file into the current file 
with a name. The imported file's named values are exposed as a tuple in the referencing 
file. It starts with the `import` keyword and is followed by a quoted path to the UCG 
file, the keyword `as`, and a name for the imported values.

```
import "dbconfigs.ucg" as dbconfigs;
let mysqlconf = dbconfigs.mysql;
```

Output Statements
-----------

Some statements in UCG exist to generate an output. Either a compiled
configuration or the results of test assertions.

### Assert Statements

The assert statement defines an expression that must evaluate to tuple with an
ok field that is either true or false and a desc field that is a string. Assert
statements are noops except during a validation compile. They give you a way to
assert certains properties about your data and can be used as a form of unit
testing for your configurations. It starts with the `assert` keyword followed
by a valid ucg expression.

```
assert {
    ok = host == "www.example.com",
    desc = "Host is www.example.com",
};

assert {
    ok = select qa, 443, {
      qa = 80,
      prod = 443,
    } == 443,
    desc = "select default was 443",
};
```

Assert statements are only evaluated when running the `ucg test` command. That
command evaluates all of the `*_test.ucg` files. When `*_test.ucg` files are
run in a test run then ucg will output a log of all the assertions to stdout as
well as a PASS or FAIL for each file. This gives you a simple test harness for
your ucg configs.

### Out Statements

The Out statement defines the output for a UCG file. It identifies the output
converter type and an expression that will be output. The output converter type
is expected to be one of the registered converters unquoted (e.g. json, exec)
and the value to convert. The generated artificact will take the same name as
the UCG file with the extension replaced by the defined extension for that
converter.

For a file named api_config.ucg with the following contents:

```
let myconf = {
    api_url = "https://example.org/api/v1/",
    api_token = env.API_TOKEN,
};
out json myconf;
```

UCG will output the myconf tuple as json to a file called api_config.json

You can get a list of the available converters as well as the extensions
defined for each one by running the `ucg converters` command.

Next: <a href="/reference/converters">Converters</a>