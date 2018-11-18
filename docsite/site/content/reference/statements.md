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
be considered a noop. If we ever create a repl for ucg statements they may prove more 
useful.

Named Value Statements
--------

### Let statements

There are two statements that can introduce a named value for a given ucg file. Let 
statnements and import statements. Any collisions in binding names inside a file are 
treated as compile errors. Bindings are immutable and once bound they can't be 
modified.

```
let name = "foo";
```

### Import Statement

The import statement imports the contents of another ucg file into the current file 
with a name. The imported files named values are exposed as a tuple in the referencing 
file. It starts with the `import` keyword and is followed by a quoted path to the ucg 
file, the keyword `as`, and a name for the imported values.

```
import "dbconfigs.ucg" as dbconfigs;
let mysqlconf = dbconfigs.mysql;
```

Output Statements
-----------

Some statements in ucg exist to generate an output. Either a compiled configuration or the results of test assertions.

### Assert Statements

The assert statement defines an expression that must evaluate to either true or false. 
Assert statements are noops except during a validation compile. They give you a way to 
assert certains properties about your data and can be used as a form of unit testing 
for your configurations. It starts with the `assert` keyword followed by a valid block 
of ucg statements delimited by `|` characters. The final statement in the in the block 
must evaluate to a boolean expression.

```
assert |
    host == "www.example.com";
|;

assert |
    select qa, 443, {
      qa = 80,
      prod = 443,
    } == 443;
|;
```

When _test.ucg files are run in a test run then ucg will output a log of all the assertions to stdout. Giving you a simple test harness for your ucg configs.

### Out Statements

The Out statement defines the output for a UCG file. It identifies the output 
converter type and an expression that will be output. The output converter type is 
expected to be one of the registered converters (e.g. json, exec) and the artifact 
file will take the same name as the ucg file with the extension replaced by the 
defined extension for that converter.

For a file named api_config.ucg with the following contents:

```
let myconf = {
    api_url = "https://example.org/api/v1/",
    api_token = env.API_TOKEN,
};
out json myconf;
```

ucg will output the myconf tuple as json to a file called api_config.json

Next: <a href="/reference/converters">Converters</a>