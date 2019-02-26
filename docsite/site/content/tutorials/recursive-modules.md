+++
title = "Recursive Modules and their Uses"
slug = "recursive-modules"
weight = 2
in_search_index = true
+++

Modules are the only expression in UCG with the capablility to perform
recursion using self import[^1]. This is safe because statements in a module
are evaluated at module copy time. This is useful when you need to traverse a
UCG value deeply. You can see an example of this in the UCG `std/schema.ucg`
module where we perform a deep type comparison between two UCG values.

As an example we will create a module that can do a dynamic deep index into a
tuple by using recursion. The module will take in a tuple to traverse and a
list of field names to descend into.

Create a file called `deep_index.ucg` and put the following into it.

```
let deep_index = module {
    tpl = {}, // We expect that tuples here.
    names = [], // We expect a list of names here.
} => {
    // todo
};
```

Our `deep_index` module expects a tuple and list of names. We set the defaults
to an empty tuple and an empty list so anything that isn't either those or NULL
will be a compile error when we call our module. Next we need to check for the
existence of the first name in the tuple.

```
let deep_index = module {
    tpl = {}, // We expect tuples here.
    names = [], // We expect a list of names here.
} => (result) {
    // Fill in

    let result = select ((mod.names.0) in mod.tpl), NULL, {
        true = mod.tpl.(mod.names.0),
    };
};
```

There are a few things to note here. We use the out expression `(result)` to
make this module more ergonomic. We reference our modulr arguments with the
`mod.` prefix. We also surround part of our selector with parens in
`(mod.tpl.(mod.names.0))` to force the last selector to pick the field
dynamically from the result of the grouped expression.

As written this module has a few problems though.

* It is unsafe for inputs where the list of names is empty
* it is unsafe for inputs where the list of names contains items that are not
  strings.

## Handling Unsafe inputs.

We want it to be a compile error if anyone calls our module with inputs of the
wrong shape. We have two possibilities to handle. NULL inputs for either
argument or an list that contains items that are not strings.

```
let deep_index = module {
    tpl = {}, // We expect tuples here.
    names = [], // We expect a list of names here.
} => (result) {
    // import some useful items from the std lib
    let schema = import "std/schema.ucg";
    let l = import "std/lists.ucg";
    
    // Assert some compile time expectations.
    // First ensure that names is a list of strings.
    (names != NULL && schema.shaped{val=names, shape=[""]}) ||
        fail "names must be non NULL and a list of strings";
    // Now ensure that tpl is not NULL
    tpl != NULL || fail "tpl must not be NULL";
    
    let select_field = func(name) select (name in mod.tpl), NULL {
        true = mod.tpl.(name),
    };

    let result = select l.len(names) > 0, mod.tpl, {
        true = select_field(mod.names.0),
    };
};
```

Our module will now throw appropriate compile time errors if we pass in invalid
input shapes. We also correctly handle the case where we have no more names to
descend into. We now need to handle calling our module recursively.

## Module Recursion

In order to do recursion we have to import ourself.

```
let deep_index = module {
    tpl = {}, // We expect that tuples here.
    names = [], // We expect a list of names here.
} => (result) {
    // import some useful items from the std lib
    let schema = import "std/schema.ucg";
    let l = import "std/lists.ucg";

    // import ourself now.
    let self = import "deep_index.ucg".deep_index;
    
    // Assert some compile time expectations.
    // First ensure that names is a list of strings.
    (names != NULL && schema.shaped{val=names, shape=[""]}) ||
        fail "names must be non NULL and a list of strings";
    // Now ensure that tpl is not NULL
    tpl != NULL || fail "tpl must not be NULL";

    let name = mod.names.0;
    let fail_msg = "Attempt to descend into a non existent name @" % (name);

    // Field selector function that throws a compile error if the field 
    // doesn't exist.
    let select_field = func(name) => select (name in mod.tpl), fail fail_message {
        true = mod.tpl.(name),
    };

    // calculate our next value.
    // If there are no more names then something has gone wrong and
    // we should hard fail.
    let next = select l.len(names) > 0, mod.tpl, fail "Impossible" {
        true = select_field(name),
    };
    
    // we need to get the tail of our list.
    let tail = l.tail(names);

    // if the tail is non empty then recurse down.
    // If the tail is empty then return the current value.
    let result = select l.len(tail) > 0, next, {
        // recurse
        true = self{tpl=result, names=tail},
    };
};
```

Our `deep_index` module can now descend down into a tuple given a list of field
names. If any of the names does not exist it will be a compile failure.

## Final notes

One consquence of this method of doing recursion is that you can not reference
the module in the file it is defined in. Doing so would result in a compile
error as UCG detects an import loop. In practice this does not prove to be a
problem since nearly all usages of the module will be outside of the file it is
defined in.

<hr>

[^1]: Technnically functions can do recursion if you manually pass in the same function as an argument to itself, but this is unwieldy and error-prone.