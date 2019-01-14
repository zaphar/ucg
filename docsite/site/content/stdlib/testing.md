+++
title = "UCG Testing Modules"
weight = 3
sort_by = "weight"
in_search_index = true
+++

The UCG testing modules can be imported like so `let t = import "std/testing.ucg";`
It has a number of helpful assertions you can use in unit testing.

## The asserts modules

The asserts module exposes a number of asserts. It contains nested modules for each
available assertion. You can configure the asserts module with a default description if desired.

Each of the asserts results in a `result` field that is a tuple with the shape that
the assert statement expects.

The asserts module has one parameter.

* `default_description` which is optional and defaults to "TODO description"

### ok assertion

The `ok` assertion module tests that something is true. It has two parameters.

* `test` which is required and is represents the expression to test.
* `desc` which is an optional description to output for your test. This defaults to
    the `default_description` that the `asserts` modules was set to.

```
let t = import "std/testing.ucg".asserts{};

assert t.ok{
    test=true,
};
```

### no_ok assertion

The `not_ok` assertion module tests that something is not true. It has two
parameters.

* `test` which is required and is represents the expression to test.
* `desc` which is an optional description to output for your test. This defaults to
    the `default_description` that the `asserts` modules was set to.

```
let t = import "std/testing.ucg".asserts{};

assert t.ok{
    test=true,
};
```

### equal assertion

The `equal` assertion module tests that two items are equal. It has two parameters.

* `left` which is required and is the left hand side expression to compare
* `right` which is required is the right hand side expression to compare.

This module will create a description for you from the compared values.

```
let t = import "std/testing.ucg".asserts{};

assert t.equal{
    left=1,
    right=1,
};
```

### not_equal assertion

The `not_equal` assertion module tests that two items are not equal. It has two
parameters.

* `left` which is required and is the left hand side expression to compare
* `right` which is required is the right hand side expression to compare.

This module will create a description for you from the compared values.

```
let t = import "std/testing.ucg".asserts{};

assert t.not_equal{
    left=1,
    right=2,
};
```