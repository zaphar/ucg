+++
title = "Testing Helpers"
weight = 3
sort_by = "weight"
in_search_index = true
+++

The UCG testing modules can be imported like so `let t = import "std/testing.ucg";`
It has a number of helpful assertions you can use in unit testing.

## The asserts modules

### ok assertion

The `ok` assertion module tests that something is true. It has two parameters.

* `test` which is required and is represents the expression to test.
* `desc` which is an optional description to output for your test. This defaults to
    the `todo_desc` that is set in the testing library.

```
let t = import "std/testing.ucg";

assert t.ok{
    test=true,
};
```

### no_ok assertion

The `not_ok` assertion module tests that something is not true. It has two
parameters.

* `test` which is required and is represents the expression to test.
* `desc` which is an optional description to output for your test. This defaults to
    the `todo_desc` that is set in the testing library.

```
let t = import "std/testing.ucg";

assert t.ok{
    test=true,
};
```

### equal assertion

The `equal` assertion module tests that two items are equal. It has three
parameters.

* `left` which is required and is the left hand side expression to compare
* `right` which is required is the right hand side expression to compare.
* `desc` which is an optional description to output for your test. This defaults to
  a description created from the compared values.

```
let t = import "std/testing.ucg";

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
* `desc` which is an optional description to output for your test. This defaults to
  a description created from the compared values.

```
let t = import "std/testing.ucg";

assert t.not_equal{
    left=1,
    right=2,
};
```