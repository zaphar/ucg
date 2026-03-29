# Parser Bug: Grouped expressions in range bounds

## Problem

A range expression with a grouped expression as the start bound does not parse correctly:

```ucg
let r = ({x = 1}.x):10;   // fails: "Expected (;) but got (:)"
```

The parser interprets `({x = 1}.x)` as a standalone `grouped_expression` (a complete statement), then sees `:10;` and expects a `;` instead of the `:`.

## Root Cause

In `src/parse/mod.rs`, the `non_op_expression` parser (line ~769) tries alternatives in order:

```rust
non_op_expression => either!(
    ...
    trace_parse!(grouped_expression),    // line 779 — matches first!
    trace_parse!(include_expression),
    trace_parse!(unprefixed_expression), // line 781 — contains range_expression
)
```

And `unprefixed_expression` (line ~756) contains:

```rust
unprefixed_expression => either!(
    trace_parse!(format_expression),
    trace_parse!(range_expression),      // never reached when start is grouped
    ...
)
```

The `range_expression` parser (line ~672) accepts `either!(simple_expression, grouped_expression)` for its start bound. But `grouped_expression` as a standalone alternative in `non_op_expression` takes priority and consumes the `(...)` before `range_expression` gets a chance.

## How to Fix

The `range_expression` needs to be tried before `grouped_expression` in `non_op_expression`, or the range parser needs to be restructured so it's attempted at the right level. Options:

1. **Move `range_expression` into `non_op_expression`** before `grouped_expression` — but this changes what `range_expression` can see and may break other parses.

2. **Make `grouped_expression` look-ahead for `:`** — if a `)` is followed by `:`, fail the grouped parse so range gets a chance.

3. **Restructure**: have `grouped_expression` succeed, then check at the statement/expression level if `:` follows and upgrade to a range.

## Test

There is an ignored test that should pass once this is fixed:

- `src/lsp/mod.rs`: `test_find_hover_range_start_tuple_field` (marked `#[ignore]`)

## Verification

```bash
cargo test --lib test_find_hover_range_start_tuple_field -- --include-ignored
```
