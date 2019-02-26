+++
title = "Overview"
slug = "overview"
weight = 0
sort_by = "weight"
in_search_index = true
+++

UCG is an expression oriented functional language with immutability,
copy-on-write semantics and forward type inference. Everything in UCG is an
expression except for a few statements that introduce named bindings or
generate an output.

UCG is purpose built for constructing configurations. Anything that is not
useful for that is excluded. All ucg output targets one of the compilation
conversion targets.

UCG can import supported configuration formats and transform them before
outputting them into one of the compilation conversion targets. UCG also has
built in support test assertions in your configuration generation and
transformation logic.

Among the possibilities this opens up are:

* Slowly migrating existing configuration formats over to UCG
* Sharing common configuration values across static configurations
* Enforcing invariants in your configurations at build time
* Unit testing your configuration generation logic
* Converting one config format to another config format
* Dynammic querying of your configurations using the `ucg eval` command