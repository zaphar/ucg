+++
title = "The UCG Language Reference"
slug = "reference"
weight = 2
sort_by = "weight"
in_search_index = true
+++
An Overview
-----------

UCG is a language specialized for generating configurations. It does not have classes,
inheritance, or a full type system. All values are immutable once bound to
a name. A valid UCG file is composed of a series of statements. Statements can be
an expression, introduce named bindings, or create different outputs. All statements
must be terminiated by a semicolon.

Some words are reserved in UCG and can not be used as a named binding.

 
* self
* assert
* true
* false
* let
* import
* as
* in
* is
* not
* fail
* select
* func
* module
* env
* map
* filter
* reduce
* NULL
* out

Next: <a href="types">Types</a>