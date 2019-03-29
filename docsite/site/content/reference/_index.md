+++
title = "The UCG Language Reference"
slug = "reference"
weight = 3
sort_by = "weight"
in_search_index = true
+++
An Overview
-----------

UCG is an immutable, expression oriented, functional programming language
specialized for generating configurations. It has limited IO and a form of
forward type inference.

A valid UCG file is composed of a series of statements. Statements can be an
expression, introduce named bindings, or create different outputs. All
statements must be terminiated by a semicolon. Statements are executed in order.
All expressions in ucg return a value. Expressions can see anything declared in
their enclosing scope up to the point of their execution.

Scopes in UCG are defined by the ucg file, a function, or a module body.

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