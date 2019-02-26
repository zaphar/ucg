+++
title = "Introduction to UCG"
in_seach_index = true
+++

[UCG](https://crates.io/crates/ucg) is a universal grammar for configuration.

# Why another config format?

That's a good question. The world certainly doesn't need another serialization
format for configuration values. We probably haven't really needed another
serialization format since `ini` files and `xml`. But now that we do have so
many to choose from people are choosing them right and left.

Chances are if you manage more than 2 systems in your job then you have at
least 2 different serialization formats for their configuration. You probably
also have parts of that configuration that are shared between multiple systems
and need to be kept in sync between those different formats.

You might also have standard configs for vm based languages or deployment
targets like kubernetes. You also probably have a configuration management
system and it probably has some form of templating support to meet all of those
needs. UCG is meant to solve the problems that the templating engine
introduces.

Templates can be difficult to manage without introducing hard to see errors in
the serialization format they are generating. Most templating engines aren't
aware of the format they are templating. They usually end up being an ad-hoc
programming language in their own right but without any way to enforce
invariants or protect the template writer from creating bad configs. UCG
attempts to solve this problem by giving you a real programming language 
that also generates the config format you need natively and safely.

## UCG is not a config serialization format

UCG's goal is not to define a configuration format like JSON, YAML, or TOML. It
is not intended to replace the other serialization formats. Instead it is
intended to provide a common grammar for generating those formats. Currently
UCG is able to compile into a number of formats including:

* Environment Variables
* An executable shell launch script
* JSON
* YAML
* TOML
* XML

## UCG is not a templating engine

UCG is not a templating language. It is a compiler whose targets are specific
serialization formats. As such it doesn't output invalid formats in, for
example, `json` or `yaml`. It allows you to check configurations against
schemas. It lets you use shared logic when assembling the datastructures that
your config format.

UCG allows you to use one common grammar to generate configuation values for
any applications that use one of provided serialization formats while also
allowing you to easily share common configuration values like hostnames, jvm
settings, and database settings.

UCG is designed to make configuration as code a first class citizen of your
deployment strategy.

# UCG is not alone

When I started UCG I was unaware of some other attempts at this that came before. If you find UCG intriguing you might be interested in these other projects with similar goals.

<style>
th, td { padding: 1em; }
th { 
    background: black;
    color: white;
}
tbody td:nth-child(odd) {
  background-color: darkgrey;
  color: black;
}

tbody td:nth-child(even) {
  background-color: lightgrey;
  color: black;
}
</style>
<table>
<thead><th></th><th>UCG</th><th>JSonnet</th><th>Dhall</th></thead>
<tr>
<td><b>Variables</b></td><td>x</td> <td>x</td><td>x</td>
</tr>
<tr>
<td><b>Conditionals</b></td><td>x</td><td>x</td><td>x</td>
</tr>
<tr>
<td><b>Functions</b></td><td>x</td><td>x</td><td>x</td>
</tr>
<tr>
<td><b>Modules/Classes</b></td><td>x</td><td>x</td><td>x</td>
</tr>
<tr>
<td><b>Imports</b></td><td>x</td><td>x</td><td>x</td>
</tr>
<tr>
<td><b>Std Lib</b></td><td>Minimal</td><td>x</td><td>Prelude</td>
</tr>
<tr>
<td><b>Type Safety</b></td><td>Inferred types and schema validation<td>Unknown</td><td>Inferred types</td>
</tr>
<tr>
<td><b>Guaranteed to terminate</b></td><td>No</td><td>No</td><td>Yes</td>
</tr>
</table>
<table>

## JSonnet

<a href="https://jsonnet.org/learning/tutorial.html">Jsonnet</a> is a project
from a Google employee that also has a programming language approach that
outputs to common formats. It has functions, shared libraries, a standard
library and more tool support than UCG currently has.

JSonnet is probably the closest thing to UCG out there with some differences in
syntax but very similar semantics.

## Dhall lang

<a href="https://github.com/dhall-lang/dhall-lang">Dhall-Lang</a> is a config
language with the interesting feature of being guaranteed to complete. It's a
pure functional language with functions, shared libraries and a standard
library as well. If a strong type system with a haskell like syntax are your
cup of tea then dhall lang is for you.

Next: <a href="/getting-started">Getting Started</a>