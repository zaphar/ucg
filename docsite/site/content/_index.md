+++
title = "Introduction to UCG"
in_seach_index = true
+++

[UCG](https://crates.io/crates/ucg) is a universal grammar for configuration.
It's goal is not to define a configuration format like JSON, YAML, or TOML. It
is not intended to replace the other configuration formats. Instead it is
intended to provide a common grammar for generating those formats. Currently
UCG is able to generate conversions for the following formats.

* Environment Variables
* Command Line Flags
* An executable shell launch script combining the two above
* JSON
* YAML
* TOML

UCG allows you to use one common grammar to generate configuation values for any applications that
use one of provided conversion outputs while also allowing you to easily share common configuration
values like hostnames, jvm settings, and database settings.

UCG can build an entire directory of files or a single file.

Next: <a href="/getting-started">Getting Started</a>