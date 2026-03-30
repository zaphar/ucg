+++
title = "Security Model"
weight = 7
sort_by = "weight"
in_search_index = true
+++

UCG is a compiler that processes source files and generates configuration output.
Understanding its trust model is important when deciding which UCG files to compile
and how to handle generated output.

Trust Model
----

UCG source files are **code**, not passive data. A UCG file can:

- **Read any file** accessible to the process user via `import` and `include`
  statements. There is no sandbox or directory restriction on file access.
- **Access all environment variables** available to the process via `env`
  expressions. This includes secrets such as API keys, database credentials,
  and cloud provider tokens if they are present in the environment.
- **Generate executable scripts** via the `exec` converter that will run
  arbitrary commands when executed.

Because of this, you should treat UCG source files with the same level of trust
as shell scripts or program source code. **Do not compile untrusted UCG files**
without reviewing them first.

Recommendations for Sensitive Environments
----

If you need to compile UCG files from sources you do not fully control:

- **Review the source** before compiling, just as you would review a shell
  script or Makefile from an external contributor.
- **Restrict the environment** by clearing or filtering environment variables
  before invoking `ucg`. Only export variables that the UCG files are expected
  to use.
- **Use OS-level sandboxing** (containers, chroot, or similar) to limit
  filesystem access if processing configuration templates from third parties.
- **Inspect generated output** before deploying it, especially scripts produced
  by the `exec` converter.

Generated Shell Output
----

The `exec`, `flags`, and `env` converters produce output intended for use in
shell contexts. UCG escapes interpolated values in this output to prevent
shell injection, but the generated output should still be treated as code
that you have reviewed and trust, not as safely sanitized data from an
untrusted source.
