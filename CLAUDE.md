# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

UCG (Universal Configuration Grammar) is a Rust compiler for a DSL that describes configuration values. It compiles to multiple output formats: JSON, YAML, TOML, XML, environment variables, shell flags, base64. It is not a parseable config format—it's a language that generates config.

Documentation: https://ucg.marzhillstudios.com

## Build & Test Commands

```bash
make test              # Run all tests (unit + integration + stdlib)
make unit              # Unit tests only (cargo test)
make integration       # Integration tests (cargo run -- test -r integration_tests)
make stdlibtest        # Stdlib tests (cargo run -- test -r std/tests)
cargo test <test_name> # Run a single unit test
make build             # Debug build
make buildrelease      # Release build
cargo fmt              # Format Rust code (required before merge)
make bench             # Benchmark integration tests (requires release build + hyperfine)
make fuzz-<target>     # Run a fuzz target: tokenize, parse, or compile (requires nix develop .#fuzz)
```

Nix flake provides the dev environment. Uses direnv for shell activation.

## Compiler Architecture

Traditional compiler pipeline in `src/`:

1. **Tokenizer** (`tokenizer/`) — Lexical analysis, produces token stream
2. **Parser** (`parse/`) — Precedence-climbing recursive descent parser, produces AST
3. **AST** (`ast/`) — Node definitions, tree walking, type checking (`ast/typecheck/`)
4. **Build** (`build/`) — Compilation stage
   - `opcode/translate.rs` — AST to opcode translation
   - `opcode/vm.rs` — Virtual machine execution
   - `opcode/environment.rs` — Execution environment
   - `opcode/runtime.rs` — Built-in functions
   - `opcode/scope.rs` — Symbol scoping
   - `ir.rs` — Intermediate representation (`Val` type)
   - `format.rs` — UCG code formatter
5. **Convert** (`convert/`) — Output format converters (json, yaml, toml, xml, env, flags, b64, exec)

6. **Dep** (`dep/`) — Package management
   - `manifest.rs` — `ucg-deps` TOML parsing and validation
   - `lockfile.rs` — `ucg.lock` parsing and staleness detection
   - `resolve.rs` — MVS (Minimum Version Selection) resolver
   - `registry.rs` — Tag listing from git/hg repos
   - `vendor.rs` — Fetch, hash, and vendor dependencies
   - `nix.rs` — Nix expression generation (`ucg-deps.nix`)
   - `url.rs` — URL normalization for deduplication
   - `error.rs` — Dep-specific error types

Key design decisions:
- VM-based execution (compiles to opcodes, not direct AST interpretation)
- Standard library (`std/`) is embedded into the binary at compile time via `bin/build_main.rs`
- Pluggable converter system for output formats
- `vendor/` is a resolution keyword in imports, not a filesystem path — resolved against the package root's configured vendor directory
- Package root detected once at startup, threaded through the build session

## CLI Subcommands

`ucg build`, `ucg test`, `ucg fmt`, `ucg repl`, `ucg eval`, `ucg converters`, `ucg importers`, `ucg env`, `ucg dep`. The `-r` flag enables recursive directory processing. `--no-strict` disables strict type checking.

### Package Management (`ucg dep`)

- `ucg dep init [--vendor "vendor"] [--nix]` — Create `ucg-deps` file
- `ucg dep add <url> [--version ">=1.0.0"] [--type "git"|"hg"]` — Add/update dependency, resolve, lock, vendor
- `ucg dep remove <url>` — Remove direct dependency, re-resolve, clean vendor
- `ucg dep lock` — Re-resolve all deps and rewrite `ucg.lock`
- `ucg dep vendor` — Fetch locked deps into vendor directory
- `ucg dep nix [--stdout]` — Generate `ucg-deps.nix` from lockfile

Package files: `ucg-deps` (manifest), `ucg.lock` (lockfile), `ucg-deps.nix` (Nix expression).
See `docs/package-management-spec.md` for full specification.

## Test Organization

- **Unit tests**: `#[cfg(test)]` modules in source files (e.g., `tokenizer/test.rs`, `parse/test.rs`, `build/compile_test.rs`)
- **Integration tests**: `integration_tests/` directory — UCG files with assertions, run via `ucg test`
- **Stdlib tests**: `std/tests/` — tests for standard library modules
- UCG test files use `assert` statements and are named `*_test.ucg`

## Code Standards

- All code must pass `cargo fmt` before merging
- Documentation and examples must be updated alongside code changes
