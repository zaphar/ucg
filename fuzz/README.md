# UCG Fuzz Testing

Coverage-guided fuzz testing for the UCG compiler using cargo-fuzz (libFuzzer).

## Requirements

Fuzzing requires a nightly Rust toolchain. The project provides a dedicated nix
dev shell for this:

```bash
nix develop .#fuzz
```

This gives you nightly rustc, cargo-fuzz, and gnumake. The default dev shell
stays on stable and is unaffected.

## Running

From the project root (not the fuzz/ directory):

```bash
# Run a specific fuzzer (runs until Ctrl-C)
make fuzz-parse
make fuzz-tokenize

# Run with a time limit (seconds)
make fuzz-parse FUZZ_DURATION=300

# Run with multiple parallel jobs
make fuzz-parse FUZZ_JOBS=4

# Adjust max input size in bytes (default 4096)
make fuzz-parse FUZZ_MAX_LEN=8192

# Combine options
make fuzz-parse FUZZ_DURATION=600 FUZZ_JOBS=4 FUZZ_MAX_LEN=8192

# Build without running (useful to check compilation after code changes)
make build-fuzz

# Run all fuzzers sequentially
make all-fuzz
```

## Fuzz Targets

### tokenize

Feeds arbitrary UTF-8 strings to the tokenizer (`tokenizer::tokenize`). This is
the fastest target since it only exercises lexical analysis. Good for finding
panics on malformed escape sequences, unterminated strings, and unexpected
character sequences.

### parse

Feeds arbitrary UTF-8 strings through the full tokenize + parse pipeline. This
is the highest-value target since the parser has complex state: precedence
climbing, recursive expression parsing, and many interacting grammar rules. It
also implicitly tests the tokenizer.

## Corpus

Seed corpus files live in `fuzz/corpus/<target>/`. These were initially
populated from the integration and stdlib test files. As the fuzzer runs, it
discovers new inputs that trigger novel code paths and adds them to the corpus
automatically. The corpus persists across runs, so each session builds on prior
work.

## Artifacts

When the fuzzer finds a crash, the triggering input is saved to
`fuzz/artifacts/<target>/`. The file name encodes the failure type:

- `crash-<hash>` -- caused a panic or abort
- `timeout-<hash>` -- exceeded the per-input time limit (likely an infinite loop)
- `oom-<hash>` -- out of memory (likely unbounded allocation)

To reproduce a crash:

```bash
nix develop .#fuzz -c cargo fuzz run parse fuzz/artifacts/parse/crash-abc123
```

## Adding a New Fuzz Target

1. Create `fuzz/fuzz_targets/<name>.rs`:

```rust
#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &str| {
    // Call the code under test, discard the result.
    // The fuzzer looks for panics, not wrong answers.
    let _ = your_function(data);
});
```

2. Add a `[[bin]]` section to `fuzz/Cargo.toml`:

```toml
[[bin]]
name = "<name>"
path = "fuzz_targets/<name>.rs"
doc = false
```

3. Optionally seed the corpus:

```bash
mkdir -p fuzz/corpus/<name>
cp integration_tests/*.ucg fuzz/corpus/<name>/
```

4. Run it -- the Makefile pattern rule picks it up automatically:

```bash
make fuzz-<name>
```

## FAQ

### "nano zone abandoned due to inability to reserve vm space"

You will see this message on every run on macOS:

```
malloc: nano zone abandoned due to inability to reserve vm space.
```

This is harmless. AddressSanitizer (ASAN) reserves large virtual address regions
for its shadow memory, which conflicts with macOS's nano zone allocator. macOS
falls back to its standard allocator. The fuzzer runs correctly -- this is not a
bug in UCG or the fuzz targets. The message comes from macOS itself and cannot be
suppressed.
