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

## Reading the Output

When the fuzzer runs, it prints a stream of log entries. Here's what they mean:

### NEW

The fuzzer found an input that reached a **code path never seen before**. Under
the hood, the compiler inserts coverage counters at every branch point in the
code. When a mutated input triggers a branch that no previous input has hit, the
fuzzer saves it to the corpus for further mutation. You'll see many of these
early in a run as the fuzzer rapidly discovers new paths. They taper off over
time as coverage saturates.

### REDUCE

The fuzzer found a **shorter input that reaches the same code path** as an
existing corpus entry. It replaces the longer entry with the shorter one. Shorter
inputs are better because they run faster and produce more focused mutations.
This is why corpus files tend to shrink over time -- the fuzzer is minimizing
them while preserving the same coverage.

### DONE

A corpus entry has been **fully explored** -- the fuzzer tried many mutations of
it without discovering any new coverage. It deprioritizes that entry and focuses
on others. The entry isn't permanently ignored; it just gets less attention in
the mutation queue.

### Typical session lifecycle

1. **Early**: many NEW entries as the fuzzer discovers paths from the seed corpus
2. **Middle**: a mix of NEW and REDUCE as it refines inputs and finds deeper paths
3. **Late**: mostly REDUCE and DONE as coverage plateaus

When you see mostly DONE entries, the fuzzer is getting diminishing returns.
At that point you can either let it run longer (it may still find something),
add new fuzz targets to exercise different code, or try structure-aware fuzzing
to generate syntactically valid inputs that get past early parser rejection.

### Summary line

At the end (or when you Ctrl-C), you'll see a line like:

```
Done 17872 runs in 309 second(s)
```

This is the total number of inputs tested. Higher throughput means the fuzzer
is exploring more mutations per second. The tokenizer target will be faster
than the parser since it does less work per input.

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

### compile

Parses the input, then runs it through the full compilation pipeline: AST to
opcode translation and VM execution. Inputs that fail to parse are skipped so
the fuzzer concentrates on syntactically valid programs that exercise the
compiler. The environment is sandboxed -- stdout/stderr go to a sink, no import
paths are configured, and no filesystem access occurs. This target is slower
than the others (it does the most work per input) but finds the deepest bugs:
panics in the VM, stack overflows from deeply nested expressions, infinite
loops, and out-of-memory conditions from unbounded allocation.

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

### The fuzzer ran for a while with no crashes. Is it working?

Yes! No crashes is a good result -- it means the code handles malformed input
gracefully. The fuzzer is still doing useful work: building up the corpus with
inputs that reach new code paths. Future runs start from this richer corpus and
can find deeper bugs. Even a run that finds nothing is making the next run more
effective.

### How long should I run the fuzzer?

A 5-minute run (`FUZZ_DURATION=300`) is a good quick check after code changes.
For more thorough testing, 30-60 minutes with multiple jobs
(`FUZZ_JOBS=4 FUZZ_DURATION=3600`) will explore significantly more of the input
space. There's no upper limit -- overnight runs on CI are common for projects
that want high assurance.

### What happens when the fuzzer finds a crash?

It prints the crash details to the terminal and saves the triggering input to
`fuzz/artifacts/<target>/`. The file is a raw input that reproduces the crash.
You can replay it to see the error:

```bash
nix develop .#fuzz -c cargo fuzz run parse fuzz/artifacts/parse/crash-<hash>
```

From there, write a regression test using that input, fix the bug, and verify
the fuzzer no longer crashes on it.
