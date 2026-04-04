# Holistic Code Review — UCG

## Package Manager (`src/dep/`) — Detailed Review

### Security Findings

**1. Command Injection Surface — MEDIUM**
`src/dep/registry.rs:70-83, 109-144, 151-165, 179-205`

All `Command::new("git")` and `Command::new("hg")` calls pass user-supplied URLs and tags via `.args()`, which is safe against shell injection (no shell expansion). However, the `repo_url` value flows directly from the manifest file, which is user-authored. A malicious `ucg-deps` manifest with a crafted URL could trigger git's own URL-handler exploits (e.g., `--upload-pack` tricks via URL). The `validate_remote_url()` function rejects `file://`, absolute paths, and relative paths, but does not reject URLs containing `--` prefixes that could be interpreted as flags.

**Recommendation:** Add a check that URLs don't start with `-` in `validate_remote_url()`, or insert `--` before the URL argument in all `Command` calls (already done implicitly for `git clone ... url` positional args, but `git ls-remote --tags <url>` could be exploited if `url` starts with `-`).

**2. Nix Expression Injection — LOW**
`src/dep/nix.rs:99-103`

`render_nix_expression()` interpolates `fetch_url`, `commit`, and `hash` directly into Nix string literals using `format!("    url = \"{}\";\n", ...)`. A URL containing `";` or `${}` could break out of the Nix string. Since the URL comes from the user's own manifest and the Nix expression is local, this is low-severity but still a correctness issue.

**Recommendation:** Escape `"`, `\`, and `${` in string values before interpolating into the Nix expression.

### Design & Correctness Findings

**3. Transitive Resolution Uses Placeholder Version — MEDIUM**
`src/dep/resolve.rs:193-200`

`collect_transitive()` creates a `ResolvedDep` with `version: Version::new(0, 0, 0)` as a placeholder to recurse into transitive deps. This placeholder version is passed to `manifest_source.get_manifest()`. If the manifest source implementation is version-sensitive (e.g., fetches a manifest at a specific tag), this will fetch the wrong manifest. The comment acknowledges this ("version doesn't matter for manifest lookup in mock") but it's a latent bug for any real `ManifestSource`.

**Recommendation:** Either pass the resolved version (from the `resolved` map in the outer loop) or redesign `collect_transitive` to receive the version separately.

**4. Fixed-Point Loop Has No Divergence Detection — LOW**
`src/dep/resolve.rs:72-142`

The loop runs up to 100 iterations but silently returns whatever state it reached if it doesn't converge. There's no error if `max_iterations` is hit. For pathological dependency graphs (e.g., version oscillation), this could return an incomplete or incorrect resolution.

**Recommendation:** After the loop, check if it exited via `break` (converged) vs. exhausting iterations, and return an error for non-convergence.

**5. `lockfile.is_stale()` Doesn't Detect Extra Lockfile Entries — LOW**
`src/dep/lockfile.rs:44-82`

The staleness check only verifies that every manifest dep has a matching lockfile entry. It does not check for lockfile entries that are *no longer* in the manifest (stale entries from removed deps). These would remain until the next `ucg dep lock`.

**6. Hash Doesn't Include File Sizes — LOW**
`src/dep/vendor.rs:38-90`

`hash_directory()` hashes `path + \0 + contents` for each file. While functionally correct, including file size in the hash input (before contents) would provide a more robust content commitment and make length-extension attacks on individual files harder. This is a minor hardening point.

### Code Quality Findings

**7. `LockedPackage::url()` Returns Empty String for Invalid State — MEDIUM**
`src/dep/lockfile.rs:87-89`

```rust
pub fn url(&self) -> &str {
    self.git.as_deref().or(self.hg.as_deref()).unwrap_or("")
}
```

If neither `git` nor `hg` is set, this silently returns `""` instead of indicating an error. Callers like `normalize_url("")` would produce garbage. This should either be a `Result`, or the struct should enforce the invariant at parse time.

**8. `vendor_from_lockfile()` Has Unused Parameter — LOW**
`src/dep/vendor.rs:244`

```rust
let _ = manifest; // used for future extensions
```

Dead parameter with a comment promising future use. If it's not needed now, remove it.

---

## Compiler Pipeline Findings

**9. Unwrap on File Parent — HIGH (panic risk)**
`src/build/mod.rs:143`

```rust
self.working_dir = file.parent().unwrap().to_path_buf();
```

If `build()` is called with a bare filename (no directory component), `.parent()` returns `Some("")` not `None`, so this won't actually panic in practice. However, the `eval_ops` unwrap at line 204 is more concerning:

```rust
if path.is_some() {
    vm.set_path(path.unwrap());
}
```

This is a classic `if let` pattern that should be `if let Some(p) = path { vm.set_path(p); }`.

**10. WalkDir Doesn't Prune `.git`/`.hg` Directories — LOW**
`src/dep/vendor.rs:52-58`

The directory hash function skips `.git`/`.hg` entries but still *walks into* them (WalkDir doesn't support pruning in its basic iterator). For large repos this wastes I/O. Using `filter_entry()` instead of `filter_map()` on the iterator would prune the walk.

**11. `hg identify --tags -r all` May Not Be Correct — MEDIUM**
`src/dep/registry.rs:153`

The `-r all` flag for `hg identify --tags` is non-standard. The standard way to list all tags in Mercurial is `hg tags`. `hg identify --tags` shows tags for the working directory parent. This may silently return incomplete results for some Hg repos.

---

## Test Coverage Gaps

**12. No Unit Tests for Opcode VM** — The 4,127-line VM (`src/build/opcode/vm.rs`) is only tested through integration tests. A bug in opcode execution could be hard to isolate.

**13. No Parser-Specific Unit Tests** — The parser (`src/parse/mod.rs`, 1,056 lines) relies entirely on integration-level testing. Edge cases in operator precedence, error recovery, and position tracking would benefit from targeted tests.

---

## Summary

| # | Area | Severity | Type |
|---|------|----------|------|
| 1 | Command arg injection surface | Medium | Security |
| 2 | Nix string interpolation | Low | Security |
| 3 | Transitive resolve placeholder version | Medium | Correctness |
| 4 | No divergence detection in MVS loop | Low | Correctness |
| 5 | Staleness check misses extra lockfile entries | Low | Correctness |
| 6 | Hash doesn't include file sizes | Low | Hardening |
| 7 | `url()` returns empty string | Medium | Correctness |
| 8 | Unused manifest parameter | Low | Cleanup |
| 9 | Unwrap patterns in build | Low | Code quality |
| 10 | WalkDir doesn't prune VCS dirs | Low | Performance |
| 11 | `hg identify --tags -r all` correctness | Medium | Correctness |
| 12 | No VM unit tests | — | Coverage |
| 13 | No parser unit tests | — | Coverage |

The package manager code is well-structured overall — good trait-based abstraction for testability, proper staged writes, deterministic hashing and sorting, and solid URL normalization. The most actionable items are **#1** (flag-injection defense), **#3** (placeholder version bug), and **#7** (silent empty URL).
