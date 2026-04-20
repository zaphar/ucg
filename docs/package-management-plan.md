# UCG Package Management -- Implementation Plan

This document describes the phased implementation plan for the package management
feature specified in [package-management-spec.md](./package-management-spec.md).

## Crate Dependencies to Add

The following crates will be needed:

- `semver` -- semver parsing, comparison, and range matching
- `sha2` -- SHA-256 hashing of vendored directory contents
- `toml` -- already in Cargo.toml (used for output conversion), reuse for ucg-deps/ucg.lock parsing
- `serde` -- already in Cargo.toml, needed for TOML deserialization
- `serde_derive` -- derive macros for Deserialize/Serialize on dep file structs
- `url` -- URL parsing for normalization (scheme extraction, host lowercasing, etc.)

For fetching repositories during `ucg dep` commands, we shell out to `git` and `hg`
rather than linking libgit2. This keeps the dependency surface small and matches what
Nix's `fetchgit` does. The compiler itself never fetches -- only the `ucg dep`
subcommands do.

For Nix hash computation, we shell out to `nix hash path` rather than reimplementing
NAR hashing. This is only needed when `nix = true` and `nix` is available.

## Module Layout

New code lives in `src/dep/`:

```
src/dep/
  mod.rs          -- public API, re-exports, resolve_import_path()
  manifest.rs     -- ucg-deps parsing and serialization (Manifest, DepEntry)
  lockfile.rs     -- ucg.lock parsing, serialization, staleness detection
  resolve.rs      -- MVS resolver, version constraints, fixed-point resolution
  vendor.rs       -- vendoring logic (fetch, hash, write to vendor dir)
  nix.rs          -- ucg-deps.nix generation (gated on nix availability)
  registry.rs     -- interface for listing tags from git/hg repos
  url.rs          -- URL normalization, identity key derivation, port stripping
  error.rs        -- dep-specific error types
```

The `src/dep/` module is a library component (`ucglib`), but the CLI commands that
trigger network access live in `src/main.rs` alongside the existing subcommands.

## Phase 1: Data Structures and Parsing

**Goal**: Parse `ucg-deps` and `ucg.lock` files, represent them as Rust types.
Implement URL normalization.

### Tasks

1. **Define `Manifest` type** (`manifest.rs`)
   ```rust
   #[derive(Deserialize, Serialize)]
   struct Manifest {
       package: PackageInfo,
       deps: BTreeMap<String, DepEntry>,  // key = repository URL
   }

   struct PackageInfo {
       vendor: Option<String>,       // default "vendor"
       nix: Option<bool>,            // default false
   }

   struct DepEntry {
       version: String,              // ">= 1.2.3" or ">= 1.2.3, < 2.0.0"
       #[serde(rename = "type")]
       repo_type: String,            // "git" or "hg", always explicit
   }
   ```

   Validation:
   - `vendor` must not be `"std"` (reserved name, parse-time error)
   - Duplicate normalized URLs are a parse-time error

2. **Define `Lockfile` type** (`lockfile.rs`)
   ```rust
   #[derive(Deserialize, Serialize)]
   struct Lockfile {
       package: Vec<LockedPackage>,   // sorted by normalized URL
   }

   struct LockedPackage {
       git: Option<String>,          // fetch URL (original, for cloning)
       hg: Option<String>,
       version: String,
       commit: String,
       sha256: String,
   }
   ```

3. **URL normalization** (`url.rs`)
   - Parse URL into components (scheme, user, host, path)
   - Handle SSH syntax: `git@host:path` -> host + path
   - Handle HTTPS/SSH scheme URLs normally
   - Strip port numbers entirely
   - Produce normalized identity key: lowercase host + path, no scheme, no
     trailing `.git`, no trailing slashes, no ports
   - Implement fetch URL preference: SSH > HTTPS, root package breaks ties
   - Validate: duplicate normalized URLs in a single manifest are a parse error
   - Validate: conflicting repo types for same normalized URL across dep graph

4. **Parse version constraints** (`resolve.rs`)
   - Parse `">= 1.2.3"` into `VersionConstraint::Min(semver::Version)`
   - Parse `">= 1.2.3, < 2.0.0"` into `VersionConstraint::Range(min, max)`
   - Implement `satisfies(&self, version: &semver::Version) -> bool`

5. **Package root detection** (`mod.rs`)
   - Utility function that walks up from a path looking for `ucg-deps`
   - Returns `Option<PathBuf>` -- `None` means no package management, use
     current behavior

### Tests

- Round-trip serialization of Manifest and Lockfile
- URL normalization (SSH, HTTPS, with/without .git, trailing slashes, mixed case,
  ports stripped, SSH colon syntax)
- Duplicate URL detection after normalization
- `vendor = "std"` rejected at parse time
- Repo type conflict detection
- Version constraint parsing (valid and invalid inputs)
- Package root detection with nested ucg-deps files
- Package root detection with no ucg-deps (returns None)
- Lockfile entries sorted by normalized URL

### Integration Points

None yet -- this phase is self-contained.

## Phase 2: Version Resolution (MVS)

**Goal**: Given a set of manifests, resolve the full dependency graph to concrete
versions.

### Tasks

1. **Tag listing interface** (`registry.rs`)
   ```rust
   trait TagSource {
       fn list_semver_tags(&self, repo_url: &str) -> Result<Vec<semver::Version>, Error>;
   }
   ```
   - `GitTagSource` -- runs `git ls-remote --tags <url>`, filters to `v*.*.*`,
     excludes pre-release tags, requires `v` prefix
   - `HgTagSource` -- runs `hg tags` or equivalent, same filtering
   - `MockTagSource` -- for testing, returns preconfigured tag lists

2. **MVS resolver** (`resolve.rs`)
   - Input: root `Manifest` + a `TagSource` + a way to read transitive manifests
   - Algorithm:
     1. Collect constraints from root manifest
     2. For each dep (keyed by normalized URL), pick minimum satisfying version
     3. Fetch/read transitive dep's `ucg-deps` at that version
     4. If dep has no `ucg-deps`, treat as leaf node (zero transitive deps)
     5. Merge new constraints, re-resolve
     6. Repeat until stable (fixed-point)
   - Output: `Vec<ResolvedDep>` with normalized URL, fetch URL, resolved version,
     commit hash
   - Error on: no semver tags, empty constraint intersection, cycles,
     conflicting repo types

3. **Constraint intersection** -- given multiple constraints on the same normalized
   URL, compute the intersection and find the minimum version within it.

4. **Circular dependency detection** -- maintain a visited set of normalized URLs
   during graph traversal. Error with the full chain when a cycle is detected.

### Tests

- Diamond dependency resolution (A depends on B and C, both depend on D at different
  minimum versions)
- Conflict detection (incompatible upper bounds)
- No-tags error
- Pre-release-only tags error (same as no-tags)
- Tags without `v` prefix are ignored
- Cycle detection with full chain in error message
- Leaf node dep (no ucg-deps file)
- Single-dep trivial case
- Same dep referenced by different URL forms (SSH vs HTTPS) resolves to one entry
- Conflicting repo types across graph produces error
- All tests use `MockTagSource`

### Integration Points

The resolver needs to read `ucg-deps` from fetched/vendored dependencies. In phase 2
we can test with in-memory manifests. Phase 3 connects it to real repos.

## Phase 3: Repository Fetching and Vendoring

**Goal**: Fetch repositories at resolved versions, hash them, write to vendor dir.

### Tasks

1. **Git/Hg fetch** (`vendor.rs`)
   - Shallow clone: `git clone --depth 1 --branch <tag>` to a temp directory
   - Remove `.git`/`.hg` directory entirely (vendored deps are pure source,
     suitable for committing)
   - Skip symlinks during copy (do not vendor symlinks)
   - Do not recurse into git submodules
   - Compute SHA-256 of directory contents (deterministic walk: sorted entries,
     hash file paths + contents)
   - Copy to `<vendor>/<normalized-url-path>/`

2. **SHA-256 hashing** (`vendor.rs`)
   - Deterministic directory hash: walk entries in sorted order, feed
     `<relative-path>\0<file-contents>` into a rolling SHA-256
   - Symlinks are skipped (not included in hash)
   - Must match what we'd verify later, and be documented so the scheme is
     reproducible

3. **Lockfile writing** (`lockfile.rs`)
   - After resolution + fetch, write `ucg.lock` with all transitive deps
   - Include fetch URL, version, commit, sha256
   - Entries sorted by normalized URL

4. **Vendor directory management** (`vendor.rs`)
   - Clean stale deps (present in vendor but not in lockfile)
   - Skip re-fetching deps where lockfile commit + sha256 match existing vendor
   - Ignore nested `vendor/` directories in vendored deps
   - Warn if vendor directory exists but no `ucg.lock` (pre-existing directory)

5. **Hash verification** (`vendor.rs`)
   - On `ucg dep vendor`, after fetching, verify sha256 matches lockfile
   - Error with actionable message on mismatch (suggest `ucg dep lock`)

6. **Staleness detection** (`lockfile.rs`)
   - Compare `ucg-deps` entries against `ucg.lock` entries
   - Check: every dep URL in `ucg-deps` has a lockfile entry
   - Check: locked versions still satisfy constraints in `ucg-deps`
   - Error with actionable message when stale

7. **Staged writes** (`vendor.rs`)
   - All network operations and resolution complete to temp directories first
   - Only after all succeed, write `ucg-deps`, `ucg.lock`, vendor, `ucg-deps.nix`
   - On failure, no files are modified

### Tests

- Vendor a mock repo (use local git repos in temp dirs for integration tests)
- SHA-256 hash stability (same content always produces same hash)
- Symlinks in source repo are skipped
- `.git` directory is removed from vendored output
- Stale dep cleanup
- Skip-if-unchanged optimization
- Hash mismatch detection
- Staleness detection (added dep, removed dep, changed constraint)
- Vendor path derived from normalized URL (not original)
- Pre-existing vendor directory warning
- Staged writes: failure leaves no partial state

### Integration Points

This phase shells out to `git`/`hg`. Integration tests should create local bare
repos with semver tags to avoid network dependency.

## Phase 4: CLI Commands

**Goal**: Wire everything into `ucg dep` subcommands.

### Tasks

1. **Add `dep` subcommand to clap** (`src/main.rs`)
   ```
   ucg dep init [--vendor "vendor"] [--nix]
   ucg dep add <url> [--version "..."] [--type "git"|"hg"]
   ucg dep remove <url>
   ucg dep lock
   ucg dep vendor
   ucg dep nix [--stdout]
   ```

2. **`ucg dep init`**
   - Error if `ucg-deps` already exists
   - Create `ucg-deps` with `[package]` (all defaults) and empty `[deps]`
   - No required flags

3. **`ucg dep add`**
   - Error if `ucg-deps` does not exist
   - Normalize URL, check for duplicates
   - If URL already exists: update constraint, print informational message
     ("updating constraint for github.com/org/lib from '>= 1.0.0' to '>= 2.0.0'")
   - If `--version` not specified, query tags and default to `">= <latest>"`
   - `--type` defaults to `"git"`, always written explicitly
   - Run full resolve -> lock -> vendor pipeline (staged)
   - If `nix = true` and `nix` available, write `ucg-deps.nix`
   - If `nix = true` and `nix` not available, warn

4. **`ucg dep remove`**
   - Error if `ucg-deps` does not exist
   - Normalize URL, find entry, error if not found
   - Remove from `ucg-deps`
   - Re-resolve full graph
   - If removed dep still present transitively, print informational message
   - Update lock + vendor (staged)
   - If `nix = true` and `nix` available, update `ucg-deps.nix`
   - If `nix = true` and `nix` not available, warn

5. **`ucg dep lock`**
   - Error if `ucg-deps` does not exist
   - Re-resolve everything from `ucg-deps`
   - Write `ucg.lock` (and `ucg-deps.nix` if `nix = true` and `nix` available)
   - If `nix = true` and `nix` not available, warn
   - Does NOT vendor

6. **`ucg dep vendor`**
   - Error if `ucg-deps` or `ucg.lock` does not exist
   - Error if lockfile is stale
   - Warn if vendor dir exists but no lockfile (pre-existing dir)
   - Read `ucg.lock`, fetch into vendor dir
   - Verify sha256 hashes

7. **`ucg dep nix`**
   - Error if `nix` not available on PATH
   - Read `ucg.lock`, generate `ucg-deps.nix`
   - `--stdout` flag for piping

### Tests

- CLI integration tests using local git repos
- `ucg dep init` creates valid ucg-deps with defaults
- `ucg dep init` errors if ucg-deps already exists
- `ucg dep add` errors if ucg-deps does not exist
- `ucg dep add` with existing URL updates constraint
- `ucg dep add` + vendor end-to-end
- `ucg dep remove` cleans lockfile and vendor
- `ucg dep remove` notes transitive dep retained
- `ucg dep vendor` errors on stale lockfile
- `ucg dep vendor` warns on pre-existing vendor dir
- `ucg dep nix` errors without nix on PATH
- `ucg dep nix` output is valid Nix syntax
- Nix warning emitted on lock-modifying ops when nix unavailable

## Phase 5: Nix Expression Generation

**Goal**: Generate `ucg-deps.nix` from lockfile.

### Tasks

1. **Nix tool detection** (`nix.rs`)
   - Check if `nix` is available on PATH
   - Compute NAR hashes via `nix hash path <dir>` for each vendored dep
   - Return error or warning based on context (explicit `ucg dep nix` = error,
     auto-generation = warning)

2. **Nix template generation** (`nix.rs`)
   - Read `Lockfile`
   - Emit Nix expression with `fetchgit`/`fetchhg` per dep
   - Assembly derivation using `runCommand`
   - Derive Nix identifiers from normalized URL path: replace dots and slashes
     with underscores (e.g. `github.com/org/shared-lib` ->
     `github_com_org_shared-lib`)
   - Entries sorted by normalized URL for deterministic output
   - Use NAR hashes (from `nix hash path`), NOT the SHA-256 from `ucg.lock`

3. **Output format**
   ```nix
   { fetchgit, runCommand }:
   let
     github_com_org_shared-lib = fetchgit {
       url = "git@github.com:org/shared-lib.git";
       rev = "abc123...";
       hash = "sha256-...";
     };
   in
   runCommand "ucg-vendor" {} ''
     mkdir -p $out/github.com/org/shared-lib
     cp -r ${github_com_org_shared-lib}/. $out/github.com/org/shared-lib/
   ''
   ```

### Tests

- Snapshot test of generated Nix output
- Nix identifier derivation from various URL paths
- Nix syntax validity (parse with `nix-instantiate --parse` if available)
- hg deps use `fetchhg` instead of `fetchgit`
- NAR hash computation via `nix hash path`

## Phase 6: Import Resolution Integration

**Goal**: Teach the compiler to resolve `vendor/` prefixed imports via the vendor
directory.

### Key Files to Modify

1. **`src/ast/rewrite.rs`** -- import path rewriting
   - Recognize `vendor/` prefix at the same level as `std/`
   - Do NOT convert `vendor/` paths to absolute by joining with the base directory
   - Instead, resolve as `<package_root>/<configured-vendor-dir>/<rest-after-vendor/>`
   - When no `ucg-deps` exists (no package root), `vendor/` has no special meaning

2. **`src/build/opcode/environment.rs`** -- `get_ops_for_path`
   - Handle paths that were resolved from `vendor/` imports
   - Package root threaded through as session-level state

3. **`src/build/opcode/runtime.rs`** -- `import` function
   - Same resolution logic for `vendor/` prefix
   - The path pushed onto the stack must be the resolved absolute path so caching
     works correctly (cache key = absolute filesystem path)

4. **`src/ast/typecheck/mod.rs`** -- `resolve_import`
   - Mirror the runtime resolution for `vendor/` prefix
   - Needs access to the package root (session-level state)

5. **`src/build/mod.rs`** -- `link_ops`
   - Transitive dependency loading already works (follows links recursively)
   - May need to apply the same path resolution when processing link paths

6. **`src/main.rs`** -- `visit_ucg_files`
   - When recursing directories, skip the vendor directory
   - Detect vendor path from `ucg-deps` if present, else default `"vendor"`
   - Determine package root once at startup, thread through build session

### Resolution Logic (shared function)

```rust
/// Resolve an import path to an absolute filesystem path.
///
/// Resolution order:
/// 1. Relative paths (./ ../) resolve against `importing_dir`
/// 2. `std/` prefix resolves to embedded stdlib (never vendor)
/// 3. `vendor/` prefix (when package_root is Some):
///    strip prefix, resolve as `<package_root>/<vendor_dir>/<rest>`
/// 4. All other paths resolve relative to `importing_dir` (current behavior)
///
/// When package_root is None (no ucg-deps), only rules 1, 2, 4 apply.
/// `vendor/` has no special meaning without a package root.
fn resolve_import_path(
    import_path: &str,
    importing_dir: &Path,
    package_root: Option<&Path>,
    vendor_dir: &str,
) -> PathBuf
```

This function lives in `src/dep/mod.rs` and is called from the AST rewriter, the
type checker, and the runtime.

### Package Root as Session State

The package root is determined **once** at the start of compilation (walk up from
the entry file or CWD to find `ucg-deps`). It is threaded through the entire build
session as immutable state. All `vendor/` resolution uses this single root,
including for files inside the vendor directory.

This replaces per-file package root detection and ensures vendored deps' `ucg-deps`
files are never used as package roots during compilation.

Cache the package root lookup in a `HashMap<PathBuf, Option<PathBuf>>` shared across
the build session (similar to the existing `shape_cache` and `val_cache` patterns).

### Import Caching

Ensure the import cache (both opcode cache and value cache) keys on the final
resolved absolute filesystem path, not the raw import string. This prevents
duplicate compilation when the same file is reached via different import paths.

### No `ucg-deps` Fallback

When no `ucg-deps` is found in any ancestor directory, `package_root` is `None` and
the compiler behaves exactly as it does today. The `vendor/` prefix has no special
meaning and is treated as a bare path. This ensures full backwards compatibility
for projects that do not use package management.

### Tests

- Compile a file that imports a vendored dependency via `vendor/` prefix
- Type checking resolves vendored import shapes
- Relative imports still work unchanged
- Bare imports (no prefix) still work unchanged
- `std/` imports resolve to stdlib, not vendor
- `vendor/` prefix resolves against package root's configured vendor dir
- Vendor directory is skipped during recursive builds/tests
- Missing vendor directory produces clear error
- No `ucg-deps` file: `vendor/` prefix treated as bare path (backwards compat)
- Monorepo: files in different packages resolve against their own package roots
- Vendored dep importing another vendored dep resolves against root's vendor
- Import caching: same file via relative and vendor/ prefix shares cache entry
- AST rewriter does not mangle `vendor/` paths

## Phase 7: Polish and Documentation

### Tasks

1. **Error messages** -- review all error paths for clarity. Include:
   - Which constraint came from which package (use normalized URL)
   - Full paths when files are missing
   - Suggestions (e.g., "run `ucg dep vendor` to fetch dependencies")
   - Actionable hash mismatch message
   - Actionable stale lockfile message
   - Nix unavailability warnings on every lock-modifying operation

2. **Documentation updates**
   - Update UCG website/docs with package management guide
   - Document the `vendor/` import prefix in import expression docs
   - Document `ucg dep remove` version shift behavior
   - Document known limitations (submodules, symlinks, ports, pre-release)
   - Add examples to `integration_tests/`
   - Update `CLAUDE.md` with new commands and module layout

3. **`ucg dep` help text** -- inline help for all subcommands

4. **Edge cases**
   - Empty `[deps]` table
   - `ucg-deps` with no `vendor` field (use default)
   - `vendor = "std"` rejected at parse time
   - Vendored dep has its own `vendor/` directory (must be ignored)
   - Monorepo with multiple `ucg-deps` files at different levels
   - Vendor path outside package root (allowed, for monorepos)
   - Dep with no `ucg-deps` (leaf node, zero transitive deps)
   - Pre-existing vendor directory warning
   - Removed dep retained as transitive dep

## Dependency Between Phases

```
Phase 1 (Data Structures + URL Normalization)
  │
  ├──> Phase 2 (MVS Resolver)
  │      │
  │      ├──> Phase 3 (Fetching/Vendoring)
  │      │      │
  │      │      └──> Phase 4 (CLI Commands)
  │      │
  │      └──> Phase 5 (Nix Generation)
  │
  └──> Phase 6 (Import Resolution)
             │
             └──> Phase 7 (Polish)
```

Phases 5 and 6 can proceed in parallel once their prerequisites are met. Phase 4
depends on Phase 3 for the full pipeline but individual subcommands (like `ucg dep
init` and `ucg dep lock`) could be wired up after Phase 2.

## Estimated Complexity

| Phase | New files | Modified files | Rough scope |
|-------|-----------|----------------|-------------|
| 1     | 5-6       | 1 (Cargo.toml) | Data types, URL normalization, parsing, tests |
| 2     | 1-2       | 0              | Core algorithm, extensive tests |
| 3     | 1-2       | 0              | Shell-out to git/hg, hashing, staged writes |
| 4     | 0         | 1 (main.rs)    | CLI wiring, subcommand dispatch |
| 5     | 1         | 0              | Template generation, nix tool detection |
| 6     | 0-1       | 5              | Import resolution, AST rewriter changes |
| 7     | 0         | several         | Error messages, docs, edge cases |
