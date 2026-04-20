# UCG Package Management Specification

## Overview

UCG package management provides dependency resolution, vendoring, and Nix-compatible
reproducible builds for UCG projects. It uses **minimum version selection** (MVS) over
strict semver to resolve dependency graphs, vendors all dependencies on disk, and
optionally generates Nix expressions for fully reproducible builds.

## Design Principles

1. **Offline compilation** -- the compiler requires all dependencies on disk. Network
   access only happens during explicit `ucg dep` commands.
2. **Minimum version selection** -- when multiple constraints exist for a dependency,
   the resolver picks the lowest version that satisfies all of them. This is
   deterministic without a lockfile and produces the most conservative upgrade behavior.
3. **Strict semver** -- all dependency versions must be semver tags of the form
   `vMAJOR.MINOR.PATCH` in the source repository. Version constraints must include
   a `>=` lower bound, and the resolved version is derived directly from those bounds
   (no remote tag listing is performed during resolution).
4. **Nix-friendly vendoring** -- the lockfile contains sha256 hashes of fetched
   dependency trees, and a Nix expression can be auto-generated to reproduce the
   vendor directory using `fetchgit`/`fetchhg` fixed-output derivations.
5. **Single vendor directory** -- each package has one flat vendor directory. Nested
   vendor directories in dependencies are ignored.
6. **Staged writes** -- all network operations and resolution happen before any disk
   writes. If any step fails (network error, resolution conflict, etc.), no files are
   modified. Disk writes (`ucg-deps`, `ucg.lock`, vendor directory, `ucg-deps.nix`)
   only happen as a final step after everything succeeds.

## Package Files

### `ucg-deps` (Dependency File)

TOML format. Declares the package configuration and its direct dependencies.

```toml
[package]
vendor = "vendor"       # relative path to vendor directory, default "vendor"
nix = true              # auto-generate ucg-deps.nix alongside ucg.lock

[deps]
"https://github.com/org/shared-lib.git" = { version = ">= 1.2.3", type = "git" }
"https://github.com/org/infra-config.git" = { version = ">= 2.0.0, < 3.0.0", type = "git" }
"https://hg.example.com/internal-cfg" = { version = ">= 0.5.0", type = "hg" }
```

#### `[package]` fields

| Field    | Required | Default    | Description                                    |
|----------|----------|------------|------------------------------------------------|
| `vendor` | no       | `"vendor"` | Relative path to vendor directory              |
| `nix`    | no       | `false`    | Auto-generate `ucg-deps.nix` on lock operations |

The `vendor` field must not be set to `"std"` -- this is a reserved name and will
produce a parse-time error. The vendor path may resolve to outside the package root
(useful for monorepos with a shared vendor directory).

The configured vendor directory is fully managed by the package manager. Users should
not place manually-managed files in it. All contents may be removed and recreated by
`ucg dep vendor`.

#### `[deps]` entries

Each entry is keyed by the repository URL and has the following fields:

| Field     | Required | Default | Description                          |
|-----------|----------|---------|--------------------------------------|
| `version` | yes      | --      | Semver version constraint            |
| `type`    | yes      | --      | Repository type: `"git"` or `"hg"`  |

The `type` field is always written explicitly for consistency, even though `ucg dep
add` defaults to `"git"` when the `--type` flag is omitted.

Duplicate repository URLs (after normalization -- see URL Normalization below) within
a single `ucg-deps` file are a parse-time error.

If two packages in the dependency graph declare the same normalized URL with different
`type` values, this is a resolution error: "conflicting repository types for
github.com/org/lib: git from root, hg from github.com/org/other".

#### Version Constraints

Version constraints use standard semver operators, parsed by the `semver` crate:

- **Minimum**: `">= 1.2.3"` -- any version >= 1.2.3
- **Upper bound**: `"< 2.0.0"` -- any version < 2.0.0
- **Caret (compatible)**: `"^1.2.3"` -- same as `>= 1.2.3, < 2.0.0` (allows
  changes that don't modify the leftmost non-zero digit)
- **Tilde (patch-level)**: `"~1.2.3"` -- same as `>= 1.2.3, < 1.3.0` (only
  patch version may vary)
- **Combined**: `">= 1.2.3, < 2.0.0"` -- comma-separated constraints are
  intersected

The resolver extracts the minimum version directly from constraint lower bounds
(the `>=` bound). It does not query remote repositories for available tags during
resolution. The version specified in the `>=` constraint must exist as a `v`-prefixed
tag in the repository (e.g. `>= 1.2.3` expects tag `v1.2.3` to exist). If the tag
does not exist, the fetch step will fail with an error.

When multiple constraints apply to the same dependency, the highest lower bound is
selected (e.g. `>= 1.0.0` and `>= 1.2.0` resolves to `1.2.0`). The selected version
must also satisfy any upper bounds (`<` constraints).

### `ucg.lock` (Lock File)

TOML format. Records the resolved version, commit hash, and content hash for every
transitive dependency. Entries are sorted by normalized URL for deterministic output.

```toml
[[package]]
git = "git@github.com:org/shared-lib.git"
version = "1.2.3"
commit = "abc123def456789..."
sha256 = "sha256-..."

[[package]]
git = "https://github.com/org/infra-config.git"
version = "2.1.0"
commit = "789fedcba..."
sha256 = "sha256-..."

[[package]]
git = "https://github.com/org/base-types.git"
version = "1.0.0"
commit = "deadbeef123..."
sha256 = "sha256-..."
```

#### Fields per `[[package]]`

| Field     | Description                                                      |
|-----------|------------------------------------------------------------------|
| `git`/`hg`| Source repository URL (used for fetching)                        |
| `version` | Resolved semver version                                         |
| `commit`  | Full commit hash for the resolved version tag                   |
| `sha256`  | SHA-256 hash of the fetched directory contents                  |

The fetch URL stored in the lockfile is selected according to the URL preference rules
described in URL Normalization below. The sha256 is computed over the fetched directory
contents using a deterministic hashing scheme (see Vendoring). This hash is used for
integrity verification during `ucg dep vendor` and is independent of Nix's NAR hash.

### `ucg-deps.nix` (Nix Expression)

Auto-generated when `nix = true` in `ucg-deps` and `nix` is available on PATH, or
manually via `ucg dep nix`. Fixed filename, not configurable.

Generation is gated on two conditions:
1. `nix = true` in `ucg-deps`
2. The `nix` command is available on PATH

If `nix = true` but `nix` is not available, any operation that modifies `ucg.lock`
will warn: "warning: nix = true but nix is not available. ucg-deps.nix may be out of
date. A contributor with nix should run `ucg dep nix` to update it."

This warning is emitted on every lock-modifying operation (add, remove, lock) to
ensure visibility, since a stale `ucg-deps.nix` could break nix builds.

```nix
{ fetchgit, runCommand }:
let
  github_com_org_shared-lib = fetchgit {
    url = "git@github.com:org/shared-lib.git";
    rev = "abc123def456789...";
    hash = "sha256-...";
  };
  github_com_org_infra-config = fetchgit {
    url = "https://github.com/org/infra-config.git";
    rev = "789fedcba...";
    hash = "sha256-...";
  };
  github_com_org_base-types = fetchgit {
    url = "https://github.com/org/base-types.git";
    rev = "deadbeef123...";
    hash = "sha256-...";
  };
in
runCommand "ucg-vendor" {} ''
  mkdir -p $out/github.com/org/shared-lib
  cp -r ${github_com_org_shared-lib}/. $out/github.com/org/shared-lib/

  mkdir -p $out/github.com/org/infra-config
  cp -r ${github_com_org_infra-config}/. $out/github.com/org/infra-config/

  mkdir -p $out/github.com/org/base-types
  cp -r ${github_com_org_base-types}/. $out/github.com/org/base-types/
''
```

The `hash` values in the Nix expression are NAR hashes computed via `nix hash path`
over each fetched dependency tree. These differ from the SHA-256 hashes in `ucg.lock`,
which use a simpler directory-walk scheme. The Nix hashes are only computed when
generating `ucg-deps.nix` and require `nix` to be available.

Entries in the Nix expression are sorted by normalized URL for deterministic output.

#### Nix Identifier Derivation

Nix let-binding identifiers are derived from the normalized URL path (see URL
Normalization) by replacing dots and slashes with underscores. For example,
`github.com/org/shared-lib` becomes `github_com_org_shared-lib`.

The individual `fetchgit`/`fetchhg` calls are fixed-output derivations (allowed
network access by Nix because of the content hash). The final `runCommand` is a pure
derivation that assembles them into the vendor directory layout.

## URL Normalization

Repository URLs are normalized to produce a canonical identity key used for
deduplication, conflict detection, and vendor directory path derivation.

### Normalization Rules

Given a repository URL, the normalized identity key is derived by:

1. **Strip scheme** -- remove `https://`, `http://`, `ssh://`, etc.
2. **Normalize SSH syntax** -- convert `git@github.com:org/lib` to
   `github.com/org/lib` (strip user prefix, replace colon with slash)
3. **Strip trailing `.git`** -- `github.com/org/lib.git` -> `github.com/org/lib`
4. **Strip trailing slashes** -- `github.com/org/lib/` -> `github.com/org/lib`
5. **Lowercase the host** -- `GitHub.com/org/lib` -> `github.com/org/lib`
6. **Strip port numbers** -- `github.com:8443/org/lib` -> `github.com/org/lib`

### Port Number Limitation

Port numbers are stripped entirely during normalization. This means that
`https://git.example.com:8443/org/lib` and `https://git.example.com/org/lib`
are treated as the same dependency. In practice, the same repository path on
the same host at different ports should not occur.

### Examples

All of the following URLs produce the identity key `github.com/org/lib`:

- `https://github.com/org/lib.git`
- `https://github.com/org/lib`
- `git@github.com:org/lib.git`
- `ssh://git@github.com/org/lib`
- `https://github.com:443/org/lib.git`

### Fetch URL Selection

When the same dependency is referenced by multiple URLs across the dependency graph
(e.g. root uses SSH, a transitive dep uses HTTPS), the fetch URL stored in the
lockfile is selected with this preference order:

1. **SSH URL** -- preferred when available (better for most forges)
2. **HTTPS URL** -- used when no SSH URL is available
3. **Root package's URL** -- breaks ties when multiple URLs of the same type exist

The normalized identity key (not the original URL) is used for:
- Deduplication during resolution
- Vendor directory path (`<vendor>/<normalized-key>/`)
- Nix identifier derivation
- Conflict detection

## Vendoring

### Fetch Process

Dependencies are fetched using shallow clones for efficiency:

1. `git clone --depth 1 --branch <tag>` (or hg equivalent) to a temp directory
2. Remove `.git`/`.hg` directory entirely -- vendored deps are pure source with no
   VCS metadata, suitable for committing to the consuming project's repository
3. Remove the dependency's own vendor directory -- if the fetched dependency has a
   `ucg-deps` file, read its `vendor` field (default `"vendor"`) and delete that
   directory from the fetched tree. This avoids vendoring transitive dependencies
   that belong to the dependency's own build and reduces disk/git bloat.
4. Skip symlinks -- symlinks are not copied to the vendor directory. Packages that
   rely on symlinks must fix their layout.
5. Compute SHA-256 hash of the directory contents (after steps 2-4)
6. Copy to `<vendor>/<normalized-url-path>/`

Git submodules are not supported. Submodules are not fetched or vendored. If a
dependency relies on submodules, it cannot be used as a UCG dependency.

### SHA-256 Hashing

The SHA-256 hash in `ucg.lock` is computed using a deterministic directory walk
designed to produce the same hash for the same directory contents regardless of
filesystem ordering or platform:

1. Recursively enumerate all files in the directory (skip symlinks, skip `.git`/`.hg`)
2. Compute relative paths from the directory root for each file
3. Sort the file list lexicographically by relative path (byte-order comparison)
4. For each file in sorted order, feed `<relative-path>\0<file-contents>` into a
   rolling SHA-256 digest

The sort order is critical for reproducibility -- different filesystems may return
directory entries in different orders, so the explicit sort ensures the hash is
platform-independent. Empty directories do not affect the hash (only files contribute).

This hash is used for integrity verification during `ucg dep vendor` and is
independent of Nix's NAR hashing scheme.

### Vendor Directory Layout

Dependencies are namespaced by their normalized repository URL path:

```
vendor/
  github.com/
    org/
      shared-lib/
        ucg-deps
        lib.ucg
      infra-config/
        ucg-deps
        network.ucg
  hg.example.com/
    internal-cfg/
      ucg-deps
      base.ucg
```

Rules:
- The vendor directory path is specified by `vendor` in `ucg-deps` (default `"vendor"`)
- The vendor directory is fully managed by the package manager
- One version of each dependency exists in the vendor directory
- Nested `vendor/` directories inside vendored dependencies are **ignored** by the
  compiler during import resolution
- Each vendored dependency retains its own `ucg-deps` file (used during resolution)
  but its vendor directory is not used
- A dependency with no `ucg-deps` file is treated as a leaf node with zero transitive
  dependencies

### Vendor Directory Warnings

If `ucg dep vendor` is run and the vendor directory already exists but `ucg.lock` does
not, the command warns: "vendor directory '<path>' already exists but no ucg.lock found.
The vendor directory is managed by ucg dep and its contents may be removed. Use a
different path via the `vendor` field in ucg-deps, or remove the existing directory."

This protects users who may have pre-existing files in a directory that happens to
match the configured vendor path.

## Import Resolution

Import path resolution uses explicit prefixes to determine resolution strategy.

### The `vendor/` Import Prefix

Vendored dependencies are imported using the `vendor/` prefix. This is a **resolution
keyword**, not a filesystem path. Regardless of what the `vendor` field in `ucg-deps`
is set to, imports always use `vendor/`:

```ucg
// Resolves to <package_root>/<configured-vendor-dir>/github.com/org/lib/lib.ucg
let lib = import "vendor/github.com/org/lib/lib.ucg";
```

The `vendor/` prefix must be recognized by the AST rewriter at the same level as
`std/` -- it must be intercepted before normal path resolution occurs, otherwise the
rewriter would attempt to resolve it relative to the importing file's directory.

### Resolution Rules

#### When `ucg-deps` exists (package mode)

1. **Relative paths** -- if the import path starts with `./` or `../`, resolve
   relative to the directory of the importing file. This is the existing behavior,
   unchanged.

2. **Standard library** -- if the import path starts with `std/`, resolve to the
   embedded standard library. `std/` imports **never** resolve against the vendor
   directory.

3. **Vendored paths** -- if the import path starts with `vendor/`, strip the prefix
   and resolve as `<configured-vendor-dir>/<rest-of-path>` relative to the
   **package root**.

4. **Bare paths** -- any other path resolves relative to the importing file's
   directory. This is the existing behavior, unchanged. Bare paths like
   `import "shared.ucg"` or `import "modules/unified.ucg"` continue to work as
   they do today.

#### When no `ucg-deps` exists

The compiler behaves exactly as it does today. All imports resolve relative to the
importing file or CWD. The `vendor/` prefix has no special meaning -- it would be
treated as a bare path and resolved relative to the importing file. No vendor
directory resolution is attempted.

This ensures full backwards compatibility for projects that do not use package
management. Projects without `ucg-deps` that use manual vendoring via relative
imports are unaffected. Adding a `ucg-deps` file opts into managed package
resolution.

### Examples

Given this project structure:
```
project/
  ucg-deps
  vendor/
    github.com/org/shared-lib/lib.ucg
  src/
    main.ucg
    util.ucg
```

In `src/main.ucg`:
```ucg
// Relative import -- resolves to project/src/util.ucg
let util = import "./util.ucg";

// Vendored import -- resolves to project/vendor/github.com/org/shared-lib/lib.ucg
let shared = import "vendor/github.com/org/shared-lib/lib.ucg";
```

### Package Root and the Single Root Rule

The package root is determined **once** at the start of compilation by walking up
from the entry file (or CWD) to find the nearest `ucg-deps` file. This root is
threaded through the entire build session.

All `vendor/` import resolution uses this single root, regardless of which file is
being compiled -- including files inside the vendor directory itself. Vendored
dependencies' `ucg-deps` files are only read during dependency resolution (by `ucg dep`
commands), never during compilation. The compiler treats the entire vendor tree as part
of the root project's dependency set.

This means when a vendored dependency imports another vendored dependency using
`import "vendor/github.com/org/other/file.ucg"`, the resolution correctly uses the
root project's vendor directory.

### Import Caching

Import caching must key on the **resolved absolute filesystem path**, not the raw
import string. This ensures that the same file accessed via different import paths
(e.g. a relative import from within the vendor directory vs a `vendor/` prefixed
import from project code) shares a single cache entry and is only compiled once.

### Package Root Detection

The compiler walks up from the file being compiled, looking for a `ucg-deps` file.
The directory containing that file is the package root. This supports monorepos:

```
monorepo/
  ucg-deps              # root package
  vendor/
  service-a/
    config.ucg          # package root = monorepo/
  service-b/
    ucg-deps            # service-b is its own package
    vendor/
    config.ucg          # package root = monorepo/service-b/
```

## Version Resolution Algorithm

UCG uses **Minimum Version Selection** (MVS), as described by Russ Cox for Go modules.

### Tag Requirements

- Resolved versions are fetched as tags of the form `vMAJOR.MINOR.PATCH` (the `v`
  prefix is required). For example, version `1.2.3` is fetched as tag `v1.2.3`.
- All version constraints must have a `>=` lower bound (e.g. `">= 1.0.0"`). The
  resolver does not query remote repositories for available tags; it derives the
  version directly from the constraint bounds.

### Algorithm

1. Collect all version constraints from the full transitive dependency graph.
2. For each unique dependency (identified by normalized repository URL):
   a. **Major version agreement**: Extract the major version from each constraint's
      minimum version. All constraints on the same dependency must agree on the major
      version. If they differ, report an error -- major version mismatches are never
      resolved, even if the constraint ranges technically overlap. This prevents a
      dependency written against v1 from silently being used with v2.
   b. Collect all constraints on this dependency from every package that requires it.
   c. Compute the intersection of all constraints.
   d. Extract the **minimum** version: the highest `>=` lower bound across all
      constraints. Verify it satisfies any upper bounds (`<` constraints).
3. If the minimum version violates an upper bound, or constraints lack a `>=` lower
   bound, report an error listing the conflicting constraints and which packages
   introduced them.

### Transitive Resolution

1. Parse the root package's `ucg-deps`.
2. For each direct dependency, resolve its version per MVS.
3. Fetch (or read from cache) the resolved version's `ucg-deps`.
4. If the dependency has no `ucg-deps` file, treat it as a leaf node (zero transitive
   dependencies).
5. Recursively collect transitive dependencies and their constraints.
6. Re-resolve with the full constraint set. Repeat until the dependency graph
   stabilizes (fixed-point iteration).

### Circular Dependency Detection

The resolver maintains a visited set of normalized repository URLs during traversal.
If a URL is encountered that is already in the visited set, the resolver reports an
error with the full dependency chain: "circular dependency detected:
github.com/org/a -> github.com/org/b -> github.com/org/a".

### Dependency Removal and Version Effects

When a direct dependency is removed (via `ucg dep remove`), the full dependency graph
is re-resolved. This can cause version changes in transitive dependencies. For example,
if the removed direct dependency had a lower version constraint than what remains in
the graph, the resolved version of that transitive dependency may increase.

Users should review the lockfile changes after `ucg dep remove` to understand any
version shifts.

If a removed direct dependency is still required transitively by other dependencies,
it remains in the lockfile and vendor directory as a transitive dependency. The remove
command will note: "removed github.com/org/a as direct dependency (still present as
transitive dependency of github.com/org/b)".

### Error Cases

- **Missing lower bound**: "dependency github.com/org/foo: constraint '< 2.0.0'
  (from root) must have a '>=' lower bound"
- **Unsatisfiable constraints**: "dependency github.com/org/foo: minimum version
  v2.0.0 does not satisfy all constraints (>= 1.0.0, < 2.0.0 from root;
  >= 2.0.0 from github.com/org/bar)"
- **Dependency cycle**: "circular dependency detected: github.com/org/a ->
  github.com/org/b -> github.com/org/a"
- **Tag not found during fetch**: git clone or hg clone will fail if the resolved
  version tag does not exist in the remote repository
- **Major version mismatch**: "dependency github.com/org/lib has conflicting major
  versions: v1 required by root (>= 1.2.0), v2 required by github.com/org/bar
  (>= 2.0.0)"
- **Conflicting repo types**: "conflicting repository types for github.com/org/lib:
  git from root, hg from github.com/org/other"

## CLI Commands

All package management commands live under `ucg dep`:

### `ucg dep init [options]`

Initialize a new `ucg-deps` file. Required before any other `ucg dep` command.

Options:
- `--vendor "<path>"` -- vendor directory path (default: `"vendor"`)
- `--nix` -- enable automatic Nix expression generation

Behavior:
1. Error if `ucg-deps` already exists
2. Create `ucg-deps` with `[package]` and empty `[deps]`

### `ucg dep add <url> [options]`

Add or update a dependency in `ucg-deps`, resolve versions, update `ucg.lock`, and
vendor.

If the URL (after normalization) already exists in `ucg-deps`, the constraint is
updated. An informational message is printed: "updating constraint for
github.com/org/lib from '>= 1.0.0' to '>= 2.0.0'".

Options:
- `--version "<constraint>"` -- version constraint (**required**, e.g. `">= 1.0.0"`)
- `--type "git"|"hg"` -- repository type (default: `"git"`)

Behavior:
1. Error if `ucg-deps` does not exist
2. Normalize the URL and check for duplicates
3. Add or update entry in `ucg-deps` `[deps]` table
4. Resolve the full dependency graph (including transitive deps)
5. Write `ucg.lock`
6. If `nix = true` and `nix` available, write `ucg-deps.nix`
7. Vendor all dependencies

All network operations and resolution happen before any files are written. If any step
fails, no files are modified.

### `ucg dep remove <url>`

Remove a direct dependency from `ucg-deps`, re-resolve, update lockfile, and clean
vendor.

Behavior:
1. Error if `ucg-deps` does not exist
2. Normalize the URL and find the entry
3. Error if the URL is not found in `ucg-deps`
4. Remove the entry from `ucg-deps`
5. Re-resolve the full dependency graph
6. Update `ucg.lock`
7. If `nix = true` and `nix` available, update `ucg-deps.nix`
8. Clean removed deps from vendor directory

If the removed dependency is still required transitively, it remains in the lockfile
and vendor directory. An informational message is printed.

### `ucg dep lock [--dry-run]`

Re-resolve all dependencies and rewrite `ucg.lock`.

Behavior:
1. Error if `ucg-deps` does not exist
2. Parse `ucg-deps`
3. Resolve the full transitive dependency graph via MVS
4. Write `ucg.lock` (removing any stale entries)
5. If `nix = true` and `nix` available, write `ucg-deps.nix`

With `--dry-run`, shows what would change (added/removed/changed deps) without writing.

Does not vendor (use `ucg dep vendor` after).

### `ucg dep vendor [--dry-run]`

Re-resolve, rewrite `ucg.lock`, and fetch all dependencies into the vendor directory.

Behavior:
1. Error if `ucg-deps` does not exist
2. Resolve the full transitive dependency graph via MVS
3. Write `ucg.lock` (removing any stale entries)
4. If `nix = true` and `nix` available, write `ucg-deps.nix`
5. Warn if vendor directory exists but `ucg.lock` does not (pre-existing directory)
6. Fetch and vendor all resolved dependencies

With `--dry-run`, shows what would change without writing or fetching.

### `ucg dep nix [options]`

Re-resolve, rewrite `ucg.lock`, and regenerate `ucg-deps.nix`.

Errors if `nix` is not available on PATH -- this command exists for users who have
nix but whose project collaborators may not.

Options:
- `--stdout` -- write to stdout instead of `ucg-deps.nix`
- `--dry-run` -- show what would change without writing

## Interaction with Existing Features

### Standard Library

The standard library is embedded in the binary and resolved before vendor paths. The
import prefix `std/` always resolves to the embedded stdlib and is never resolved
against the vendor directory.

### Recursive Mode (`-r`)

The `-r` flag for `ucg build`, `ucg test`, and `ucg fmt` continues to work as before.
It processes files within the project but does **not** descend into the vendor
directory.

### Type Checking

Import type resolution (`ImportShape`) is extended to support vendor path resolution.
The type checker uses the same package root detection and import resolution rules as
the runtime.

### AST Rewriter

The AST rewriter (`src/ast/rewrite.rs`) currently converts non-`std/` import paths to
absolute paths by joining with the base directory. The `vendor/` prefix must be
recognized at this level -- intercepted before normal path resolution occurs -- so that
`vendor/github.com/org/lib/file.ucg` is resolved against the package root's vendor
directory rather than relative to the importing file.

## Known Limitations

- **Git submodules**: Not supported. Submodules in dependencies are not fetched.
  Dependencies that rely on submodules cannot be used.
- **Symlinks**: Symlinks in dependency repos are skipped during vendoring. Packages
  that rely on symlinks must restructure their layout.
- **Port numbers in URLs**: Port numbers are stripped during URL normalization. Repos
  on the same host with different ports are treated as the same dependency.
- **Pre-release versions**: Cannot be used as dependency versions. There is no opt-in
  mechanism.
