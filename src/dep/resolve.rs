use std::collections::BTreeMap;

use semver::{Version, VersionReq};

use super::error::DepError;
use super::manifest::Manifest;
use super::registry::TagSource;
use super::url::normalize_url;

/// A resolved dependency with all resolution information.
#[derive(Debug, Clone)]
pub struct ResolvedDep {
    pub normalized_url: String,
    pub fetch_url: String,
    pub repo_type: String,
    pub version: Version,
}

/// A constraint on a dependency, with provenance.
#[derive(Debug, Clone)]
struct Constraint {
    req: VersionReq,
    from_package: String,
    repo_type: String,
    fetch_url: String,
}

/// Trait for reading transitive manifests at a specific version.
/// In real usage this fetches/reads repos; in tests it returns mock manifests.
pub trait ManifestSource {
    fn get_manifest(
        &self,
        normalized_url: &str,
        version: &Version,
    ) -> Result<Option<Manifest>, DepError>;
}

/// Resolve the full dependency graph using Minimum Version Selection.
///
/// Returns a list of resolved dependencies sorted by normalized URL.
pub fn resolve_mvs(
    root_manifest: &Manifest,
    tag_source: &dyn TagSource,
    manifest_source: &dyn ManifestSource,
) -> Result<Vec<ResolvedDep>, DepError> {
    let root_deps = root_manifest.deps();
    if root_deps.is_empty() {
        return Ok(vec![]);
    }

    // Collect all constraints, keyed by normalized URL.
    // Each entry has a list of (VersionReq, source_package) pairs.
    let mut all_constraints: BTreeMap<String, Vec<Constraint>> = BTreeMap::new();

    // Seed with root constraints
    for (url, entry) in &root_deps {
        let normalized = normalize_url(url);
        let req = parse_version_constraint(&entry.version)?;
        all_constraints
            .entry(normalized.clone())
            .or_default()
            .push(Constraint {
                req,
                from_package: "root".to_string(),
                repo_type: entry.repo_type.clone(),
                fetch_url: url.clone(),
            });
    }

    // Fixed-point iteration: MVS guarantees convergence because constraints
    // only grow and versions only increase (the minimum satisfying version
    // rises monotonically as constraints tighten). Unsatisfiable constraints
    // are caught immediately by find_minimum_satisfying, and cycles are
    // caught by check_for_cycles.
    let mut resolved: BTreeMap<String, ResolvedDep> = BTreeMap::new();

    loop {
        let prev_resolved = resolved.clone();

        // Resolve each dependency
        for (normalized_url, constraints) in &all_constraints {
            // Check for conflicting repo types
            check_repo_type_conflicts(normalized_url, constraints)?;

            // Check for major version conflicts
            check_major_version_conflicts(normalized_url, constraints)?;

            // Intersect all constraints
            let combined = intersect_constraints(constraints)?;

            // Get available versions
            let repo_type = &constraints[0].repo_type;
            let fetch_url = select_fetch_url(constraints);
            let versions = tag_source.list_semver_tags(&fetch_url, repo_type)?;

            if versions.is_empty() {
                return Err(DepError::ParseError(format!(
                    "dependency {} has no semver tags (v*.*.*) -- the package must have at least one semver version tag to be used as a dependency",
                    normalized_url
                )));
            }

            // MVS: pick minimum satisfying version
            let version = find_minimum_satisfying(&combined, &versions).ok_or_else(|| {
                let constraint_desc: Vec<String> = constraints
                    .iter()
                    .map(|c| format!("{} from {}", c.req, c.from_package))
                    .collect();
                DepError::ParseError(format!(
                    "dependency {}: no version satisfies all constraints ({})",
                    normalized_url,
                    constraint_desc.join("; ")
                ))
            })?;

            resolved.insert(
                normalized_url.clone(),
                ResolvedDep {
                    normalized_url: normalized_url.clone(),
                    fetch_url: fetch_url.clone(),
                    repo_type: repo_type.clone(),
                    version,
                },
            );
        }

        // Collect transitive dependencies
        let mut new_constraints: BTreeMap<String, Vec<Constraint>> = all_constraints.clone();

        for (normalized_url, dep) in &resolved {
            collect_transitive(
                normalized_url,
                dep,
                manifest_source,
                &mut new_constraints,
            )?;
        }

        // Check for circular dependencies in the constraint graph
        check_for_cycles(&new_constraints)?;

        // Check if we've reached a fixed point
        if new_constraints == all_constraints && resolved == prev_resolved {
            break;
        }
        all_constraints = new_constraints;
    }

    let mut result: Vec<ResolvedDep> = resolved.into_values().collect();
    result.sort_by(|a, b| a.normalized_url.cmp(&b.normalized_url));
    Ok(result)
}

/// Collect the direct transitive dependencies from a resolved dep's manifest.
///
/// Does not recurse — the outer fixed-point loop in `resolve_mvs` handles
/// deeper transitive deps by resolving newly discovered deps and calling
/// this again on subsequent iterations with their correct versions.
fn collect_transitive(
    normalized_url: &str,
    dep: &ResolvedDep,
    manifest_source: &dyn ManifestSource,
    constraints: &mut BTreeMap<String, Vec<Constraint>>,
) -> Result<(), DepError> {
    let manifest = manifest_source.get_manifest(normalized_url, &dep.version)?;
    let manifest = match manifest {
        Some(m) => m,
        None => return Ok(()), // Leaf node
    };

    for (url, entry) in manifest.deps() {
        let trans_normalized = normalize_url(&url);
        let req = parse_version_constraint(&entry.version)?;

        // Add constraint if not already present from this source
        let entry_constraints = constraints.entry(trans_normalized.clone()).or_default();
        let already_has = entry_constraints
            .iter()
            .any(|c| c.from_package == normalized_url && c.req.to_string() == req.to_string());

        if !already_has {
            entry_constraints.push(Constraint {
                req,
                from_package: normalized_url.to_string(),
                repo_type: entry.repo_type.clone(),
                fetch_url: url.clone(),
            });
        }
    }

    Ok(())
}

/// Check for circular dependencies in the constraint graph.
///
/// Builds a dependency graph from the `from_package` fields in constraints
/// and detects cycles using DFS.
fn check_for_cycles(
    constraints: &BTreeMap<String, Vec<Constraint>>,
) -> Result<(), DepError> {
    // Build adjacency list: package -> packages it depends on
    // A constraint with from_package P on dep D means P depends on D.
    let mut edges: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for (dep_url, dep_constraints) in constraints {
        for c in dep_constraints {
            if c.from_package != "root" {
                edges
                    .entry(c.from_package.as_str())
                    .or_default()
                    .push(dep_url.as_str());
            }
        }
    }

    // DFS cycle detection
    let mut visited: BTreeMap<&str, bool> = BTreeMap::new(); // true = in current path
    for &node in edges.keys() {
        if let Some(cycle) = dfs_find_cycle(node, &edges, &mut visited, &mut vec![]) {
            return Err(DepError::ParseError(format!(
                "circular dependency detected: {}",
                cycle.join(" -> ")
            )));
        }
    }
    Ok(())
}

fn dfs_find_cycle<'a>(
    node: &'a str,
    edges: &BTreeMap<&'a str, Vec<&'a str>>,
    visited: &mut BTreeMap<&'a str, bool>,
    path: &mut Vec<&'a str>,
) -> Option<Vec<String>> {
    match visited.get(node) {
        Some(true) => {
            // Found a cycle — build the cycle path
            path.push(node);
            let start = path.iter().position(|&n| n == node).unwrap();
            return Some(path[start..].iter().map(|s| s.to_string()).collect());
        }
        Some(false) => return None, // Already fully explored
        None => {}
    }

    visited.insert(node, true);
    path.push(node);

    if let Some(neighbors) = edges.get(node) {
        for &neighbor in neighbors {
            if let Some(cycle) = dfs_find_cycle(neighbor, edges, visited, path) {
                return Some(cycle);
            }
        }
    }

    path.pop();
    visited.insert(node, false);
    None
}

fn check_repo_type_conflicts(
    normalized_url: &str,
    constraints: &[Constraint],
) -> Result<(), DepError> {
    let first_type = &constraints[0].repo_type;
    for c in &constraints[1..] {
        if c.repo_type != *first_type {
            return Err(DepError::ConflictingRepoTypes {
                url: normalized_url.to_string(),
                type1: format!("{} from {}", first_type, constraints[0].from_package),
                type2: format!("{} from {}", c.repo_type, c.from_package),
            });
        }
    }
    Ok(())
}

fn check_major_version_conflicts(
    normalized_url: &str,
    constraints: &[Constraint],
) -> Result<(), DepError> {
    let mut major_versions: Vec<(u64, &str)> = Vec::new();
    for c in constraints {
        if let Some(major) = extract_major_version(&c.req) {
            if let Some((existing_major, _)) = major_versions.first() {
                if major != *existing_major {
                    return Err(DepError::MajorVersionConflict {
                        url: normalized_url.to_string(),
                        major1: *existing_major,
                        major2: major,
                    });
                }
            }
            major_versions.push((major, &c.from_package));
        }
    }
    Ok(())
}

fn intersect_constraints(constraints: &[Constraint]) -> Result<VersionReq, DepError> {
    // Build a combined VersionReq by joining all constraint strings with commas
    let combined_str: Vec<String> = constraints.iter().map(|c| c.req.to_string()).collect();
    let joined = combined_str.join(", ");
    parse_version_constraint(&joined)
}

/// Select the best fetch URL from constraints.
/// Prefers SSH URLs, then root package's URL.
fn select_fetch_url(constraints: &[Constraint]) -> String {
    use super::url::is_ssh_url;

    // Prefer SSH from root
    for c in constraints {
        if c.from_package == "root" && is_ssh_url(&c.fetch_url) {
            return c.fetch_url.clone();
        }
    }
    // Then any SSH
    for c in constraints {
        if is_ssh_url(&c.fetch_url) {
            return c.fetch_url.clone();
        }
    }
    // Then root's URL
    for c in constraints {
        if c.from_package == "root" {
            return c.fetch_url.clone();
        }
    }
    // Fallback to first
    constraints[0].fetch_url.clone()
}

impl PartialEq for Constraint {
    fn eq(&self, other: &Self) -> bool {
        self.req.to_string() == other.req.to_string()
            && self.from_package == other.from_package
            && self.repo_type == other.repo_type
            && self.fetch_url == other.fetch_url
    }
}

impl Eq for Constraint {}

impl PartialEq for ResolvedDep {
    fn eq(&self, other: &Self) -> bool {
        self.normalized_url == other.normalized_url
            && self.version == other.version
            && self.repo_type == other.repo_type
    }
}

impl Eq for ResolvedDep {}

/// Parse a version constraint string into a semver VersionReq.
pub fn parse_version_constraint(s: &str) -> Result<VersionReq, DepError> {
    VersionReq::parse(s).map_err(|e| DepError::InvalidVersionConstraint(format!("{}: {}", s, e)))
}

/// Find the minimum version that satisfies the given requirement (MVS).
///
/// Sorts the versions and returns the first (lowest) that matches.
pub fn find_minimum_satisfying(req: &VersionReq, versions: &[Version]) -> Option<Version> {
    let mut sorted = versions.to_vec();
    sorted.sort();
    sorted.into_iter().find(|v| req.matches(v))
}

/// Extract the major version from the first comparator in a VersionReq.
///
/// Returns None if the requirement has no comparators.
pub fn extract_major_version(req: &VersionReq) -> Option<u64> {
    // VersionReq doesn't expose comparators directly in semver 1.x.
    // We parse the string representation to extract the major version.
    // The string form is like ">=1.2.3" or "^1.2.3, <2.0.0".
    // We find the first digit sequence that represents a version.
    let s = req.to_string();
    for part in s.split(',') {
        let trimmed = part
            .trim()
            .trim_start_matches(|c: char| !c.is_ascii_digit());
        if let Some(dot_pos) = trimmed.find('.') {
            if let Ok(major) = trimmed[..dot_pos].parse::<u64>() {
                return Some(major);
            }
        } else if let Ok(major) = trimmed.parse::<u64>() {
            return Some(major);
        }
    }
    None
}

/// Filter a list of tag names to valid semver versions.
///
/// Tags must have a `v` prefix (e.g., `v1.2.3`).
/// Pre-release versions are excluded.
pub fn filter_semver_tags(tags: &[String]) -> Vec<Version> {
    tags.iter()
        .filter_map(|tag| {
            let stripped = tag.strip_prefix('v')?;
            let version = Version::parse(stripped).ok()?;
            if version.pre.is_empty() {
                Some(version)
            } else {
                None
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::super::registry::MockTagSource;
    use super::*;

    struct MockManifestSource {
        manifests: std::collections::HashMap<String, Manifest>,
    }

    impl MockManifestSource {
        fn new() -> Self {
            Self {
                manifests: std::collections::HashMap::new(),
            }
        }

        fn add_manifest(&mut self, normalized_url: &str, manifest: Manifest) {
            self.manifests.insert(normalized_url.to_string(), manifest);
        }
    }

    impl ManifestSource for MockManifestSource {
        fn get_manifest(
            &self,
            normalized_url: &str,
            _version: &Version,
        ) -> Result<Option<Manifest>, DepError> {
            Ok(self.manifests.get(normalized_url).cloned())
        }
    }

    fn make_manifest(deps: &[(&str, &str, &str)]) -> Manifest {
        let mut toml = String::from("[deps]\n");
        for (url, version, repo_type) in deps {
            toml.push_str(&format!(
                "\"{}\" = {{ version = \"{}\", type = \"{}\" }}\n",
                url, version, repo_type
            ));
        }
        Manifest::from_toml(&toml).unwrap()
    }

    #[test]
    fn resolve_single_dep() {
        let root = make_manifest(&[("https://github.com/org/lib", ">= 1.0.0", "git")]);
        let mut tags = MockTagSource::new();
        tags.add_tags(
            "github.com/org/lib",
            vec![
                Version::new(1, 0, 0),
                Version::new(1, 1, 0),
                Version::new(2, 0, 0),
            ],
        );
        let manifests = MockManifestSource::new();

        let resolved = resolve_mvs(&root, &tags, &manifests).unwrap();
        assert_eq!(resolved.len(), 1);
        assert_eq!(resolved[0].version, Version::new(1, 0, 0));
    }

    #[test]
    fn resolve_diamond_dependency() {
        // Root -> B (>= 1.0.0), C (>= 1.0.0)
        // B -> D (>= 1.1.0)
        // C -> D (>= 1.2.0)
        // Should resolve D to 1.2.0 (minimum satisfying both)
        let root = make_manifest(&[
            ("https://github.com/org/b", ">= 1.0.0", "git"),
            ("https://github.com/org/c", ">= 1.0.0", "git"),
        ]);

        let b_manifest = make_manifest(&[("https://github.com/org/d", ">= 1.1.0", "git")]);
        let c_manifest = make_manifest(&[("https://github.com/org/d", ">= 1.2.0", "git")]);

        let mut tags = MockTagSource::new();
        tags.add_tags("github.com/org/b", vec![Version::new(1, 0, 0)]);
        tags.add_tags("github.com/org/c", vec![Version::new(1, 0, 0)]);
        tags.add_tags(
            "github.com/org/d",
            vec![
                Version::new(1, 0, 0),
                Version::new(1, 1, 0),
                Version::new(1, 2, 0),
                Version::new(1, 3, 0),
            ],
        );

        let mut manifests = MockManifestSource::new();
        manifests.add_manifest("github.com/org/b", b_manifest);
        manifests.add_manifest("github.com/org/c", c_manifest);

        let resolved = resolve_mvs(&root, &tags, &manifests).unwrap();
        assert_eq!(resolved.len(), 3); // b, c, d
        let d = resolved
            .iter()
            .find(|r| r.normalized_url == "github.com/org/d")
            .unwrap();
        assert_eq!(d.version, Version::new(1, 2, 0));
    }

    #[test]
    fn resolve_no_tags_error() {
        let root = make_manifest(&[("https://github.com/org/lib", ">= 1.0.0", "git")]);
        let tags = MockTagSource::new(); // No tags
        let manifests = MockManifestSource::new();

        let err = resolve_mvs(&root, &tags, &manifests).unwrap_err();
        assert!(err.to_string().contains("no semver tags"));
    }

    #[test]
    fn resolve_conflict_error() {
        // Root requires >= 2.0.0, but only 1.x available
        let root = make_manifest(&[("https://github.com/org/lib", ">= 2.0.0", "git")]);
        let mut tags = MockTagSource::new();
        tags.add_tags(
            "github.com/org/lib",
            vec![Version::new(1, 0, 0), Version::new(1, 5, 0)],
        );
        let manifests = MockManifestSource::new();

        let err = resolve_mvs(&root, &tags, &manifests).unwrap_err();
        assert!(err.to_string().contains("no version satisfies"));
    }

    #[test]
    fn resolve_major_version_conflict() {
        // Root -> lib >= 1.0.0
        // Transitive dep -> lib >= 2.0.0
        let root = make_manifest(&[
            ("https://github.com/org/lib", ">= 1.0.0", "git"),
            ("https://github.com/org/other", ">= 1.0.0", "git"),
        ]);

        let other_manifest = make_manifest(&[("https://github.com/org/lib", ">= 2.0.0", "git")]);

        let mut tags = MockTagSource::new();
        tags.add_tags(
            "github.com/org/lib",
            vec![Version::new(1, 0, 0), Version::new(2, 0, 0)],
        );
        tags.add_tags("github.com/org/other", vec![Version::new(1, 0, 0)]);

        let mut manifests = MockManifestSource::new();
        manifests.add_manifest("github.com/org/other", other_manifest);

        let err = resolve_mvs(&root, &tags, &manifests).unwrap_err();
        assert!(
            err.to_string().contains("major version conflict")
                || err.to_string().contains("major version")
        );
    }

    #[test]
    fn resolve_circular_dependency_error() {
        // A -> B -> A (cycle)
        let root = make_manifest(&[("https://github.com/org/a", ">= 1.0.0", "git")]);
        let a_manifest = make_manifest(&[("https://github.com/org/b", ">= 1.0.0", "git")]);
        let b_manifest = make_manifest(&[("https://github.com/org/a", ">= 1.0.0", "git")]);

        let mut tags = MockTagSource::new();
        tags.add_tags("github.com/org/a", vec![Version::new(1, 0, 0)]);
        tags.add_tags("github.com/org/b", vec![Version::new(1, 0, 0)]);

        let mut manifests = MockManifestSource::new();
        manifests.add_manifest("github.com/org/a", a_manifest);
        manifests.add_manifest("github.com/org/b", b_manifest);

        let err = resolve_mvs(&root, &tags, &manifests).unwrap_err();
        assert!(err.to_string().contains("circular dependency"));
    }

    #[test]
    fn resolve_leaf_node_no_manifest() {
        let root = make_manifest(&[("https://github.com/org/leaf", ">= 1.0.0", "git")]);
        let mut tags = MockTagSource::new();
        tags.add_tags("github.com/org/leaf", vec![Version::new(1, 0, 0)]);
        let manifests = MockManifestSource::new(); // No manifest = leaf

        let resolved = resolve_mvs(&root, &tags, &manifests).unwrap();
        assert_eq!(resolved.len(), 1);
        assert_eq!(resolved[0].version, Version::new(1, 0, 0));
    }

    #[test]
    fn resolve_same_dep_different_url_forms() {
        // Two deps via different URL forms that normalize to the same thing
        let root = make_manifest(&[
            ("https://github.com/org/lib.git", ">= 1.0.0", "git"),
            ("https://github.com/org/other", ">= 1.0.0", "git"),
        ]);

        let other_manifest = make_manifest(&[("git@github.com:org/lib.git", ">= 1.1.0", "git")]);

        let mut tags = MockTagSource::new();
        tags.add_tags(
            "github.com/org/lib",
            vec![
                Version::new(1, 0, 0),
                Version::new(1, 1, 0),
                Version::new(1, 2, 0),
            ],
        );
        tags.add_tags("github.com/org/other", vec![Version::new(1, 0, 0)]);

        let mut manifests = MockManifestSource::new();
        manifests.add_manifest("github.com/org/other", other_manifest);

        let resolved = resolve_mvs(&root, &tags, &manifests).unwrap();
        let lib = resolved
            .iter()
            .find(|r| r.normalized_url == "github.com/org/lib")
            .unwrap();
        // Should pick 1.1.0 (minimum satisfying both >= 1.0.0 and >= 1.1.0)
        assert_eq!(lib.version, Version::new(1, 1, 0));
    }

    #[test]
    fn resolve_conflicting_repo_types() {
        let root = make_manifest(&[
            ("https://github.com/org/lib", ">= 1.0.0", "git"),
            ("https://github.com/org/other", ">= 1.0.0", "git"),
        ]);

        let other_manifest = make_manifest(&[("https://github.com/org/lib", ">= 1.0.0", "hg")]);

        let mut tags = MockTagSource::new();
        tags.add_tags("github.com/org/lib", vec![Version::new(1, 0, 0)]);
        tags.add_tags("github.com/org/other", vec![Version::new(1, 0, 0)]);

        let mut manifests = MockManifestSource::new();
        manifests.add_manifest("github.com/org/other", other_manifest);

        let err = resolve_mvs(&root, &tags, &manifests).unwrap_err();
        assert!(err.to_string().contains("conflicting") || err.to_string().contains("Conflicting"));
    }

    #[test]
    fn resolve_empty_deps() {
        let root = make_manifest(&[]);
        let tags = MockTagSource::new();
        let manifests = MockManifestSource::new();

        let resolved = resolve_mvs(&root, &tags, &manifests).unwrap();
        assert!(resolved.is_empty());
    }

    #[test]
    fn parse_gte_constraint() {
        let req = parse_version_constraint(">= 1.2.3").unwrap();
        assert!(req.matches(&Version::new(1, 2, 3)));
        assert!(req.matches(&Version::new(2, 0, 0)));
        assert!(!req.matches(&Version::new(1, 2, 2)));
    }

    #[test]
    fn parse_range_constraint() {
        let req = parse_version_constraint(">= 1.2.3, < 2.0.0").unwrap();
        assert!(req.matches(&Version::new(1, 2, 3)));
        assert!(req.matches(&Version::new(1, 9, 0)));
        assert!(!req.matches(&Version::new(2, 0, 0)));
    }

    #[test]
    fn parse_caret_constraint() {
        let req = parse_version_constraint("^1.2.3").unwrap();
        assert!(req.matches(&Version::new(1, 2, 3)));
        assert!(req.matches(&Version::new(1, 9, 0)));
        assert!(!req.matches(&Version::new(2, 0, 0)));
    }

    #[test]
    fn parse_tilde_constraint() {
        let req = parse_version_constraint("~1.2.3").unwrap();
        assert!(req.matches(&Version::new(1, 2, 3)));
        assert!(req.matches(&Version::new(1, 2, 9)));
        assert!(!req.matches(&Version::new(1, 3, 0)));
    }

    #[test]
    fn parse_invalid_constraint() {
        assert!(parse_version_constraint("not a version").is_err());
    }

    #[test]
    fn find_minimum_picks_lowest_match() {
        let req = parse_version_constraint(">= 1.2.0").unwrap();
        let versions = vec![
            Version::new(1, 0, 0),
            Version::new(1, 2, 0),
            Version::new(1, 3, 0),
            Version::new(2, 0, 0),
        ];
        assert_eq!(
            find_minimum_satisfying(&req, &versions),
            Some(Version::new(1, 2, 0))
        );
    }

    #[test]
    fn find_minimum_none_when_no_match() {
        let req = parse_version_constraint(">= 3.0.0").unwrap();
        let versions = vec![Version::new(1, 0, 0), Version::new(2, 0, 0)];
        assert_eq!(find_minimum_satisfying(&req, &versions), None);
    }

    #[test]
    fn find_minimum_unordered_input() {
        let req = parse_version_constraint(">= 1.0.0").unwrap();
        let versions = vec![
            Version::new(2, 0, 0),
            Version::new(1, 0, 0),
            Version::new(1, 5, 0),
        ];
        assert_eq!(
            find_minimum_satisfying(&req, &versions),
            Some(Version::new(1, 0, 0))
        );
    }

    #[test]
    fn filter_semver_tags_basic() {
        let tags: Vec<String> = vec![
            "v1.0.0".into(),
            "v2.1.3".into(),
            "v0.1.0-beta.1".into(),
            "not-a-version".into(),
            "1.0.0".into(), // no v prefix
            "v3.0.0".into(),
        ];
        let mut versions = filter_semver_tags(&tags);
        versions.sort();
        assert_eq!(
            versions,
            vec![
                Version::new(1, 0, 0),
                Version::new(2, 1, 3),
                Version::new(3, 0, 0),
            ]
        );
    }

    #[test]
    fn extract_major_version_from_gte() {
        let req = parse_version_constraint(">= 1.2.3").unwrap();
        assert_eq!(extract_major_version(&req), Some(1));
    }

    #[test]
    fn extract_major_version_from_caret() {
        let req = parse_version_constraint("^2.0.0").unwrap();
        assert_eq!(extract_major_version(&req), Some(2));
    }

    #[test]
    fn extract_major_version_from_range() {
        let req = parse_version_constraint(">= 3.1.0, < 4.0.0").unwrap();
        assert_eq!(extract_major_version(&req), Some(3));
    }
}
