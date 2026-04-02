use semver::{Version, VersionReq};

use super::error::DepError;

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
        let trimmed = part.trim().trim_start_matches(|c: char| !c.is_ascii_digit());
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
    use super::*;

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
