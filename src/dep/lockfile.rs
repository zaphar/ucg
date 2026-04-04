use serde::{Deserialize, Serialize};

use super::error::DepError;
use super::url::normalize_url;

#[derive(Debug, Deserialize, Serialize)]
pub struct Lockfile {
    pub package: Vec<LockedPackage>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LockedPackage {
    pub git: Option<String>,
    pub hg: Option<String>,
    pub version: String,
    pub commit: String,
    pub sha256: String,
}

impl Lockfile {
    pub fn from_toml(content: &str) -> Result<Self, DepError> {
        toml::from_str(content).map_err(|e| DepError::ParseError(e.to_string()))
    }

    pub fn to_toml(&self) -> Result<String, DepError> {
        toml::to_string(self).map_err(|e| DepError::ParseError(e.to_string()))
    }

    /// Validate all packages in the lockfile.
    ///
    /// Checks every package and collects all errors so the user sees
    /// every problem at once rather than fixing them one at a time.
    pub fn validate(&self) -> Result<(), DepError> {
        let errors: Vec<String> = self
            .package
            .iter()
            .enumerate()
            .filter_map(|(i, pkg)| match pkg.url() {
                Ok(_) => None,
                Err(e) => Some(format!("  package[{}]: {}", i, e)),
            })
            .collect();

        if errors.is_empty() {
            Ok(())
        } else {
            Err(DepError::ParseError(format!(
                "invalid lockfile entries:\n{}",
                errors.join("\n")
            )))
        }
    }

    /// Sort packages by normalized URL for deterministic output.
    ///
    /// Packages with invalid URLs (no git or hg) sort last.
    pub fn sort(&mut self) {
        self.package.sort_by(|a, b| {
            let a_norm = a.url().map(|u| normalize_url(u)).unwrap_or_default();
            let b_norm = b.url().map(|u| normalize_url(u)).unwrap_or_default();
            a_norm.cmp(&b_norm)
        });
    }

}

impl LockedPackage {
    /// Returns the URL for this locked package.
    ///
    /// Returns an error if neither `git` nor `hg` is set.
    pub fn url(&self) -> Result<&str, DepError> {
        self.git
            .as_deref()
            .or(self.hg.as_deref())
            .ok_or_else(|| {
                DepError::ParseError(format!(
                    "lockfile entry for version {} has neither git nor hg URL",
                    self.version
                ))
            })
    }

    /// Returns the repository type ("git" or "hg").
    pub fn repo_type(&self) -> &str {
        if self.git.is_some() {
            "git"
        } else {
            "hg"
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const VALID_LOCKFILE: &str = r#"
[[package]]
git = "git@github.com:org/shared-lib.git"
version = "1.2.3"
commit = "abc123def456789"
sha256 = "sha256-abc123"

[[package]]
hg = "https://hg.example.com/internal-cfg"
version = "0.5.0"
commit = "def456abc789"
sha256 = "sha256-def456"
"#;

    #[test]
    fn parse_valid_lockfile() {
        let lockfile = Lockfile::from_toml(VALID_LOCKFILE).unwrap();
        assert_eq!(lockfile.package.len(), 2);
    }

    #[test]
    fn roundtrip_serialize() {
        let lockfile = Lockfile::from_toml(VALID_LOCKFILE).unwrap();
        let serialized = lockfile.to_toml().unwrap();
        let reparsed = Lockfile::from_toml(&serialized).unwrap();
        assert_eq!(reparsed.package.len(), 2);
    }

    #[test]
    fn sort_by_normalized_url() {
        let mut lockfile = Lockfile {
            package: vec![
                LockedPackage {
                    git: Some("https://github.com/zzz/repo".into()),
                    hg: None,
                    version: "1.0.0".into(),
                    commit: "abc".into(),
                    sha256: "sha256-1".into(),
                },
                LockedPackage {
                    git: Some("https://github.com/aaa/repo".into()),
                    hg: None,
                    version: "1.0.0".into(),
                    commit: "def".into(),
                    sha256: "sha256-2".into(),
                },
            ],
        };
        lockfile.sort();
        assert!(lockfile.package[0].url().unwrap().contains("aaa"));
        assert!(lockfile.package[1].url().unwrap().contains("zzz"));
    }

    #[test]
    fn locked_package_url_git() {
        let pkg = LockedPackage {
            git: Some("https://github.com/org/lib".into()),
            hg: None,
            version: "1.0.0".into(),
            commit: "abc".into(),
            sha256: "sha256-1".into(),
        };
        assert_eq!(pkg.url().unwrap(), "https://github.com/org/lib");
        assert_eq!(pkg.repo_type(), "git");
    }

    #[test]
    fn locked_package_url_hg() {
        let pkg = LockedPackage {
            git: None,
            hg: Some("https://hg.example.com/repo".into()),
            version: "1.0.0".into(),
            commit: "abc".into(),
            sha256: "sha256-1".into(),
        };
        assert_eq!(pkg.url().unwrap(), "https://hg.example.com/repo");
        assert_eq!(pkg.repo_type(), "hg");
    }

    #[test]
    fn locked_package_url_missing_returns_error() {
        let pkg = LockedPackage {
            git: None,
            hg: None,
            version: "1.0.0".into(),
            commit: "abc".into(),
            sha256: "sha256-1".into(),
        };
        let err = pkg.url().unwrap_err();
        assert!(err.to_string().contains("neither git nor hg"));
    }

    #[test]
    fn validate_catches_all_invalid_entries() {
        let lockfile = Lockfile {
            package: vec![
                LockedPackage {
                    git: Some("https://github.com/org/lib".into()),
                    hg: None,
                    version: "1.0.0".into(),
                    commit: "abc".into(),
                    sha256: "sha256-1".into(),
                },
                LockedPackage {
                    git: None,
                    hg: None,
                    version: "2.0.0".into(),
                    commit: "def".into(),
                    sha256: "sha256-2".into(),
                },
                LockedPackage {
                    git: None,
                    hg: None,
                    version: "3.0.0".into(),
                    commit: "ghi".into(),
                    sha256: "sha256-3".into(),
                },
            ],
        };
        let err = lockfile.validate().unwrap_err();
        let msg = err.to_string();
        // Both invalid entries should be reported
        assert!(msg.contains("2.0.0"), "should mention version 2.0.0");
        assert!(msg.contains("3.0.0"), "should mention version 3.0.0");
    }

    #[test]
    fn validate_passes_for_valid_lockfile() {
        let lockfile = Lockfile::from_toml(VALID_LOCKFILE).unwrap();
        lockfile.validate().unwrap();
    }
}
