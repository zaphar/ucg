use serde::{Deserialize, Serialize};

use super::error::DepError;
use super::manifest::Manifest;
use super::resolve::parse_version_constraint;
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

    /// Sort packages by normalized URL for deterministic output.
    pub fn sort(&mut self) {
        self.package.sort_by(|a, b| {
            let a_norm = normalize_url(a.url());
            let b_norm = normalize_url(b.url());
            a_norm.cmp(&b_norm)
        });
    }

    /// Check if this lockfile is stale relative to the given manifest.
    ///
    /// Returns Ok(()) if the lockfile is up-to-date.
    /// Returns Err with a description of what's stale.
    pub fn is_stale(&self, manifest: &Manifest) -> Result<(), DepError> {
        let deps = manifest.deps();

        // Check each manifest dep has a matching lockfile entry
        for (url, entry) in &deps {
            let normalized = normalize_url(url);
            let locked = self.package.iter().find(|p| normalize_url(p.url()) == normalized);

            match locked {
                None => {
                    return Err(DepError::ParseError(format!(
                        "dependency '{}' not found in lockfile",
                        url
                    )));
                }
                Some(locked) => {
                    // Check that the locked version still satisfies the constraint
                    let req = parse_version_constraint(&entry.version)?;
                    let locked_version = semver::Version::parse(&locked.version).map_err(|e| {
                        DepError::ParseError(format!(
                            "invalid version '{}' in lockfile: {}",
                            locked.version, e
                        ))
                    })?;
                    if !req.matches(&locked_version) {
                        return Err(DepError::ParseError(format!(
                            "locked version {} for '{}' no longer satisfies constraint '{}'",
                            locked.version, url, entry.version
                        )));
                    }
                }
            }
        }

        Ok(())
    }
}

impl LockedPackage {
    /// Returns the URL for this locked package.
    pub fn url(&self) -> &str {
        self.git
            .as_deref()
            .or(self.hg.as_deref())
            .unwrap_or("")
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
        assert!(lockfile.package[0].url().contains("aaa"));
        assert!(lockfile.package[1].url().contains("zzz"));
    }

    #[test]
    fn staleness_missing_dep() {
        let lockfile = Lockfile {
            package: vec![],
        };
        let manifest_toml = r#"
[deps]
"https://github.com/org/lib" = { version = ">= 1.0.0", type = "git" }
"#;
        let manifest = Manifest::from_toml(manifest_toml).unwrap();
        let err = lockfile.is_stale(&manifest).unwrap_err();
        assert!(err.to_string().contains("not found"));
    }

    #[test]
    fn staleness_constraint_no_longer_satisfied() {
        let lockfile = Lockfile {
            package: vec![LockedPackage {
                git: Some("https://github.com/org/lib".into()),
                hg: None,
                version: "1.0.0".into(),
                commit: "abc".into(),
                sha256: "sha256-1".into(),
            }],
        };
        let manifest_toml = r#"
[deps]
"https://github.com/org/lib" = { version = ">= 2.0.0", type = "git" }
"#;
        let manifest = Manifest::from_toml(manifest_toml).unwrap();
        let err = lockfile.is_stale(&manifest).unwrap_err();
        assert!(err.to_string().contains("no longer satisfies"));
    }

    #[test]
    fn staleness_clean_lockfile() {
        let lockfile = Lockfile {
            package: vec![LockedPackage {
                git: Some("https://github.com/org/lib".into()),
                hg: None,
                version: "1.2.3".into(),
                commit: "abc".into(),
                sha256: "sha256-1".into(),
            }],
        };
        let manifest_toml = r#"
[deps]
"https://github.com/org/lib" = { version = ">= 1.0.0", type = "git" }
"#;
        let manifest = Manifest::from_toml(manifest_toml).unwrap();
        lockfile.is_stale(&manifest).unwrap();
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
        assert_eq!(pkg.url(), "https://github.com/org/lib");
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
        assert_eq!(pkg.url(), "https://hg.example.com/repo");
        assert_eq!(pkg.repo_type(), "hg");
    }
}
