use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::error::DepError;
use super::resolve::parse_version_constraint;
use super::url::normalize_url;

#[derive(Debug, Deserialize, Serialize)]
pub struct Manifest {
    pub package: Option<PackageInfo>,
    pub deps: Option<BTreeMap<String, DepEntry>>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct PackageInfo {
    pub vendor: Option<String>,
    pub nix: Option<bool>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct DepEntry {
    pub version: String,
    #[serde(rename = "type")]
    pub repo_type: String,
}

impl Manifest {
    pub fn from_toml(content: &str) -> Result<Self, DepError> {
        toml::from_str(content).map_err(|e| DepError::ParseError(e.to_string()))
    }

    pub fn to_toml(&self) -> Result<String, DepError> {
        toml::to_string(self).map_err(|e| DepError::ParseError(e.to_string()))
    }

    /// Validate the manifest for correctness.
    ///
    /// Checks:
    /// - vendor name is not "std"
    /// - no duplicate URLs after normalization
    /// - all repo_type values are "git" or "hg"
    /// - all version constraints are valid
    pub fn validate(&self) -> Result<(), DepError> {
        // Check vendor name
        let vendor = self.vendor_dir();
        if vendor == "std" {
            return Err(DepError::ReservedVendorName("std".to_string()));
        }

        // Check deps
        if let Some(deps) = &self.deps {
            let mut seen_normalized: BTreeMap<String, String> = BTreeMap::new();

            for (url, entry) in deps {
                // Validate repo type
                if entry.repo_type != "git" && entry.repo_type != "hg" {
                    return Err(DepError::InvalidRepoType(entry.repo_type.clone()));
                }

                // Validate version constraint
                parse_version_constraint(&entry.version)?;

                // Check for duplicate normalized URLs
                let normalized = normalize_url(url);
                if let Some(original) = seen_normalized.get(&normalized) {
                    return Err(DepError::DuplicateUrl(format!(
                        "'{}' and '{}' normalize to the same URL: {}",
                        original, url, normalized
                    )));
                }
                seen_normalized.insert(normalized, url.clone());
            }
        }

        Ok(())
    }

    pub fn vendor_dir(&self) -> &str {
        self.package
            .as_ref()
            .and_then(|p| p.vendor.as_deref())
            .unwrap_or("vendor")
    }

    pub fn nix_enabled(&self) -> bool {
        self.package.as_ref().and_then(|p| p.nix).unwrap_or(false)
    }

    pub fn deps(&self) -> BTreeMap<String, DepEntry> {
        self.deps.clone().unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const VALID_MANIFEST: &str = r#"
[package]
vendor = "vendor"
nix = true

[deps]
"https://github.com/org/shared-lib.git" = { version = ">= 1.2.3", type = "git" }
"https://hg.example.com/internal-cfg" = { version = ">= 0.5.0", type = "hg" }
"#;

    #[test]
    fn parse_valid_manifest() {
        let manifest = Manifest::from_toml(VALID_MANIFEST).unwrap();
        assert_eq!(manifest.vendor_dir(), "vendor");
        assert!(manifest.nix_enabled());
        assert_eq!(manifest.deps().len(), 2);
    }

    #[test]
    fn roundtrip_serialize() {
        let manifest = Manifest::from_toml(VALID_MANIFEST).unwrap();
        let serialized = manifest.to_toml().unwrap();
        let reparsed = Manifest::from_toml(&serialized).unwrap();
        assert_eq!(reparsed.deps().len(), 2);
        assert_eq!(reparsed.vendor_dir(), "vendor");
    }

    #[test]
    fn vendor_std_rejected() {
        let toml = r#"
[package]
vendor = "std"
"#;
        let manifest = Manifest::from_toml(toml).unwrap();
        let err = manifest.validate().unwrap_err();
        assert!(err.to_string().contains("reserved"));
    }

    #[test]
    fn duplicate_normalized_urls_rejected() {
        let toml = r#"
[deps]
"https://github.com/org/lib.git" = { version = ">= 1.0.0", type = "git" }
"https://github.com/org/lib" = { version = ">= 1.0.0", type = "git" }
"#;
        let manifest = Manifest::from_toml(toml).unwrap();
        let err = manifest.validate().unwrap_err();
        assert!(err.to_string().contains("duplicate") || err.to_string().contains("normalize"));
    }

    #[test]
    fn invalid_repo_type_rejected() {
        let toml = r#"
[deps]
"https://github.com/org/lib" = { version = ">= 1.0.0", type = "svn" }
"#;
        let manifest = Manifest::from_toml(toml).unwrap();
        let err = manifest.validate().unwrap_err();
        assert!(err.to_string().contains("svn"));
    }

    #[test]
    fn invalid_version_constraint_rejected() {
        let toml = r#"
[deps]
"https://github.com/org/lib" = { version = "not valid", type = "git" }
"#;
        let manifest = Manifest::from_toml(toml).unwrap();
        let err = manifest.validate().unwrap_err();
        assert!(err.to_string().contains("version constraint"));
    }

    #[test]
    fn default_vendor_dir() {
        let toml = r#"
[deps]
"https://github.com/org/lib" = { version = ">= 1.0.0", type = "git" }
"#;
        let manifest = Manifest::from_toml(toml).unwrap();
        assert_eq!(manifest.vendor_dir(), "vendor");
    }

    #[test]
    fn empty_deps_table() {
        let toml = r#"
[package]
nix = false

[deps]
"#;
        let manifest = Manifest::from_toml(toml).unwrap();
        assert_eq!(manifest.deps().len(), 0);
        manifest.validate().unwrap();
    }

    #[test]
    fn missing_deps_table() {
        let toml = r#"
[package]
vendor = "deps"
"#;
        let manifest = Manifest::from_toml(toml).unwrap();
        assert_eq!(manifest.deps().len(), 0);
        manifest.validate().unwrap();
    }

    #[test]
    fn missing_package_table() {
        let toml = r#"
[deps]
"https://github.com/org/lib" = { version = ">= 1.0.0", type = "git" }
"#;
        let manifest = Manifest::from_toml(toml).unwrap();
        assert_eq!(manifest.vendor_dir(), "vendor");
        assert!(!manifest.nix_enabled());
        manifest.validate().unwrap();
    }

    #[test]
    fn valid_manifest_passes_validation() {
        let manifest = Manifest::from_toml(VALID_MANIFEST).unwrap();
        manifest.validate().unwrap();
    }
}
