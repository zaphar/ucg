pub mod error;
pub mod lockfile;
pub mod manifest;
pub mod nix;
pub mod registry;
pub mod resolve;
pub mod url;
pub mod vendor;

use std::path::{Path, PathBuf};

pub const MANIFEST_FILE: &str = "ucg-deps";
pub const LOCK_FILE: &str = "ucg.lock";
pub const NIX_FILE: &str = "ucg-deps.nix";
pub const VENDOR_PREFIX: &str = "vendor/";
pub const STD_PREFIX: &str = "std/";

/// Walk up from `start` looking for MANIFEST_FILE.
/// Returns the directory containing it, or None.
pub fn find_package_root(start: &Path) -> Option<PathBuf> {
    let mut current = start.to_path_buf();
    loop {
        if current.join(MANIFEST_FILE).exists() {
            return Some(current);
        }
        if !current.pop() {
            return None;
        }
    }
}

/// Resolve an import path to an absolute filesystem path.
///
/// 1. `./` or `../` - relative to importing_dir
/// 2. `std/` - returned as-is (handled by stdlib system)
/// 3. `vendor/` prefix + package_root - `<root>/<vendor_dir>/<rest after vendor/>`
/// 4. anything else - relative to importing_dir
pub fn resolve_import_path(
    import_path: &str,
    importing_dir: &Path,
    package_root: Option<&Path>,
    vendor_dir: &str,
) -> PathBuf {
    // std/ prefix: return as-is for stdlib system
    if import_path.starts_with(STD_PREFIX) {
        return PathBuf::from(import_path);
    }

    // Relative paths
    if import_path.starts_with("./") || import_path.starts_with("../") {
        return importing_dir.join(import_path);
    }

    // vendor/ prefix: resolve against package root's vendor dir
    if let Some(rest) = import_path.strip_prefix(VENDOR_PREFIX) {
        if let Some(root) = package_root {
            return root.join(vendor_dir).join(rest);
        }
        // No package root: treat as relative to importing dir
        return importing_dir.join(import_path);
    }

    // Bare path: relative to importing dir
    importing_dir.join(import_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn find_package_root_in_current_dir() {
        let dir = std::env::temp_dir().join("ucg_test_pkg_root_current");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join(MANIFEST_FILE), "").unwrap();

        assert_eq!(find_package_root(&dir), Some(dir.clone()));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn find_package_root_in_ancestor() {
        let root = std::env::temp_dir().join("ucg_test_pkg_root_ancestor");
        let child = root.join("sub").join("deep");
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(&child).unwrap();
        fs::write(root.join(MANIFEST_FILE), "").unwrap();

        assert_eq!(find_package_root(&child), Some(root.clone()));

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn find_package_root_not_found() {
        let dir = std::env::temp_dir().join("ucg_test_pkg_root_none");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        // This could find a ucg-deps in some parent of /tmp, but very unlikely.
        // We test the negative case by checking a deeply nested path with no manifest.
        let deep = dir.join("a").join("b").join("c");
        fs::create_dir_all(&deep).unwrap();
        // No MANIFEST_FILE anywhere under dir
        // Note: might find one above /tmp, so we just ensure it doesn't return `deep` or `dir`
        let result = find_package_root(&deep);
        assert!(result.is_none() || !result.as_ref().unwrap().starts_with(&dir));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn resolve_relative_path() {
        let importing = Path::new("/project/src");
        let result = resolve_import_path("./utils.ucg", importing, None, "vendor");
        assert_eq!(result, PathBuf::from("/project/src/./utils.ucg"));
    }

    #[test]
    fn resolve_parent_relative_path() {
        let importing = Path::new("/project/src");
        let result = resolve_import_path("../lib.ucg", importing, None, "vendor");
        assert_eq!(result, PathBuf::from("/project/src/../lib.ucg"));
    }

    #[test]
    fn resolve_std_prefix() {
        let importing = Path::new("/project/src");
        let result = resolve_import_path("std/testing.ucg", importing, None, "vendor");
        assert_eq!(result, PathBuf::from("std/testing.ucg"));
    }

    #[test]
    fn resolve_vendor_prefix_with_root() {
        let importing = Path::new("/project/src");
        let root = Path::new("/project");
        let result = resolve_import_path(
            "vendor/github.com/org/lib/config.ucg",
            importing,
            Some(root),
            "vendor",
        );
        assert_eq!(
            result,
            PathBuf::from("/project/vendor/github.com/org/lib/config.ucg")
        );
    }

    #[test]
    fn resolve_vendor_prefix_custom_dir() {
        let importing = Path::new("/project/src");
        let root = Path::new("/project");
        let result = resolve_import_path(
            "vendor/github.com/org/lib/config.ucg",
            importing,
            Some(root),
            "deps",
        );
        assert_eq!(
            result,
            PathBuf::from("/project/deps/github.com/org/lib/config.ucg")
        );
    }

    #[test]
    fn resolve_vendor_prefix_without_root() {
        let importing = Path::new("/project/src");
        let result = resolve_import_path(
            "vendor/github.com/org/lib/config.ucg",
            importing,
            None,
            "vendor",
        );
        assert_eq!(
            result,
            PathBuf::from("/project/src/vendor/github.com/org/lib/config.ucg")
        );
    }

    #[test]
    fn resolve_bare_path() {
        let importing = Path::new("/project/src");
        let result = resolve_import_path("utils.ucg", importing, None, "vendor");
        assert_eq!(result, PathBuf::from("/project/src/utils.ucg"));
    }
}

/// Integration tests using mock fetcher.
/// These test the full pipeline without network access.
#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::dep::lockfile::Lockfile;
    use crate::dep::manifest::Manifest;
    use crate::dep::registry::MockFetcher;
    use crate::dep::resolve::{resolve_mvs, ManifestSource};
    use crate::dep::vendor;
    use semver::Version;
    use std::fs;

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
        ) -> Result<Option<Manifest>, error::DepError> {
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

    /// Helper: set up a full project directory, resolve, vendor, and return
    /// the project root path and lockfile.
    fn full_pipeline(
        test_name: &str,
        manifest: &Manifest,
        manifests: &MockManifestSource,
        fetcher: &MockFetcher,
    ) -> (PathBuf, Lockfile) {
        let root = std::env::temp_dir().join(format!("ucg_integ_{}", test_name));
        let _ = fs::remove_dir_all(&root);
        fs::create_dir_all(&root).unwrap();

        // Write manifest
        fs::write(root.join(MANIFEST_FILE), manifest.to_toml().unwrap()).unwrap();

        let vendor_dir = root.join(manifest.vendor_dir());
        let temp_dir = root.join(".ucg_tmp");
        fs::create_dir_all(&temp_dir).unwrap();

        let resolved = resolve_mvs(manifest, manifests).unwrap();
        let locked_packages =
            vendor::vendor_resolved(&resolved, &vendor_dir, &temp_dir, fetcher).unwrap();

        let mut lockfile = Lockfile {
            package: locked_packages,
        };
        lockfile.sort();
        fs::write(root.join(LOCK_FILE), lockfile.to_toml().unwrap()).unwrap();

        let _ = fs::remove_dir_all(&temp_dir);
        (root, lockfile)
    }

    #[test]
    fn pipeline_single_dep() {
        let manifest = make_manifest(&[("https://github.com/org/lib", ">= 1.0.0", "git")]);
        let manifests = MockManifestSource::new();

        let mut fetcher = MockFetcher::new();
        fetcher.add_repo(
            "github.com/org/lib",
            "abc123",
            vec![("lib.ucg", "let x = 1;")],
        );

        let (root, lockfile) = full_pipeline("single_dep", &manifest, &manifests, &fetcher);

        // Verify vendor directory
        assert!(root.join("vendor/github.com/org/lib/lib.ucg").exists());
        assert_eq!(
            fs::read_to_string(root.join("vendor/github.com/org/lib/lib.ucg")).unwrap(),
            "let x = 1;"
        );

        // Verify lockfile
        assert_eq!(lockfile.package.len(), 1);
        assert_eq!(lockfile.package[0].version, "1.0.0");
        assert_eq!(lockfile.package[0].commit, "abc123");
        assert!(lockfile.package[0].sha256.starts_with("sha256-"));

        // Verify lockfile on disk
        let lock_content = fs::read_to_string(root.join(LOCK_FILE)).unwrap();
        let parsed_lock = Lockfile::from_toml(&lock_content).unwrap();
        assert_eq!(parsed_lock.package.len(), 1);

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn pipeline_transitive_deps() {
        // root -> A (>= 1.0.0), A -> B (>= 1.0.0)
        let manifest = make_manifest(&[("https://github.com/org/a", ">= 1.0.0", "git")]);
        let a_manifest = make_manifest(&[("https://github.com/org/b", ">= 1.0.0", "git")]);

        let mut manifests = MockManifestSource::new();
        manifests.add_manifest("github.com/org/a", a_manifest);

        let mut fetcher = MockFetcher::new();
        fetcher.add_repo("github.com/org/a", "aaa111", vec![("a.ucg", "let a = 1;")]);
        fetcher.add_repo("github.com/org/b", "bbb222", vec![("b.ucg", "let b = 2;")]);

        let (root, lockfile) = full_pipeline("transitive", &manifest, &manifests, &fetcher);

        // Both deps should be vendored
        assert!(root.join("vendor/github.com/org/a/a.ucg").exists());
        assert!(root.join("vendor/github.com/org/b/b.ucg").exists());
        assert_eq!(lockfile.package.len(), 2);

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn pipeline_vendor_directory_structure() {
        let manifest = make_manifest(&[
            ("https://github.com/org/lib", ">= 1.0.0", "git"),
            ("https://github.com/other/tool", ">= 2.0.0", "git"),
        ]);
        let manifests = MockManifestSource::new();

        let mut fetcher = MockFetcher::new();
        fetcher.add_repo(
            "github.com/org/lib",
            "aaa",
            vec![("lib.ucg", "let x = 1;"), ("sub/helper.ucg", "let h = 2;")],
        );
        fetcher.add_repo(
            "github.com/other/tool",
            "bbb",
            vec![("tool.ucg", "let t = 3;")],
        );

        let (root, _) = full_pipeline("vendor_structure", &manifest, &manifests, &fetcher);

        // Verify normalized URL becomes vendor directory path
        assert!(root.join("vendor/github.com/org/lib/lib.ucg").exists());
        assert!(root
            .join("vendor/github.com/org/lib/sub/helper.ucg")
            .exists());
        assert!(root.join("vendor/github.com/other/tool/tool.ucg").exists());

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn pipeline_hash_consistency() {
        let manifest = make_manifest(&[("https://github.com/org/lib", ">= 1.0.0", "git")]);
        let manifests = MockManifestSource::new();

        let mut fetcher = MockFetcher::new();
        fetcher.add_repo(
            "github.com/org/lib",
            "abc123",
            vec![("lib.ucg", "let x = 1;")],
        );

        let (root, lockfile) =
            full_pipeline("hash_consistency", &manifest, &manifests, &fetcher);

        // Hash of vendored directory should match lockfile
        let vendor_path = root.join("vendor/github.com/org/lib");
        let actual_hash = vendor::hash_directory(&vendor_path).unwrap();
        assert_eq!(actual_hash, lockfile.package[0].sha256);

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn pipeline_dep_vendor_dir_stripped() {
        let manifest = make_manifest(&[("https://github.com/org/lib", ">= 1.0.0", "git")]);
        let manifests = MockManifestSource::new();

        let mut fetcher = MockFetcher::new();
        fetcher.add_repo(
            "github.com/org/lib",
            "abc123",
            vec![
                ("lib.ucg", "let x = 1;"),
                ("ucg-deps", "[package]\nvendor = \"vendor\"\n"),
                ("vendor/some/dep/file.ucg", "let y = 2;"),
            ],
        );

        let (root, _) = full_pipeline("strip_vendor", &manifest, &manifests, &fetcher);

        // The dep's own vendor directory should have been stripped
        assert!(root.join("vendor/github.com/org/lib/lib.ucg").exists());
        assert!(root.join("vendor/github.com/org/lib/ucg-deps").exists());
        assert!(!root.join("vendor/github.com/org/lib/vendor").exists());

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn pipeline_empty_deps() {
        let manifest = make_manifest(&[]);
        let manifests = MockManifestSource::new();
        let fetcher = MockFetcher::new();

        let (root, lockfile) = full_pipeline("empty_deps", &manifest, &manifests, &fetcher);

        assert!(lockfile.package.is_empty());

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn pipeline_mvs_picks_minimum() {
        // Root requires >= 1.0.0 — MVS picks 1.0.0 (the lower bound)
        let manifest = make_manifest(&[("https://github.com/org/lib", ">= 1.0.0", "git")]);
        let manifests = MockManifestSource::new();

        let mut fetcher = MockFetcher::new();
        fetcher.add_repo("github.com/org/lib", "abc", vec![("lib.ucg", "let x = 1;")]);

        let (root, lockfile) = full_pipeline("mvs_minimum", &manifest, &manifests, &fetcher);

        assert_eq!(lockfile.package[0].version, "1.0.0");

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn pipeline_url_normalization_flows_to_vendor_path() {
        // Use SSH URL - should normalize to github.com/org/lib for vendor path
        let manifest = make_manifest(&[("git@github.com:org/lib.git", ">= 1.0.0", "git")]);
        let manifests = MockManifestSource::new();

        let mut fetcher = MockFetcher::new();
        fetcher.add_repo("github.com/org/lib", "abc", vec![("lib.ucg", "let x = 1;")]);

        let (root, _) = full_pipeline("url_normalization", &manifest, &manifests, &fetcher);

        // Vendor path should use normalized URL, not original SSH URL
        assert!(root.join("vendor/github.com/org/lib/lib.ucg").exists());

        let _ = fs::remove_dir_all(&root);
    }
}
