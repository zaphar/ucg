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
    if import_path.starts_with(VENDOR_PREFIX) {
        let rest = &import_path[VENDOR_PREFIX.len()..];
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
