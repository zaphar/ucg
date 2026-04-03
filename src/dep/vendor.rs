use std::fs;
use std::path::{Path, PathBuf};

use sha2::{Digest, Sha256};
use walkdir::WalkDir;

use super::error::DepError;
use super::lockfile::{LockedPackage, Lockfile};
use super::manifest::Manifest;
use super::registry::RepoFetcher;
use super::resolve::ResolvedDep;
use super::url::normalize_url;

/// Strip the dependency's own vendor directory from a fetched tree.
///
/// Reads the dep's ucg-deps file (if present) to determine its vendor dir,
/// then removes that directory.
pub fn strip_dep_vendor_dir(dep_dir: &Path) -> Result<(), DepError> {
    let dep_manifest_path = dep_dir.join(super::MANIFEST_FILE);
    if dep_manifest_path.exists() {
        let content = fs::read_to_string(&dep_manifest_path)?;
        if let Ok(manifest) = Manifest::from_toml(&content) {
            let vendor_dir = dep_dir.join(manifest.vendor_dir());
            if vendor_dir.exists() {
                fs::remove_dir_all(&vendor_dir)?;
            }
        }
    }
    Ok(())
}

/// Compute a deterministic SHA-256 hash of a directory's contents.
///
/// Algorithm:
/// 1. Enumerate all files recursively (skip symlinks, skip .git/.hg)
/// 2. Sort by relative path (lexicographic, byte-order)
/// 3. For each file: feed `<relative-path>\0<file-contents>` into SHA-256
pub fn hash_directory(dir: &Path) -> Result<String, DepError> {
    let mut hasher = Sha256::new();
    let mut entries: Vec<(String, PathBuf)> = Vec::new();

    for entry in WalkDir::new(dir).follow_links(false).into_iter() {
        let entry = entry.map_err(|e| DepError::IoError(e.into()))?;
        let path = entry.path();

        // Skip symlinks
        if entry.path_is_symlink() {
            continue;
        }

        // Skip directories (we only hash files)
        if entry.file_type().is_dir() {
            // Skip .git and .hg directories
            let name = entry.file_name().to_string_lossy();
            if name == ".git" || name == ".hg" {
                continue;
            }
            continue;
        }

        let rel_path = path
            .strip_prefix(dir)
            .map_err(|e| DepError::ParseError(e.to_string()))?;
        let rel_str = rel_path.to_string_lossy().to_string();

        // Skip files inside .git or .hg directories
        if rel_str.starts_with(".git/")
            || rel_str.starts_with(".hg/")
            || rel_str == ".git"
            || rel_str == ".hg"
        {
            continue;
        }

        entries.push((rel_str, path.to_path_buf()));
    }

    // Sort by relative path for determinism
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    for (rel_path, abs_path) in &entries {
        hasher.update(rel_path.as_bytes());
        hasher.update(b"\0");
        let contents = fs::read(abs_path)?;
        hasher.update(&contents);
    }

    let hash = hasher.finalize();
    Ok(format!("sha256-{}", base64_encode(&hash)))
}

fn base64_encode(bytes: &[u8]) -> String {
    use base64::Engine;
    base64::engine::general_purpose::STANDARD.encode(bytes)
}

/// Vendor all resolved dependencies.
///
/// This is the staged write operation:
/// 1. Fetch all deps to temp directories
/// 2. Compute hashes
/// 3. Only then write to the vendor directory
///
/// Returns the list of LockedPackages for the lockfile.
pub fn vendor_resolved(
    resolved: &[ResolvedDep],
    vendor_dir: &Path,
    temp_base: &Path,
    fetcher: &dyn RepoFetcher,
) -> Result<Vec<LockedPackage>, DepError> {
    let mut locked_packages: Vec<LockedPackage> = Vec::new();
    let mut staged: Vec<(PathBuf, PathBuf)> = Vec::new(); // (temp_dir, final_dir)

    // Phase 1: Fetch everything to temp dirs
    for dep in resolved {
        let tag = format!("v{}", dep.version);
        let temp_dir = temp_base.join(&dep.normalized_url);
        if temp_dir.exists() {
            fs::remove_dir_all(&temp_dir)?;
        }
        fs::create_dir_all(temp_dir.parent().unwrap_or(temp_base))?;

        let commit = fetcher.fetch(&dep.fetch_url, &dep.repo_type, &tag, &temp_dir)?;

        // Strip the dep's own vendor directory
        strip_dep_vendor_dir(&temp_dir)?;

        // Compute hash
        let sha256 = hash_directory(&temp_dir)?;

        let final_dir = vendor_dir.join(&dep.normalized_url);

        let locked = LockedPackage {
            git: if dep.repo_type == "git" {
                Some(dep.fetch_url.clone())
            } else {
                None
            },
            hg: if dep.repo_type == "hg" {
                Some(dep.fetch_url.clone())
            } else {
                None
            },
            version: dep.version.to_string(),
            commit,
            sha256,
        };

        locked_packages.push(locked);
        staged.push((temp_dir, final_dir));
    }

    // Phase 2: Write everything to vendor dir (staged)
    for (temp_dir, final_dir) in &staged {
        if final_dir.exists() {
            fs::remove_dir_all(final_dir)?;
        }
        if let Some(parent) = final_dir.parent() {
            fs::create_dir_all(parent)?;
        }
        copy_dir_skip_symlinks(temp_dir, final_dir)?;
    }

    // Clean stale deps from vendor dir
    clean_stale_deps(vendor_dir, resolved)?;

    Ok(locked_packages)
}

/// Vendor from an existing lockfile (no resolution needed).
pub fn vendor_from_lockfile(
    lockfile: &Lockfile,
    manifest: &Manifest,
    vendor_dir: &Path,
    temp_base: &Path,
    fetcher: &dyn RepoFetcher,
) -> Result<(), DepError> {
    let mut staged: Vec<(PathBuf, PathBuf)> = Vec::new();

    for pkg in &lockfile.package {
        let normalized = normalize_url(pkg.url());
        let final_dir = vendor_dir.join(&normalized);

        // Skip if already vendored and hash matches
        if final_dir.exists() {
            if let Ok(existing_hash) = hash_directory(&final_dir) {
                if existing_hash == pkg.sha256 {
                    continue;
                }
            }
        }

        let tag = format!("v{}", pkg.version);
        let temp_dir = temp_base.join(&normalized);
        if temp_dir.exists() {
            fs::remove_dir_all(&temp_dir)?;
        }
        fs::create_dir_all(temp_dir.parent().unwrap_or(temp_base))?;

        let commit = fetcher.fetch(pkg.url(), pkg.repo_type(), &tag, &temp_dir)?;

        // Strip dep's own vendor dir
        strip_dep_vendor_dir(&temp_dir)?;

        // Verify hash
        let hash = hash_directory(&temp_dir)?;
        if hash != pkg.sha256 {
            return Err(DepError::ParseError(format!(
                "hash mismatch for {} at v{}: expected {}, got {}. The tag may have been force-pushed. Run `ucg dep lock` to re-resolve.",
                normalized, pkg.version, pkg.sha256, hash
            )));
        }

        // Verify commit
        if commit != pkg.commit {
            return Err(DepError::ParseError(format!(
                "commit mismatch for {} at v{}: expected {}, got {}. The tag may have been force-pushed. Run `ucg dep lock` to re-resolve.",
                normalized, pkg.version, pkg.commit, commit
            )));
        }

        staged.push((temp_dir, final_dir));
    }

    // Write staged
    for (temp_dir, final_dir) in &staged {
        if final_dir.exists() {
            fs::remove_dir_all(final_dir)?;
        }
        if let Some(parent) = final_dir.parent() {
            fs::create_dir_all(parent)?;
        }
        copy_dir_skip_symlinks(temp_dir, final_dir)?;
    }

    // Clean stale deps
    let normalized_urls: Vec<String> = lockfile
        .package
        .iter()
        .map(|p| normalize_url(p.url()))
        .collect();
    clean_stale_deps_by_urls(vendor_dir, &normalized_urls)?;

    let _ = manifest; // used for future extensions
    Ok(())
}

/// Copy a directory tree, skipping symlinks.
fn copy_dir_skip_symlinks(src: &Path, dst: &Path) -> Result<(), DepError> {
    fs::create_dir_all(dst)?;

    for entry in WalkDir::new(src).follow_links(false).into_iter() {
        let entry = entry.map_err(|e| DepError::IoError(e.into()))?;
        let path = entry.path();

        // Skip symlinks
        if entry.path_is_symlink() {
            continue;
        }

        let rel = path
            .strip_prefix(src)
            .map_err(|e| DepError::ParseError(e.to_string()))?;
        let dest_path = dst.join(rel);

        if entry.file_type().is_dir() {
            fs::create_dir_all(&dest_path)?;
        } else {
            if let Some(parent) = dest_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::copy(path, &dest_path)?;
        }
    }

    Ok(())
}

/// Remove vendor subdirectories that are not in the resolved deps.
fn clean_stale_deps(vendor_dir: &Path, resolved: &[ResolvedDep]) -> Result<(), DepError> {
    let urls: Vec<String> = resolved.iter().map(|d| d.normalized_url.clone()).collect();
    clean_stale_deps_by_urls(vendor_dir, &urls)
}

/// Remove vendor subdirectories not in the given normalized URL list.
fn clean_stale_deps_by_urls(vendor_dir: &Path, normalized_urls: &[String]) -> Result<(), DepError> {
    if !vendor_dir.exists() {
        return Ok(());
    }

    // Walk vendor dir to find all host/org/repo directories
    // We need to find leaf directories that correspond to dep paths
    let mut existing_paths: Vec<PathBuf> = Vec::new();
    collect_dep_dirs(vendor_dir, vendor_dir, &mut existing_paths)?;

    for existing in existing_paths {
        let rel = existing
            .strip_prefix(vendor_dir)
            .map_err(|e| DepError::ParseError(e.to_string()))?;
        let rel_str = rel.to_string_lossy().to_string();

        if !normalized_urls.contains(&rel_str) {
            if existing.exists() {
                fs::remove_dir_all(&existing)?;
            }
        }
    }

    // Clean up empty parent directories
    cleanup_empty_dirs(vendor_dir)?;

    Ok(())
}

/// Collect directories that likely correspond to vendored deps.
/// These are directories at depth 3+ (host/org/repo).
fn collect_dep_dirs(
    base: &Path,
    current: &Path,
    result: &mut Vec<PathBuf>,
) -> Result<(), DepError> {
    if !current.is_dir() {
        return Ok(());
    }

    let depth = current
        .strip_prefix(base)
        .map(|p| p.components().count())
        .unwrap_or(0);

    // At depth 3 or more (host/org/repo), check if this is a dep directory
    // A dep directory typically contains files (not just subdirectories)
    if depth >= 3 {
        let has_files = fs::read_dir(current)?
            .filter_map(|e| e.ok())
            .any(|e| e.path().is_file());
        if has_files {
            result.push(current.to_path_buf());
            return Ok(());
        }
    }

    for entry in fs::read_dir(current)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_dep_dirs(base, &path, result)?;
        }
    }

    Ok(())
}

/// Remove empty directories recursively.
fn cleanup_empty_dirs(dir: &Path) -> Result<(), DepError> {
    if !dir.is_dir() {
        return Ok(());
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            cleanup_empty_dirs(&path)?;
            // Try to remove if empty
            let _ = fs::remove_dir(&path); // Fails silently if not empty
        }
    }

    Ok(())
}

/// Check if a vendor directory exists and warn if there's no lockfile.
pub fn check_vendor_dir_warning(vendor_dir: &Path, lock_exists: bool) -> Option<String> {
    if vendor_dir.exists() && !lock_exists {
        Some(format!(
            "vendor directory '{}' already exists but no ucg.lock found. \
             The vendor directory is managed by ucg dep and its contents may be removed. \
             Use a different path via the `vendor` field in ucg-deps, or remove the existing directory.",
            vendor_dir.display()
        ))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_directory_deterministic() {
        let dir = std::env::temp_dir().join("ucg_test_hash_det");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("sub")).unwrap();
        fs::write(dir.join("a.txt"), "hello").unwrap();
        fs::write(dir.join("sub/b.txt"), "world").unwrap();

        let hash1 = hash_directory(&dir).unwrap();
        let hash2 = hash_directory(&dir).unwrap();
        assert_eq!(hash1, hash2);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn hash_directory_different_content() {
        let dir1 = std::env::temp_dir().join("ucg_test_hash_d1");
        let dir2 = std::env::temp_dir().join("ucg_test_hash_d2");
        let _ = fs::remove_dir_all(&dir1);
        let _ = fs::remove_dir_all(&dir2);
        fs::create_dir_all(&dir1).unwrap();
        fs::create_dir_all(&dir2).unwrap();
        fs::write(dir1.join("a.txt"), "hello").unwrap();
        fs::write(dir2.join("a.txt"), "world").unwrap();

        let hash1 = hash_directory(&dir1).unwrap();
        let hash2 = hash_directory(&dir2).unwrap();
        assert_ne!(hash1, hash2);

        let _ = fs::remove_dir_all(&dir1);
        let _ = fs::remove_dir_all(&dir2);
    }

    #[test]
    fn hash_directory_ignores_git_dir() {
        let dir = std::env::temp_dir().join("ucg_test_hash_git");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join(".git")).unwrap();
        fs::write(dir.join("a.txt"), "hello").unwrap();
        fs::write(dir.join(".git/config"), "gitconfig").unwrap();

        let hash_with_git = hash_directory(&dir).unwrap();

        // Remove .git and hash again
        fs::remove_dir_all(dir.join(".git")).unwrap();
        let hash_without_git = hash_directory(&dir).unwrap();

        assert_eq!(hash_with_git, hash_without_git);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn copy_dir_skip_symlinks_works() {
        let src = std::env::temp_dir().join("ucg_test_copy_src");
        let dst = std::env::temp_dir().join("ucg_test_copy_dst");
        let _ = fs::remove_dir_all(&src);
        let _ = fs::remove_dir_all(&dst);
        fs::create_dir_all(src.join("sub")).unwrap();
        fs::write(src.join("a.txt"), "hello").unwrap();
        fs::write(src.join("sub/b.txt"), "world").unwrap();

        copy_dir_skip_symlinks(&src, &dst).unwrap();

        assert!(dst.join("a.txt").exists());
        assert!(dst.join("sub/b.txt").exists());
        assert_eq!(fs::read_to_string(dst.join("a.txt")).unwrap(), "hello");
        assert_eq!(fs::read_to_string(dst.join("sub/b.txt")).unwrap(), "world");

        let _ = fs::remove_dir_all(&src);
        let _ = fs::remove_dir_all(&dst);
    }

    #[test]
    fn strip_dep_vendor_dir_removes_default() {
        let dir = std::env::temp_dir().join("ucg_test_strip_vendor");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("vendor/some/dep")).unwrap();
        fs::write(dir.join("vendor/some/dep/file.ucg"), "content").unwrap();
        fs::write(dir.join("lib.ucg"), "main content").unwrap();
        // Create a ucg-deps with default vendor
        fs::write(dir.join("ucg-deps"), "[package]\n").unwrap();

        strip_dep_vendor_dir(&dir).unwrap();

        assert!(!dir.join("vendor").exists());
        assert!(dir.join("lib.ucg").exists());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn strip_dep_vendor_dir_custom() {
        let dir = std::env::temp_dir().join("ucg_test_strip_vendor_custom");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("deps/some/dep")).unwrap();
        fs::write(dir.join("deps/some/dep/file.ucg"), "content").unwrap();
        fs::write(dir.join("lib.ucg"), "main content").unwrap();
        fs::write(dir.join("ucg-deps"), "[package]\nvendor = \"deps\"\n").unwrap();

        strip_dep_vendor_dir(&dir).unwrap();

        assert!(!dir.join("deps").exists());
        assert!(dir.join("lib.ucg").exists());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn strip_dep_vendor_dir_no_manifest() {
        let dir = std::env::temp_dir().join("ucg_test_strip_no_manifest");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("vendor")).unwrap();
        fs::write(dir.join("lib.ucg"), "content").unwrap();

        // Should not error even without ucg-deps
        strip_dep_vendor_dir(&dir).unwrap();
        // vendor dir stays since there's no manifest to read
        assert!(dir.join("vendor").exists());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn check_vendor_dir_warning_present() {
        let dir = std::env::temp_dir().join("ucg_test_vendor_warn");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let warning = check_vendor_dir_warning(&dir, false);
        assert!(warning.is_some());
        assert!(warning.unwrap().contains("already exists"));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn check_vendor_dir_warning_none_with_lock() {
        let dir = std::env::temp_dir().join("ucg_test_vendor_warn_lock");
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();

        let warning = check_vendor_dir_warning(&dir, true);
        assert!(warning.is_none());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn check_vendor_dir_warning_none_no_dir() {
        let dir = std::env::temp_dir().join("ucg_test_vendor_warn_nodir");
        let _ = fs::remove_dir_all(&dir);

        let warning = check_vendor_dir_warning(&dir, false);
        assert!(warning.is_none());
    }
}
