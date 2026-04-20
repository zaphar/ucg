use std::path::Path;

use super::error::DepError;
use super::lockfile::Lockfile;
use super::url::normalize_url;

/// Check if `nix` is available on PATH.
pub fn is_nix_available() -> bool {
    std::process::Command::new("nix")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Compute a NAR hash for a directory using `nix hash path`.
pub fn nar_hash(dir: &Path) -> Result<String, DepError> {
    let output = std::process::Command::new("nix")
        .args(["hash", "path", &dir.to_string_lossy()])
        .output()
        .map_err(DepError::IoError)?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(DepError::ParseError(format!(
            "nix hash path failed: {}",
            stderr
        )));
    }

    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Generate a Nix expression from a lockfile.
///
/// The vendor_dir is used to compute NAR hashes if nix is available.
/// If nix is not available, falls back to the lockfile's sha256 hashes.
pub fn generate_nix_expression(lockfile: &Lockfile, vendor_dir: &Path) -> Result<String, DepError> {
    let mut packages: Vec<NixPackage> = Vec::new();

    for pkg in &lockfile.package {
        let url = pkg.url()?;
        let normalized = normalize_url(url);
        let nix_id = super::url::nix_identifier(&normalized);
        let vendor_path = vendor_dir.join(&normalized);

        // Try to compute NAR hash, fall back to lockfile hash
        let hash = if vendor_path.exists() && is_nix_available() {
            nar_hash(&vendor_path)?
        } else {
            pkg.sha256.clone()
        };

        let fetch_fn = match pkg.repo_type() {
            "hg" => "fetchhg",
            _ => "fetchgit",
        };

        packages.push(NixPackage {
            nix_id,
            normalized_url: normalized,
            fetch_fn: fetch_fn.to_string(),
            fetch_url: url.to_string(),
            commit: pkg.commit.clone(),
            hash,
        });
    }

    // Sort by normalized URL for determinism
    packages.sort_by(|a, b| a.normalized_url.cmp(&b.normalized_url));

    Ok(render_nix_expression(&packages))
}

struct NixPackage {
    nix_id: String,
    normalized_url: String,
    fetch_fn: String,
    fetch_url: String,
    commit: String,
    hash: String,
}

/// Escape a string for use inside a Nix double-quoted string literal.
/// Escapes backslashes, double quotes, and `${` interpolation sequences.
fn escape_nix_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '$' if chars.peek() == Some(&'{') => {
                out.push_str("\\${");
                chars.next();
            }
            _ => out.push(c),
        }
    }
    out
}

fn render_nix_expression(packages: &[NixPackage]) -> String {
    let mut out = String::new();

    // Collect fetch functions needed
    let mut fetch_fns: Vec<&str> = packages.iter().map(|p| p.fetch_fn.as_str()).collect();
    fetch_fns.sort();
    fetch_fns.dedup();

    let args: Vec<String> = fetch_fns.iter().map(|f| f.to_string()).collect();
    let mut all_args = args;
    all_args.push("runCommand".to_string());

    out.push_str(&format!("{{ {} }}:\n", all_args.join(", ")));
    out.push_str("let\n");

    for pkg in packages {
        out.push_str(&format!("  {} = {} {{\n", pkg.nix_id, pkg.fetch_fn));
        out.push_str(&format!("    url = \"{}\";\n", escape_nix_string(&pkg.fetch_url)));
        out.push_str(&format!("    rev = \"{}\";\n", escape_nix_string(&pkg.commit)));
        out.push_str(&format!("    hash = \"{}\";\n", escape_nix_string(&pkg.hash)));
        out.push_str("  };\n");
    }

    out.push_str("in\n");
    out.push_str("runCommand \"ucg-vendor\" {} ''\n");

    for pkg in packages {
        out.push_str(&format!("  mkdir -p $out/{}\n", pkg.normalized_url));
        out.push_str(&format!(
            "  cp -r ${{{}}}/.  $out/{}/\n",
            pkg.nix_id, pkg.normalized_url
        ));
        out.push('\n');
    }

    out.push_str("''\n");
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dep::lockfile::LockedPackage;

    #[test]
    fn generate_nix_single_git_dep() {
        let lockfile = Lockfile {
            package: vec![LockedPackage {
                git: Some("git@github.com:org/lib.git".into()),
                hg: None,
                version: "1.2.3".into(),
                commit: "abc123def456".into(),
                sha256: "sha256-test123".into(),
            }],
        };

        // Don't need vendor dir for this test since nix won't be available in test
        let vendor_dir = std::path::PathBuf::from("/nonexistent");
        let result = generate_nix_expression(&lockfile, &vendor_dir).unwrap();

        assert!(result.contains("fetchgit"));
        assert!(result.contains("git@github.com:org/lib.git"));
        assert!(result.contains("abc123def456"));
        assert!(result.contains("github_com_org_lib"));
        assert!(result.contains("runCommand"));
        assert!(result.contains("mkdir -p $out/github.com/org/lib"));
    }

    #[test]
    fn generate_nix_hg_dep() {
        let lockfile = Lockfile {
            package: vec![LockedPackage {
                git: None,
                hg: Some("https://hg.example.com/repo".into()),
                version: "1.0.0".into(),
                commit: "deadbeef".into(),
                sha256: "sha256-test456".into(),
            }],
        };

        let vendor_dir = std::path::PathBuf::from("/nonexistent");
        let result = generate_nix_expression(&lockfile, &vendor_dir).unwrap();

        assert!(result.contains("fetchhg"));
        assert!(result.contains("hg.example.com/repo"));
    }

    #[test]
    fn generate_nix_sorted_output() {
        let lockfile = Lockfile {
            package: vec![
                LockedPackage {
                    git: Some("https://github.com/zzz/repo".into()),
                    hg: None,
                    version: "1.0.0".into(),
                    commit: "aaa".into(),
                    sha256: "sha256-1".into(),
                },
                LockedPackage {
                    git: Some("https://github.com/aaa/repo".into()),
                    hg: None,
                    version: "1.0.0".into(),
                    commit: "bbb".into(),
                    sha256: "sha256-2".into(),
                },
            ],
        };

        let vendor_dir = std::path::PathBuf::from("/nonexistent");
        let result = generate_nix_expression(&lockfile, &vendor_dir).unwrap();

        // aaa should appear before zzz
        let aaa_pos = result.find("github_com_aaa_repo").unwrap();
        let zzz_pos = result.find("github_com_zzz_repo").unwrap();
        assert!(aaa_pos < zzz_pos);
    }

    #[test]
    fn nix_identifier_special_chars() {
        assert_eq!(
            super::super::url::nix_identifier("github.com/org/my-lib"),
            "github_com_org_my-lib"
        );
    }

    #[test]
    fn escape_nix_string_basics() {
        assert_eq!(escape_nix_string("simple"), "simple");
        assert_eq!(escape_nix_string(r#"has"quote"#), r#"has\"quote"#);
        assert_eq!(escape_nix_string(r"has\backslash"), r"has\\backslash");
        assert_eq!(escape_nix_string("has${interp}"), "has\\${interp}");
    }

    #[test]
    fn nix_expression_escapes_values() {
        let lockfile = Lockfile {
            package: vec![LockedPackage {
                git: Some("https://example.com/org/lib\";evil".into()),
                hg: None,
                version: "1.0.0".into(),
                commit: "abc123".into(),
                sha256: "sha256-test".into(),
            }],
        };

        let vendor_dir = std::path::PathBuf::from("/nonexistent");
        let result = generate_nix_expression(&lockfile, &vendor_dir).unwrap();

        // The quote should be escaped, not raw
        assert!(result.contains(r#"\";evil"#));
        assert!(!result.contains("\";\nevil"));
    }
}
