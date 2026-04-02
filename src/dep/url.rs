use std::path::PathBuf;

/// Normalize a repository URL for deduplication.
///
/// Rules applied in order:
/// 1. Detect SSH colon syntax: `git@host:path` -> `host/path`
/// 2. Strip scheme: `https://`, `http://`, `ssh://`
/// 3. Strip user prefix: `git@` (after scheme strip)
/// 4. Strip port: `host:1234/path` -> `host/path`
/// 5. Strip trailing `.git`
/// 6. Strip trailing `/`
/// 7. Lowercase the host (everything before the first `/`)
pub fn normalize_url(raw: &str) -> String {
    let mut s = raw.to_string();

    // Step 1: SSH colon syntax (git@host:org/repo) - detect BEFORE stripping scheme
    // This pattern: no scheme prefix, contains ':', path after colon doesn't start with '//'
    if !s.contains("://") {
        if let Some(at_pos) = s.find('@') {
            let after_at = &s[at_pos + 1..];
            if let Some(colon_pos) = after_at.find(':') {
                let after_colon = &after_at[colon_pos + 1..];
                // It's SSH colon syntax if after colon is not a port number followed by /
                if !after_colon.is_empty()
                    && !after_colon.starts_with('/')
                    && !is_port_prefix(after_colon)
                {
                    // Convert to host/path form, stripping user@
                    s = format!("{}/{}", &after_at[..colon_pos], after_colon);
                }
            }
        }
    }

    // Step 2: Strip scheme
    for scheme in &["https://", "http://", "ssh://"] {
        if s.starts_with(scheme) {
            s = s[scheme.len()..].to_string();
            break;
        }
    }

    // Step 3: Strip user prefix (git@ or similar user@)
    if let Some(at_pos) = s.find('@') {
        // Only strip if @ comes before the first /
        let slash_pos = s.find('/').unwrap_or(s.len());
        if at_pos < slash_pos {
            s = s[at_pos + 1..].to_string();
        }
    }

    // Step 4: Strip port - host:1234/path -> host/path
    if let Some(colon_pos) = s.find(':') {
        let slash_pos = s.find('/').unwrap_or(s.len());
        if colon_pos < slash_pos {
            let between = &s[colon_pos + 1..slash_pos];
            if between.chars().all(|c| c.is_ascii_digit()) && !between.is_empty() {
                s = format!("{}{}", &s[..colon_pos], &s[slash_pos..]);
            }
        }
    }

    // Step 5: Strip trailing .git
    if s.ends_with(".git") {
        s.truncate(s.len() - 4);
    }

    // Step 6: Strip trailing /
    while s.ends_with('/') {
        s.pop();
    }

    // Step 7: Lowercase the host (everything before first /)
    if let Some(slash_pos) = s.find('/') {
        let host = s[..slash_pos].to_lowercase();
        s = format!("{}{}", host, &s[slash_pos..]);
    } else {
        s = s.to_lowercase();
    }

    s
}

/// Check if a string starts with digits followed by '/' (i.e., a port number).
fn is_port_prefix(s: &str) -> bool {
    let mut chars = s.chars();
    let first = chars.next();
    match first {
        Some(c) if c.is_ascii_digit() => {
            for c in chars {
                if c == '/' {
                    return true;
                }
                if !c.is_ascii_digit() {
                    return false;
                }
            }
            false
        }
        _ => false,
    }
}

/// Convert a normalized URL to a filesystem path for vendoring.
///
/// e.g., "github.com/org/repo" -> PathBuf("github.com/org/repo")
pub fn vendor_path(normalized: &str) -> PathBuf {
    PathBuf::from(normalized)
}

/// Convert a normalized URL to a valid Nix identifier.
///
/// Replaces dots, slashes, and other invalid characters with underscores.
pub fn nix_identifier(normalized: &str) -> String {
    normalized
        .chars()
        .map(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' => c,
            _ => '_',
        })
        .collect()
}

/// Check if the original (non-normalized) URL uses SSH syntax.
///
/// Returns true for:
/// - `git@host:path` (SSH colon syntax)
/// - `ssh://...`
pub fn is_ssh_url(raw: &str) -> bool {
    if raw.starts_with("ssh://") {
        return true;
    }
    // SSH colon syntax: user@host:path (no scheme)
    if !raw.contains("://") {
        if let Some(at_pos) = raw.find('@') {
            let after_at = &raw[at_pos + 1..];
            if let Some(colon_pos) = after_at.find(':') {
                let after_colon = &after_at[colon_pos + 1..];
                if !after_colon.is_empty() && !after_colon.starts_with('/') {
                    return true;
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize_https_with_git_suffix() {
        assert_eq!(
            normalize_url("https://github.com/org/lib.git"),
            "github.com/org/lib"
        );
    }

    #[test]
    fn normalize_https_without_git() {
        assert_eq!(
            normalize_url("https://github.com/org/lib"),
            "github.com/org/lib"
        );
    }

    #[test]
    fn normalize_ssh_colon_syntax() {
        assert_eq!(
            normalize_url("git@github.com:org/lib.git"),
            "github.com/org/lib"
        );
    }

    #[test]
    fn normalize_ssh_scheme() {
        assert_eq!(
            normalize_url("ssh://git@github.com/org/lib"),
            "github.com/org/lib"
        );
    }

    #[test]
    fn normalize_strips_port() {
        assert_eq!(
            normalize_url("https://github.com:443/org/lib"),
            "github.com/org/lib"
        );
    }

    #[test]
    fn normalize_strips_nonstandard_port() {
        assert_eq!(
            normalize_url("https://git.example.com:8080/org/lib"),
            "git.example.com/org/lib"
        );
    }

    #[test]
    fn normalize_strips_trailing_slash() {
        assert_eq!(
            normalize_url("https://github.com/org/lib/"),
            "github.com/org/lib"
        );
    }

    #[test]
    fn normalize_lowercases_host() {
        assert_eq!(
            normalize_url("https://GitHub.COM/Org/Lib"),
            "github.com/Org/Lib"
        );
    }

    #[test]
    fn normalize_all_forms_equal() {
        let urls = [
            "https://github.com/org/lib.git",
            "git@github.com:org/lib.git",
            "ssh://git@github.com/org/lib",
            "https://GitHub.com/org/lib/",
            "http://github.com/org/lib.git",
        ];
        let normalized: Vec<String> = urls.iter().map(|u| normalize_url(u)).collect();
        for n in &normalized {
            assert_eq!(n, &normalized[0], "all forms should normalize to the same value");
        }
    }

    #[test]
    fn nix_identifier_derivation() {
        assert_eq!(
            nix_identifier("github.com/org/lib"),
            "github_com_org_lib"
        );
    }

    #[test]
    fn nix_identifier_with_hyphens() {
        assert_eq!(
            nix_identifier("github.com/org/my-lib"),
            "github_com_org_my-lib"
        );
    }

    #[test]
    fn is_ssh_url_colon_syntax() {
        assert!(is_ssh_url("git@github.com:org/lib.git"));
    }

    #[test]
    fn is_ssh_url_scheme() {
        assert!(is_ssh_url("ssh://git@github.com/org/lib"));
    }

    #[test]
    fn is_ssh_url_https_false() {
        assert!(!is_ssh_url("https://github.com/org/lib"));
    }

    #[test]
    fn is_ssh_url_http_false() {
        assert!(!is_ssh_url("http://github.com/org/lib"));
    }

    #[test]
    fn vendor_path_basic() {
        assert_eq!(
            vendor_path("github.com/org/lib"),
            PathBuf::from("github.com/org/lib")
        );
    }
}
