use std::path::Path;
use std::process::Command;

use semver::Version;

use super::error::DepError;
use super::resolve::filter_semver_tags;

/// Trait for listing semver tags from a repository.
pub trait TagSource {
    fn list_semver_tags(&self, repo_url: &str, repo_type: &str) -> Result<Vec<Version>, DepError>;
}

/// Trait for fetching a repository at a specific tag.
pub trait RepoFetcher {
    /// Clone a repo at a specific tag into dest. Returns the commit hash.
    fn fetch(&self, url: &str, repo_type: &str, tag: &str, dest: &Path)
        -> Result<String, DepError>;
}

// ---------------------------------------------------------------------------
// Remote (real) implementations
// ---------------------------------------------------------------------------

/// Lists tags by shelling out to git/hg.
pub struct RemoteTagSource;

impl TagSource for RemoteTagSource {
    fn list_semver_tags(&self, repo_url: &str, repo_type: &str) -> Result<Vec<Version>, DepError> {
        match repo_type {
            "git" => {
                let output = run_git_ls_remote(repo_url)?;
                let tags = parse_git_ls_remote(&output);
                Ok(filter_semver_tags(&tags))
            }
            "hg" => {
                let output = run_hg_tags(repo_url)?;
                let tags = parse_hg_tags(&output);
                Ok(filter_semver_tags(&tags))
            }
            _ => Err(DepError::InvalidRepoType(repo_type.to_string())),
        }
    }
}

/// Fetches repositories by shelling out to git/hg.
pub struct RemoteFetcher;

impl RepoFetcher for RemoteFetcher {
    fn fetch(
        &self,
        url: &str,
        repo_type: &str,
        tag: &str,
        dest: &Path,
    ) -> Result<String, DepError> {
        match repo_type {
            "git" => fetch_git(url, tag, dest),
            "hg" => fetch_hg(url, tag, dest),
            _ => Err(DepError::InvalidRepoType(repo_type.to_string())),
        }
    }
}

// ---------------------------------------------------------------------------
// Git operations
// ---------------------------------------------------------------------------

fn run_git_ls_remote(repo_url: &str) -> Result<String, DepError> {
    let output = Command::new("git")
        .args(["ls-remote", "--tags", repo_url])
        .output()
        .map_err(|e| DepError::IoError(e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(DepError::ParseError(format!(
            "git ls-remote failed for '{}': {}",
            repo_url, stderr
        )));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

/// Parse `git ls-remote --tags` output into tag name strings.
///
/// Input format: `<hash>\trefs/tags/<tagname>` per line.
/// Filters out dereferenced tags (`^{}`).
pub fn parse_git_ls_remote(output: &str) -> Vec<String> {
    output
        .lines()
        .filter_map(|line| {
            let parts: Vec<&str> = line.split('\t').collect();
            if parts.len() >= 2 {
                let refname = parts[1];
                let tag = refname.strip_prefix("refs/tags/")?;
                if tag.ends_with("^{}") {
                    return None;
                }
                Some(tag.to_string())
            } else {
                None
            }
        })
        .collect()
}

fn fetch_git(url: &str, tag: &str, dest: &Path) -> Result<String, DepError> {
    let output = Command::new("git")
        .args([
            "clone",
            "--depth",
            "1",
            "--branch",
            tag,
            url,
            &dest.to_string_lossy(),
        ])
        .output()
        .map_err(|e| DepError::IoError(e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(DepError::ParseError(format!(
            "git clone failed for '{}' at tag '{}': {}",
            url, tag, stderr
        )));
    }

    let output = Command::new("git")
        .args(["-C", &dest.to_string_lossy(), "rev-parse", "HEAD"])
        .output()
        .map_err(|e| DepError::IoError(e))?;

    let commit = String::from_utf8_lossy(&output.stdout).trim().to_string();

    // Remove .git directory
    let git_dir = dest.join(".git");
    if git_dir.exists() {
        std::fs::remove_dir_all(&git_dir)?;
    }

    Ok(commit)
}

// ---------------------------------------------------------------------------
// Hg operations
// ---------------------------------------------------------------------------

fn run_hg_tags(repo_url: &str) -> Result<String, DepError> {
    let output = Command::new("hg")
        .args(["identify", "--tags", "-r", "all", repo_url])
        .output()
        .map_err(|e| DepError::IoError(e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(DepError::ParseError(format!(
            "hg identify failed for '{}': {}",
            repo_url, stderr
        )));
    }

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

/// Parse `hg identify --tags` output into tag name strings.
///
/// Filters out empty lines and the "tip" pseudo-tag.
pub fn parse_hg_tags(output: &str) -> Vec<String> {
    output
        .lines()
        .map(|line| line.trim().to_string())
        .filter(|t| !t.is_empty() && t != "tip")
        .collect()
}

fn fetch_hg(url: &str, tag: &str, dest: &Path) -> Result<String, DepError> {
    let output = Command::new("hg")
        .args(["clone", "-r", tag, url, &dest.to_string_lossy()])
        .output()
        .map_err(|e| DepError::IoError(e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(DepError::ParseError(format!(
            "hg clone failed for '{}' at tag '{}': {}",
            url, tag, stderr
        )));
    }

    let output = Command::new("hg")
        .args(["-R", &dest.to_string_lossy(), "id", "-i", "--debug"])
        .output()
        .map_err(|e| DepError::IoError(e))?;

    let commit = String::from_utf8_lossy(&output.stdout).trim().to_string();

    let hg_dir = dest.join(".hg");
    if hg_dir.exists() {
        std::fs::remove_dir_all(&hg_dir)?;
    }

    Ok(commit)
}

// ---------------------------------------------------------------------------
// Mock implementations (available for integration tests via #[cfg(test)])
// ---------------------------------------------------------------------------

#[cfg(test)]
pub struct MockTagSource {
    pub tags: std::collections::HashMap<String, Vec<Version>>,
}

#[cfg(test)]
impl MockTagSource {
    pub fn new() -> Self {
        Self {
            tags: std::collections::HashMap::new(),
        }
    }

    pub fn add_tags(&mut self, normalized_url: &str, versions: Vec<Version>) {
        self.tags.insert(normalized_url.to_string(), versions);
    }
}

#[cfg(test)]
impl TagSource for MockTagSource {
    fn list_semver_tags(&self, repo_url: &str, _repo_type: &str) -> Result<Vec<Version>, DepError> {
        let normalized = super::url::normalize_url(repo_url);
        if let Some(tags) = self.tags.get(&normalized) {
            return Ok(tags.clone());
        }
        if let Some(tags) = self.tags.get(repo_url) {
            return Ok(tags.clone());
        }
        Ok(vec![])
    }
}

/// Mock fetcher that writes pre-configured files to the destination directory.
#[cfg(test)]
pub struct MockFetcher {
    /// Map from normalized URL to (commit_hash, files) where files is (relative_path, content).
    pub repos: std::collections::HashMap<String, (String, Vec<(String, String)>)>,
}

#[cfg(test)]
impl MockFetcher {
    pub fn new() -> Self {
        Self {
            repos: std::collections::HashMap::new(),
        }
    }

    /// Register a mock repo with its files.
    pub fn add_repo(&mut self, normalized_url: &str, commit: &str, files: Vec<(&str, &str)>) {
        self.repos.insert(
            normalized_url.to_string(),
            (
                commit.to_string(),
                files
                    .into_iter()
                    .map(|(p, c)| (p.to_string(), c.to_string()))
                    .collect(),
            ),
        );
    }
}

#[cfg(test)]
impl RepoFetcher for MockFetcher {
    fn fetch(
        &self,
        url: &str,
        _repo_type: &str,
        _tag: &str,
        dest: &Path,
    ) -> Result<String, DepError> {
        let normalized = super::url::normalize_url(url);
        let (commit, files) = self.repos.get(&normalized).ok_or_else(|| {
            DepError::ParseError(format!(
                "MockFetcher: no repo configured for '{}'",
                normalized
            ))
        })?;

        std::fs::create_dir_all(dest)?;
        for (rel_path, content) in files {
            let file_path = dest.join(rel_path);
            if let Some(parent) = file_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            std::fs::write(&file_path, content)?;
        }

        Ok(commit.clone())
    }
}

// ---------------------------------------------------------------------------
// Unit tests for output parsing
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_git_ls_remote_basic() {
        let output = "\
abc123\trefs/tags/v1.0.0
def456\trefs/tags/v1.0.0^{}
789abc\trefs/tags/v2.0.0
111222\trefs/tags/v2.0.0^{}
333444\trefs/tags/not-semver
555666\trefs/tags/v1.0.0-beta.1
";
        let tags = parse_git_ls_remote(output);
        assert_eq!(
            tags,
            vec![
                "v1.0.0".to_string(),
                "v2.0.0".to_string(),
                "not-semver".to_string(),
                "v1.0.0-beta.1".to_string(),
            ]
        );
    }

    #[test]
    fn parse_git_ls_remote_filters_derefs() {
        let output = "abc123\trefs/tags/v1.0.0^{}\n";
        let tags = parse_git_ls_remote(output);
        assert!(tags.is_empty());
    }

    #[test]
    fn parse_git_ls_remote_empty() {
        let tags = parse_git_ls_remote("");
        assert!(tags.is_empty());
    }

    #[test]
    fn parse_git_ls_remote_malformed_lines() {
        let output = "no-tab-here\nstill-no-tab\n";
        let tags = parse_git_ls_remote(output);
        assert!(tags.is_empty());
    }

    #[test]
    fn parse_git_ls_remote_with_semver_filter() {
        let output = "\
abc123\trefs/tags/v1.0.0
def456\trefs/tags/v2.1.3
789abc\trefs/tags/not-a-version
111222\trefs/tags/v1.0.0-beta.1
";
        let tags = parse_git_ls_remote(output);
        let versions = filter_semver_tags(&tags);
        assert_eq!(versions, vec![Version::new(1, 0, 0), Version::new(2, 1, 3)]);
    }

    #[test]
    fn parse_hg_tags_basic() {
        let output = "v1.0.0\nv2.0.0\ntip\n\nv1.0.0-beta.1\n";
        let tags = parse_hg_tags(output);
        assert_eq!(
            tags,
            vec![
                "v1.0.0".to_string(),
                "v2.0.0".to_string(),
                "v1.0.0-beta.1".to_string(),
            ]
        );
    }

    #[test]
    fn parse_hg_tags_empty() {
        let tags = parse_hg_tags("");
        assert!(tags.is_empty());
    }

    #[test]
    fn parse_hg_tags_only_tip() {
        let tags = parse_hg_tags("tip\n");
        assert!(tags.is_empty());
    }

    #[test]
    fn parse_hg_tags_with_semver_filter() {
        let output = "v1.0.0\nv2.1.3\nnot-a-version\nv1.0.0-beta.1\n";
        let tags = parse_hg_tags(output);
        let versions = filter_semver_tags(&tags);
        assert_eq!(versions, vec![Version::new(1, 0, 0), Version::new(2, 1, 3)]);
    }

    #[test]
    fn mock_fetcher_creates_files() {
        let mut fetcher = MockFetcher::new();
        fetcher.add_repo(
            "github.com/org/lib",
            "abc123",
            vec![("lib.ucg", "let x = 1;"), ("sub/other.ucg", "let y = 2;")],
        );

        let dest = std::env::temp_dir().join("ucg_test_mock_fetch");
        let _ = std::fs::remove_dir_all(&dest);

        let commit = fetcher
            .fetch("https://github.com/org/lib", "git", "v1.0.0", &dest)
            .unwrap();

        assert_eq!(commit, "abc123");
        assert!(dest.join("lib.ucg").exists());
        assert!(dest.join("sub/other.ucg").exists());
        assert_eq!(
            std::fs::read_to_string(dest.join("lib.ucg")).unwrap(),
            "let x = 1;"
        );

        let _ = std::fs::remove_dir_all(&dest);
    }
}
