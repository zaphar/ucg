use std::path::Path;
use std::process::Command;

use super::error::DepError;

/// Trait for fetching a repository at a specific tag.
pub trait RepoFetcher {
    /// Clone a repo at a specific tag into dest. Returns the commit hash.
    fn fetch(&self, url: &str, repo_type: &str, tag: &str, dest: &Path)
        -> Result<String, DepError>;
}

// ---------------------------------------------------------------------------
// Remote (real) implementations
// ---------------------------------------------------------------------------

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

fn fetch_git(url: &str, tag: &str, dest: &Path) -> Result<String, DepError> {
    let output = Command::new("git")
        .args([
            "clone",
            "--depth",
            "1",
            "--branch",
            tag,
            "--",
            url,
            &dest.to_string_lossy(),
        ])
        .output()
        .map_err(DepError::IoError)?;

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
        .map_err(DepError::IoError)?;

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

fn fetch_hg(url: &str, tag: &str, dest: &Path) -> Result<String, DepError> {
    let output = Command::new("hg")
        .args(["clone", "-r", tag, "--", url, &dest.to_string_lossy()])
        .output()
        .map_err(DepError::IoError)?;

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
        .map_err(DepError::IoError)?;

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
// Unit tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

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
