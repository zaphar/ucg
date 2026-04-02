use std::process::Command;

use semver::Version;

use super::error::DepError;
use super::resolve::filter_semver_tags;

/// Trait for listing semver tags from a repository.
pub trait TagSource {
    fn list_semver_tags(&self, repo_url: &str, repo_type: &str) -> Result<Vec<Version>, DepError>;
}

/// Lists tags by shelling out to git/hg.
pub struct RemoteTagSource;

impl TagSource for RemoteTagSource {
    fn list_semver_tags(&self, repo_url: &str, repo_type: &str) -> Result<Vec<Version>, DepError> {
        match repo_type {
            "git" => list_git_tags(repo_url),
            "hg" => list_hg_tags(repo_url),
            _ => Err(DepError::InvalidRepoType(repo_type.to_string())),
        }
    }
}

fn list_git_tags(repo_url: &str) -> Result<Vec<Version>, DepError> {
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

    let stdout = String::from_utf8_lossy(&output.stdout);
    let tags: Vec<String> = stdout
        .lines()
        .filter_map(|line| {
            // Format: <hash>\trefs/tags/<tagname>
            let parts: Vec<&str> = line.split('\t').collect();
            if parts.len() >= 2 {
                let refname = parts[1];
                let tag = refname.strip_prefix("refs/tags/")?;
                // Skip dereferenced tags (^{})
                if tag.ends_with("^{}") {
                    return None;
                }
                Some(tag.to_string())
            } else {
                None
            }
        })
        .collect();

    Ok(filter_semver_tags(&tags))
}

fn list_hg_tags(repo_url: &str) -> Result<Vec<Version>, DepError> {
    // hg tags requires a local repo, so we use hg identify to list tags remotely
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

    let stdout = String::from_utf8_lossy(&output.stdout);
    let tags: Vec<String> = stdout
        .lines()
        .map(|line| line.trim().to_string())
        .filter(|t| !t.is_empty() && t != "tip")
        .collect();

    Ok(filter_semver_tags(&tags))
}

/// Mock tag source for testing.
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
        // Try normalized URL first, then raw URL
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
