use std::fmt;

#[derive(Debug)]
pub enum DepError {
    ReservedVendorName(String),
    DuplicateUrl(String),
    InvalidRepoType(String),
    ConflictingRepoTypes {
        url: String,
        type1: String,
        type2: String,
    },
    InvalidVersionConstraint(String),
    MajorVersionConflict {
        url: String,
        major1: u64,
        major2: u64,
    },
    ParseError(String),
    IoError(std::io::Error),
}

impl fmt::Display for DepError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DepError::ReservedVendorName(name) => {
                write!(f, "vendor name '{}' is reserved", name)
            }
            DepError::DuplicateUrl(url) => {
                write!(f, "duplicate dependency URL after normalization: {}", url)
            }
            DepError::InvalidRepoType(t) => {
                write!(f, "invalid repository type '{}': must be 'git' or 'hg'", t)
            }
            DepError::ConflictingRepoTypes { url, type1, type2 } => {
                write!(
                    f,
                    "conflicting repository types for '{}': '{}' vs '{}'",
                    url, type1, type2
                )
            }
            DepError::InvalidVersionConstraint(s) => {
                write!(f, "invalid version constraint: {}", s)
            }
            DepError::MajorVersionConflict {
                url,
                major1,
                major2,
            } => {
                write!(
                    f,
                    "major version conflict for '{}': v{} vs v{}",
                    url, major1, major2
                )
            }
            DepError::ParseError(msg) => write!(f, "parse error: {}", msg),
            DepError::IoError(e) => write!(f, "IO error: {}", e),
        }
    }
}

impl std::error::Error for DepError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            DepError::IoError(e) => Some(e),
            _ => None,
        }
    }
}

impl From<std::io::Error> for DepError {
    fn from(e: std::io::Error) -> Self {
        DepError::IoError(e)
    }
}
