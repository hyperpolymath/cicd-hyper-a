// SPDX-License-Identifier: AGPL-3.0-or-later
//! Common forge traits and types

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use crate::error::Result;

/// Supported forge types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Forge {
    GitHub,
    GitLab,
    Bitbucket,
    Codeberg,
    Sourcehut,
    Gitea,
    Radicle,
}

impl std::fmt::Display for Forge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Forge::GitHub => write!(f, "github"),
            Forge::GitLab => write!(f, "gitlab"),
            Forge::Bitbucket => write!(f, "bitbucket"),
            Forge::Codeberg => write!(f, "codeberg"),
            Forge::Sourcehut => write!(f, "sourcehut"),
            Forge::Gitea => write!(f, "gitea"),
            Forge::Radicle => write!(f, "radicle"),
        }
    }
}

/// Repository information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Repository {
    pub id: String,
    pub name: String,
    pub owner: String,
    pub forge: Forge,
    pub url: String,
    pub visibility: Visibility,
    pub default_branch: String,
    pub languages: Vec<String>,
}

/// Repository visibility
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Visibility {
    Public,
    Private,
    Internal,
}

/// Security alert
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Alert {
    pub id: String,
    pub rule_id: String,
    pub severity: Severity,
    pub category: AlertCategory,
    pub description: String,
    pub file: Option<String>,
    pub line: Option<u32>,
    pub auto_fixable: bool,
}

/// Alert severity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Info,
    Low,
    Medium,
    High,
    Critical,
}

/// Alert category
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum AlertCategory {
    WorkflowSecurity,
    CodeSecurity,
    CodeQuality,
    DependencyVuln,
    ProcessHygiene,
    MissingTests,
}

/// Workflow information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Workflow {
    pub id: String,
    pub name: String,
    pub file: String,
    pub state: WorkflowState,
}

/// Workflow state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum WorkflowState {
    Active,
    Disabled,
    Unknown,
}

/// Common forge adapter trait
#[async_trait]
pub trait ForgeAdapter: Send + Sync {
    /// Get forge type
    fn forge(&self) -> Forge;

    /// List repositories for an organization/user
    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>>;

    /// Get security alerts for a repository
    async fn get_alerts(&self, owner: &str, repo: &str) -> Result<Vec<Alert>>;

    /// List workflows for a repository
    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>>;

    /// Deploy a workflow file
    async fn deploy_workflow(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
        content: &str,
        message: &str,
    ) -> Result<()>;

    /// Enable branch protection
    async fn enable_branch_protection(&self, owner: &str, repo: &str, branch: &str) -> Result<()>;

    /// Create a pull request
    async fn create_pr(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        head: &str,
        base: &str,
    ) -> Result<String>;

    /// Trigger a workflow run
    async fn trigger_workflow(
        &self,
        owner: &str,
        repo: &str,
        workflow: &str,
        ref_name: &str,
    ) -> Result<()>;
}
