// SPDX-License-Identifier: AGPL-3.0-or-later
//! GitLab forge adapter

use async_trait::async_trait;
use reqwest::header::{HeaderMap, HeaderValue, CONTENT_TYPE};
use serde::Deserialize;
use crate::error::{AdapterError, Result};
use crate::forge::{
    Alert, Forge, ForgeAdapter, Repository, Severity, Visibility, Workflow, WorkflowState,
    AlertCategory,
};

/// GitLab adapter configuration
pub struct GitLabAdapter {
    client: reqwest::Client,
    base_url: String,
}

impl GitLabAdapter {
    /// Create new GitLab adapter
    pub fn new(token: &str) -> Result<Self> {
        Self::with_base_url(token, "https://gitlab.com/api/v4")
    }

    /// Create with custom base URL (for self-hosted GitLab)
    pub fn with_base_url(token: &str, base_url: &str) -> Result<Self> {
        let mut headers = HeaderMap::new();
        headers.insert(
            "PRIVATE-TOKEN",
            HeaderValue::from_str(token)
                .map_err(|_| AdapterError::ConfigError("Invalid token".into()))?,
        );
        headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));

        let client = reqwest::Client::builder()
            .default_headers(headers)
            .build()?;

        Ok(Self {
            client,
            base_url: base_url.trim_end_matches('/').to_string(),
        })
    }

    fn encode_path(path: &str) -> String {
        urlencoding::encode(path).to_string()
    }
}

#[async_trait]
impl ForgeAdapter for GitLabAdapter {
    fn forge(&self) -> Forge {
        Forge::GitLab
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        let url = format!(
            "{}/groups/{}/projects?per_page=100",
            self.base_url,
            Self::encode_path(owner)
        );
        let response: Vec<GitLabProject> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|p| Repository {
                id: p.id.to_string(),
                name: p.path,
                owner: owner.to_string(),
                forge: Forge::GitLab,
                url: p.web_url,
                visibility: match p.visibility.as_str() {
                    "public" => Visibility::Public,
                    "internal" => Visibility::Internal,
                    _ => Visibility::Private,
                },
                default_branch: p.default_branch.unwrap_or_else(|| "main".to_string()),
                languages: vec![],
            })
            .collect())
    }

    async fn get_alerts(&self, owner: &str, repo: &str) -> Result<Vec<Alert>> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/vulnerability_findings",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let response: Vec<GitLabVulnerability> = match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => resp.json().await?,
            Ok(resp) if resp.status().as_u16() == 404 => return Ok(vec![]),
            Ok(resp) => {
                return Err(AdapterError::ApiError(format!(
                    "HTTP {}",
                    resp.status()
                )))
            }
            Err(e) => return Err(e.into()),
        };

        Ok(response
            .into_iter()
            .map(|v| Alert {
                id: v.id.to_string(),
                rule_id: v.scanner.id,
                severity: match v.severity.as_str() {
                    "critical" => Severity::Critical,
                    "high" => Severity::High,
                    "medium" => Severity::Medium,
                    "low" => Severity::Low,
                    _ => Severity::Info,
                },
                category: AlertCategory::CodeSecurity,
                description: v.name,
                file: v.location.file,
                line: v.location.start_line,
                auto_fixable: false,
            })
            .collect())
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>> {
        // GitLab uses .gitlab-ci.yml, not GitHub Actions
        // Return a single "workflow" representing the CI configuration
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/repository/files/.gitlab-ci.yml?ref=main",
            self.base_url,
            Self::encode_path(&project_path)
        );

        match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => Ok(vec![Workflow {
                id: "gitlab-ci".to_string(),
                name: "GitLab CI".to_string(),
                file: ".gitlab-ci.yml".to_string(),
                state: WorkflowState::Active,
            }]),
            _ => Ok(vec![]),
        }
    }

    async fn deploy_workflow(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
        content: &str,
        message: &str,
    ) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/repository/files/{}",
            self.base_url,
            Self::encode_path(&project_path),
            Self::encode_path(path)
        );

        let body = serde_json::json!({
            "branch": "main",
            "content": content,
            "commit_message": message
        });

        // Try PUT first (update), then POST (create)
        let response = self.client.put(&url).json(&body).send().await?;

        if response.status().as_u16() == 404 {
            let response = self.client.post(&url).json(&body).send().await?;
            if !response.status().is_success() {
                return Err(AdapterError::ApiError(format!(
                    "Failed to create file: HTTP {}",
                    response.status()
                )));
            }
        } else if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to update file: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    async fn enable_branch_protection(
        &self,
        owner: &str,
        repo: &str,
        branch: &str,
    ) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/protected_branches",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let body = serde_json::json!({
            "name": branch,
            "push_access_level": 40,  // Maintainers
            "merge_access_level": 40,
            "allow_force_push": false
        });

        let response = self.client.post(&url).json(&body).send().await?;

        if !response.status().is_success() && response.status().as_u16() != 409 {
            return Err(AdapterError::ApiError(format!(
                "Failed to protect branch: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    async fn create_pr(
        &self,
        owner: &str,
        repo: &str,
        title: &str,
        body: &str,
        head: &str,
        base: &str,
    ) -> Result<String> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/merge_requests",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let request_body = serde_json::json!({
            "source_branch": head,
            "target_branch": base,
            "title": title,
            "description": body
        });

        let response: GitLabMergeRequest = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(response.web_url)
    }

    async fn trigger_workflow(
        &self,
        owner: &str,
        repo: &str,
        _workflow: &str,
        ref_name: &str,
    ) -> Result<()> {
        let project_path = format!("{}/{}", owner, repo);
        let url = format!(
            "{}/projects/{}/pipeline",
            self.base_url,
            Self::encode_path(&project_path)
        );

        let body = serde_json::json!({
            "ref": ref_name
        });

        let response = self.client.post(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to trigger pipeline: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }
}

// GitLab API response types
#[derive(Debug, Deserialize)]
struct GitLabProject {
    id: u64,
    path: String,
    web_url: String,
    visibility: String,
    default_branch: Option<String>,
}

#[derive(Debug, Deserialize)]
struct GitLabVulnerability {
    id: u64,
    name: String,
    severity: String,
    scanner: GitLabScanner,
    location: GitLabLocation,
}

#[derive(Debug, Deserialize)]
struct GitLabScanner {
    id: String,
}

#[derive(Debug, Deserialize)]
struct GitLabLocation {
    file: Option<String>,
    start_line: Option<u32>,
}

#[derive(Debug, Deserialize)]
struct GitLabMergeRequest {
    web_url: String,
}
