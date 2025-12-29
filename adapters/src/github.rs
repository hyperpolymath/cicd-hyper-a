// SPDX-License-Identifier: AGPL-3.0-or-later
//! GitHub forge adapter

use crate::error::{AdapterError, Result};
use crate::forge::{
    Alert, AlertCategory, Forge, ForgeAdapter, Repository, Severity, Visibility, Workflow,
    WorkflowState,
};
use async_trait::async_trait;
use reqwest::header::{HeaderMap, HeaderValue, ACCEPT, AUTHORIZATION, USER_AGENT};
use serde::Deserialize;

/// GitHub adapter configuration
pub struct GitHubAdapter {
    client: reqwest::Client,
    base_url: String,
    token: String,
}

impl GitHubAdapter {
    /// Create new GitHub adapter
    pub fn new(token: &str) -> Result<Self> {
        Self::with_base_url(token, "https://api.github.com")
    }

    /// Create with custom base URL (for GitHub Enterprise)
    pub fn with_base_url(token: &str, base_url: &str) -> Result<Self> {
        let mut headers = HeaderMap::new();
        headers.insert(
            ACCEPT,
            HeaderValue::from_static("application/vnd.github+json"),
        );
        headers.insert(
            AUTHORIZATION,
            HeaderValue::from_str(&format!("Bearer {}", token))
                .map_err(|_| AdapterError::ConfigError("Invalid token".into()))?,
        );
        headers.insert(USER_AGENT, HeaderValue::from_static("cicd-hyper-a/0.1.0"));
        headers.insert(
            "X-GitHub-Api-Version",
            HeaderValue::from_static("2022-11-28"),
        );

        let client = reqwest::Client::builder()
            .default_headers(headers)
            .build()?;

        Ok(Self {
            client,
            base_url: base_url.trim_end_matches('/').to_string(),
            token: token.to_string(),
        })
    }
}

#[async_trait]
impl ForgeAdapter for GitHubAdapter {
    fn forge(&self) -> Forge {
        Forge::GitHub
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        let url = format!("{}/orgs/{}/repos?per_page=100", self.base_url, owner);
        let response: Vec<GitHubRepo> = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .into_iter()
            .map(|r| Repository {
                id: r.id.to_string(),
                name: r.name,
                owner: owner.to_string(),
                forge: Forge::GitHub,
                url: r.html_url,
                visibility: if r.private {
                    Visibility::Private
                } else {
                    Visibility::Public
                },
                default_branch: r.default_branch,
                languages: vec![],
            })
            .collect())
    }

    async fn get_alerts(&self, owner: &str, repo: &str) -> Result<Vec<Alert>> {
        let url = format!(
            "{}/repos/{}/{}/code-scanning/alerts",
            self.base_url, owner, repo
        );
        let response: Vec<GitHubAlert> = match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => resp.json().await?,
            Ok(resp) if resp.status().as_u16() == 404 => return Ok(vec![]),
            Ok(resp) => return Err(AdapterError::ApiError(format!("HTTP {}", resp.status()))),
            Err(e) => return Err(e.into()),
        };

        Ok(response
            .into_iter()
            .map(|a| Alert {
                id: a.number.to_string(),
                rule_id: a.rule.id,
                severity: match a.rule.severity.as_str() {
                    "critical" | "error" => Severity::Critical,
                    "high" | "warning" => Severity::High,
                    "medium" => Severity::Medium,
                    "low" | "note" => Severity::Low,
                    _ => Severity::Info,
                },
                category: AlertCategory::CodeSecurity,
                description: a.rule.description,
                file: a.most_recent_instance.location.path,
                line: a.most_recent_instance.location.start_line,
                auto_fixable: false,
            })
            .collect())
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>> {
        let url = format!(
            "{}/repos/{}/{}/actions/workflows",
            self.base_url, owner, repo
        );
        let response: GitHubWorkflows = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .workflows
            .into_iter()
            .map(|w| Workflow {
                id: w.id.to_string(),
                name: w.name,
                file: w.path,
                state: match w.state.as_str() {
                    "active" => WorkflowState::Active,
                    "disabled" | "disabled_manually" => WorkflowState::Disabled,
                    _ => WorkflowState::Unknown,
                },
            })
            .collect())
    }

    async fn deploy_workflow(
        &self,
        owner: &str,
        repo: &str,
        path: &str,
        content: &str,
        message: &str,
    ) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/contents/{}",
            self.base_url, owner, repo, path
        );

        let encoded = base64::Engine::encode(
            &base64::engine::general_purpose::STANDARD,
            content.as_bytes(),
        );

        let body = serde_json::json!({
            "message": message,
            "content": encoded,
        });

        let response = self.client.put(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to deploy workflow: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }

    async fn enable_branch_protection(&self, owner: &str, repo: &str, branch: &str) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/branches/{}/protection",
            self.base_url, owner, repo, branch
        );

        let body = serde_json::json!({
            "required_status_checks": null,
            "enforce_admins": false,
            "required_pull_request_reviews": {
                "required_approving_review_count": 1
            },
            "restrictions": null,
            "allow_force_pushes": false,
            "allow_deletions": false
        });

        let response = self.client.put(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to enable branch protection: HTTP {}",
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
        let url = format!("{}/repos/{}/{}/pulls", self.base_url, owner, repo);

        let request_body = serde_json::json!({
            "title": title,
            "body": body,
            "head": head,
            "base": base
        });

        let response: GitHubPullRequest = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(response.html_url)
    }

    async fn trigger_workflow(
        &self,
        owner: &str,
        repo: &str,
        workflow: &str,
        ref_name: &str,
    ) -> Result<()> {
        let url = format!(
            "{}/repos/{}/{}/actions/workflows/{}/dispatches",
            self.base_url, owner, repo, workflow
        );

        let body = serde_json::json!({
            "ref": ref_name
        });

        let response = self.client.post(&url).json(&body).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to trigger workflow: HTTP {}",
                response.status()
            )));
        }

        Ok(())
    }
}

// GitHub API response types
#[derive(Debug, Deserialize)]
struct GitHubRepo {
    id: u64,
    name: String,
    html_url: String,
    private: bool,
    default_branch: String,
}

#[derive(Debug, Deserialize)]
struct GitHubAlert {
    number: u64,
    rule: GitHubAlertRule,
    most_recent_instance: GitHubAlertInstance,
}

#[derive(Debug, Deserialize)]
struct GitHubAlertRule {
    id: String,
    severity: String,
    description: String,
}

#[derive(Debug, Deserialize)]
struct GitHubAlertInstance {
    location: GitHubAlertLocation,
}

#[derive(Debug, Deserialize)]
struct GitHubAlertLocation {
    path: Option<String>,
    start_line: Option<u32>,
}

#[derive(Debug, Deserialize)]
struct GitHubWorkflows {
    workflows: Vec<GitHubWorkflow>,
}

#[derive(Debug, Deserialize)]
struct GitHubWorkflow {
    id: u64,
    name: String,
    path: String,
    state: String,
}

#[derive(Debug, Deserialize)]
struct GitHubPullRequest {
    html_url: String,
}
