// SPDX-License-Identifier: AGPL-3.0-or-later
//! Bitbucket forge adapter

use async_trait::async_trait;
use reqwest::header::{HeaderMap, HeaderValue, AUTHORIZATION, CONTENT_TYPE};
use serde::Deserialize;
use crate::error::{AdapterError, Result};
use crate::forge::{
    Alert, Forge, ForgeAdapter, Repository, Visibility, Workflow, WorkflowState,
};

/// Bitbucket adapter configuration
pub struct BitbucketAdapter {
    client: reqwest::Client,
    base_url: String,
}

impl BitbucketAdapter {
    /// Create new Bitbucket adapter with app password
    pub fn new(username: &str, app_password: &str) -> Result<Self> {
        Self::with_base_url(username, app_password, "https://api.bitbucket.org/2.0")
    }

    /// Create with custom base URL (for Bitbucket Server)
    pub fn with_base_url(username: &str, app_password: &str, base_url: &str) -> Result<Self> {
        let auth = base64::Engine::encode(
            &base64::engine::general_purpose::STANDARD,
            format!("{}:{}", username, app_password).as_bytes(),
        );

        let mut headers = HeaderMap::new();
        headers.insert(
            AUTHORIZATION,
            HeaderValue::from_str(&format!("Basic {}", auth))
                .map_err(|_| AdapterError::ConfigError("Invalid credentials".into()))?,
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
}

#[async_trait]
impl ForgeAdapter for BitbucketAdapter {
    fn forge(&self) -> Forge {
        Forge::Bitbucket
    }

    async fn list_repos(&self, owner: &str) -> Result<Vec<Repository>> {
        let url = format!("{}/repositories/{}", self.base_url, owner);
        let response: BitbucketRepoList = self.client.get(&url).send().await?.json().await?;

        Ok(response
            .values
            .into_iter()
            .map(|r| Repository {
                id: r.uuid,
                name: r.slug,
                owner: owner.to_string(),
                forge: Forge::Bitbucket,
                url: r.links.html.href,
                visibility: if r.is_private {
                    Visibility::Private
                } else {
                    Visibility::Public
                },
                default_branch: r.mainbranch.map(|b| b.name).unwrap_or_else(|| "main".to_string()),
                languages: r.language.map(|l| vec![l]).unwrap_or_default(),
            })
            .collect())
    }

    async fn get_alerts(&self, _owner: &str, _repo: &str) -> Result<Vec<Alert>> {
        // Bitbucket doesn't have built-in code scanning like GitHub/GitLab
        // Would need integration with Snyk, SonarCloud, etc.
        Ok(vec![])
    }

    async fn list_workflows(&self, owner: &str, repo: &str) -> Result<Vec<Workflow>> {
        // Bitbucket uses bitbucket-pipelines.yml
        let url = format!(
            "{}/repositories/{}/{}/src/main/bitbucket-pipelines.yml",
            self.base_url, owner, repo
        );

        match self.client.get(&url).send().await {
            Ok(resp) if resp.status().is_success() => Ok(vec![Workflow {
                id: "pipelines".to_string(),
                name: "Bitbucket Pipelines".to_string(),
                file: "bitbucket-pipelines.yml".to_string(),
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
        let url = format!(
            "{}/repositories/{}/{}/src",
            self.base_url, owner, repo
        );

        // Bitbucket uses form data for file uploads
        let form = reqwest::multipart::Form::new()
            .text(path.to_string(), content.to_string())
            .text("message", message.to_string());

        let response = self.client.post(&url).multipart(form).send().await?;

        if !response.status().is_success() {
            return Err(AdapterError::ApiError(format!(
                "Failed to deploy workflow: HTTP {}",
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
        let url = format!(
            "{}/repositories/{}/{}/branch-restrictions",
            self.base_url, owner, repo
        );

        let body = serde_json::json!({
            "kind": "push",
            "branch_match_kind": "glob",
            "pattern": branch,
            "users": [],
            "groups": []
        });

        let response = self.client.post(&url).json(&body).send().await?;

        if !response.status().is_success() && response.status().as_u16() != 409 {
            return Err(AdapterError::ApiError(format!(
                "Failed to add branch restriction: HTTP {}",
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
        let url = format!(
            "{}/repositories/{}/{}/pullrequests",
            self.base_url, owner, repo
        );

        let request_body = serde_json::json!({
            "title": title,
            "description": body,
            "source": {
                "branch": {
                    "name": head
                }
            },
            "destination": {
                "branch": {
                    "name": base
                }
            }
        });

        let response: BitbucketPullRequest = self
            .client
            .post(&url)
            .json(&request_body)
            .send()
            .await?
            .json()
            .await?;

        Ok(response.links.html.href)
    }

    async fn trigger_workflow(
        &self,
        owner: &str,
        repo: &str,
        _workflow: &str,
        ref_name: &str,
    ) -> Result<()> {
        let url = format!(
            "{}/repositories/{}/{}/pipelines/",
            self.base_url, owner, repo
        );

        let body = serde_json::json!({
            "target": {
                "ref_type": "branch",
                "type": "pipeline_ref_target",
                "ref_name": ref_name
            }
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

// Bitbucket API response types
#[derive(Debug, Deserialize)]
struct BitbucketRepoList {
    values: Vec<BitbucketRepo>,
}

#[derive(Debug, Deserialize)]
struct BitbucketRepo {
    uuid: String,
    slug: String,
    is_private: bool,
    language: Option<String>,
    mainbranch: Option<BitbucketBranch>,
    links: BitbucketLinks,
}

#[derive(Debug, Deserialize)]
struct BitbucketBranch {
    name: String,
}

#[derive(Debug, Deserialize)]
struct BitbucketLinks {
    html: BitbucketLink,
}

#[derive(Debug, Deserialize)]
struct BitbucketLink {
    href: String,
}

#[derive(Debug, Deserialize)]
struct BitbucketPullRequest {
    links: BitbucketLinks,
}
