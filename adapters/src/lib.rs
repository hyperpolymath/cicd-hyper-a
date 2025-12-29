// SPDX-License-Identifier: AGPL-3.0-or-later
//! Multi-forge adapters for cicd-hyper-a
//!
//! Provides forge adapters (GitHub, GitLab, Bitbucket) integrated with
//! the data layer (ArangoDB, Dragonfly) via the service module.

pub mod forge;
pub mod github;
pub mod gitlab;
pub mod bitbucket;
pub mod error;
pub mod service;

pub use forge::{Forge, ForgeAdapter, Repository, Alert, Workflow};
pub use error::AdapterError;
pub use service::{ForgeService, SyncResult, ServiceHealth};
