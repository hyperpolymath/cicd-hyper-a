// SPDX-License-Identifier: AGPL-3.0-or-later
//! Multi-forge adapters for cicd-hyper-a
//!
//! Provides forge adapters (GitHub, GitLab, Bitbucket) integrated with
//! the data layer (ArangoDB, Dragonfly) via the service module.

pub mod bitbucket;
pub mod error;
pub mod forge;
pub mod github;
pub mod gitlab;
pub mod service;

pub use error::AdapterError;
pub use forge::{Alert, Forge, ForgeAdapter, Repository, Workflow};
pub use service::{ForgeService, ServiceHealth, SyncResult};
