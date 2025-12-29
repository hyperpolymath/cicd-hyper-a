// SPDX-License-Identifier: AGPL-3.0-or-later
//! Multi-forge adapters for cicd-hyper-a

pub mod forge;
pub mod github;
pub mod gitlab;
pub mod bitbucket;
pub mod error;

pub use forge::{Forge, ForgeAdapter, Repository, Alert, Workflow};
pub use error::AdapterError;
