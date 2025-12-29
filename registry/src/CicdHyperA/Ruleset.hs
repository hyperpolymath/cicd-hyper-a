{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | Type-safe ruleset DSL for cicd-hyper-a

module CicdHyperA.Ruleset
  ( -- * Effect types
    Effect(..)
    -- * Rule types
  , Rule(..)
  , RuleName
  , Condition(..)
  , Action(..)
  , Pattern
  , Fix(..)
  , Check(..)
  , Report(..)
    -- * Severity
  , Severity(..)
    -- * Pre-built rules
  , requireDependabot
  , requireSecurityMd
  , blockTypeScript
  , blockGolang
  , pinGitHubActions
  , requireWorkflowPermissions
  , requireSpdxHeader
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Rule effect types
data Effect = Preventive | Curative | Diagnostic
  deriving (Show, Eq)

-- | Severity levels (from 1922 alert analysis)
data Severity = Critical | High | Medium | Low | Info
  deriving (Show, Eq, Ord)

type RuleName = Text
type Pattern = Text
type FilePath' = Text

-- | Type-safe rule definition
data Rule (e :: Effect) where
  PreventiveRule :: RuleName -> Condition -> Action -> Rule 'Preventive
  CurativeRule   :: RuleName -> Pattern -> Fix -> Rule 'Curative
  DiagnosticRule :: RuleName -> Check -> Report -> Rule 'Diagnostic

deriving instance Show (Rule e)

-- | Condition language for preventive rules
data Condition
  = FileExists FilePath'
  | FileContains FilePath' Pattern
  | FileExtension FilePath' Text
  | LanguageUsed Language
  | DependencyPresent Text
  | WorkflowHas Pattern
  | And Condition Condition
  | Or Condition Condition
  | Not Condition
  deriving (Show, Eq)

-- | Languages
data Language
  = TypeScript | JavaScript | Golang | Python | Rust
  | ReScript | Gleam | Julia | Haskell | Logtalk
  | Nickel | Guile | OCaml | Ada
  deriving (Show, Eq)

-- | Actions for preventive rules
data Action
  = InjectFile FilePath' Text
  | RejectCommit Text
  | RequireApproval Text
  | AddToWorkflow Text
  | Alert Severity Text
  deriving (Show, Eq)

-- | Fixes for curative rules
data Fix
  = ReplaceInFile FilePath' Pattern Text
  | DeleteFile FilePath'
  | AddFile FilePath' Text
  | RunCommand Text
  | CreatePR Text Text  -- title, body
  deriving (Show, Eq)

-- | Checks for diagnostic rules
data Check
  = CheckFileExists FilePath'
  | CheckPattern FilePath' Pattern
  | CheckCommand Text
  | CheckApi Text
  deriving (Show, Eq)

-- | Reports for diagnostic rules
data Report
  = ReportToConsole
  | ReportToFile FilePath'
  | ReportToApi Text
  | ReportToPR
  deriving (Show, Eq)

-- ============================================================
-- Pre-built Rules (distilled from 228 repos)
-- ============================================================

-- | Require dependabot.yml for repos with dependencies
requireDependabot :: Rule 'Preventive
requireDependabot = PreventiveRule
  "require-dependabot"
  (Not (FileExists ".github/dependabot.yml"))
  (InjectFile ".github/dependabot.yml" dependabotTemplate)

-- | Require SECURITY.md for public repos
requireSecurityMd :: Rule 'Preventive
requireSecurityMd = PreventiveRule
  "require-security-md"
  (Not (FileExists "SECURITY.md"))
  (InjectFile "SECURITY.md" securityMdTemplate)

-- | Block TypeScript (RSR policy)
blockTypeScript :: Rule 'Preventive
blockTypeScript = PreventiveRule
  "block-typescript"
  (Or (LanguageUsed TypeScript) (FileExtension "*" ".ts"))
  (RejectCommit "TypeScript not allowed per RSR policy. Use ReScript instead.")

-- | Block Golang (RSR policy)
blockGolang :: Rule 'Preventive
blockGolang = PreventiveRule
  "block-golang"
  (Or (LanguageUsed Golang) (FileExtension "*" ".go"))
  (RejectCommit "Go not allowed per RSR policy. Use Rust instead.")

-- | Pin GitHub Actions to SHA
pinGitHubActions :: Rule 'Curative
pinGitHubActions = CurativeRule
  "pin-github-actions"
  "uses:\\s+([^@]+)@(v\\d+|main|master)"
  (ReplaceInFile ".github/workflows/*.yml"
    "uses: $1@$2"
    "uses: $1@SHA # $2")

-- | Require workflow permissions
requireWorkflowPermissions :: Rule 'Preventive
requireWorkflowPermissions = PreventiveRule
  "require-workflow-permissions"
  (And
    (FileExists ".github/workflows/*.yml")
    (Not (WorkflowHas "permissions:")))
  (AddToWorkflow "permissions: read-all")

-- | Require SPDX headers
requireSpdxHeader :: Rule 'Preventive
requireSpdxHeader = PreventiveRule
  "require-spdx-header"
  (Not (FileContains "*" "SPDX-License-Identifier"))
  (Alert Medium "File missing SPDX license header")

-- ============================================================
-- Templates
-- ============================================================

dependabotTemplate :: Text
dependabotTemplate = T.unlines
  [ "# SPDX-License-Identifier: AGPL-3.0-or-later"
  , "version: 2"
  , "updates:"
  , "  - package-ecosystem: \"github-actions\""
  , "    directory: \"/\""
  , "    schedule:"
  , "      interval: \"weekly\""
  ]

securityMdTemplate :: Text
securityMdTemplate = T.unlines
  [ "# Security Policy"
  , ""
  , "## Reporting a Vulnerability"
  , ""
  , "Please report security vulnerabilities to security@hyperpolymath.dev"
  , ""
  , "We will respond within 48 hours and provide a fix within 7 days for critical issues."
  ]
