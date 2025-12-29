{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | Property-based verification for CI/CD rules

module CicdHyperA.Verify
  ( -- * Verification results
    VerifyResult(..)
  , VerifyError(..)
    -- * Rule verification
  , verifyRule
  , verifyRuleset
    -- * Property checks
  , checkNoConflicts
  , checkCoverage
  , checkCompleteness
    -- * QuickCheck properties
  , prop_ruleIdempotent
  , prop_fixResolves
  , prop_noRuleConflicts
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import CicdHyperA.Ruleset

-- | Result of verification
data VerifyResult
  = Verified
  | VerifyFailed [VerifyError]
  deriving (Show, Eq)

-- | Verification errors
data VerifyError
  = ConflictingRules RuleName RuleName Text
  | IncompleteCoverage Text [Text]
  | CircularDependency [RuleName]
  | UnreachableRule RuleName
  | InvalidPattern RuleName Text
  deriving (Show, Eq)

-- | Verify a single rule
verifyRule :: Rule e -> VerifyResult
verifyRule rule = case rule of
  PreventiveRule name cond action ->
    if validCondition cond && validAction action
    then Verified
    else VerifyFailed [InvalidPattern name "Invalid condition or action"]
  CurativeRule name pattern fix ->
    if validPattern pattern && validFix fix
    then Verified
    else VerifyFailed [InvalidPattern name "Invalid pattern or fix"]
  DiagnosticRule name check report ->
    if validCheck check && validReport report
    then Verified
    else VerifyFailed [InvalidPattern name "Invalid check or report"]

-- | Verify a set of rules
verifyRuleset :: [Rule e] -> VerifyResult
verifyRuleset rules =
  let results = map verifyRule rules
      errors = concatMap extractErrors results
  in if null errors
     then Verified
     else VerifyFailed errors
  where
    extractErrors Verified = []
    extractErrors (VerifyFailed errs) = errs

-- | Check for conflicting rules
checkNoConflicts :: [Rule e] -> VerifyResult
checkNoConflicts _ = Verified  -- TODO: Implement conflict detection

-- | Check coverage of alert types
checkCoverage :: [Rule e] -> Set Text -> VerifyResult
checkCoverage rules alertTypes =
  let covered = Set.fromList $ map getRuleName rules
      uncovered = Set.difference alertTypes covered
  in if Set.null uncovered
     then Verified
     else VerifyFailed [IncompleteCoverage "Alert types" (Set.toList uncovered)]
  where
    getRuleName :: Rule e -> Text
    getRuleName (PreventiveRule n _ _) = n
    getRuleName (CurativeRule n _ _) = n
    getRuleName (DiagnosticRule n _ _) = n

-- | Check completeness of fix suggestions
checkCompleteness :: [Rule e] -> VerifyResult
checkCompleteness _ = Verified  -- TODO: Implement completeness check

-- ============================================================
-- QuickCheck Properties
-- ============================================================

-- | Property: Applying a rule twice has same effect as once
prop_ruleIdempotent :: Rule e -> Bool
prop_ruleIdempotent _ = True  -- TODO: Implement with actual rule application

-- | Property: Fix actually resolves the issue
prop_fixResolves :: Rule 'Curative -> Bool
prop_fixResolves _ = True  -- TODO: Implement with actual fix verification

-- | Property: No two rules conflict
prop_noRuleConflicts :: Rule e -> Rule e -> Bool
prop_noRuleConflicts _ _ = True  -- TODO: Implement conflict detection

-- ============================================================
-- Validation Helpers
-- ============================================================

validCondition :: Condition -> Bool
validCondition (FileExists _) = True
validCondition (FileContains _ _) = True
validCondition (FileExtension _ _) = True
validCondition (LanguageUsed _) = True
validCondition (DependencyPresent _) = True
validCondition (WorkflowHas _) = True
validCondition (And c1 c2) = validCondition c1 && validCondition c2
validCondition (Or c1 c2) = validCondition c1 && validCondition c2
validCondition (Not c) = validCondition c

validAction :: Action -> Bool
validAction (InjectFile _ _) = True
validAction (RejectCommit _) = True
validAction (RequireApproval _) = True
validAction (AddToWorkflow _) = True
validAction (Alert _ _) = True

validPattern :: Pattern -> Bool
validPattern p = not (T.null p)

validFix :: Fix -> Bool
validFix (ReplaceInFile _ _ _) = True
validFix (DeleteFile _) = True
validFix (AddFile _ _) = True
validFix (RunCommand _) = True
validFix (CreatePR _ _) = True

validCheck :: Check -> Bool
validCheck (CheckFileExists _) = True
validCheck (CheckPattern _ _) = True
validCheck (CheckCommand _) = True
validCheck (CheckApi _) = True

validReport :: Report -> Bool
validReport ReportToConsole = True
validReport (ReportToFile _) = True
validReport (ReportToApi _) = True
validReport ReportToPR = True
