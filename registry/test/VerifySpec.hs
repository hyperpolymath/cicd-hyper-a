{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | Tests for Verify module

module VerifySpec (spec) where

import Test.Hspec
import CicdHyperA.Ruleset
import CicdHyperA.Verify

spec :: Spec
spec = do
  describe "verifyRule" $ do
    it "accepts valid preventive rules" $ do
      verifyRule requireDependabot `shouldBe` Verified

    it "accepts valid curative rules" $ do
      verifyRule pinGitHubActions `shouldBe` Verified

  describe "verifyRuleset" $ do
    it "verifies multiple valid rules" $ do
      let rules = [requireDependabot, requireSecurityMd]
      verifyRuleset rules `shouldBe` Verified

  describe "checkNoConflicts" $ do
    it "finds no conflicts in pre-built rules" $ do
      let rules = [requireDependabot, requireSecurityMd, blockTypeScript]
      checkNoConflicts rules `shouldBe` Verified

  describe "QuickCheck properties" $ do
    it "rules are idempotent" $ do
      prop_ruleIdempotent requireDependabot `shouldBe` True

    it "fixes resolve issues" $ do
      prop_fixResolves pinGitHubActions `shouldBe` True
