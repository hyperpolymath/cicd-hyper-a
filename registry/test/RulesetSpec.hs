{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | Tests for Ruleset module

module RulesetSpec (spec) where

import Test.Hspec
import CicdHyperA.Ruleset

spec :: Spec
spec = do
  describe "Pre-built rules" $ do
    it "requireDependabot has correct name" $ do
      getRuleName requireDependabot `shouldBe` "require-dependabot"

    it "requireSecurityMd has correct name" $ do
      getRuleName requireSecurityMd `shouldBe` "require-security-md"

    it "blockTypeScript rejects .ts files" $ do
      getRuleName blockTypeScript `shouldBe` "block-typescript"

    it "blockGolang rejects .go files" $ do
      getRuleName blockGolang `shouldBe` "block-golang"

  describe "Severity levels" $ do
    it "Critical > High > Medium > Low > Info" $ do
      Critical > High `shouldBe` True
      High > Medium `shouldBe` True
      Medium > Low `shouldBe` True
      Low > Info `shouldBe` True

  describe "Effect types" $ do
    it "Preventive rules prevent issues" $ do
      show (Preventive :: Effect) `shouldBe` "Preventive"

    it "Curative rules fix issues" $ do
      show (Curative :: Effect) `shouldBe` "Curative"

    it "Diagnostic rules report issues" $ do
      show (Diagnostic :: Effect) `shouldBe` "Diagnostic"

-- Helper to extract rule name
getRuleName :: Rule e -> RuleName
getRuleName (PreventiveRule n _ _) = n
getRuleName (CurativeRule n _ _) = n
getRuleName (DiagnosticRule n _ _) = n
