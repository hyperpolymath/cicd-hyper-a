-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | Test suite entry point

module Main where

import Test.Hspec
import qualified RulesetSpec
import qualified VerifySpec

main :: IO ()
main = hspec $ do
  describe "Ruleset" RulesetSpec.spec
  describe "Verify" VerifySpec.spec
