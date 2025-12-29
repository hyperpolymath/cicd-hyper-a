{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | Tests for API module

module APISpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import CicdHyperA.API
import CicdHyperA.Registry (initRegistry)
import System.IO.Temp (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "Request/Response serialization" $ do
    it "serializes DepositRequest" $ do
      let req = DepositRequest
            { depositName = "test-rule"
            , depositDescription = "Test description"
            , depositContent = "-- Haskell code"
            , depositEffect = "preventive"
            , depositSign = True
            , depositVerify = True
            }
      (decode . encode $ req) `shouldBe` Just req

    it "serializes DepositResponse" $ do
      let resp = DepositResponse
            { depositSuccess = True
            , depositVersion = "1.0.0"
            , depositSignature = Just "sig:test"
            , depositVerified = True
            , depositErrors = []
            }
      (decode . encode $ resp) `shouldBe` Just resp

    it "serializes SearchRequest" $ do
      let req = SearchRequest
            { searchEffect = Just "curative"
            , searchLanguage = Just "rust"
            , searchCategory = Nothing
            , searchQuery = Just "security"
            , searchLimit = Just 10
            , searchOffset = Nothing
            }
      (decode . encode $ req) `shouldBe` Just req

    it "serializes HealthResponse" $ do
      let resp = HealthResponse
            { healthStatus = "healthy"
            , healthVersion = "0.1.0"
            , healthUptime = 3600
            , healthChecks =
                [ HealthCheck "arangodb" "pass" Nothing (Just 5)
                , HealthCheck "dragonfly" "pass" Nothing (Just 1)
                ]
            }
      (decode . encode $ resp) `shouldBe` Just resp

  describe "Deposit handler" $ do
    it "validates empty name" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "" "desc" "content" "preventive" False False
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` False
        depositErrors resp `shouldContain` ["Name is required"]

    it "validates empty content" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "test" "desc" "" "preventive" False False
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` False
        depositErrors resp `shouldContain` ["Content is required"]

    it "validates effect type" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "test" "desc" "content" "invalid" False False
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` False

    it "accepts valid deposit" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = DepositRequest "test-rule" "desc" "content" "preventive" False False
        resp <- depositHandler reg req
        depositSuccess resp `shouldBe` True
        depositVersion resp `shouldBe` "1.0.0"

  describe "Withdraw handler" $ do
    it "returns failure for unknown ruleset" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = WithdrawRequest "nonexistent" Nothing False
        resp <- withdrawHandler reg req
        withdrawSuccess resp `shouldBe` False

  describe "Search handler" $ do
    it "returns empty for empty registry" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = SearchRequest Nothing Nothing Nothing Nothing Nothing Nothing
        resp <- searchHandler reg req
        searchTotal resp `shouldBe` 0
        searchResults resp `shouldBe` []

    it "respects limit parameter" $ do
      withSystemTempDirectory "test-registry" $ \tmpDir -> do
        reg <- initRegistry tmpDir
        let req = SearchRequest Nothing Nothing Nothing Nothing (Just 5) Nothing
        resp <- searchHandler reg req
        length (searchResults resp) `shouldSatisfy` (<= 5)

  describe "Health handler" $ do
    it "returns healthy status" $ do
      resp <- healthHandler
      healthStatus resp `shouldBe` "healthy"
      healthVersion resp `shouldBe` "0.1.0"

    it "includes dependency checks" $ do
      resp <- healthHandler
      length (healthChecks resp) `shouldBe` 2
      map checkName (healthChecks resp) `shouldContain` ["arangodb", "dragonfly"]
