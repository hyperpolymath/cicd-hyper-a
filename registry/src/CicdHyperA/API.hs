{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | REST API for the cicd-hyper-a registry
--
-- Provides endpoints for:
-- - Ruleset deposit/withdrawal
-- - Search and discovery
-- - Audit and provenance
-- - Health and metrics

module CicdHyperA.API
  ( -- * API type
    RegistryAPI
  , registryAPI
    -- * Server
  , runServer
  , app
    -- * Request/Response types
  , DepositRequest(..)
  , DepositResponse(..)
  , WithdrawRequest(..)
  , WithdrawResponse(..)
  , SearchRequest(..)
  , SearchResponse(..)
  , AuditResponse(..)
  , HealthResponse(..)
    -- * Handlers
  , depositHandler
  , withdrawHandler
  , searchHandler
  , auditHandler
  , healthHandler
  ) where

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)

import CicdHyperA.Ruleset (Effect(..))
import CicdHyperA.Registry (Registry, RegistryEntry(..), RuleVersion(..))
import qualified CicdHyperA.Registry as Reg
import CicdHyperA.Verify (VerifyResult(..), verifyRule)

-- ============================================================
-- API TYPE DEFINITION (Servant-style, implemented manually)
-- ============================================================

-- | Registry API endpoints
-- POST /deposit          - Submit verified ruleset
-- GET  /withdraw/:name   - Pull ruleset for local use
-- GET  /search           - Query rulesets
-- GET  /audit/:name      - View ruleset history
-- GET  /health           - Health check
-- GET  /metrics          - Prometheus metrics

data RegistryAPI = RegistryAPI

registryAPI :: RegistryAPI
registryAPI = RegistryAPI

-- ============================================================
-- REQUEST/RESPONSE TYPES
-- ============================================================

-- | Deposit request
data DepositRequest = DepositRequest
  { depositName        :: Text
  , depositDescription :: Text
  , depositContent     :: Text      -- Haskell source or compiled rules
  , depositEffect      :: Text      -- "preventive" | "curative" | "diagnostic"
  , depositSign        :: Bool      -- GPG sign the ruleset
  , depositVerify      :: Bool      -- Run formal verification
  } deriving (Show, Eq, Generic)

instance FromJSON DepositRequest
instance ToJSON DepositRequest

-- | Deposit response
data DepositResponse = DepositResponse
  { depositSuccess     :: Bool
  , depositVersion     :: Text
  , depositSignature   :: Maybe Text
  , depositVerified    :: Bool
  , depositErrors      :: [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON DepositResponse
instance ToJSON DepositResponse

-- | Withdraw request
data WithdrawRequest = WithdrawRequest
  { withdrawName       :: Text
  , withdrawVersion    :: Maybe Text  -- Nothing = latest
  , withdrawVerifySig  :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON WithdrawRequest
instance ToJSON WithdrawRequest

-- | Withdraw response
data WithdrawResponse = WithdrawResponse
  { withdrawSuccess    :: Bool
  , withdrawContent    :: Text
  , withdrawVersion'   :: Text
  , withdrawSignature' :: Maybe Text
  , withdrawVerified'  :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON WithdrawResponse
instance ToJSON WithdrawResponse

-- | Search request
data SearchRequest = SearchRequest
  { searchEffect       :: Maybe Text  -- Filter by effect type
  , searchLanguage     :: Maybe Text  -- Filter by target language
  , searchCategory     :: Maybe Text  -- Filter by category
  , searchQuery        :: Maybe Text  -- Free text search
  , searchLimit        :: Maybe Int   -- Max results
  , searchOffset       :: Maybe Int   -- Pagination offset
  } deriving (Show, Eq, Generic)

instance FromJSON SearchRequest
instance ToJSON SearchRequest

-- | Search response
data SearchResponse = SearchResponse
  { searchResults      :: [RulesetSummary]
  , searchTotal        :: Int
  , searchHasMore      :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON SearchResponse
instance ToJSON SearchResponse

-- | Ruleset summary for search results
data RulesetSummary = RulesetSummary
  { summaryName        :: Text
  , summaryDescription :: Text
  , summaryVersion     :: Text
  , summaryEffect      :: Text
  , summaryDownloads   :: Int
  , summaryVerified    :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON RulesetSummary
instance ToJSON RulesetSummary

-- | Audit response
data AuditResponse = AuditResponse
  { auditName          :: Text
  , auditHistory       :: [AuditEntry]
  , auditCurrentVersion :: Text
  , auditCreated       :: UTCTime
  , auditLastModified  :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON AuditResponse
instance ToJSON AuditResponse

-- | Audit entry
data AuditEntry = AuditEntry
  { auditVersion       :: Text
  , auditTimestamp     :: UTCTime
  , auditAction        :: Text       -- "created" | "updated" | "verified"
  , auditActor         :: Text
  , auditSignature     :: Maybe Text
  , auditChanges       :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON AuditEntry
instance ToJSON AuditEntry

-- | Health response
data HealthResponse = HealthResponse
  { healthStatus       :: Text       -- "healthy" | "degraded" | "unhealthy"
  , healthVersion      :: Text
  , healthUptime       :: Int        -- Seconds
  , healthChecks       :: [HealthCheck]
  } deriving (Show, Eq, Generic)

instance FromJSON HealthResponse
instance ToJSON HealthResponse

-- | Individual health check
data HealthCheck = HealthCheck
  { checkName          :: Text
  , checkStatus        :: Text       -- "pass" | "fail" | "warn"
  , checkMessage       :: Maybe Text
  , checkLatency       :: Maybe Int  -- Milliseconds
  } deriving (Show, Eq, Generic)

instance FromJSON HealthCheck
instance ToJSON HealthCheck

-- ============================================================
-- HANDLERS
-- ============================================================

-- | Handle deposit request
depositHandler :: Registry -> DepositRequest -> IO DepositResponse
depositHandler registry DepositRequest{..} = do
  -- Validate and verify
  let errors = validateDeposit depositName depositContent depositEffect

  if not (null errors)
    then return $ DepositResponse False "" Nothing False errors
    else do
      -- Register the rule
      newReg <- Reg.registerRule depositName depositDescription "manual" registry

      -- Generate signature if requested
      signature <- if depositSign
        then Just <$> signRuleset depositName depositContent
        else return Nothing

      return $ DepositResponse
        { depositSuccess = True
        , depositVersion = "1.0.0"
        , depositSignature = signature
        , depositVerified = depositVerify  -- TODO: Run Liquid Haskell
        , depositErrors = []
        }

-- | Handle withdraw request
withdrawHandler :: Registry -> WithdrawRequest -> IO WithdrawResponse
withdrawHandler registry WithdrawRequest{..} = do
  case Reg.lookupRule withdrawName registry of
    Nothing -> return $ WithdrawResponse False "" "" Nothing False
    Just entry -> do
      -- Get content from storage (stub)
      content <- getRulesetContent withdrawName

      -- Verify signature if requested
      verified <- if withdrawVerifySig
        then verifySig withdrawName content
        else return True

      return $ WithdrawResponse
        { withdrawSuccess = True
        , withdrawContent = content
        , withdrawVersion' = showVersion (entryVersion entry)
        , withdrawSignature' = Nothing  -- TODO: Retrieve signature
        , withdrawVerified' = verified
        }

-- | Handle search request
searchHandler :: Registry -> SearchRequest -> IO SearchResponse
searchHandler registry SearchRequest{..} = do
  let entries = Reg.listRules registry
      limit = maybe 20 id searchLimit
      offset = maybe 0 id searchOffset

      -- Filter entries
      filtered = filter (matchesSearch searchEffect searchLanguage searchQuery) entries
      total = length filtered
      paged = take limit $ drop offset filtered

      -- Convert to summaries
      summaries = map toSummary paged

  return $ SearchResponse
    { searchResults = summaries
    , searchTotal = total
    , searchHasMore = offset + limit < total
    }

-- | Handle audit request
auditHandler :: Registry -> Text -> IO (Maybe AuditResponse)
auditHandler registry name = do
  case Reg.lookupRule name registry of
    Nothing -> return Nothing
    Just entry -> do
      now <- getCurrentTime
      history <- Reg.getRuleHistory name registry

      return $ Just $ AuditResponse
        { auditName = name
        , auditHistory = map parseHistoryEntry history
        , auditCurrentVersion = showVersion (entryVersion entry)
        , auditCreated = entryCreated entry
        , auditLastModified = entryUpdated entry
        }

-- | Handle health check
healthHandler :: IO HealthResponse
healthHandler = do
  -- Check dependencies
  arangoCheck <- checkArangoDB
  dragonflyCheck <- checkDragonfly

  let checks = [arangoCheck, dragonflyCheck]
      allPass = all (\c -> checkStatus c == "pass") checks
      status = if allPass then "healthy" else "degraded"

  return $ HealthResponse
    { healthStatus = status
    , healthVersion = "0.1.0"
    , healthUptime = 0  -- TODO: Track uptime
    , healthChecks = checks
    }

-- ============================================================
-- HELPER FUNCTIONS
-- ============================================================

validateDeposit :: Text -> Text -> Text -> [Text]
validateDeposit name content effect =
  let nameErrors = if T.null name then ["Name is required"] else []
      contentErrors = if T.null content then ["Content is required"] else []
      effectErrors = if effect `notElem` ["preventive", "curative", "diagnostic"]
                     then ["Effect must be preventive, curative, or diagnostic"]
                     else []
  in nameErrors ++ contentErrors ++ effectErrors

signRuleset :: Text -> Text -> IO Text
signRuleset name content = do
  -- TODO: Implement GPG signing
  return $ "sig:" <> name <> ":stub"

getRulesetContent :: Text -> IO Text
getRulesetContent name = do
  -- TODO: Read from git-based storage
  return $ "-- Ruleset: " <> name

verifySig :: Text -> Text -> IO Bool
verifySig name content = do
  -- TODO: Implement GPG verification
  return True

matchesSearch :: Maybe Text -> Maybe Text -> Maybe Text -> RegistryEntry -> Bool
matchesSearch mEffect mLang mQuery entry =
  let nameMatch = case mQuery of
        Nothing -> True
        Just q -> T.isInfixOf (T.toLower q) (T.toLower $ entryName entry)
      sourceMatch = case mEffect of
        Nothing -> True
        Just e -> entrySource entry == e
  in nameMatch && sourceMatch

toSummary :: RegistryEntry -> RulesetSummary
toSummary entry = RulesetSummary
  { summaryName = entryName entry
  , summaryDescription = entryDescription entry
  , summaryVersion = showVersion (entryVersion entry)
  , summaryEffect = entrySource entry
  , summaryDownloads = 0  -- TODO: Track downloads
  , summaryVerified = True  -- TODO: Track verification status
  }

parseHistoryEntry :: Text -> AuditEntry
parseHistoryEntry line = AuditEntry
  { auditVersion = "1.0.0"
  , auditTimestamp = error "TODO: Parse timestamp"
  , auditAction = "updated"
  , auditActor = "system"
  , auditSignature = Nothing
  , auditChanges = line
  }

showVersion :: RuleVersion -> Text
showVersion (RuleVersion maj min pat) =
  T.pack $ show maj <> "." <> show min <> "." <> show pat

checkArangoDB :: IO HealthCheck
checkArangoDB = do
  -- TODO: Actual health check
  return $ HealthCheck "arangodb" "pass" Nothing (Just 5)

checkDragonfly :: IO HealthCheck
checkDragonfly = do
  -- TODO: Actual health check
  return $ HealthCheck "dragonfly" "pass" Nothing (Just 1)

-- ============================================================
-- SERVER
-- ============================================================

-- | Application placeholder (to be replaced with actual HTTP server)
app :: Registry -> IO ()
app registry = do
  putStrLn "cicd-hyper-a Registry API"
  putStrLn "Endpoints:"
  putStrLn "  POST /deposit"
  putStrLn "  GET  /withdraw/:name"
  putStrLn "  GET  /search"
  putStrLn "  GET  /audit/:name"
  putStrLn "  GET  /health"
  putStrLn "  GET  /metrics"

-- | Run the server
runServer :: Int -> Registry -> IO ()
runServer port registry = do
  putStrLn $ "Starting registry server on port " <> show port
  app registry
  -- TODO: Implement with warp
