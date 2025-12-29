{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | Git-based rule registry with versioning

module CicdHyperA.Registry
  ( -- * Registry types
    Registry(..)
  , RegistryEntry(..)
  , RuleVersion(..)
    -- * Registry operations
  , initRegistry
  , loadRegistry
  , saveRegistry
    -- * Rule management
  , registerRule
  , unregisterRule
  , lookupRule
  , listRules
    -- * Version management
  , getVersion
  , setVersion
  , getRuleHistory
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))

-- | Rule version
data RuleVersion = RuleVersion
  { vMajor :: Int
  , vMinor :: Int
  , vPatch :: Int
  } deriving (Show, Eq, Ord)

-- | Registry entry
data RegistryEntry = RegistryEntry
  { entryName        :: Text
  , entryVersion     :: RuleVersion
  , entryDescription :: Text
  , entryCreated     :: UTCTime
  , entryUpdated     :: UTCTime
  , entryEnabled     :: Bool
  , entrySource      :: Text  -- "learned" | "manual" | "imported"
  } deriving (Show, Eq)

-- | Rule registry
data Registry = Registry
  { regPath    :: FilePath
  , regEntries :: Map Text RegistryEntry
  , regVersion :: RuleVersion
  } deriving (Show)

-- ============================================================
-- Registry Operations
-- ============================================================

-- | Initialize a new registry
initRegistry :: FilePath -> IO Registry
initRegistry path = do
  createDirectoryIfMissing True path
  now <- getCurrentTime
  let reg = Registry
        { regPath = path
        , regEntries = Map.empty
        , regVersion = RuleVersion 1 0 0
        }
  saveRegistry reg
  return reg

-- | Load registry from disk
loadRegistry :: FilePath -> IO (Maybe Registry)
loadRegistry path = do
  let indexFile = path </> "index.rules"
  exists <- doesFileExist indexFile
  if exists
    then do
      content <- TIO.readFile indexFile
      return $ parseRegistry path content
    else return Nothing

-- | Save registry to disk
saveRegistry :: Registry -> IO ()
saveRegistry Registry{..} = do
  let indexFile = regPath </> "index.rules"
  createDirectoryIfMissing True regPath
  TIO.writeFile indexFile (serializeRegistry regEntries regVersion)

-- ============================================================
-- Rule Management
-- ============================================================

-- | Register a new rule
registerRule :: Text -> Text -> Text -> Registry -> IO Registry
registerRule name desc source reg = do
  now <- getCurrentTime
  let entry = RegistryEntry
        { entryName = name
        , entryVersion = RuleVersion 1 0 0
        , entryDescription = desc
        , entryCreated = now
        , entryUpdated = now
        , entryEnabled = True
        , entrySource = source
        }
      newEntries = Map.insert name entry (regEntries reg)
      newReg = reg { regEntries = newEntries }
  saveRegistry newReg
  return newReg

-- | Unregister a rule
unregisterRule :: Text -> Registry -> IO Registry
unregisterRule name reg = do
  let newEntries = Map.delete name (regEntries reg)
      newReg = reg { regEntries = newEntries }
  saveRegistry newReg
  return newReg

-- | Look up a rule by name
lookupRule :: Text -> Registry -> Maybe RegistryEntry
lookupRule name reg = Map.lookup name (regEntries reg)

-- | List all rules
listRules :: Registry -> [RegistryEntry]
listRules reg = Map.elems (regEntries reg)

-- ============================================================
-- Version Management
-- ============================================================

-- | Get current registry version
getVersion :: Registry -> RuleVersion
getVersion = regVersion

-- | Set registry version
setVersion :: RuleVersion -> Registry -> IO Registry
setVersion ver reg = do
  let newReg = reg { regVersion = ver }
  saveRegistry newReg
  return newReg

-- | Get rule change history (stub - needs git integration)
getRuleHistory :: Text -> Registry -> IO [Text]
getRuleHistory _ _ = return []  -- TODO: Implement git log parsing

-- ============================================================
-- Serialization
-- ============================================================

parseRegistry :: FilePath -> Text -> Maybe Registry
parseRegistry path content =
  let entries = parseEntries content
  in Just Registry
       { regPath = path
       , regEntries = entries
       , regVersion = RuleVersion 1 0 0
       }

parseEntries :: Text -> Map Text RegistryEntry
parseEntries _ = Map.empty  -- TODO: Implement proper parsing

serializeRegistry :: Map Text RegistryEntry -> RuleVersion -> Text
serializeRegistry entries ver = T.unlines $
  [ "# SPDX-License-Identifier: AGPL-3.0-or-later"
  , "# cicd-hyper-a Rule Registry"
  , "# Version: " <> showVersion ver
  , ""
  ] ++ map serializeEntry (Map.elems entries)

serializeEntry :: RegistryEntry -> Text
serializeEntry RegistryEntry{..} = T.unlines
  [ "rule " <> entryName
  , "  version: " <> showVersion entryVersion
  , "  description: " <> entryDescription
  , "  source: " <> entrySource
  , "  enabled: " <> if entryEnabled then "true" else "false"
  ]

showVersion :: RuleVersion -> Text
showVersion RuleVersion{..} =
  T.pack $ show vMajor <> "." <> show vMinor <> "." <> show vPatch
