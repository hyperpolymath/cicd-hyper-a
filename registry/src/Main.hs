{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: AGPL-3.0-or-later
-- | cicd-hyper-a Rule Registry CLI

module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import CicdHyperA.Ruleset
import CicdHyperA.Verify
import CicdHyperA.Registry

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init", path] -> cmdInit path
    ["list", path] -> cmdList path
    ["verify", path] -> cmdVerify path
    ["show", path, name] -> cmdShow path name
    ["register", path, name, desc] -> cmdRegister path name desc
    ["help"] -> cmdHelp
    [] -> cmdHelp
    _ -> do
      TIO.putStrLn "Unknown command. Use 'help' for usage."
      exitFailure

cmdInit :: FilePath -> IO ()
cmdInit path = do
  _ <- initRegistry path
  TIO.putStrLn $ "Initialized registry at " <> T.pack path
  exitSuccess

cmdList :: FilePath -> IO ()
cmdList path = do
  mReg <- loadRegistry path
  case mReg of
    Nothing -> do
      TIO.putStrLn "Registry not found. Run 'init' first."
      exitFailure
    Just reg -> do
      let entries = listRules reg
      TIO.putStrLn $ "Rules (" <> T.pack (show $ length entries) <> "):"
      mapM_ printEntry entries
      exitSuccess

cmdVerify :: FilePath -> IO ()
cmdVerify path = do
  mReg <- loadRegistry path
  case mReg of
    Nothing -> do
      TIO.putStrLn "Registry not found."
      exitFailure
    Just _ -> do
      -- Verify pre-built rules
      let rules = [requireDependabot, requireSecurityMd, blockTypeScript, blockGolang]
      case verifyRuleset rules of
        Verified -> do
          TIO.putStrLn "All rules verified."
          exitSuccess
        VerifyFailed errs -> do
          TIO.putStrLn "Verification failed:"
          mapM_ (TIO.putStrLn . T.pack . show) errs
          exitFailure

cmdShow :: FilePath -> String -> IO ()
cmdShow path name = do
  mReg <- loadRegistry path
  case mReg of
    Nothing -> do
      TIO.putStrLn "Registry not found."
      exitFailure
    Just reg -> case lookupRule (T.pack name) reg of
      Nothing -> do
        TIO.putStrLn $ "Rule not found: " <> T.pack name
        exitFailure
      Just entry -> do
        printEntry entry
        exitSuccess

cmdRegister :: FilePath -> String -> String -> IO ()
cmdRegister path name desc = do
  mReg <- loadRegistry path
  case mReg of
    Nothing -> do
      TIO.putStrLn "Registry not found. Run 'init' first."
      exitFailure
    Just reg -> do
      _ <- registerRule (T.pack name) (T.pack desc) "manual" reg
      TIO.putStrLn $ "Registered rule: " <> T.pack name
      exitSuccess

cmdHelp :: IO ()
cmdHelp = TIO.putStrLn $ T.unlines
  [ "cicd-hyper-a Rule Registry"
  , ""
  , "Usage:"
  , "  registry init <path>              Initialize new registry"
  , "  registry list <path>              List all rules"
  , "  registry verify <path>            Verify rules"
  , "  registry show <path> <name>       Show rule details"
  , "  registry register <path> <name> <desc>  Register a rule"
  , "  registry help                     Show this help"
  , ""
  , "Pre-built rules:"
  , "  require-dependabot"
  , "  require-security-md"
  , "  block-typescript"
  , "  block-golang"
  , "  pin-github-actions"
  , "  require-workflow-permissions"
  , "  require-spdx-header"
  ]

printEntry :: RegistryEntry -> IO ()
printEntry entry = TIO.putStrLn $
  "  " <> entryName entry <> " (" <> entrySource entry <> ")"
