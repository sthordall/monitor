{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Monitor.Engine.Internal
  ( detectScripts
  , executeScripts
  , formatReport
  , reportsToExitCode
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.List (elem)
import Data.Maybe (fromMaybe)
import Monitor.Models
import Monitor.Engine.Models
import System.Exit (ExitCode(..))
import System.FilePath.Find ((~~?), always, fileName, find)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

mergeOutputs :: String -> String -> String
mergeOutputs "\n" "\n" = ""
mergeOutputs output "" = output
mergeOutputs output "\n" = output
mergeOutputs "" err = err
mergeOutputs "\n" err = err
mergeOutputs output err = output ++ "\nERROR(s):\n" ++ err

exitToResultCode :: ExitCode -> ResultCode
exitToResultCode ExitSuccess = OK
exitToResultCode (ExitFailure 2) = Warning
exitToResultCode (ExitFailure _) = Error

detectScripts :: FilePath -> IO [FilePath]
detectScripts = find always (fileName ~~? "*.sh")

runScript :: EngineOptions -> (FilePath, MVar Result) -> IO ()
runScript EngineOptions {..} (path, var) = do
  result <- timeout (optsCheckTimeout * 1000000) $ readProcessWithExitCode path [] ""
  let (rc, out, err) = fromMaybe (ExitFailure 1, "", "Check timed out") result
  putMVar var $ Result (exitToResultCode rc) $ mergeOutputs out err

startScript :: EngineOptions -> (FilePath, MVar Result) -> IO ()
startScript opts x = void $ forkIO $ runScript opts x

waitScriptFinish :: (FilePath, MVar Result) -> IO Report
waitScriptFinish (path, var) = do
  result <- takeMVar var
  return $ Report path result

executeScripts :: EngineOptions -> [FilePath] -> IO [Report]
executeScripts opts scripts = do
  xs <- mapM initEmptyMVar scripts
  mapM_ (startScript opts) xs
  mapM waitScriptFinish xs
  where
    initEmptyMVar :: FilePath -> IO (FilePath, MVar Result)
    initEmptyMVar path = do
      var <- newEmptyMVar :: IO (MVar Result)
      return (path, var)

formatReport :: Report -> String
formatReport Report {..} = do
  let Result {..} = result
  case resultCode of
    OK -> "[ OK] " ++ path
    Warning
      | null output -> "[WRN] " ++ path ++ "\n" ++ output
      | otherwise -> "[WRN] " ++ path
    Error
      | null output -> "[ERR] " ++ path ++ "\n" ++ output
      | otherwise -> "[ERR] " ++ path

reportsToExitCode :: [Report] -> ExitCode
reportsToExitCode reports = do
  let xs = map (\ Report {..} -> code result) reports
      hasErrors = Error `elem` xs
      hasWarnings = Warning `elem` xs
  case (hasErrors, hasWarnings) of
    (True, _) -> ExitFailure 1
    (False, True) -> ExitFailure 2
    (False, False) -> ExitSuccess
  where
    code :: Result -> ResultCode
    code Result {..} = resultCode
