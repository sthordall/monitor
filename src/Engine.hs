{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Engine
  ( detectScripts
  , executeScripts
  , formatReport
  , reportsToExitCode
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM_, void)
import Data.List (elem)
import Data.Text.Lazy (pack)
import Models
import System.Exit (ExitCode(..))
import System.FilePath.Find ((~~?), always, fileName, find)
import System.Process (readProcessWithExitCode)

mergeOutputs :: String -> String -> String
mergeOutputs "\n" "\n" = ""
mergeOutputs output "" = output
mergeOutputs output "\n" = output
mergeOutputs "" error = error
mergeOutputs "\n" error = error
mergeOutputs output error = output ++ "\nERROR(s):\n" ++ error

exitToResultCode :: ExitCode -> ResultCode
exitToResultCode ExitSuccess = OK
exitToResultCode (ExitFailure 2) = Warning
exitToResultCode (ExitFailure _) = Error

detectScripts :: FilePath -> IO [FilePath]
detectScripts = find always (fileName ~~? "*.sh")

runScript :: (FilePath, MVar Result) -> IO ()
runScript (path, var) = do
  (rc, out, err) <- readProcessWithExitCode path [] ""
  putMVar var $ Result (exitToResultCode rc) $ mergeOutputs out err

startScript :: (FilePath, MVar Result) -> IO ()
startScript = void . forkIO . runScript

waitScriptFinish :: (FilePath, MVar Result) -> IO Report
waitScriptFinish (path, var) = do
  result <- takeMVar var
  return $ Report path result

executeScripts :: [FilePath] -> IO [Report]
executeScripts scripts = do
  xs <- mapM initEmptyMVar scripts
  mapM_ startScript xs
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
