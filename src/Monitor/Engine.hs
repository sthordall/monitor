{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Monitor.Engine
  ( module Monitor.Engine.Models
  , startEngine
  , detectScripts
  , executeScripts
  , formatReport
  , reportsToExitCode
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.Time (getCurrentTime)
import Monitor.Engine.Internal
import Monitor.Engine.Models
import System.IO (hFlush, stdout)
import System.TimeIt (timeItT)

initState :: IO State
initState = do
  now <- getCurrentTime
  return ([], now)

process :: EngineOptions -> MVar State -> IO ()
process opts@EngineOptions {..} var = do
  putStr "Checking ... "
  hFlush stdout
  (duration, reports) <- timeItT (detectScripts optsPath >>= executeScripts)
  now <- getCurrentTime
  void $ swapMVar var (reports, now)
  putStrLn $ "done, took " ++ show duration ++ "sec"
  threadDelay $ optsDelayBetweenChecks * 10000000
  process opts var

startEngine :: EngineOptions -> IO (MVar State)
startEngine opts = do
  state <- initState
  var <- newMVar state
  void $ forkIO $ process opts var
  return var
