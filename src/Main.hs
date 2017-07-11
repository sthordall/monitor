{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (pack)
import Data.Time (UTCTime, getCurrentTime)
import Engine
import Models
import Network.Wai.Middleware.RequestLogger
import Opts
import System.Exit (exitWith)
import System.IO (hFlush, stdout)
import System.TimeIt (timeItT)
import Web.Scotty

type LastUpdated = UTCTime

type State = ([Report], LastUpdated)

initState :: IO State
initState = do
  now <- getCurrentTime
  return ([], now)

process :: Opts -> MVar State -> IO ()
process opts@Opts {..} var = do
  putStr "Checking ... "
  hFlush stdout
  (duration, reports) <- timeItT (detectScripts optsPath >>= executeScripts)
  now <- getCurrentTime
  void $ swapMVar var (reports, now)
  putStrLn $ "done, took " ++ show duration ++ "sec"
  threadDelay $ optsDelayBetweenChecks * 10 ^ 6
  process opts var

startEngine :: Opts -> MVar State -> IO ()
startEngine opts var = void $ forkIO $ process opts var

main :: IO ()
main = do
  opts@Opts {..} <- parseOpts
  when optsMonitor $ do
    state <- initState
    var <- newMVar state
    startEngine opts var
    scotty optsMonitorPort $ do
      middleware logStdoutDev
      get "/status" $ do
        (reports, _) <- liftIO $ readMVar var
        json reports
      get "/health" $ do
        (_, lastUpdated) <- liftIO $ readMVar var
        text $ pack $ show lastUpdated
  unless optsMonitor $ do
    reports <- detectScripts optsPath >>= executeScripts
    forM_ reports $ putStrLn . formatReport
    exitWith $ reportsToExitCode reports
