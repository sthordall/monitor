{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.MVar
import Control.Exception (SomeException, handle)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)
import EngineOptionsParser
import Monitor
import Network.Wai.Middleware.Cors (simpleCors)
-- import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import System.Exit (exitWith)
import Web.Scotty

main :: IO ()
main = do
  opts@EngineOptions {..} <- parseOptions
  print opts
  when optsMonitor $ handle onError $ do
    state <- startEngine opts
    scotty optsMonitorPort $ do
      -- middleware logStdoutDev
      middleware simpleCors
      middleware $ staticPolicy (noDots >-> addBase "static")
      get "/status" $ do
        (monitorReport, reports, _) <- liftIO $ readMVar state
        json $ monitorReport : reports
      get "/health" $ do
        (_, _, lastUpdated) <- liftIO $ readMVar state
        text $ pack $ show lastUpdated
      get "/ui" $ do
        setHeader "Content-Type" "text/html"
        file "static/index.html"
  unless optsMonitor $ do
    reports <- detectScripts optsPath >>= executeScripts opts
    forM_ reports $ putStrLn . formatReport
    exitWith $ reportsToExitCode reports
  where
    onError :: SomeException -> IO ()
    onError ex = putStrLn $ "Monitor failed with error: " ++ show ex

