{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)
import Monitor
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import EngineOptionsParser
import System.Exit (exitWith)
import Web.Scotty

main :: IO ()
main = do
  opts@EngineOptions {..} <- parseOptions
  when optsMonitor $ do
    var <- startEngine opts
    scotty optsMonitorPort $ do
      middleware logStdoutDev
      middleware simpleCors
      middleware $ staticPolicy (noDots >-> addBase "static")
      get "/status" $ do
        (reports, _) <- liftIO $ readMVar var
        json reports
      get "/health" $ do
        (_, lastUpdated) <- liftIO $ readMVar var
        text $ pack $ show lastUpdated
      get "/ui" $ do
        setHeader "Content-Type" "text/html"
        file "static/index.html"
  unless optsMonitor $ do
    reports <- detectScripts optsPath >>= executeScripts
    forM_ reports $ putStrLn . formatReport
    exitWith $ reportsToExitCode reports
