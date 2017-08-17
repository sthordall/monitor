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
import Control.Monad (void, when)
import Data.Time (getCurrentTime)
import Icinga
import Monitor.Engine.Internal
import Monitor.Engine.Models
import Network.AMQP.Connector
import System.Environment (lookupEnv)
import System.TimeIt (timeItT)

startEngine :: EngineOptions -> IO (MVar State)
startEngine opts = do
  cntr <- startConnector
  state <- initState
  var <- newMVar state
  void $ forkIO $ process opts cntr var
  return var
  where
    initState :: IO State
    initState = do
      now <- getCurrentTime
      return ([], now, True)

process :: EngineOptions -> Maybe Connector -> MVar State -> IO ()
process opts@EngineOptions {..} mcntr var = do
  putStrLn "Round started ... "
  (duration, reports) <- timeItT (detectScripts optsPath >>= executeScripts opts)
  now <- getCurrentTime
  (_, _, isFirstRun) <- swapMVar var (reports, now, False)
  case mcntr of
    Just cntr -> do
      when isFirstRun $ do
        putStrLn "Ensuring checks with Monitoring Service"
        postScriptReports cntr reports
      putStrLn "Reporting results to Monitoring Service"
      sendScriptReports cntr reports
    Nothing -> return ()
  putStrLn $ "Round completed, took " ++ show duration ++ "sec"
  threadDelay $ optsDelayBetweenChecks * 1000000
  process opts mcntr var

startConnector :: IO (Maybe Connector)
startConnector = do
  minfo <- resolveConnectorInfo
  case minfo of
    Nothing -> do
      putStrLn "No RabbitMQ details detected ... "
      return Nothing
    Just info -> do
      putStrLn "RabbitMQ details detected!"
      let opts = defConnectionOpts {optsLogger = mkInfoLogger, optsSpeedRefreshInterval = 10 * 1000000}
      cntr <- start opts info
      return $ Just cntr

resolveConnectorInfo :: IO (Maybe ConnectorInfo)
resolveConnectorInfo = do
  menv <- lookupEnv "RABBITMQ_CONNECTOR_INFO"
  case menv of
    Nothing -> return Nothing
    Just env -> return $ deserialize env
