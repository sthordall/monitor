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
import Control.Exception (SomeException, handle)
import Control.Monad (void, when)
import Data.Time (getCurrentTime)
import Data.Time.Clock (diffUTCTime)
import Helpers
import Icinga
import Monitor.Engine.Internal
import Monitor.Engine.Models
import Monitor.Models
import Network.AMQP.Connector
import Prelude hiding (log)
import System.Environment (lookupEnv)
import System.TimeIt (timeItT)

startEngine :: EngineOptions -> IO (MVar State)
startEngine opts = do
  cntr <- startConnector
  state <- initState
  state' <- newMVar state
  let publish = publishReports cntr
  void $ forkIO $ process opts state' publish True
  void $ forkIO $ monitorProcess opts state' publish True
  return state'
  where
    initState :: IO State
    initState = do
      now <- getCurrentTime
      return (Report "" $ Result OK "", [], now)

monitorProcess :: EngineOptions -> MVar State -> Publish -> FirstRun -> IO ()
monitorProcess opts@EngineOptions {..} state publish isFirstRun = do
  log "Process monitor started ..."
  (_, _, lastUpdated) <- readMVar state
  report <- monitorReport lastUpdated
  modifyMVar_ state (\(_, rs, lu) -> pure (report, rs, lu))
  publish [report] isFirstRun
  threadDelay $ optsDelayBetweenChecks * 1000000
  monitorProcess opts state publish False
  where
    delayOutput = (++) "Process delay: " . show
    toNominalDiff = fromInteger . toInteger
    checkPath = "./checks/monitor/check-process-delay-OK.sh"
    monitorResult diff
      | diff >= toNominalDiff optsErrorDelay = Result Error $ delayOutput diff
      | diff >= toNominalDiff optsWarnDelay = Result Warning $ delayOutput diff
      | otherwise = Result OK $ delayOutput diff
    monitorReport lastUpdated =
      Report checkPath . monitorResult . diffUTCTime lastUpdated <$> getCurrentTime

process :: EngineOptions -> MVar State -> Publish -> FirstRun -> IO ()
process opts@EngineOptions {..} state publish isFirstRun = do
  log "Round started ... "
  handle onError $ do
    (duration, reports) <- timeItT (detectScripts optsPath >>= executeScripts opts)
    now <- getCurrentTime
    modifyMVar_ state (\(mr, _, _) -> pure (mr, reports, now))
    publish reports isFirstRun
    log $ "Round completed, took " ++ show duration ++ "sec"
  threadDelay $ optsDelayBetweenChecks * 1000000
  process opts state publish False
  where
    onError :: SomeException -> IO ()
    onError ex = log $ "Round completed with error: " ++ show ex

publishReports :: Maybe Connector -> [Report] -> FirstRun -> IO ()
publishReports mcntr reports isFirstRun =
  handle onError $
    case mcntr of
      Just cntr -> do
        when isFirstRun $ do
          log "Ensuring checks with Monitoring Service"
          postScriptReports cntr reports
        log "Reporting results to Monitoring Service"
        sendScriptReports cntr reports
      Nothing -> return ()
  where
    onError :: SomeException -> IO ()
    onError ex = log $ "Publish failed with error: " ++ show ex

startConnector :: IO (Maybe Connector)
startConnector = do
  minfo <- resolveConnectorInfo
  case minfo of
    Nothing -> do
      log "No RabbitMQ details detected ... "
      return Nothing
    Just info -> do
      log "RabbitMQ details detected!"
      let opts = defConnectionOpts {optsLogger = mkInfoLogger, optsSpeedRefreshInterval = 10 * 1000000}
      cntr <- start opts info
      return $ Just cntr

resolveConnectorInfo :: IO (Maybe ConnectorInfo)
resolveConnectorInfo = do
  menv <- lookupEnv "RABBITMQ_CONNECTOR_INFO"
  case menv of
    Nothing -> return Nothing
    Just env -> return $ deserialize env
