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
import Control.Monad (void)
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
  publishSucceeded <- publish [report] isFirstRun
  threadDelay $ optsDelayBetweenChecks * 1000000
  monitorProcess opts state publish $ not publishSucceeded && False
  where
    delayOutput = (++) "Process delay: " . show
    toNominalDiff = fromInteger . toInteger
    checkPath = "./checks/monitor/check-is-monitor-active.sh"
    monitorResult diff
      | diff >= toNominalDiff optsErrorDelay = Result Error $ delayOutput diff
      | diff >= toNominalDiff optsWarnDelay = Result Warning $ delayOutput diff
      | otherwise = Result OK $ delayOutput diff
    monitorReport lastUpdated =
      Report checkPath . monitorResult . diffUTCTime lastUpdated <$> getCurrentTime

process :: EngineOptions -> MVar State -> Publish -> FirstRun -> IO ()
process opts@EngineOptions {..} state publish isFirstRun = do
  log "Round started ... "
  succeeded <- handle onError $ do
    (duration, reports) <- timeItT (detectScripts optsPath >>= executeScripts opts)
    now <- getCurrentTime
    modifyMVar_ state (\(mr, _, _) -> pure (mr, reports, now))
    publishSucceeded <- publish reports isFirstRun
    log $ "Round completed, took " ++ show duration ++ "sec"
    return publishSucceeded
  threadDelay $ optsDelayBetweenChecks * 1000000
  process opts state publish $ not succeeded && isFirstRun
  where
    onError :: SomeException -> IO Bool
    onError ex = do
      log $ "Round completed with error: " ++ show ex
      return False

publishReports :: Maybe Connector -> [Report] -> FirstRun -> IO Bool
publishReports mcntr reports isFirstRun =
  handle onError $
    case mcntr of
      Just cntr -> do
        firstRunSucceeded <- (||) (not isFirstRun) <$> do
          log "Ensuring checks with Monitoring Service"
          postScriptReports cntr reports
        return $ (&&) firstRunSucceeded <$> do
          log "Reporting results to Monitoring Service"
          sendScriptReports cntr reports
      Nothing -> return False
  where
    onError :: SomeException -> IO Bool
    onError ex = do
      log $ "Publish failed with error: " ++ show ex
      return False

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
