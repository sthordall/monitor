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
import qualified Data.Map as M
import Data.Time (getCurrentTime)
import Monitor.Models
import Monitor.Engine.Internal
import Monitor.Engine.Models
import Network.AMQP.Bus
import Network.AMQP.Connector
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import System.TimeIt (timeItT)

initState :: IO State
initState = do
  now <- getCurrentTime
  return ([], now)

tryReportToRabbit :: Maybe Connector -> [Report] -> IO ()
tryReportToRabbit Nothing _ = return ()
tryReportToRabbit _ [] = return ()
tryReportToRabbit (Just cntr) (r:rs) = do
  -- TODO: create Icinga data points and post/report
  void $ publish cntr M.empty r
  tryReportToRabbit (Just cntr) rs

process :: EngineOptions -> Maybe Connector -> MVar State -> IO ()
process opts@EngineOptions {..} cntr var = do
  putStr "Checking ... "
  hFlush stdout
  (duration, reports) <- timeItT (detectScripts optsPath >>= executeScripts)
  now <- getCurrentTime
  void $ swapMVar var (reports, now)
  tryReportToRabbit cntr reports
  putStrLn $ "done, took " ++ show duration ++ "sec"
  threadDelay $ optsDelayBetweenChecks * 10000000
  process opts cntr var

resolveConnectorInfo :: IO (Maybe ConnectorInfo)
resolveConnectorInfo = do
  menv <- lookupEnv "RABBITMQ_CONNECTOR_INFO"
  case menv of
    Nothing -> return Nothing
    Just env -> return $ deserialize env

connectionOpts :: ConnectionOpts
connectionOpts = defOpts {optsLogger = Just putStrLn, optsSpeedRefreshInterval = 10 * 1000000}

startConnector :: IO (Maybe Connector)
startConnector = do
  minfo <- resolveConnectorInfo
  case minfo of
    Nothing -> return Nothing
    Just info -> do
      cntr <- start connectionOpts info
      return $ Just cntr

startEngine :: EngineOptions -> IO (MVar State)
startEngine opts = do
  cntr <- startConnector
  state <- initState
  var <- newMVar state
  void $ forkIO $ process opts cntr var
  return var
