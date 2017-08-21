{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AMQP.Connector.Internal
  ( startConnector
  , stopConnector
  , getConnection
  , getConnection_
  , mkEmptyLogger
  , mkTraceLogger
  , logTrace
  , mkInfoLogger
  , logInfo
  , mkWarnLogger
  , logWarn
  , mkErrorLogger
  , logError
  , defConnectionOpts
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Monad (when, void)
import Data.Foldable (forM_)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, catMaybes)
import Helpers
import qualified Network.AMQP as A
import Network.AMQP.Connector.Models
import Prelude hiding (log)
import System.TimeIt (timeItT)

startConnector :: ConnectionOpts -> ConnectorInfo -> IO Connector
startConnector opts ConnectorInfo {..} = do
  cons <- mapM (\p -> initConnection opts p cntrVirtualHost cntrCredentials) cntrAddresses
  return $ Connector cons

stopConnector :: Connector -> IO ()
stopConnector Connector {..} = mapM_ releaseConnection availableConnections

getConnection :: Connector -> IO (Maybe (Connection, ServerAddress, ConnectionSpeed))
getConnection Connector {..} = do
  cons <- catMaybes <$> mapM (tryReadMVar . infoConnection) availableConnections
  let orderedConnections =
        sortBy (\(_, _, speedX, _) (_, _, speedY, _) -> compare speedX speedY) cons
  case orderedConnections of
    [] -> return Nothing
    ((con, point, speed, _):_) -> return $ Just (Connection con, point, speed)

getConnection_ :: Connector -> IO (Maybe Connection)
getConnection_ cntr = do
  mcon <- getConnection cntr
  return $ Just fst_ <*> mcon
  where
    fst_ :: (a, b, c) -> a
    fst_ (x, _, _) = x

ilogTrace :: ConnectionInfo -> String -> IO ()
ilogTrace ConnectionInfo {..} msg = logTrace infoLogger $ "TRACE: { " ++ show infoPoint ++ " } " ++ msg

ilogInfo :: ConnectionInfo -> String -> IO ()
ilogInfo ConnectionInfo {..} msg = logInfo infoLogger $ "INFO: { " ++ show infoPoint ++ " } " ++ msg

ilogWarn :: ConnectionInfo -> String -> IO ()
ilogWarn ConnectionInfo {..} msg = logWarn infoLogger $ "WARN: { " ++ show infoPoint ++ " } " ++ msg

initConnection :: ConnectionOpts -> ServerAddress -> VirtualHost -> Credentials -> IO ConnectionInfo
initConnection opts@ConnectionOpts {..} point vhost creds = do
  let mkInfo = ConnectionInfo point optsLogger
  info <- mkInfo <$> newEmptyMVar <*> newEmptyMVar <*> newEmptyMVar
  void $ forkIO $ upConnection info opts point vhost creds False
  return info

releaseConnection :: ConnectionInfo -> IO ()
releaseConnection info@ConnectionInfo {..} = do
  shutdown <- tryPutMVar infoShuttingDownFlag ()
  when shutdown $ do
    mcon <- tryTakeMVar infoConnection
    forM_ mcon $ \(con, _, _, _) -> do
      ilogInfo info "closing"
      result <- closeConnection con
      case result of
        Just _ -> ilogInfo info "disconnected"
        Nothing -> ilogWarn info "failed to close the connection"

whenRunning :: ConnectionInfo -> Maybe (IO ()) -> IO () -> IO ()
whenRunning ConnectionInfo {..} g f = do
  flag <- tryReadMVar infoShuttingDownFlag
  case (flag, g) of
    (Nothing, _) -> f
    (Just _, Just notf) -> notf
    (Just _, _) -> return ()

updateConnectionSpeed :: ConnectionInfo -> ConnectionOpts -> ServerAddress -> VirtualHost -> Credentials -> IO ()
updateConnectionSpeed info@ConnectionInfo {..} ConnectionOpts {..} point vhost creds = do
  (speed, t) <-
    modifyMVar
      infoConnection
      (\(c, p, s, t) -> return ((c, p, s, t + optsRetryInterval), (s, t + optsRetryInterval)))
  when (t >= optsSpeedRefreshInterval) $ do
    (newSpeed, mcon) <- timeItT (openConnection point vhost creds)
    case mcon of
      Just con -> do
        modifyMVar_ infoConnection (\(c, p, _, _) -> return (c, p, newSpeed, 0))
        void $ closeConnection con
        ilogTrace info $ "connection speed: " ++ show speed ++ " sec -> " ++ show newSpeed ++ " sec"
      _ -> return ()

upConnection :: ConnectionInfo -> ConnectionOpts -> ServerAddress -> VirtualHost -> Credentials -> Bool -> IO ()
upConnection info@ConnectionInfo {..} opts@ConnectionOpts {..} point vhost creds False =
  whenRunning info (Just $ ilogTrace info "stop connecting (closing)") $ do
    ilogTrace info "connecting"
    (t, mcon) <- timeItT (openConnection point vhost creds)
    case mcon of
      Nothing -> do
        threadDelay optsRecoveryInterval
        ilogWarn info $ "failed to connect in " ++ show t ++ " sec"
        upConnection info opts point vhost creds False
      Just con -> do
        A.addConnectionClosedHandler con True (putMVar infoClosedFlag ())
        putMVar infoConnection (con, infoPoint, t, 0)
        ilogInfo info $ "connected (" ++ show t ++ " sec)"
        upConnection info opts point vhost creds True
upConnection info@ConnectionInfo {..} opts@ConnectionOpts {..} point vhost creds True =
  whenRunning info Nothing $ do
    x <- tryReadMVar infoClosedFlag
    case x of
      Nothing -> do
        threadDelay optsRetryInterval
        updateConnectionSpeed info opts point vhost creds
        upConnection info opts point vhost creds True
      Just _ -> do
        void $ takeMVar infoClosedFlag
        (con, _, _, _) <- takeMVar infoConnection
        catch (A.closeConnection con) (\(_ :: A.AMQPException) -> return ())
        ilogInfo info "disconnected"
        upConnection info opts point vhost creds False

openConnection :: ServerAddress -> VirtualHost -> Credentials -> IO (Maybe A.Connection)
openConnection ServerAddress {..} vhost Credentials {..} = do
  let port = fromMaybe defaultPort serverPort
  catch
    (Just <$> A.openConnection' serverHost port vhost credLogin credPassword)
    (\(e :: A.AMQPException) -> do print e; return Nothing)
  where
    defaultPort = snd $ head $ A.coServers A.defaultConnectionOpts

closeConnection :: A.Connection -> IO (Maybe ())
closeConnection con =
  catch (A.closeConnection con >> return (Just ())) (\(_ :: A.AMQPException) -> return Nothing)

mkLineLogger:: Maybe (String -> IO ())
mkLineLogger = Just log

mkEmptyLogger :: Logger
mkEmptyLogger = Logger Nothing Nothing Nothing Nothing

mkTraceLogger :: Logger
mkTraceLogger = Logger mkLineLogger mkLineLogger mkLineLogger mkLineLogger

mkInfoLogger :: Logger
mkInfoLogger = Logger Nothing mkLineLogger mkLineLogger mkLineLogger

mkWarnLogger :: Logger
mkWarnLogger = Logger Nothing Nothing mkLineLogger mkLineLogger

mkErrorLogger :: Logger
mkErrorLogger = Logger Nothing Nothing Nothing mkLineLogger

logTrace :: Logger -> String -> IO ()
logTrace Logger {..} msg =
  case loggerTrace of
    Just logger -> logger msg
    Nothing -> return ()

logInfo :: Logger -> String -> IO ()
logInfo Logger {..} msg =
  case loggerInfo of
    Just logger -> logger msg
    Nothing -> return ()

logWarn :: Logger -> String -> IO ()
logWarn Logger {..} msg =
  case loggerWarn of
    Just logger -> logger msg
    Nothing -> return ()

logError :: Logger -> String -> IO ()
logError Logger {..} msg =
  case loggerError of
    Just logger -> logger msg
    Nothing -> return ()

pow :: Int -> Int -> Int
pow x p = x ^ p

ms :: Int -> Int
ms n = n * (10 `pow` 3)

sec :: Int -> Int
sec n = n * (10 `pow` 6)

defConnectionOpts :: ConnectionOpts
defConnectionOpts = ConnectionOpts (sec 5) (ms 10) (sec 30) mkEmptyLogger
