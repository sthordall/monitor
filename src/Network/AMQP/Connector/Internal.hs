{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AMQP.Connector.Internal
  ( startConnector
  , stopConnector
  , getConnection
  , getConnection_
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Monad (when, void)
import Data.Foldable (forM_)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Network.AMQP as A
import Network.AMQP.Connector.Models
import Prelude hiding (log)
import System.TimeIt (timeItT)

startConnector :: ConnectionOpts -> [ConnectionPoint] -> VirtualHost -> Credentials -> IO Connector
startConnector opts points vhost creds = do
  cons <- mapM (\p -> initConnection opts p vhost creds) points
  return $ Connector cons

stopConnector :: Connector -> IO ()
stopConnector Connector {..} = mapM_ releaseConnection availableConnections

getConnection :: Connector -> IO (Maybe (Connection, ConnectionPoint, ConnectionSpeed))
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

log :: ConnectionInfo -> String -> IO ()
log ConnectionInfo {..} line =
  case infoLogger of
    Just logger -> logger $ "{ " ++ show infoPoint ++ " } " ++ line
    Nothing -> return ()

initConnection :: ConnectionOpts -> ConnectionPoint -> VirtualHost -> Credentials -> IO ConnectionInfo
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
      log info "closing"
      result <- closeConnection con
      case result of
        Just _ -> log info "disconnected"
        Nothing -> log info "failed to close the connection"

whenRunning :: ConnectionInfo -> Maybe (IO ()) -> IO () -> IO ()
whenRunning ConnectionInfo {..} g f = do
  flag <- tryReadMVar infoShuttingDownFlag
  case (flag, g) of
    (Nothing, _) -> f
    (Just _, Just notf) -> notf
    (Just _, _) -> return ()

updateConnectionSpeed :: ConnectionInfo -> ConnectionOpts -> ConnectionPoint -> VirtualHost -> Credentials -> IO ()
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
        log info $ "connection speed: " ++ show speed ++ " sec -> " ++ show newSpeed ++ " sec"
      _ -> return ()

upConnection :: ConnectionInfo -> ConnectionOpts -> ConnectionPoint -> VirtualHost -> Credentials -> Bool -> IO ()
upConnection info@ConnectionInfo {..} opts@ConnectionOpts {..} point vhost creds False =
  whenRunning info (Just $ log info "stop connecting (closing)") $ do
    log info "connecting"
    (t, mcon) <- timeItT (openConnection point vhost creds)
    case mcon of
      Nothing -> do
        threadDelay optsRecoveryInterval
        log info $ "failed to connect in " ++ show t ++ " sec"
        upConnection info opts point vhost creds False
      Just con -> do
        A.addConnectionClosedHandler con True (putMVar infoClosedFlag ())
        putMVar infoConnection (con, infoPoint, t, 0)
        log info $ "connected (" ++ show t ++ " sec)"
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
        log info "disconnected"
        upConnection info opts point vhost creds False

openConnection :: ConnectionPoint -> VirtualHost -> Credentials -> IO (Maybe A.Connection)
openConnection ConnectionPoint {..} vhost Credentials {..} = do
  let port = fromMaybe defaultPort pointPort
  catch
    (Just <$> A.openConnection' pointHost port vhost credLogin credPassword)
    (\(_ :: A.AMQPException) -> return Nothing)
  where
    defaultPort = snd $ head $ A.coServers A.defaultConnectionOpts

closeConnection :: A.Connection -> IO (Maybe ())
closeConnection con =
  catch (A.closeConnection con >> return (Just ())) (\(_ :: A.AMQPException) -> return Nothing)
