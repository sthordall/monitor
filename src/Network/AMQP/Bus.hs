{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AMQP.Bus
  ( publish
  ) where

import Control.Exception (catch)
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.Map as M
import Data.Text
import Data.IntSet
import Helpers
import Network.AMQP
import Network.AMQP.Types
import Network.AMQP.Connector
import Network.AMQP.Connector.Internal
import Network.AMQP.Connector.Models
import Prelude hiding (log)

mkMessage_ :: (ToJSON a) => a -> Message
mkMessage_ x =
  Message
    { msgBody = encode x
    , msgDeliveryMode = Nothing
    , msgTimestamp = Nothing
    , msgID = Nothing
    , msgType = Nothing
    , msgUserID = Nothing
    , msgApplicationID = Nothing
    , msgClusterID = Nothing
    , msgContentType = Nothing
    , msgContentEncoding = Nothing
    , msgReplyTo = Nothing
    , msgPriority = Nothing
    , msgCorrelationID = Nothing
    , msgExpiration = Nothing
    , msgHeaders = Nothing
    }

mkMessage :: (ToJSON a) => M.Map Text Text -> a -> Message
mkMessage headers x =
  let hs = FieldTable (M.map FVString headers)
  in (mkMessage_ x)
       { msgHeaders = Just hs
       , msgDeliveryMode = Just Persistent
       }

newChannel :: Connector -> IO (Maybe Channel)
newChannel cntr = do
  mcon <- getConnection_ cntr
  case mcon of
    Nothing -> return Nothing
    Just (Connection con) ->
      (Just <$> openChannel con) `catch` \(_ :: AMQPException) -> return Nothing

publish :: Connector -> [(M.Map Text Text, Value)] -> IO Bool
publish cntr msgs =
  publish' cntr msgs `catch` \(e :: AMQPException) -> do
    log $ "failed to publish: " ++ show e
    return False

publish' :: Connector -> [(M.Map Text Text, Value)] -> IO Bool
publish' cntr msgs = do
  mch <- newChannel cntr
  case mch of
    Nothing -> return False
    Just ch -> do
      confirmSelect ch False
      forM_ msgs (\(headers, msg) -> publishMsg ch "monitor" "" $ mkMessage headers msg)
      showResult =<< waitForConfirms ch
      closeChannel ch
      return True

showResult :: ConfirmationResult -> IO ()
showResult (Partial (a, n, p)) = log $ "Acks: "++(show . size $ a)++" Nacks: "++(show . size $ n)++" pending: "++(show . size $ p)
showResult (Complete (a, n)) = log $ "Acks: "++(show . size $ a)++" Nacks: "++(show . size $ n)
