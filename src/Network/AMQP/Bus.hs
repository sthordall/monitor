{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AMQP.Bus
  ( publish
  ) where

import Control.Exception (catch)
import Data.Aeson
import qualified Data.Map as M
import Data.Text
import Network.AMQP
import Network.AMQP.Types
import Network.AMQP.Connector
import Network.AMQP.Connector.Internal
import Network.AMQP.Connector.Models

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

publish :: ToJSON a => Connector -> M.Map Text Text -> a -> IO Bool
publish cntr headers msg = publish' cntr headers msg `catch` \(_ :: AMQPException) -> return False

publish' :: ToJSON a => Connector -> M.Map Text Text -> a -> IO Bool
publish' cntr headers msg = do
  mch <- newChannel cntr
  case mch of
    Nothing -> return False
    Just ch -> do
      let x = mkMessage headers msg
      mseq <- publishMsg ch "monitoring" "" x
      closeChannel ch
      case mseq of
        Nothing -> return False
        Just _ -> return True
