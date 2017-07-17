{-# LANGUAGE ScopedTypeVariables #-}

module Network.AMQP.Bus.Internal where

import Control.Exception (catch)
import qualified Network.AMQP as A
import Network.AMQP.Connector.Internal
import Network.AMQP.Connector.Models

openChannel :: Connector -> IO (Maybe A.Channel)
openChannel cntr = do
  mcon <- getConnection_ cntr
  case mcon of
    Nothing -> return Nothing
    Just (Connection con) ->
      (Just <$> A.openChannel con) `catch` \(_ :: A.AMQPException) -> return Nothing
