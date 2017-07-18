{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.AMQP.Connector
  ( start
  , stop
  , get
  , get_
  , ConnectionOpts(..)
  , defOpts
  , ServerAddress(..)
  , VirtualHost
  , Credentials(..)
  , ConnectorInfo(..)
  , ConnectionSpeed
  , Connection
  , Connector
  ) where

import Network.AMQP.Connector.Internal
import Network.AMQP.Connector.Models

start :: ConnectionOpts -> ConnectorInfo -> IO Connector
start = startConnector

stop :: Connector -> IO ()
stop = stopConnector

get :: Connector -> IO (Maybe (Connection, ServerAddress, ConnectionSpeed))
get = getConnection

get_ :: Connector -> IO (Maybe Connection)
get_ = getConnection_
