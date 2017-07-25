{-# LANGUAGE RecordWildCards #-}

module Network.AMQP.Connector.Models
  ( ServerAddress(..)
  , VirtualHost
  , Credentials(..)
  , ConnectorInfo(..)
  , Logger(..)
  , ConnectionOpts(..)
  , ConnectionInfo(..)
  , ConnectionSpeed
  , LastMeasured
  , Connector(..)
  , Connection(..)
  ) where

import Control.Concurrent.MVar (MVar)
import Data.Text (Text)
import Network (PortNumber)
import qualified Network.AMQP as A

type VirtualHost = Text

data ServerAddress = ServerAddress
  { serverHost :: String
  , serverPort :: Maybe PortNumber
  }
  deriving (Eq)

instance Show ServerAddress where
  show ServerAddress {..} = do
    let port =
          case serverPort of
            Nothing -> ""
            Just p -> ":" ++ show p
    serverHost ++ port

data Credentials = Credentials
  { credLogin :: Text
  , credPassword :: Text
  }
  deriving (Eq, Show)

data ConnectorInfo = ConnectorInfo
  { cntrAddresses :: [ServerAddress]
  , cntrVirtualHost :: VirtualHost
  , cntrCredentials :: Credentials
  }
  deriving (Eq, Show)

data Logger = Logger
  { loggerTrace :: Maybe (String -> IO ())
  , loggerInfo :: Maybe (String -> IO ())
  , loggerWarn :: Maybe (String -> IO ())
  , loggerError :: Maybe (String -> IO ())
  }

data ConnectionOpts = ConnectionOpts
  { optsRecoveryInterval :: Int
  , optsRetryInterval :: Int
  , optsSpeedRefreshInterval :: Int
  , optsLogger :: Logger
  }

type ConnectionSpeed = Double

type LastMeasured = Int

data ConnectionInfo = ConnectionInfo
  { infoPoint :: ServerAddress
  , infoLogger :: Logger
  , infoConnection :: MVar (A.Connection, ServerAddress, ConnectionSpeed, LastMeasured)
  , infoClosedFlag :: MVar ()
  , infoShuttingDownFlag :: MVar ()
  }

newtype Connector = Connector
  { availableConnections :: [ConnectionInfo]
  }

newtype Connection =
  Connection A.Connection
