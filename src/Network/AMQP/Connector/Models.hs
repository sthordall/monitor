{-# LANGUAGE RecordWildCards #-}

module Network.AMQP.Connector.Models
  ( ServerAddress(..)
  , VirtualHost
  , Credentials(..)
  , ConnectorInfo(..)
  , ConnectionOpts(..)
  , defOpts
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

data ConnectionOpts = ConnectionOpts
  { optsRecoveryInterval :: Int
  , optsRetryInterval :: Int
  , optsSpeedRefreshInterval :: Int
  , optsLogger :: Maybe (String -> IO ())
  }

pow :: Int -> Int -> Int
pow x p = x ^ p

ms :: Int -> Int
ms n = n * (10 `pow` 3)

sec :: Int -> Int
sec n = n * (10 `pow` 6)

defOpts :: ConnectionOpts
defOpts = ConnectionOpts (sec 5) (ms 10) (sec 30) Nothing

type ConnectionSpeed = Double

type LastMeasured = Int

data ConnectionInfo = ConnectionInfo
  { infoPoint :: ServerAddress
  , infoLogger :: Maybe (String -> IO ())
  , infoConnection :: MVar (A.Connection, ServerAddress, ConnectionSpeed, LastMeasured)
  , infoClosedFlag :: MVar ()
  , infoShuttingDownFlag :: MVar ()
  }

newtype Connector = Connector
  { availableConnections :: [ConnectionInfo]
  }

newtype Connection =
  Connection A.Connection
