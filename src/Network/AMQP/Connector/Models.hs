{-# LANGUAGE RecordWildCards #-}

module Network.AMQP.Connector.Models
  ( ConnectionPoint(..)
  , VirtualHost
  , Credentials(..)
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

data ConnectionPoint = ConnectionPoint
  { pointHost :: String
  , pointPort :: Maybe PortNumber
  }

instance Show ConnectionPoint where
  show ConnectionPoint {..} = do
    let port =
          case pointPort of
            Nothing -> ""
            Just p -> ":" ++ show p
    pointHost ++ port

data Credentials = Credentials
  { credLogin :: Text
  , credPassword :: Text
  }

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
  { infoPoint :: ConnectionPoint
  , infoLogger :: Maybe (String -> IO ())
  , infoConnection :: MVar (A.Connection, ConnectionPoint, ConnectionSpeed, LastMeasured)
  , infoClosedFlag :: MVar ()
  , infoShuttingDownFlag :: MVar ()
  }

newtype Connector = Connector
  { availableConnections :: [ConnectionInfo]
  }

newtype Connection =
  Connection A.Connection
