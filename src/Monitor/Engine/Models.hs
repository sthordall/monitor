module Monitor.Engine.Models
  ( LastUpdated
  , State
  , Publish
  , FirstRun
  , EngineOptions(..)
  ) where

import Data.Time (UTCTime)
import Monitor.Models (Report)

type LastUpdated = UTCTime

type FirstRun = Bool

type Publish = [Report] -> FirstRun -> IO ()

type State = (Report, [Report], LastUpdated)

data EngineOptions = EngineOptions
  { optsMonitor :: Bool
  , optsMonitorPort :: Int
  , optsDelayBetweenChecks :: Int
  , optsCheckTimeout :: Int
  , optsPath :: FilePath
  , optsWarnDelay :: Int
  , optsErrorDelay :: Int
  }
  deriving (Show)
