module Monitor.Engine.Models
  ( LastUpdated
  , State
  , EngineOptions(..)
  ) where

import Data.Time (UTCTime)
import Monitor.Models (Report)

type LastUpdated = UTCTime

type FirstRun = Bool

type State = ([Report], LastUpdated, FirstRun)

data EngineOptions = EngineOptions
  { optsMonitor :: Bool
  , optsMonitorPort :: Int
  , optsDelayBetweenChecks :: Int
  , optsCheckTimeout :: Int
  , optsPath :: FilePath
  }
  deriving (Show)
