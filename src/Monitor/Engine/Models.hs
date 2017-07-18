module Monitor.Engine.Models
  ( LastUpdated
  , State
  , EngineOptions(..)
  ) where

import Data.Time (UTCTime)
import Monitor.Models (Report)

type LastUpdated = UTCTime

type State = ([Report], LastUpdated)

data EngineOptions = EngineOptions
  { optsMonitor :: Bool
  , optsMonitorPort :: Int
  , optsDelayBetweenChecks :: Int
  , optsPath :: FilePath
  }

