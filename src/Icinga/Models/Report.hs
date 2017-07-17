{-# LANGUAGE DeriveGeneric #-}

module Icinga.Models.Report
  ( Report
  ) where

import Data.Aeson (FromJSON, ToJSON(..), genericToJSON)
import GHC.Generics
import Helpers

data Report = Report
  { exitStatus :: Int
  , pluginOutput :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Report where
  toJSON = genericToJSON snakeOptions

instance FromJSON Report
