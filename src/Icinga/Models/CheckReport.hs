{-# LANGUAGE DeriveGeneric #-}

module Icinga.Models.CheckReport
  ( CheckReport(..)
  ) where

import Data.Aeson (FromJSON, ToJSON(..), genericToJSON)
import GHC.Generics
import Helpers

data CheckReport = CheckReport
  { exitStatus :: Int
  , pluginOutput :: String
  } deriving (Eq, Show, Generic)

instance ToJSON CheckReport where
  toJSON = genericToJSON snakeOptions

instance FromJSON CheckReport
