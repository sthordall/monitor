{-# LANGUAGE DeriveGeneric #-}

module Icinga.Models.PassiveCheck
  ( PassiveCheck
  , mkPassiveCheck
  ) where

import Data.Aeson (FromJSON, ToJSON(..), genericToJSON)
import GHC.Generics
import Helpers

data PassiveCheck = PassiveCheck
  { displayName :: String
  , checkCommand :: String
  , enableActiveChecks :: Bool
  , maxCheckAttempts :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON PassiveCheck where
  toJSON = genericToJSON snakeOptions

instance FromJSON PassiveCheck

mkPassiveCheck :: String -> PassiveCheck
mkPassiveCheck disp = PassiveCheck
  { displayName = disp
  , checkCommand = "passive"
  , enableActiveChecks = False
  , maxCheckAttempts = 1
  }
