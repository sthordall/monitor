{-# LANGUAGE DeriveGeneric #-}

module Icinga.Models.ActiveCheck
  ( ActiveCheck
  , mkActiveCheck
  ) where

import Data.Aeson (FromJSON, ToJSON(..), genericToJSON)
import GHC.Generics
import Helpers

data Vars = Vars
  { dummyState :: String
  , dummyText :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Vars where
  toJSON = genericToJSON snakeOptions

instance FromJSON Vars

data ActiveCheck = ActiveCheck
  { displayName :: String
  , checkCommand :: String
  , enableActiveChecks :: Bool
  , maxCheckAttempts :: Int
  , checkInterval :: Int
  , retryInterval :: Int
  , vars :: Vars
  } deriving (Eq, Show, Generic)

instance ToJSON ActiveCheck where
  toJSON = genericToJSON snakeOptions

instance FromJSON ActiveCheck

mkActiveCheck :: String -> Int -> Int -> ActiveCheck
mkActiveCheck disp checkInt retryInt =
  ActiveCheck
  { displayName = disp
  , checkCommand = "passive"
  , enableActiveChecks = True
  , maxCheckAttempts = 1
  , checkInterval = checkInt
  , retryInterval = retryInt
  , vars = Vars "2" "timeout"
  }
