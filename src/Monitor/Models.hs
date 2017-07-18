{-# LANGUAGE DeriveGeneric #-}

module Monitor.Models where

import Data.Aeson (FromJSON, ToJSON(..), genericToJSON)
import GHC.Generics
import Helpers (snakeOptions)

data ResultCode
  = OK
  | Warning
  | Error
  deriving (Eq, Show, Generic)

instance ToJSON ResultCode where
  toJSON = genericToJSON snakeOptions

instance FromJSON ResultCode

data Result = Result
  { resultCode :: ResultCode
  , output :: String
  }
  deriving (Show, Generic)

instance ToJSON Result where
  toJSON = genericToJSON snakeOptions

instance FromJSON Result

data Report = Report
  { path :: FilePath
  , result :: Result
  } deriving (Show, Generic)

instance ToJSON Report where
  toJSON = genericToJSON snakeOptions

instance FromJSON Report
