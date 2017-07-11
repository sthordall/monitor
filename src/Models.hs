{-# LANGUAGE DeriveGeneric #-}

module Models where

import qualified Data.Aeson.Types as AT
import Data.Aeson (FromJSON, ToJSON, defaultOptions, genericToJSON)
import Data.Char (toLower, isUpper)
import GHC.Generics

snakeIt :: String -> String
snakeIt = change ""
  where
    change :: String -> String -> String
    -- TODO: improve it to avoid repetitive "_" signs with abbreviations
    change aux [] = aux
    change aux (x:xs)
      | isUpper x = change (aux ++ "_" ++ [toLower x]) xs
      | otherwise = change (aux ++ [toLower x]) xs

snakeOptions :: AT.Options
snakeOptions = defaultOptions { AT.fieldLabelModifier = snakeIt }

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
