{-# LANGUAGE RecordWildCards #-}

module Helpers
  ( snakeOptions
  , serialize
  , deserialize
  ) where

import qualified Data.Aeson.Types as AT
import Data.Aeson (defaultOptions)
import Data.Char (toLower, isUpper)
import Data.Text (unpack)
import Network.AMQP.Connector

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

serialize :: ConnectorInfo -> String
serialize ConnectorInfo {..} = do
  let Credentials {..} = cntrCredentials
  zipAddresses "" cntrAddresses
    ++ "|" ++ unpack cntrVirtualHost ++ "||"
    ++ unpack credLogin ++ ":" ++ unpack credPassword
  where
    zipAddresses :: String -> [ServerAddress] -> String
    zipAddresses aux [] = aux
    zipAddresses aux (ServerAddress {..}:xs) =
      zipAddresses (aux ++ serverHost ++ ":" ++ show serverPort ++ "|") xs

deserialize :: String -> Maybe ConnectorInfo
deserialize = undefined
