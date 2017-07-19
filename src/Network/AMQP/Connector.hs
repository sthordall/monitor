{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Network.AMQP.Connector
  ( start
  , stop
  , get
  , get_
  , ConnectionOpts(..)
  , defOpts
  , ServerAddress(..)
  , VirtualHost
  , Credentials(..)
  , ConnectorInfo(..)
  , serialize
  , deserialize
  , ConnectionSpeed
  , Connection
  , Connector
  ) where

import Data.List (isPrefixOf)
import Data.Text (pack, unpack)
import Network.AMQP.Connector.Internal
import Network.AMQP.Connector.Models
import Network.Socket (PortNumber(..))

start :: ConnectionOpts -> ConnectorInfo -> IO Connector
start = startConnector

stop :: Connector -> IO ()
stop = stopConnector

get :: Connector -> IO (Maybe (Connection, ServerAddress, ConnectionSpeed))
get = getConnection

get_ :: Connector -> IO (Maybe Connection)
get_ = getConnection_

serialize :: ConnectorInfo -> String
serialize ConnectorInfo {..} = do
  let Credentials {..} = cntrCredentials
  drop 1 (zipAddresses "" cntrAddresses)
    ++ "||" ++ unpack cntrVirtualHost ++ "||"
    ++ unpack credLogin ++ ":" ++ unpack credPassword
  where
    zipAddresses :: String -> [ServerAddress] -> String
    zipAddresses aux [] = aux
    zipAddresses aux (ServerAddress {..}:xs) =
      let portNo =
            case serverPort of
              Nothing -> ""
              Just x -> show x
      in zipAddresses (aux ++ "|" ++ serverHost ++ ":" ++ portNo) xs

deserialize :: String -> Maybe ConnectorInfo
deserialize input = do
  (a, i) <- parseBefore "||" input
  as <- mapM (parseBefore ":") $ parseMany "|" a
  let addresses = map (toServerAddress . readAddress) as
  (vhost, c) <- parseBefore "||" i
  (login, password) <- parseBefore ":" c
  Just $ ConnectorInfo addresses (pack vhost) $ Credentials (pack login) (pack password)
  where
    toServerAddress :: (String, Maybe Int) -> ServerAddress
    toServerAddress (a, Nothing) = ServerAddress a Nothing
    toServerAddress (a, Just p) = ServerAddress a $ Just (fromIntegral p :: PortNumber)

readAddress :: (String, String) -> (String, Maybe Int)
readAddress (address, y) = (address, readInt y)

readInt :: String -> Maybe Int
readInt x =
  case reads x :: [(Int, String)] of
    [] -> Nothing
    ((result, _):_) -> Just result

parseMany :: String -> String -> [String]
parseMany = parseMany' []
  where
    parseMany' :: [String] -> String -> String -> [String]
    parseMany' aux _ [] = aux
    parseMany' aux delim input =
      case parseBefore delim input of
        Nothing -> aux ++ [input]
        Just (x, rest) -> parseMany' (aux ++ [x]) delim rest

parseBefore :: String -> String -> Maybe (String, String)
parseBefore = parseBefore' ""
  where
    parseBefore' :: String -> String -> String -> Maybe (String, String)
    parseBefore' aux delim input =
      if delim `isPrefixOf` input
        then Just (aux, drop (length delim) input)
        else case input of
               [] -> Nothing
               (x:xs) -> parseBefore' (aux ++ [x]) delim xs
