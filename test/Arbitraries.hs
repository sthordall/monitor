module Arbitraries where

import Data.Text (pack)
import Network.AMQP.Connector
import Network.Socket (PortNumber)
import Test.QuickCheck

clense :: String -> String
clense = clense' ""
  where
    clense' :: String -> String -> String
    clense' aux [] = aux
    clense' aux ('|':xs) = clense' aux xs
    clense' aux (':':xs) = clense' aux xs
    clense' aux (x:xs) = clense' (aux ++ [x]) xs

instance Arbitrary PortNumber where
  arbitrary = do
    Positive portNo <- arbitrary
    return portNo

instance Arbitrary ServerAddress where
  arbitrary = do
    address <- clense <$> arbitrary
    portNo <- arbitrary :: Gen (Maybe PortNumber)
    return $ ServerAddress address portNo

instance Arbitrary Credentials where
  arbitrary = do
    login <- clense <$> arbitrary
    password <- clense <$> arbitrary
    return $ Credentials (pack login) (pack password)

instance Arbitrary ConnectorInfo where
  arbitrary = do
    addresses <- arbitrary :: Gen [ServerAddress]
    vhost <- clense <$> arbitrary
    creds <- arbitrary
    return $ ConnectorInfo addresses (pack vhost) creds
