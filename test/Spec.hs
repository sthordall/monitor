{-# LANGUAGE OverloadedStrings #-}

import Arbitraries()
import Network.AMQP.Connector
import Test.QuickCheck

prop_connector_info_doesnt_loose_data :: ConnectorInfo -> Bool
prop_connector_info_doesnt_loose_data info = deserialize (serialize info) == Just info

main :: IO ()
main = quickCheckWith stdArgs { maxSize = 1 } prop_connector_info_doesnt_loose_data

-- main :: IO ()
-- main = hspec $
--   describe "ConnectorInfo" $
--     it "does not loose data while serializing/deserializes" $
--       property $ \x -> deserialize (serialize x) == Just x
