{-# LANGUAGE OverloadedStrings #-}

import Arbitraries()
import Network.AMQP.Connector
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $
  describe "ConnectorInfo" $
    it "does not loose data while serializing/deserializing" $
      property $ \x -> deserialize (serialize x) == Just x
