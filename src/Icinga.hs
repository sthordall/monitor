{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga
  ( module Icinga.Models
  , postScriptReport
  , sendScriptReport
  ) where

import Control.Monad (void)
import qualified Data.Map as M
import Data.List (reverse)
import Data.Text (Text, pack, unpack)
import Icinga.Models
import Monitor.Models
import Network.AMQP.Bus (publish)
import Network.AMQP.Connector
import System.FilePath.Posix (splitPath, dropExtension)

scriptCategory :: FilePath -> Text
scriptCategory path = do
  let parts = splitPath path
  pack $ case parts of
    (_:_:x:_) -> take (length x - 1) x
    _ -> ""

scriptName :: FilePath -> Text
scriptName path = do
  let parts = reverse $ splitPath $ dropExtension path
  pack $ case parts of
    [] -> ""
    (x:_) -> x

toCheckInfo :: Report -> ServiceCheckInfo
toCheckInfo Report {..} = do
  let hostName = scriptCategory path
      serviceName = "monitor"
      checkName = scriptName path
  ServiceCheckInfo
    { hostName = hostName
    , serviceName = serviceName
    , checkName = checkName
    }

toCheck :: Report -> ServiceCheck
toCheck Report {..} = ActiveCheck $
  mkActiveCheck (unpack $ scriptName path) 2 1

toCheckReport :: Report -> CheckReport
toCheckReport Report {..} =
  let
    err = case resultCode result of
            OK -> 0
            Warning -> 1
            Error -> 2
    details = output result
  in CheckReport
       { exitStatus = err
       , pluginOutput = details
       }

postScriptReport :: Connector -> Report -> IO ()
postScriptReport cntr rep =
  let info = toCheckInfo rep
      check = toCheck rep
  in post cntr info check

post :: Connector -> ServiceCheckInfo -> ServiceCheck -> IO ()
post cntr info check = do
  let reportOk =
        case check of
          PassiveCheck _ -> "true"
          ActiveCheck _ -> "false"
  let headers = M.fromList
        [ ("request", "ensure-service")
        , ("hostname", hostName info)
        , ("servicename", serviceName info)
        , ("checkname", checkName info)
        , ("report-ok", reportOk)
        ]
  case check of
    PassiveCheck ch -> void $ publish cntr headers ch
    ActiveCheck ch -> void $ publish cntr headers ch

sendScriptReport :: Connector -> Report -> IO ()
sendScriptReport cntr rep =
  let info = toCheckInfo rep
  in send cntr info rep

send :: Connector -> ServiceCheckInfo -> Report -> IO ()
send cntr info rep = do
  let headers = M.fromList
        [ ("request", "report")
        , ("hostname", hostName info)
        , ("checkname", checkName info)
        ]
  void $ publish cntr headers $ toCheckReport rep
