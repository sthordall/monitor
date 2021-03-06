{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Icinga
  ( module Icinga.Models
  , postScriptReports
  , sendScriptReports
  ) where

import Control.Monad (void)
import qualified Data.Map as M
import Data.Aeson (toJSON)
import Data.List (reverse)
import Data.Text (Text, pack, unpack, append)
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
      category = scriptCategory path `append` "--"
      name = pack $ case parts of
               [] -> ""
               (x:_) -> x
  category `append` name

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
  mkActiveCheck (unpack $ scriptName path) 120 60

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

postScriptReports :: Connector -> [Report] -> IO Bool
postScriptReports cntr reps = publish cntr $ map prep reps
  where
    prep rep = do
      let info = toCheckInfo rep
          check = toCheck rep
          reportOk =
            case check of
              PassiveCheck _ -> "true"
              ActiveCheck _ -> "false"
          headers = M.fromList
            [ ("request", "ensure-service")
            , ("hostname", hostName info)
            , ("servicename", serviceName info)
            , ("checkname", checkName info)
            , ("report-ok", reportOk)
            ]
      case check of
        PassiveCheck ch -> (headers, toJSON ch)
        ActiveCheck ch -> (headers, toJSON ch)

sendScriptReports :: Connector -> [Report] -> IO Bool
sendScriptReports cntr reps = publish cntr $ map prep reps
  where
    prep rep = do
      let info = toCheckInfo rep
          headers = M.fromList
            [ ("request", "report")
            , ("hostname", hostName info)
            , ("checkname", checkName info)
            ]
      (headers, toJSON $ toCheckReport rep)
