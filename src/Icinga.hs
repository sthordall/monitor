module Icinga
  ( post
  , report
  ) where

import Data.Aeson (encode)
import Icinga.Models

post :: ServiceCheckInfo -> ServiceCheck -> IO ()
post info check = do
  let reportOk =
        case check of
          PassiveCheck _ -> "true"
          ActiveCheck _ -> "false"
  let headers =
        [ ("request", "ensure-service")
        , ("hostname", hostName info)
        , ("servicename", serviceName info)
        , ("checkname", checkName info)
        , ("report-ok", reportOk)
        ]
  let body =
        case check of
          PassiveCheck ch -> encode ch
          ActiveCheck ch -> encode ch
  -- TODO: send message to RabbitMQ "monitoring" exchange
  undefined

report :: ServiceCheckInfo -> Report -> IO ()
report info rep = do
  let headers =
        [ ("request", "report")
        , ("hostname", hostName info)
        , ("checkname", checkName info)
        ]
  let body = encode rep
  -- TODO: send message to RabbitMQ "monitoring" exchange
  undefined
