module Icinga.Models
  ( module X
  , ServiceCheck(..)
  , ServiceCheckInfo(..)
  ) where

import Icinga.Models.ActiveCheck as X
import Icinga.Models.PassiveCheck as X
import Icinga.Models.Report as X

data ServiceCheck
  = PassiveCheck X.PassiveCheck
  | ActiveCheck X.ActiveCheck

data ServiceCheckInfo = ServiceCheckInfo
  { hostName :: String
  , serviceName :: String
  , checkName :: String
  }
