module Icinga.Models
  ( module X
  , ServiceCheck(..)
  , ServiceCheckInfo(..)
  ) where

import Data.Text
import Icinga.Models.ActiveCheck as X
import Icinga.Models.PassiveCheck as X
import Icinga.Models.CheckReport as X

data ServiceCheck
  = PassiveCheck X.PassiveCheck
  | ActiveCheck X.ActiveCheck

data ServiceCheckInfo = ServiceCheckInfo
  { hostName :: Text
  , serviceName :: Text
  , checkName :: Text
  }
