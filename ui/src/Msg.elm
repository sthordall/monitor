module Msg exposing (..)

import Date exposing (Date)
import Http
import Model exposing (..)
import Time exposing (Time)


type Msg
    = SetupMsg SetupCmd
    | StatusMsg StatusCmd


type ErrorMessage
    = String


type SetupCmd
    = InputAddressCmd String
    | ConnectCmd


type StatusCmd
    = QueryStatusCmd
    | QueryStatusCompletedCmd (Result Http.Error ( Date, List Record ))
    | InputFilterCmd String String
