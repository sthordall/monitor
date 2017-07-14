module Msg exposing (..)

import Date exposing (Date)
import Http
import Model exposing (..)
import Time exposing (Time)
import Window exposing (Size)


type Msg
    = SetupMsg SetupCmd
    | DashboardMsg DashboardCmd


type ErrorMessage
    = String


type SetupCmd
    = InputAddressCmd String
    | ConnectCmd


type DashboardCmd
    = QueryDataCmd
    | QueryDataCompletedCmd (Result Http.Error ( Date, List Record ))
    | InputFilterCmd String String
    | WindowResizesCmd Size
