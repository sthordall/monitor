module Msg exposing (..)

import Date exposing (Date)
import Http
import Model exposing (..)
import Time exposing (Time)


type Msg
    = SetupMsg SetupCmd
    | ListViewStatusMsg ListViewStatusCmd
    | DashboardStatusMsg DashboardStatusCmd


type ErrorMessage
    = String


type ViewMode
    = ListView
    | Dashboard


type SetupCmd
    = InputAddressCmd String
    | ConnectCmd ViewMode


type DashboardStatusCmd
    = QueryDashboardStatusCmd
    | QueryDashboardStatusCompletedCmd (Result Http.Error ( Date, List Record ))
    | InputDashboardFilterCmd String String


type ListViewStatusCmd
    = QueryListViewStatusCmd
    | QueryListViewStatusCompletedCmd (Result Http.Error ( Date, List Record ))
    | InputListViewFilterCmd String String
