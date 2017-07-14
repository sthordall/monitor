module Processors.Processor exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Processors.SetupProcessor as SetupProc
import Processors.DashboardProcessor as DashboardProc


processor : Msg -> Model -> ( Model, Cmd Msg )
processor msg model =
    case msg of
        SetupMsg cmd ->
            SetupProc.process model cmd

        DashboardMsg cmd ->
            DashboardProc.process model cmd
