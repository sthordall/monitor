module Processors.Processor exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Processors.StatusProcessor as StatusProc
import Processors.SetupProcessor as SetupProc


processor : Msg -> Model -> ( Model, Cmd Msg )
processor msg model =
    case msg of
        SetupMsg cmd ->
            SetupProc.process model cmd

        StatusMsg cmd ->
            StatusProc.process model cmd
