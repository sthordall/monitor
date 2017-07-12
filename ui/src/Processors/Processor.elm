module Processors.Processor exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Processors.StatusListViewProcessor as StatusListViewProc
import Processors.SetupProcessor as SetupProc


processor : Msg -> Model -> ( Model, Cmd Msg )
processor msg model =
    case msg of
        SetupMsg cmd ->
            SetupProc.process model cmd

        ListViewStatusMsg cmd ->
            StatusListViewProc.process model cmd
