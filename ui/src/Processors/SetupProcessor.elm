module Processors.SetupProcessor exposing (..)

import Http
import Model exposing (..)
import Msg exposing (..)
import Processors.StatusListViewProcessor as StatusListViewProc


runWithAddress : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
runWithAddress model x =
    case model.config.server |> String.isEmpty of
        True ->
            ( { model | errorMessage = Just "Server address cannot be empty" }, Cmd.none )

        False ->
            x


process : Model -> SetupCmd -> ( Model, Cmd Msg )
process model cmd =
    case cmd of
        InputAddressCmd address ->
            ( { model | config = (Config address), errorMessage = Nothing }, Cmd.none )

        ConnectCmd _ ->
            runWithAddress model (StatusListViewProc.process { model | isConnected = True } QueryListViewStatusCmd)
