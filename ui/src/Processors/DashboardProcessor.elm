module Processors.DashboardProcessor exposing (..)

import Common.HttpHelpers exposing (..)
import Date exposing (Date)
import Http exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Server.API exposing (loadRecords)
import Task exposing (map2)
import Window


process : Model -> DashboardCmd -> ( Model, Cmd Msg )
process model cmd =
    case cmd of
        QueryDataCmd ->
            let
                task =
                    map2 (\now ps -> ( now, ps )) Date.now (Http.toTask <| loadRecords model)
            in
                if not model.isConnected then
                    ( model, Cmd.none )
                else
                    ( { model | errorMessage = Nothing, view = DashboardView }
                    , Task.attempt (\x -> DashboardMsg (QueryDataCompletedCmd x)) task
                    )

        QueryDataCompletedCmd (Err err) ->
            ( { model | errorMessage = Just (showHttpError err) }, Cmd.none )

        QueryDataCompletedCmd (Ok ( now, records )) ->
            let
                data =
                    model.data
            in
                ( { model | data = { data | records = records, lastUpdated = Just now } }
                , Task.perform (\x -> DashboardMsg (WindowResizesCmd x)) Window.size
                )

        InputFilterCmd path resultCode ->
            let
                data =
                    model.data

                filter =
                    model.data.filter
            in
                ( { model | data = { data | filter = { filter | path = path, resultCode = resultCode } } }, Cmd.none )

        WindowResizesCmd size ->
            ( { model | windowSize = size }, Cmd.none )
