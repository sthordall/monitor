module Processors.StatusDashboardProcessor exposing (..)

import Common.HttpHelpers exposing (..)
import Date exposing (Date)
import Http exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Task exposing (map2)
import Window


loadRecords : Model -> Http.Request (List Record)
loadRecords model =
    let
        url =
            model.config.server ++ "status"

        headers =
            [ header "Accept" "application/json"
            ]
    in
        -- Http.get url positionListDecoder
        request
            { method = "GET"
            , headers = headers
            , url = url
            , body = emptyBody
            , expect = expectJson recordListDecoder
            , timeout = Nothing
            , withCredentials = False
            }


process : Model -> DashboardStatusCmd -> ( Model, Cmd Msg )
process model cmd =
    case cmd of
        QueryDashboardStatusCmd ->
            let
                task =
                    map2 (\now ps -> ( now, ps )) Date.now (Http.toTask <| loadRecords model)
            in
                if not model.isConnected then
                    ( model, Cmd.none )
                else
                    ( { model | errorMessage = Nothing, view = StatusDashboard }
                    , Task.attempt (\x -> DashboardStatusMsg (QueryDashboardStatusCompletedCmd x)) task
                    )

        QueryDashboardStatusCompletedCmd (Err err) ->
            ( { model | errorMessage = Just (showHttpError err) }, Cmd.none )

        QueryDashboardStatusCompletedCmd (Ok ( now, records )) ->
            let
                status =
                    model.status
            in
                ( { model | status = { status | records = records, lastUpdated = Just now } }
                , Task.perform (\x -> DashboardStatusMsg (WindowResizesCmd x)) Window.size
                )

        InputDashboardFilterCmd path resultCode ->
            let
                status =
                    model.status

                filter =
                    model.status.filter
            in
                ( { model | status = { status | filter = { filter | path = path, resultCode = resultCode } } }, Cmd.none )

        WindowResizesCmd size ->
            ( { model | windowSize = size }, Cmd.none )
