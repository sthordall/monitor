module Processors.StatusProcessor exposing (..)

import Common.HttpHelpers exposing (..)
import Date exposing (Date)
import Http exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Task exposing (map2)


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


process : Model -> StatusCmd -> ( Model, Cmd Msg )
process model cmd =
    case cmd of
        QueryStatusCmd ->
            let
                task =
                    map2 (\now ps -> ( now, ps )) Date.now (Http.toTask <| loadRecords model)
            in
                if not model.isConnected then
                    ( model, Cmd.none )
                else
                    ( { model | errorMessage = Nothing, view = StatusView }
                    , Task.attempt (\x -> StatusMsg (QueryStatusCompletedCmd x)) task
                    )

        QueryStatusCompletedCmd (Err err) ->
            ( { model | errorMessage = Just (showHttpError err) }, Cmd.none )

        QueryStatusCompletedCmd (Ok ( now, records )) ->
            ( { model | status = Status records ( Just now ) }, Cmd.none )

        -- QueryPositionsCompletedCmd (Ok ( now, positions )) ->
        --     let
        --         risk =
        --             model.risk
        --     in
        --         ( { model | risk = { risk | positions = positions, lastUpdated = Just now } }, Cmd.none )

        InputFilterCmd ccy storage ->
            ( model, Cmd.none )
            -- let
            --     risk =
            --         model.risk

            --     filter =
            --         model.risk.filter
            -- in
            --     ( { model | risk = { risk | filter = { filter | ccy = ccy, storage = storage } } }, Cmd.none )
