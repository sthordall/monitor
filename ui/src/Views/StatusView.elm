module Views.StatusView exposing (..)

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Model exposing (..)
import Msg exposing (..)
import Regex as R


errorDetailsIfAny : Model -> Html Msg
errorDetailsIfAny model =
    case model.errorMessage of
        Just err ->
            div [ style [ ( "color", "red" ), ( "font-family", "'Fira Mono', monospace" ) ] ]
                [ text ("Failed to load status: " ++ err)
                ]

        Nothing ->
            div [] []


config : Model -> Html Msg
config model =
    table [ style [ ( "position", "fixed" ), ( "bottom", "0" ), ( "font-family", "'Fira Mono', monospace" ), ( "font-size", "1em" ), ( "margin", "0em 0em 1em -2em" ) ] ]
        [ tr []
            [ td [ style [ ( "padding", "3px 5px 3px 10px" ), ( "border-radius", "5px" ), ( "color", "white" ), ( "background-color", "#333" ), ( "font-weight", "bold" ) ] ] [ text "Server:" ]
            , td [ style [ ( "padding", "3px 10px 3px 10px" ), ( "border-radius", "5px" ), ( "color", "rgb(223, 236, 23)" ), ( "background-color", "#333" ) ] ] [ text model.config.server ]
            ]
        ]


status : Model -> Html Msg
status model =
    let
        ps =
            model.status.records

        headerCellStyle =
            style [ ( "padding-right", "35px" ), ( "padding-top", "10px" ), ( "padding-bottom", "10px" ), ( "text-align", "center" ), ( "font-size", "1.2em" ) ]

        cellCStyle =
            style [ ( "padding-right", "35px" ), ( "padding-bottom", "3px" ), ( "text-align", "center" ) ]

        dateTimeStyle =
            style [ ( "padding-right", "35px" ), ( "text-align", "right" ), ( "font-family", "sans-serif" ), ( "color", "#286090" ) ]

        valueCellStyle =
            style [ ( "padding-right", "35px" ), ( "padding-bottom", "3px" ), ( "color", "#286090" ), ( "text-align", "right" ), ( "font-weight", "lighter" ) ]

        resultValueCellStyle =
            style [ ( "padding-right", "35px" ), ( "padding-bottom", "3px" ), ( "color", "#a94442" ), ( "text-align", "right" ) ]

        filterStyle =
            style [ ( "font-family", "'Fira Mono', monospace" ), ( "width", "100px" ), ( "text-align", "center" ), ( "border-style", "outset" ), ( "border-width", "0px 0px 1px 0px" ), ( "border-color", "#e8e8e8" ), ( "color", "#d9534f" ) ]

        mkRow : Record -> Html Msg
        mkRow record =
            tr []
                [ td [] [ b [] [ text record.result.resultCode ] ]
                , td [] [ text record.path ]
                ]

        orderedRecords =
            ps |> List.sortBy .path

        padWithN : Int -> a -> String
        padWithN n =
            String.padLeft n '0' << toString

        lastUpdated =
            case model.status.lastUpdated of
                Nothing ->
                    ""

                Just x ->
                    padWithN 2 (Date.hour x)
                        ++ ":"
                        ++ padWithN 2 (Date.minute x)
                        ++ ":"
                        ++ padWithN 2 (Date.second x)
    in
        table [ style [ ( "font-family", "'Fira Mono', monospace" ), ( "font-size", "1em" ), ( "margin-bottom", "10px" ), ( "margin-top", "20px" ) ] ]
            ([ tr []
                [ th [] []
                , th [] []
                ]
             ]
                ++ (ps |> List.map mkRow)
            )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Status Records" ]
        , errorDetailsIfAny model
        , status model
        , div [ style [ ( "padding-bottom", "3em" ) ] ] []
        , config model
        ]
