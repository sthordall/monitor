module Views.StatusDashboard exposing (..)

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
        sortMode =
            model.status.sortBy

        filter =
            model.status.filter

        rs =
            model.status.records

        okStyle =
            style [ ( "width", "100px" ), ( "padding-right", "35px" ), ( "padding-bottom", "3px" ), ( "color", "green" ), ( "text-align", "center" ) ]

        warningStyle =
            style [ ( "width", "100px" ), ( "padding-right", "35px" ), ( "padding-bottom", "3px" ), ( "color", "orange" ), ( "text-align", "center" ), ( "font-weight", "bold" ) ]

        errorStyle =
            style [ ( "width", "100px" ), ( "padding-right", "35px" ), ( "padding-bottom", "3px" ), ( "color", "#a94442" ), ( "text-align", "center" ), ( "font-weight", "bold" ) ]

        rcStyle resultCode =
            case resultCode of
                "OK" ->
                    okStyle

                "Warning" ->
                    warningStyle

                _ ->
                    errorStyle

        headerCellStyle =
            style
                [ ( "padding-right", "35px" )
                , ( "padding-top", "10px" )
                , ( "padding-bottom", "10px" )
                , ( "text-align", "center" )
                , ( "font-size", "1.2em" )
                ]

        cell120pxLStyle =
            style
                [ ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "text-align", "left" )
                , ( "width", "120px" )
                ]

        cellLStyle =
            style
                [ ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "text-align", "left" )
                ]

        cellFillLStyle =
            style
                [ ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "text-align", "left" )
                , ( "width", "80%" )
                ]

        dateTimeStyle =
            style
                [ ( "padding-bottom", "1.3em" )
                , ( "text-align", "left" )
                , ( "font-family", "sans-serif" )
                , ( "color", "#286090" )
                ]

        filterStyle =
            style
                [ ( "font-family", "'Fira Mono', monospace" )
                , ( "margin-bottom", "1.2em" )
                , ( "text-align", "center" )
                , ( "border-style", "outset" )
                , ( "border-width", "0px 0px 1px 0px" )
                , ( "border-color", "#e8e8e8" )
                , ( "color", "#d9534f" )
                ]

        mkRow : Record -> List (Html Msg)
        mkRow entry =
            [ tr []
                [ td [ rcStyle entry.result.resultCode ] [ text entry.result.resultCode ]
                , td [ cellFillLStyle, colspan 2 ] [ text entry.path ]
                , td [] []
                ]
            ]
                ++ (if entry.result.resultCode /= "OK" then
                        [ tr []
                            [ td [ colspan 3 ] [ pre [ style [ ( "width", "90%" ) ] ] [ text entry.result.output ] ]
                            , td [] []
                            , td [] []
                            ]
                        ]
                    else
                        []
                   )

        resultCodeWeight : String -> Int
        resultCodeWeight resultCode =
            case resultCode of
                "OK" -> 2
                "Warning" -> 1
                _ -> 0

        orderedRecords =
            case sortMode of
                ByPath ->
                    rs |> List.sortBy .path

                BySeverity ->
                    rs |> List.sortBy (\r -> resultCodeWeight r.result.resultCode)

        matchesFilter : Record -> Bool
        matchesFilter r =
            let
                reduce : Char -> String -> String
                reduce ch line =
                    case ch of
                        '*' ->
                            ".+" ++ line

                        _ ->
                            String.fromChar ch ++ line

                prepRegex : String -> String
                prepRegex line =
                    line |> R.escape |> String.split "\\*" |> String.join "*" |> String.foldr reduce ""

                path =
                    filter.path |> prepRegex

                resultCode =
                    filter.resultCode |> prepRegex

                matchesPath =
                    r.path |> R.contains (R.regex path |> R.caseInsensitive)

                matchesResultCode =
                    r.result.resultCode |> R.contains (R.regex resultCode |> R.caseInsensitive)
            in
                matchesPath && matchesResultCode

        filterRecords =
            orderedRecords |> List.filter matchesFilter

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
        table [ style [ ( "width", "100%" ), ( "font-family", "'Fira Mono', monospace" ), ( "font-size", "1em" ), ( "margin-bottom", "10px" ), ( "margin-top", "20px" ) ] ]
            ([ tr []
                [ td [ cell120pxLStyle ]
                    [ input
                        [ id "result-code"
                        , class "form-control"
                        , onInput (\resultCode -> DashboardStatusMsg (InputDashboardFilterCmd model.status.filter.path resultCode))
                        , spellcheck False
                        , filterStyle
                        ]
                        []
                    ]
                , td [ cellLStyle ]
                    [ input
                        [ id "path"
                        , class "form-control"
                        , onInput (\path -> DashboardStatusMsg (InputDashboardFilterCmd path model.status.filter.resultCode))
                        , spellcheck False
                        , filterStyle
                        ]
                        []
                    ]
                , td [ dateTimeStyle ] [ text lastUpdated ]
                ]
             ]
                ++ (filterRecords |> List.map mkRow |> List.concat)
            )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Dashboard" ]
        , errorDetailsIfAny model
        , status model
        , div [ style [ ( "padding-bottom", "3em" ) ] ] []
        , config model
        ]
