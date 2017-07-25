module Views.DashboardView exposing (..)

import Date
import Html exposing (..)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick, onInput)
import Maybe exposing (withDefault)
import Model exposing (..)
import Msg exposing (..)
import Regex as R
import Svg exposing (..)
import Svg.Attributes as SvgA


details : ViewSettings -> Model -> Html Msg
details settings model =
    let
        sortMode =
            model.data.sortBy

        filter =
            model.data.filter

        rs =
            if settings.hideNonFailingRecords then
                model.data.records |> List.filter (\r -> r.result.resultCode /= "OK")
            else
                model.data.records

        okStyle =
            HtmlA.style
                [ ( "width", "100px" )
                , ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "color", "green" )
                , ( "text-align", "center" )
                ]

        warningStyle =
            HtmlA.style
                [ ( "width", "100px" )
                , ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "color", "orange" )
                , ( "text-align", "center" )
                , ( "font-weight", "bold" )
                ]

        errorStyle =
            HtmlA.style
                [ ( "width", "100px" )
                , ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "color", "#a94442" )
                , ( "text-align", "center" )
                , ( "font-weight", "bold" )
                ]

        rcStyle resultCode =
            case resultCode of
                "OK" ->
                    okStyle

                "Warning" ->
                    warningStyle

                _ ->
                    errorStyle

        headerCellStyle =
            HtmlA.style
                [ ( "padding-right", "35px" )
                , ( "padding-top", "10px" )
                , ( "padding-bottom", "10px" )
                , ( "text-align", "center" )
                , ( "font-size", "1.2em" )
                ]

        cell120pxLStyle =
            HtmlA.style
                [ ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "text-align", "left" )
                , ( "width", "120px" )
                ]

        cellLStyle =
            HtmlA.style
                [ ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "text-align", "left" )
                ]

        cellFillLStyle =
            HtmlA.style
                [ ( "padding-right", "35px" )
                , ( "padding-bottom", "3px" )
                , ( "text-align", "left" )
                , ( "width", "80%" )
                ]

        filterStyle =
            HtmlA.style
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
                [ td [ rcStyle entry.result.resultCode ] [ Html.text entry.result.resultCode ]
                , td [ cellFillLStyle ] [ Html.text entry.path ]
                ]
            ]
                ++ (if entry.result.resultCode /= "OK" then
                        [ tr []
                            [ td [ HtmlA.colspan 2 ] [ pre [] [ Html.text entry.result.output ] ]
                            , td [] []
                            ]
                        ]
                    else
                        []
                   )

        resultCodeWeight : String -> Int
        resultCodeWeight resultCode =
            case resultCode of
                "OK" ->
                    2

                "Warning" ->
                    1

                _ ->
                    0

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
    in
        table [ HtmlA.style [ ( "width", "100%" ), ( "font-family", "'Fira Mono', monospace" ), ( "font-size", "1em" ), ( "margin-bottom", "10px" ), ( "margin-top", "20px" ) ] ]
            ([ tr []
                [ td [ cell120pxLStyle ]
                    (if settings.hideFilters then
                        []
                     else
                        [ input
                            [ HtmlA.id "result-code"
                            , HtmlA.class "form-control"
                            , onInput (\resultCode -> DashboardMsg (InputFilterCmd model.data.filter.path resultCode))
                            , HtmlA.spellcheck False
                            , filterStyle
                            ]
                            []
                        ]
                    )
                , td [ cellLStyle ]
                    (if settings.hideFilters then
                        []
                     else
                        [ input
                            [ HtmlA.id "path"
                            , HtmlA.class "form-control"
                            , onInput (\path -> DashboardMsg (InputFilterCmd path model.data.filter.resultCode))
                            , HtmlA.spellcheck False
                            , filterStyle
                            ]
                            []
                        ]
                    )
                ]
             ]
                ++ (filterRecords |> List.map mkRow |> List.concat)
            )


config : Model -> Html Msg
config model =
    let
        tableStyle =
            HtmlA.style
                [ ( "position", "fixed" )
                , ( "bottom", "0" )
                , ( "font-family", "'Fira Mono', monospace" )
                , ( "font-size", "1em" )
                , ( "margin", "0em 0em 1em 1em" )
                ]

        td1Style =
            HtmlA.style
                [ ( "padding", "3px 5px 3px 10px" )
                , ( "border-radius", "5px" )
                , ( "color", "white" )
                , ( "background-color", "rgba(0,0,0,.5)" )
                , ( "font-weight", "bold" )
                ]

        td2Style =
            HtmlA.style
                [ ( "padding", "3px 10px 3px 10px" )
                , ( "border-radius", "5px" )
                , ( "color", "rgb(223, 236, 23)" )
                , ( "background-color", "rgba(0,0,0,.5)" )
                ]
    in
        table [ tableStyle ]
            [ tr []
                [ td [ td1Style ] [ Html.text "Server:" ]
                , td [ td2Style ] [ Html.text model.config.server ]
                ]
            ]


recordBox : Model -> ( Float, Float ) -> ( ( Int, Int ), Record ) -> Svg Msg
recordBox model ( iw, ih ) ( ( j, i ), record ) =
    let
        margin =
            2

        ix =
            (toFloat i) * iw

        iy =
            (toFloat j) * ih

        fillColor =
            case model.errorMessage of
                Just _ -> "gray"
                Nothing ->
                    case record.result.resultCode of
                        "OK" ->
                            "#5cb85c"

                        "Warning" ->
                            "#f0ad4e"

                        _ ->
                            "#d9534f"

        pathParts =
            record.path
                |> String.split "/"

        label =
            pathParts
                |> List.drop 2
                |> List.head
                |> withDefault ""

        checkName =
            pathParts
                |> List.reverse
                |> List.head
                |> withDefault ""
                |> String.split "."
                |> List.head
                |> withDefault ""
                |> String.split "-"
                |> List.drop 1
    in
        g []
            [ rect
                [ SvgA.x (toString (ix + margin))
                , SvgA.y (toString (iy + margin))
                , SvgA.width (toString (iw - margin * 2))
                , SvgA.height (toString (ih - margin * 2))
                , SvgA.fill fillColor
                ]
                []
            , text_
                [ SvgA.x (toString (ix + iw / 2))
                , SvgA.y (toString (iy + ih / 3))
                , SvgA.textAnchor "middle"
                , SvgA.fontSize (toString (iw / 6))
                , SvgA.fill "aliceblue"
                , SvgA.textLength (toString (iw * 2 / 3))
                ]
                [ Svg.text label ]
            , text_
                [ SvgA.x (toString (ix + iw / 2))
                , SvgA.y (toString (iy + 4 * ih / 9))
                , SvgA.textAnchor "middle"
                , SvgA.fontSize (toString (iw / 12))
                , SvgA.fill "aliceblue"
                ]
                (checkName
                    |> List.map
                        (\name ->
                            tspan
                                ([ SvgA.x (toString (ix + iw / 2))
                                 , SvgA.dy "1.2em"
                                 , SvgA.fontFamily "Monospace"
                                 ]
                                )
                                [ Svg.text name ]
                        )
                )
            ]


recordsTable : Model -> ( Int, Int, Int, Int ) -> List Record -> List (Svg Msg)
recordsTable model ( tx, ty, tw, th ) records =
    let
        count =
            records |> List.length

        proportion =
            (toFloat th) / (toFloat tw)

        m =
            round (sqrt ((toFloat count) * proportion))

        n =
            ceiling ((toFloat count) / (toFloat m))

        indices =
            List.range 0 (count - 1)
                |> List.map (\i -> ( floor ((toFloat i) / (toFloat n)), i - n * floor ((toFloat i) / (toFloat n)) ))

        iw =
            (toFloat tw) / (toFloat n)

        ih =
            (toFloat th) / (toFloat m)
    in
        records |> List.map2 (,) indices |> List.map (recordBox model ( iw, ih ))


view : Model -> Html Msg
view model =
    let
        showDetails =
            model.data.records
                |> List.filter (\r -> r.result.resultCode /= "OK")
                |> List.isEmpty
                |> not

        size =
            model.windowSize

        areaHeight =
            size.height

        areaHeight812 =
            size.height

        areaWidth =
            size.width

        areaWidth812 =
            round (8 * (toFloat size.width) / 12)

        strViewBoxSize =
            "0 0 " ++ (toString areaWidth) ++ " " ++ (toString areaHeight)

        strViewBoxSize_8_12 =
            "0 0 " ++ (toString areaWidth812) ++ " " ++ (toString areaHeight812)

        settings =
            ViewSettings False False
    in
        div [ HtmlA.class "row" ]
            ((if showDetails then
                [ div [ HtmlA.class "col-sm-8" ]
                    [ svg [ SvgA.viewBox strViewBoxSize_8_12, SvgA.width "100%", SvgA.height "100%" ]
                        (model.data.records |> recordsTable model ( 0, 0, areaWidth812, areaHeight812 ))
                    ]
                , div [ HtmlA.class "col-sm-4" ]
                    [ details settings model
                    ]
                ]
              else
                [ div [ HtmlA.class "col-sm-12" ]
                    [ svg [ SvgA.viewBox strViewBoxSize, SvgA.width "100%", SvgA.height "100%" ]
                        (model.data.records |> recordsTable model ( 0, 0, areaWidth, areaHeight ))
                    ]
                ]
             )
                ++ [ config model ]
            )
