module Views.StatusDashboard exposing (..)

import Date
import Debug exposing (log)
import Html exposing (Html, div, text)
import Maybe exposing (withDefault)
import Model exposing (..)
import Msg exposing (..)
import Regex as R
import Svg exposing (..)
import Svg.Attributes exposing (..)


recordBox : ( Float, Float ) -> ( ( Int, Int ), Record ) -> Svg Msg
recordBox ( iw, ih ) ( ( j, i ), record ) =
    let
        margin =
            2

        ix =
            (toFloat i) * iw

        iy =
            (toFloat j) * ih

        fillColor =
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
                |> List.drop 1
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
                [ x (toString (ix + margin))
                , y (toString (iy + margin))
                , width (toString (iw - margin * 2))
                , height (toString (ih - margin * 2))
                , fill fillColor
                ]
                []
            , text_
                [ x (toString (ix + iw / 2))
                , y (toString (iy + ih / 3))
                , textAnchor "middle"
                , fontSize (toString (iw / 6))
                , fill "aliceblue"
                , textLength (toString (iw * 2 / 3))
                ]
                [ Svg.text label ]
            , text_
                [ x (toString (ix + iw / 2))
                , y (toString (iy + 4 * ih / 9))
                , textAnchor "middle"
                , fontSize (toString (iw / 12))
                , fill "aliceblue"
                ]
                (checkName
                    |> List.map
                        (\name ->
                            tspan
                                ([ x (toString (ix + iw / 2))
                                 , dy "1.2em"
                                 , fontFamily "Monospace"
                                 ]
                                )
                                [ Svg.text name ]
                        )
                )
            ]


recordsTable : ( Int, Int, Int, Int ) -> List Record -> List (Svg Msg)
recordsTable ( tx, ty, tw, th ) records =
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
        records |> List.map2 (,) indices |> List.map (recordBox ( iw, ih ))


view : Model -> Html Msg
view model =
    let
        size =
            model.windowSize

        strWidth =
            toString size.width

        strHeight =
            toString size.height

        strViewBoxSize =
            "0 0 " ++ strWidth ++ " " ++ strHeight
    in
        svg [ viewBox strViewBoxSize, width "100%", height "90%" ]
            (model.status.records |> recordsTable ( 0, 0, size.width, size.height ))
