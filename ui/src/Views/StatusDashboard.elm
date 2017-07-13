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
            1

        ix =
            (toFloat i) * iw

        iy =
            (toFloat j) * ih
    in
        rect
            [ x (toString (ix + margin))
            , y (toString (iy + margin))
            , width (toString (iw - margin * 2))
            , height (toString (ih - margin * 2))
            , fill "#0B79CE"
            ]
            []


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
