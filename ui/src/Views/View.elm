module Views.View exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Views.DashboardView as Dashboard
import Views.SetupView as Setup
import Html exposing (..)
import Html.Attributes exposing (..)


startView : Model -> Html Msg
startView model =
    let
        ( container, view ) =
            case model.view of
                SetupView ->
                    ( "container", Setup.view )

                DashboardView ->
                    ( "container-fluid", Dashboard.view )
    in
        div [ class container ]
            [ view model
            ]
