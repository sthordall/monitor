module Views.View exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Views.StatusListView as StatusListView
import Views.StatusDashboard as StatusDashboard
import Views.SetupView as Setup
import Html exposing (..)
import Html.Attributes exposing (..)


startView : Model -> Html Msg
startView model =
    let
        view =
            case model.view of
                SetupView ->
                    Setup.view

                StatusListView ->
                    StatusListView.view

                StatusDashboard ->
                    StatusDashboard.view
    in
        div [ class "container" ]
            [ p [] []
            , view model
            ]
