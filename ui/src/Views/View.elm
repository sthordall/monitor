module Views.View exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Views.StatusView as Status
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
                StatusView ->
                    Status.view
    in
        div [ class "container" ]
            [ p [] []
            , view model
            ]