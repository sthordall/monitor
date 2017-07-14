module Views.SetupView exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
import Maybe exposing (withDefault)
import Model exposing (..)
import Msg exposing (..)


addressField : Config -> Html Msg
addressField config =
    let
        inputStyle =
            style [ ( "font-family", "'Fira Mono', monospace" ), ( "border-style", "outset" ), ( "border-width", "0px 0px 1px 0px" ), ( "border-color", "#e8e8e8" ), ( "color", "#d9534f" ) ]
    in
        fieldset [ class "form-group" ]
            [ input
                [ id "address"
                , class "form-control"
                , onInput (\i -> SetupMsg (InputAddressCmd i))
                , placeholder "Specify server address to connect to"
                , spellcheck False
                , inputStyle
                ]
                []
            ]


serverExamples : List ( String, String ) -> Html Msg
serverExamples xs =
    let
        mkRow : ( String, String ) -> Html Msg
        mkRow ( key, value ) =
            tr []
                [ td [ style [ ( "padding", "3px" ) ] ] [ b [] [ text key ] ]
                , td [ style [ ( "padding", "3px" ), ( "color", "#286090" ) ] ] [ text value ]
                ]
    in
        table [ style [ ( "font-family", "'Fira Mono', monospace" ), ( "font-size", "1em" ), ( "margin-bottom", "10px" ) ] ]
            (xs |> List.map mkRow)


view : Model -> Html Msg
view model =
    let
        btnStyle =
            style [ ( "margin-right", "0.5em" ) ]
    in
        div []
            [ h2 [] [ text "Server details" ]
            , Html.form
                [ class "form"
                , onSubmit (SetupMsg ConnectCmd)
                ]
                [ serverExamples
                    [ ( "Local server", "http://127.0.0.1:3000/" )
                    ]
                , addressField model.config
                ]
            , div [ class "form-actions", style [ ( "float", "left" ) ] ]
                [ button [ btnStyle, class "btn btn-primary", onClick (SetupMsg ConnectCmd) ]
                    [ text "Connect ..."
                    ]
                ]
            , div [ class "error-message", style [ ( "float", "left" ), ( "margin", "5px 0 0 10px" ) ] ] [ text (withDefault "" model.errorMessage) ]
            ]
