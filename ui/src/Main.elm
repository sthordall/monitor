module Main exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Processors.Processor exposing (processor)
import Views.View exposing (startView)
import Html exposing (programWithFlags)
import Task
import Time exposing (Time, second)


main : Program Flags Model Msg
main =
    programWithFlags
        { init = initModel
        , view = startView
        , update = processor
        , subscriptions = subscriptions
        }


initModel : Flags -> ( Model, Cmd Msg )
initModel flags =
    let
        isConnected =
            flags.url |> String.isEmpty |> not

        model =
            { config = { server = flags.url }
            , isConnected = isConnected
            , view =
                if isConnected then
                    StatusView
                else
                    SetupView
            , errorMessage = Nothing
            , status = Status [] Nothing
            }
    in
        ( model, StatusMsg QueryStatusCmd |> Task.succeed |> Task.perform identity )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (3 * second) (\_ -> StatusMsg QueryStatusCmd)
