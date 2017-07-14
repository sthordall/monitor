module Main exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Processors.Processor exposing (processor)
import Views.View exposing (startView)
import Html exposing (programWithFlags)
import Task
import Time exposing (Time, second)
import Window


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
                    DashboardView
                else
                    SetupView
            , errorMessage = Nothing
            , data = initData
            , windowSize = Window.Size 800 600
            }
    in
        ( model, DashboardMsg QueryDataCmd |> Task.succeed |> Task.perform identity )


initData : Data
initData =
    { records = []
    , lastUpdated = Nothing
    , sortBy = BySeverity
    , filter =
        { path = ""
        , resultCode = ""
        }
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5 * second) (\_ -> DashboardMsg QueryDataCmd)
        , Window.resizes (\size -> DashboardMsg (WindowResizesCmd size))
        ]
