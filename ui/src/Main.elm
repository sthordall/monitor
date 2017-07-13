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
                    StatusListView
                else
                    SetupView
            , errorMessage = Nothing
            , status = initStatus
            , windowSize = Window.Size 800 600
            }
    in
        ( model, ListViewStatusMsg QueryListViewStatusCmd |> Task.succeed |> Task.perform identity )


initStatus : Status
initStatus =
    { records = []
    , lastUpdated = Nothing
    , sortBy = BySeverity
    , filter =
        { path = ""
        , resultCode = ""
        }
    }


queryListViewSubscription : Sub Msg
queryListViewSubscription =
    Time.every (5 * second) (\_ -> ListViewStatusMsg QueryListViewStatusCmd)


queryDashboardSubscription : Sub Msg
queryDashboardSubscription =
    Time.every (5 * second) (\_ -> DashboardStatusMsg QueryDashboardStatusCmd)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (5 * second)
            (\_ ->
                case model.view of
                    StatusListView ->
                        ListViewStatusMsg QueryListViewStatusCmd

                    StatusDashboard ->
                        DashboardStatusMsg QueryDashboardStatusCmd

                    _ ->
                        DashboardStatusMsg QueryDashboardStatusCmd
            )
        , Window.resizes (\size -> DashboardStatusMsg (WindowResizesCmd size))
        ]
