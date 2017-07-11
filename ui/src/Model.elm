module Model exposing (..)

import Date exposing (Date)
import Json.Decode exposing (string, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Flags =
    { url : String
    }


type View
    = SetupView
    | StatusView


type alias Model =
    { config : Config
    , isConnected : Bool
    , view : View
    , errorMessage : Maybe String
    , status : Status
    }


type alias Config =
    { server : String
    }


type alias Status =
    { records : List Record
    , lastUpdated : Maybe Date
    }


type alias Record =
    { path : String
    , result : CheckResult
    }


type alias CheckResult =
    { output : String
    , resultCode : String
    }


checkResultDecoder : Decoder CheckResult
checkResultDecoder =
    decode CheckResult
        |> required "output" string
        |> required "result_code" string


recordDecoder : Decoder Record
recordDecoder =
    decode Record
        |> required "path" string
        |> required "result" checkResultDecoder


recordListDecoder : Decoder (List Record)
recordListDecoder =
    list recordDecoder
