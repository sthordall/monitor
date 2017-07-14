module Model exposing (..)

import Date exposing (Date)
import Json.Decode exposing (string, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Window exposing (Size)


type alias Flags =
    { url : String
    }


type View
    = SetupView
    | DashboardView


type alias Model =
    { config : Config
    , isConnected : Bool
    , view : View
    , errorMessage : Maybe String
    , windowSize : Size
    , data : Data
    }


type alias Config =
    { server : String
    }


type RecordsSortMode
    = ByPath
    | BySeverity


type alias RecordsFilter =
    { path : String
    , resultCode : String
    }


type alias Data =
    { records : List Record
    , lastUpdated : Maybe Date
    , sortBy : RecordsSortMode
    , filter : RecordsFilter
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


type alias ViewSettings =
    { hideFilters : Bool
    , hideNonFailingRecords : Bool
    }
