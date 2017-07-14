module Server.API exposing (..)

import Http exposing (..)
import Model exposing (..)


loadRecords : Model -> Http.Request (List Record)
loadRecords model =
    let
        url =
            model.config.server ++ "status"

        headers =
            [ header "Accept" "application/json"
            ]
    in
        -- Http.get url positionListDecoder
        request
            { method = "GET"
            , headers = headers
            , url = url
            , body = emptyBody
            , expect = expectJson recordListDecoder
            , timeout = Nothing
            , withCredentials = False
            }
