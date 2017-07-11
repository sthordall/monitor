module Common.HttpHelpers exposing (..)

import Http

showHttpError : Http.Error -> String
showHttpError error =
  case error of
    Http.BadUrl x -> "bad url: " ++ x
    Http.Timeout -> "timeout"
    Http.NetworkError -> "network error"
    Http.BadStatus x -> "bad status: [" ++ toString x.status.code ++ "] " ++ x.status.message
    Http.BadPayload s x -> "bad payload: [" ++ toString x.status.code ++ "] " ++ x.status.message ++ "; " ++ s
