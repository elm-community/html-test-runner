module Test.Runner.Html.View exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time)
import Expect exposing (Expectation)


type alias Model =
    { available : Dict Int (() -> ( List String, List Expectation ))
    , running : Set Int
    , queue : List Int
    , completed : List ( List String, List Expectation )
    , startTime : Time
    , finishTime : Maybe Time
    }
