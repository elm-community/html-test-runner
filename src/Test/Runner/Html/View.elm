module Test.Runner.Html.View exposing (..)

import Test.Runner.Outcome as Outcome
import Time exposing (Time)


type alias Model =
    Maybe ( Time, Outcome.Status )
