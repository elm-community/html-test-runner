module Test.Runner.Html.View exposing (..)

import Time exposing (Time)
import Test.Runner.Outcome exposing (Outcome)


type Model
    = NotStarted
    | Running { completed : Int, remaining : Int, outcome : Outcome }
    | Finished { duration : Time, passed : Int, outcome : Outcome }
