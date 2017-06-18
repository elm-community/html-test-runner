module Test.Runner.Html.View exposing (..)

import Test.Runner.Outcome as Outcome
import Time exposing (Time)


type Model
    = NotStarted
    | Running
        { completed : Int
        , remaining : Int
        , status : Outcome.Status
        }
    | Finished
        { duration : Time
        , passed : Int
        , status : Outcome.Status
        }
