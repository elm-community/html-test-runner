module Test.Runner.Html.View exposing (..)

import Time exposing (Time)
import Test.Runner.Outcome as Outcome


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
