module Test.Runner.Html.View exposing (..)

import Time exposing (Time)


type alias FailGroup =
    ( List String, List { given : String, message : String } )


type Model
    = NotStarted
    | Running { completed : Int, remaining : Int, failures : List FailGroup }
    | Finished { duration : Time, passed : Int, failures : List FailGroup }
