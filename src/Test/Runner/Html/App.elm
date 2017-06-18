module Test.Runner.Html.App
    exposing
        ( Model
        , Msg(..)
        , init
        , present
        , update
        )

import Expect exposing (Expectation)
import Random.Pcg as Random
import Task
import Test exposing (Test)
import Test.Runner as Runner
import Test.Runner.Html.View as View
import Test.Runner.Outcome as Outcome
import Time exposing (Time)


type Model
    = NotStarted (Maybe Random.Seed) Int Test
    | Started Time Time Outcome.Status


type Msg
    = Dispatch Time


dispatch : Cmd Msg
dispatch =
    Task.perform Dispatch Time.now


start : Int -> Test -> Random.Seed -> Outcome.Status
start runs test seed =
    Outcome.fromTest runs seed test
        |> Outcome.step


init : Int -> Maybe Random.Seed -> Test -> ( Model, Cmd Msg )
init runs maybeSeed test =
    ( NotStarted maybeSeed runs test, dispatch )


update : Msg -> Model -> ( Model, Cmd Msg )
update (Dispatch now) model =
    case model of
        NotStarted Nothing runs test ->
            ( floor now
                |> Random.initialSeed
                |> start runs test
                |> Started now now
            , dispatch
            )

        NotStarted (Just seed) runs test ->
            ( Started now now (start runs test seed)
            , dispatch
            )

        Started startTime _ (Outcome.Running { next }) ->
            ( Started startTime now (Outcome.step next)
            , dispatch
            )

        Started startTime _ status ->
            ( Started startTime now status
            , Cmd.none
            )


present : Model -> View.Model
present model =
    case model of
        NotStarted _ _ _ ->
            Nothing

        Started startTime now status ->
            Just ( now - startTime, status )
