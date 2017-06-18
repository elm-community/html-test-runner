module Test.Runner.Html.App
    exposing
        ( Msg(..)
        , Model
        , init
        , update
        , present
        )

import Task
import Time exposing (Time)
import Random.Pcg as Random
import Expect exposing (Expectation)
import Test exposing (Test)
import Test.Runner as Runner
import Test.Runner.Html.View as View
import Test.Runner.Outcome as Outcome


type Msg
    = Start Time
    | Dispatch
    | Finish Time


type Model
    = NotStarted (Maybe Random.Seed) Int Test
    | Running State
    | Finished Time State


type alias State =
    { completed : Int
    , startTime : Time
    , outcome : Outcome.Outcome
    }


{-| Dispatch as a Cmd so as to yield to the UI
    thread in between test executions.
-}
dispatch : Cmd Msg
dispatch =
    Task.succeed Dispatch
        |> Task.perform identity


now : (Time -> a) -> Cmd a
now msg =
    Task.perform msg Time.now


start : Int -> Time -> Test -> Random.Seed -> ( Model, Cmd Msg )
start runs time test seed =
    let
        state =
            { completed = 0
            , startTime = time
            , outcome = Outcome.fromTest runs seed test
            }
    in
        ( Running state, dispatch )


init : Int -> Maybe Random.Seed -> Test -> ( Model, Cmd Msg )
init runs maybeSeed test =
    ( NotStarted maybeSeed runs test, now Start )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start time, NotStarted Nothing runs test ) ->
            floor time
                |> Random.initialSeed
                |> start runs time test

        ( Start time, NotStarted (Just seed) runs test ) ->
            start runs time test seed

        ( Finish time, Running state ) ->
            ( Finished time state, Cmd.none )

        ( Dispatch, Running state ) ->
            if Outcome.remaining state.outcome == 0 then
                ( model, now Finish )
            else
                ( Running
                    { state
                        | outcome = Outcome.step state.outcome
                        , completed = state.completed + 1
                    }
                , dispatch
                )

        ( Start _, _ ) ->
            Debug.crash "Attempted to start twice!"

        ( _, NotStarted _ _ _ ) ->
            Debug.crash "Attempted to run a Msg pre-Start!"

        ( Finish _, Finished _ _ ) ->
            Debug.crash "Attempted to Finish more than once!"

        ( Dispatch, Finished _ _ ) ->
            Debug.crash "Attempted to run tests after program finished!"


present : Model -> View.Model
present model =
    case model of
        NotStarted _ _ _ ->
            View.NotStarted

        Running state ->
            View.Running
                { completed = state.completed
                , remaining = Outcome.remaining state.outcome
                , status = Outcome.status state.outcome
                }

        Finished finishTime state ->
            View.Finished
                { duration = finishTime - state.startTime
                , passed = Outcome.passed state.outcome
                , status = Outcome.status state.outcome
                }
