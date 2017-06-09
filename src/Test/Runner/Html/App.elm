module Test.Runner.Html.App
    exposing
        ( Msg(..)
        , Model
        , init
        , update
        , present
        )

import Time exposing (Time)
import Task
import Tuple
import Random.Pcg as Random
import Test exposing (Test)
import Test.Runner as Runner
import Test.Runner.Html.View as View
import Expect exposing (Expectation)


type Msg
    = Start Time
    | Dispatch
    | Finish Time


type Model
    = NotStarted (Maybe Random.Seed) Int Test
    | Running (List Runner.Runner) State
    | Finished Time State


type alias State =
    { completed : List ( List String, List Expectation )
    , startTime : Time
    , finishTime : Maybe Time
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
        thunks =
            Runner.fromTest runs seed test |> extractRunners

        state =
            { completed = []
            , startTime = time
            , finishTime = Nothing
            }
    in
        ( Running thunks state, dispatch )


run : Runner.Runner -> State -> State
run runnable state =
    { state
        | completed = state.completed ++ [ ( runnable.labels, runnable.run () ) ]
    }


init : Maybe Int -> Maybe Random.Seed -> Test -> ( Model, Cmd Msg )
init maybeRuns maybeSeed test =
    let
        runs =
            Maybe.withDefault defaultRunCount maybeRuns
    in
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

        ( Finish time, Running _ state ) ->
            ( Finished time state, Cmd.none )

        ( Dispatch, Running [] state ) ->
            ( Running [] state, now Finish )

        ( Dispatch, Running (runner :: queue) state ) ->
            ( Running queue (run runner state), dispatch )

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

        Running queue state ->
            View.Running
                { completed = List.length state.completed
                , remaining = List.length (Debug.log "queue" queue)
                , failures = formatFailures state.completed
                }

        Finished finishTime state ->
            let
                failures =
                    formatFailures state.completed
            in
                View.Finished
                    { duration = finishTime - state.startTime
                    , passed = List.length state.completed - List.length failures
                    , failures = failures
                    }


formatFailures : List ( List String, List Expectation ) -> List View.FailGroup
formatFailures =
    List.filterMap <|
        \( labels, expectations ) ->
            case List.filterMap Runner.getFailure expectations of
                [] ->
                    Nothing

                failures ->
                    Just ( labels, failures )


extractRunners : Runner.SeededRunners -> List Runner.Runner
extractRunners seeded =
    case seeded of
        Runner.Plain runners ->
            Debug.log "runners" runners

        _ ->
            Debug.crash "TODO: implement other runners"


defaultRunCount : Int
defaultRunCount =
    100
